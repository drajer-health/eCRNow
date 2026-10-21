package com.drajer.bsa.kar.model;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * This class caches the resolved values of static PlanDefinition variables (for e.g
 * normalReportingDuration = 14) so that they are evaluated once per Knowledge Artifact instead of
 * once per condition evaluation. Variables whose value depends on the patient, encounter, request
 * or the current time are never cached.
 *
 * <p>Entries are keyed by the Knowledge Artifact version unique id, the variable name, language and
 * expression text. Since the expression text is part of the key, a changed expression in a reloaded
 * KAR can never be served a stale value, and two concurrent resolutions of the same key always
 * produce the same value.
 */
@Component
public class KarVariableCache {

  private final Logger logger = LoggerFactory.getLogger(KarVariableCache.class);

  private static final String FHIRPATH_LANGUAGE = "text/fhirpath";

  /** Variables that are resolved from the processing context and hence are always dynamic. */
  private static final Set<String> CONTEXT_VARIABLE_NAMES =
      Set.of(
          "encounterStartDate", "encounterEndDate", "lastReportSubmissionDate", "encounterClass");

  /**
   * Expressions that reference other variables / context data (%), context placeholders, or
   * functions that depend on the current time cannot be cached.
   */
  private static final Pattern DYNAMIC_EXPRESSION_PATTERN =
      Pattern.compile("%|\\{\\{|context\\.|\\b(now|today|timeOfDay)\\s*\\(");

  private final Map<CacheKey, CachedValue> cache = new ConcurrentHashMap<>();

  private final AtomicLong hits = new AtomicLong();
  private final AtomicLong misses = new AtomicLong();

  /**
   * Returns true when the variable value does not depend on the patient, encounter, request or
   * time, and hence can be safely shared across all evaluations of the Knowledge Artifact.
   */
  public static boolean isStaticVariable(Expression exp) {

    return exp != null
        && exp.hasName()
        && exp.hasExpression()
        && exp.hasLanguage()
        && exp.getLanguage().contentEquals(FHIRPATH_LANGUAGE)
        && !CONTEXT_VARIABLE_NAMES.contains(exp.getName())
        && !DYNAMIC_EXPRESSION_PATTERN.matcher(exp.getExpression()).find();
  }

  public static boolean isContextVariable(String name) {
    return CONTEXT_VARIABLE_NAMES.contains(name);
  }

  /**
   * Returns a copy of the cached value for the static variable, resolving and caching it on a miss.
   * Exceptions from the resolver are propagated and nothing is cached.
   *
   * @param karUniqueId The version unique id of the Knowledge Artifact.
   * @param exp The static variable expression.
   * @param resolver Resolves the value of the expression, may return null.
   * @return A copy of the resolved value or null.
   */
  public Type getOrResolve(String karUniqueId, Expression exp, Supplier<Type> resolver) {

    CacheKey key = new CacheKey(karUniqueId, exp);
    CachedValue cached = cache.get(key);

    if (cached != null) {
      hits.incrementAndGet();
    } else {
      misses.incrementAndGet();

      // Resolve outside of the map lock so that a slow evaluation does not block other keys.
      // A concurrent resolution of the same key produces the same value, so first one wins.
      CachedValue resolved = new CachedValue(resolver.get());
      CachedValue existing = cache.putIfAbsent(key, resolved);
      cached = (existing != null) ? existing : resolved;

      logger.debug(
          " Cached static variable {} for KAR {} with value {}",
          exp.getName(),
          karUniqueId,
          cached.value);
    }

    // FHIR types are mutable, never hand out the shared instance.
    return cached.value != null ? cached.value.copy() : null;
  }

  public boolean contains(String karUniqueId, Expression exp) {
    return cache.containsKey(new CacheKey(karUniqueId, exp));
  }

  /** Removes all the entries for Knowledge Artifacts that are no longer loaded. */
  public void retainKars(Collection<String> karUniqueIds) {
    cache.keySet().removeIf(key -> !karUniqueIds.contains(key.karUniqueId));
  }

  public void invalidateKar(String karUniqueId) {
    cache.keySet().removeIf(key -> key.karUniqueId.equals(karUniqueId));
  }

  public void invalidateAll() {
    cache.clear();
  }

  public int size() {
    return cache.size();
  }

  public long getHits() {
    return hits.get();
  }

  public long getMisses() {
    return misses.get();
  }

  private static final class CachedValue {

    private final Type value;

    private CachedValue(Type value) {
      this.value = value;
    }
  }

  private static final class CacheKey {

    private final String karUniqueId;
    private final String name;
    private final String language;
    private final String expression;

    private CacheKey(String karUniqueId, Expression exp) {
      this.karUniqueId = karUniqueId;
      this.name = exp.getName();
      this.language = exp.getLanguage();
      this.expression = exp.getExpression();
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof CacheKey)) return false;
      CacheKey other = (CacheKey) o;
      return karUniqueId.equals(other.karUniqueId)
          && Objects.equals(name, other.name)
          && Objects.equals(language, other.language)
          && Objects.equals(expression, other.expression);
    }

    @Override
    public int hashCode() {
      return Objects.hash(karUniqueId, name, language, expression);
    }
  }
}
