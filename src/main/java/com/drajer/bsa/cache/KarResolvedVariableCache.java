package com.drajer.bsa.cache;

import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Application-local, in-memory cache for the STATIC PlanDefinition variables resolved for each
 * Knowledge Artifact. The cache is keyed by {@code KnowledgeArtifact.getVersionUniqueId()} (i.e.
 * {@code karId + "|" + karVersion}), so each KAR's variables are stored and retrieved completely
 * independently of every other KAR - there is no sharing or merging of values across KARs.
 *
 * <p>This is a plain in-memory cache backed by a {@link ConcurrentHashMap}. There is no Redis, no
 * database, and no other external/distributed cache involved.
 *
 * @author nbashyam
 */
@Component
public class KarResolvedVariableCache {

  private final Logger logger = LoggerFactory.getLogger(KarResolvedVariableCache.class);

  private final ConcurrentHashMap<String, ResolvedVariables> cache = new ConcurrentHashMap<>();

  private final AtomicBoolean initialized = new AtomicBoolean(false);

  private final AtomicLong hitCount = new AtomicLong(0);
  private final AtomicLong missCount = new AtomicLong(0);
  private final AtomicLong fallbackCount = new AtomicLong(0);

  /**
   * Stores the resolved STATIC variables for a single KAR. Replaces any previously cached entry for
   * the same karVersionUniqueId.
   */
  public void put(String karVersionUniqueId, ResolvedVariables variables) {

    if (karVersionUniqueId == null || variables == null) {
      logger.warn(" Ignoring attempt to cache null KAR key or null resolved variables ");
      return;
    }

    cache.put(karVersionUniqueId, variables);
  }

  /** Returns the resolved variables for the given KAR, if present, and tracks the hit/miss. */
  public Optional<ResolvedVariables> get(String karVersionUniqueId) {

    ResolvedVariables variables = cache.get(karVersionUniqueId);

    if (variables != null) {
      hitCount.incrementAndGet();
      return Optional.of(variables);
    }

    missCount.incrementAndGet();
    return Optional.empty();
  }

  public boolean contains(String karVersionUniqueId) {
    return cache.containsKey(karVersionUniqueId);
  }

  public void remove(String karVersionUniqueId) {
    cache.remove(karVersionUniqueId);
  }

  public void clear() {
    cache.clear();
  }

  /** Records that a static variable had to be resolved inline because of a cache miss. */
  public void recordFallback() {
    fallbackCount.incrementAndGet();
  }

  public long getHitCount() {
    return hitCount.get();
  }

  public long getMissCount() {
    return missCount.get();
  }

  public long getFallbackCount() {
    return fallbackCount.get();
  }

  public int size() {
    return cache.size();
  }

  /** Indicates whether the background resolution of all KARs has completed. */
  public boolean isInitialized() {
    return initialized.get();
  }

  /** Marks the cache as fully initialized once every known KAR has been processed. */
  public void markInitialized() {
    initialized.set(true);
    logger.info(" KAR resolved variable cache initialized with {} KAR(s) ", cache.size());
  }
}
