package com.drajer.bsa.kar.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.IntegerType;
import org.hl7.fhir.r4.model.Type;
import org.junit.Before;
import org.junit.Test;

public class KarVariableCacheTest {

  private static final String KAR_1 = "ersd-kar|1.0";
  private static final String KAR_2 = "ersd-kar|2.0";

  private KarVariableCache cache;

  @Before
  public void setUp() {
    cache = new KarVariableCache();
  }

  @Test
  public void isStaticVariable_WithConstantExpression_ReturnsTrue() {
    assertTrue(KarVariableCache.isStaticVariable(fhirPath("normalReportingDuration", "14")));
    assertTrue(KarVariableCache.isStaticVariable(fhirPath("threshold", "72 'hours'")));
  }

  @Test
  public void isStaticVariable_WithDynamicExpressions_ReturnsFalse() {
    assertFalse(KarVariableCache.isStaticVariable(fhirPath("ref", "%encounter.period.start")));
    assertFalse(
        KarVariableCache.isStaticVariable(fhirPath("pid", "'{{context.patientId}}'.length()")));
    assertFalse(KarVariableCache.isStaticVariable(fhirPath("ctx", "context.encounterClass")));
    assertFalse(KarVariableCache.isStaticVariable(fhirPath("cutoff", "now() - 72 hours")));
    assertFalse(KarVariableCache.isStaticVariable(fhirPath("day", "today()")));
    assertFalse(KarVariableCache.isStaticVariable(fhirPath("time", "timeOfDay ()")));
  }

  @Test
  public void isStaticVariable_WithContextVariableNames_ReturnsFalse() {
    for (String name :
        Arrays.asList(
            "encounterStartDate",
            "encounterEndDate",
            "lastReportSubmissionDate",
            "encounterClass")) {
      assertFalse(name, KarVariableCache.isStaticVariable(fhirPath(name, "'2025-01-01'")));
    }
  }

  @Test
  public void isStaticVariable_WithNonFhirPathOrIncompleteExpression_ReturnsFalse() {
    Expression cql = fhirPath("cql", "14");
    cql.setLanguage("text/cql");
    assertFalse(KarVariableCache.isStaticVariable(cql));

    Expression noName = new Expression().setLanguage("text/fhirpath").setExpression("14");
    assertFalse(KarVariableCache.isStaticVariable(noName));
    assertFalse(KarVariableCache.isStaticVariable(null));
  }

  @Test
  public void getOrResolve_CalledRepeatedly_ResolvesOnce() {
    Expression exp = fhirPath("normalReportingDuration", "14");
    AtomicInteger resolutions = new AtomicInteger();

    for (int i = 0; i < 5; i++) {
      Type value =
          cache.getOrResolve(
              KAR_1,
              exp,
              () -> {
                resolutions.incrementAndGet();
                return new IntegerType(14);
              });
      assertEquals(14, ((IntegerType) value).getValue().intValue());
    }

    assertEquals(1, resolutions.get());
    assertEquals(1, cache.getMisses());
    assertEquals(4, cache.getHits());
    assertEquals(1, cache.size());
  }

  @Test
  public void getOrResolve_ReturnsCopies_SoCallersCannotCorruptCache() {
    Expression exp = fhirPath("normalReportingDuration", "14");

    Type first = cache.getOrResolve(KAR_1, exp, () -> new IntegerType(14));
    ((IntegerType) first).setValue(99);
    Type second = cache.getOrResolve(KAR_1, exp, () -> new IntegerType(-1));

    assertNotSame(first, second);
    assertEquals(14, ((IntegerType) second).getValue().intValue());
  }

  @Test
  public void getOrResolve_WithNullValue_CachesAbsence() {
    Expression exp = fhirPath("missing", "{}");
    AtomicInteger resolutions = new AtomicInteger();

    assertNull(cache.getOrResolve(KAR_1, exp, () -> count(resolutions, null)));
    assertNull(cache.getOrResolve(KAR_1, exp, () -> count(resolutions, null)));

    assertEquals(1, resolutions.get());
  }

  @Test
  public void getOrResolve_WhenResolverFails_DoesNotCacheAndRetries() {
    Expression exp = fhirPath("normalReportingDuration", "14");

    assertThrows(
        IllegalStateException.class,
        () ->
            cache.getOrResolve(
                KAR_1,
                exp,
                () -> {
                  throw new IllegalStateException("evaluator unavailable");
                }));
    assertFalse(cache.contains(KAR_1, exp));

    Type value = cache.getOrResolve(KAR_1, exp, () -> new IntegerType(14));
    assertEquals(14, ((IntegerType) value).getValue().intValue());
  }

  @Test
  public void getOrResolve_ScopesByKarAndExpression() {
    Expression v1 = fhirPath("normalReportingDuration", "14");
    Expression v2 = fhirPath("normalReportingDuration", "21");

    cache.getOrResolve(KAR_1, v1, () -> new IntegerType(14));
    Type otherKar = cache.getOrResolve(KAR_2, v1, () -> new IntegerType(15));
    // A KAR reloaded with a changed expression must not get the old value.
    Type changedExpression = cache.getOrResolve(KAR_1, v2, () -> new IntegerType(21));

    assertEquals(15, ((IntegerType) otherKar).getValue().intValue());
    assertEquals(21, ((IntegerType) changedExpression).getValue().intValue());
    assertEquals(3, cache.size());
  }

  @Test
  public void invalidation_RemovesOnlyTargetedEntries() {
    Expression exp = fhirPath("normalReportingDuration", "14");
    cache.getOrResolve(KAR_1, exp, () -> new IntegerType(14));
    cache.getOrResolve(KAR_2, exp, () -> new IntegerType(14));

    cache.invalidateKar(KAR_1);
    assertFalse(cache.contains(KAR_1, exp));
    assertTrue(cache.contains(KAR_2, exp));

    cache.getOrResolve(KAR_1, exp, () -> new IntegerType(14));
    cache.retainKars(Arrays.asList(KAR_1));
    assertTrue(cache.contains(KAR_1, exp));
    assertFalse(cache.contains(KAR_2, exp));

    cache.invalidateAll();
    assertEquals(0, cache.size());
  }

  @Test
  public void getOrResolve_UnderConcurrentAccessAndInvalidation_IsConsistent() throws Exception {
    Expression exp = fhirPath("normalReportingDuration", "14");
    int threads = 16;
    ExecutorService pool = Executors.newFixedThreadPool(threads);
    CountDownLatch start = new CountDownLatch(1);
    AtomicInteger resolutions = new AtomicInteger();

    try {
      List<Future<Integer>> results = new ArrayList<>();
      for (int t = 0; t < threads; t++) {
        final boolean invalidator = t == 0;
        results.add(
            pool.submit(
                () -> {
                  start.await();
                  int last = 0;
                  for (int i = 0; i < 500; i++) {
                    if (invalidator && i % 50 == 0) {
                      cache.invalidateAll();
                    }
                    Type v =
                        cache.getOrResolve(
                            KAR_1, exp, () -> count(resolutions, new IntegerType(14)));
                    last = ((IntegerType) v).getValue();
                  }
                  return last;
                }));
      }

      start.countDown();
      for (Future<Integer> result : results) {
        assertEquals(14, result.get(30, TimeUnit.SECONDS).intValue());
      }
    } finally {
      pool.shutdownNow();
    }

    // Every call either hit or resolved, and resolutions are bounded by the misses.
    assertEquals(threads * 500L, cache.getHits() + cache.getMisses());
    assertEquals(cache.getMisses(), resolutions.get());
    assertTrue(resolutions.get() < threads * 500);
  }

  private static Type count(AtomicInteger counter, Type value) {
    counter.incrementAndGet();
    return value;
  }

  private static Expression fhirPath(String name, String expression) {
    return new Expression().setName(name).setLanguage("text/fhirpath").setExpression(expression);
  }
}
