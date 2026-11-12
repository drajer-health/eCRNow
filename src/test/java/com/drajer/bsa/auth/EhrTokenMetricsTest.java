package com.drajer.bsa.auth;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.bsa.auth.EhrTokenMetrics.Outcome;
import com.drajer.bsa.auth.EhrTokenMetrics.Timing;
import com.drajer.bsa.model.HealthcareSetting;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * EHR token metrics, and an evaluation of the in-memory cache under concurrency: run this class to
 * see the summary the application logs when ecr.ehr.token.metrics.enabled=true.
 */
public class EhrTokenMetricsTest {

  private static final int THREADS = 10;
  private static final long ENDPOINT_MILLIS = 300;

  private static final class MutableClock extends Clock {
    private volatile Instant now = Instant.parse("2026-01-01T00:00:00Z");

    void advanceSeconds(long seconds) {
      now = now.plusSeconds(seconds);
    }

    @Override
    public ZoneId getZone() {
      return ZoneOffset.UTC;
    }

    @Override
    public Clock withZone(ZoneId zone) {
      return this;
    }

    @Override
    public Instant instant() {
      return now;
    }
  }

  private AuthorizationUtils authUtils;
  private MutableClock clock;
  private ExecutorService pool;

  @Before
  public void setUp() {
    authUtils = mock(AuthorizationUtils.class);
    clock = new MutableClock();
    pool = Executors.newFixedThreadPool(THREADS);
  }

  @After
  public void tearDown() {
    pool.shutdownNow();
  }

  private static HealthcareSetting hs() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setFhirServerBaseURL("https://ehr.example.com/fhir");
    hs.setClientId("client-1");
    return hs;
  }

  private void slowTokenEndpoint() {
    when(authUtils.getToken(any()))
        .thenAnswer(
            inv -> {
              Thread.sleep(ENDPOINT_MILLIS);
              return new JSONObject().put("access_token", "t").put("expires_in", 3600);
            });
  }

  /** Starts THREADS token requests at the same moment and waits for all of them. */
  private void runConcurrently(EhrAccessTokenCache cache) throws Exception {
    CountDownLatch start = new CountDownLatch(1);
    List<Future<?>> futures = new ArrayList<>();
    for (int i = 0; i < THREADS; i++) {
      futures.add(
          pool.submit(
              () -> {
                start.await();
                return cache.getToken(hs());
              }));
    }
    start.countDown();
    for (Future<?> f : futures) {
      f.get(30, TimeUnit.SECONDS);
    }
  }

  @Test
  public void disabled_recordsNothing() {
    EhrTokenMetrics metrics = EhrTokenMetrics.disabled();
    EhrTokenMetrics.Sample sample = metrics.begin();
    sample.outcome(Outcome.MEMORY_REQUESTED);
    sample.finish();

    assertFalse(metrics.isEnabled());
    assertEquals(0, metrics.count(Outcome.MEMORY_REQUESTED));
    assertEquals(0, metrics.now());
  }

  @Test
  public void coldCache_oneThreadCallsTheEhr_theOthersWaitThenReuseItsToken() throws Exception {
    EhrTokenMetrics metrics = new EhrTokenMetrics(true, 0, true);
    slowTokenEndpoint();
    EhrAccessTokenCache cache = new EhrAccessTokenCache(authUtils, metrics, clock, 20, 45);

    runConcurrently(cache);

    assertEquals(1, metrics.count(Outcome.MEMORY_REQUESTED));
    assertEquals(THREADS - 1, metrics.count(Outcome.MEMORY_WAITED_THEN_REUSED));
    assertEquals("one call to the token endpoint", 1, metrics.timingCount(Timing.TOKEN_ENDPOINT));
    assertEquals(
        "every thread went through the lock", THREADS, metrics.timingCount(Timing.LOCK_WAIT));
    assertTrue("threads waited together", metrics.maxThreadsWaiting() > 1);
    assertTrue(
        "the longest wait is about one token endpoint call",
        metrics.timingMaxNanos(Timing.LOCK_WAIT)
            < TimeUnit.MILLISECONDS.toNanos(ENDPOINT_MILLIS * 3));

    // Now the token is cached: no lock, no endpoint call.
    runConcurrently(cache);
    assertEquals(THREADS, metrics.count(Outcome.MEMORY_CACHE_HIT));
    assertEquals(1, metrics.timingCount(Timing.TOKEN_ENDPOINT));

    System.out.println("=== cold cache, then cache hits" + metrics.summarySinceStart());
  }

  @Test
  public void refreshAhead_oneThreadRefreshes_othersUseTheCurrentTokenWithoutWaiting()
      throws Exception {
    EhrTokenMetrics metrics = new EhrTokenMetrics(true, 0, true);
    slowTokenEndpoint();
    EhrAccessTokenCache cache = new EhrAccessTokenCache(authUtils, metrics, clock, 20, 45);
    cache.getToken(hs()); // first token

    clock.advanceSeconds(1801); // past half of the 3600 s lifetime
    runConcurrently(cache);

    assertEquals(1, metrics.count(Outcome.MEMORY_REFRESHED_AHEAD));
    assertEquals(
        "nobody waited for the refreshing thread",
        THREADS - 1,
        metrics.count(Outcome.MEMORY_REFRESH_IN_PROGRESS_USED_CURRENT)
            + metrics.count(Outcome.MEMORY_REFRESHED_BY_OTHER_THREAD));
    assertEquals(
        "only the first token went through the blocking lock",
        1,
        metrics.timingCount(Timing.LOCK_WAIT));

    System.out.println("=== refresh ahead under load" + metrics.summarySinceStart());
  }

  @Test
  public void summary_listsOutcomesAndTimings() {
    EhrTokenMetrics metrics = new EhrTokenMetrics(true, 0, false);
    EhrTokenMetrics.Sample sample = metrics.begin();
    sample.lockWait(TimeUnit.MILLISECONDS.toNanos(5));
    sample.endpoint(TimeUnit.MILLISECONDS.toNanos(250));
    sample.outcome(Outcome.DATABASE_REQUESTED);
    sample.finish();

    String summary = metrics.summarySinceStart();
    assertTrue(summary, summary.contains("token calls: 1"));
    assertTrue(summary, summary.contains("DATABASE_REQUESTED"));
    assertTrue(summary, summary.contains("TOKEN_ENDPOINT"));
    assertTrue(summary, summary.contains("LOCK_WAIT"));
  }
}
