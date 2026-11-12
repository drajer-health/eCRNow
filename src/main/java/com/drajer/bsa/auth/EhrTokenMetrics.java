package com.drajer.bsa.auth;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import java.util.EnumMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Optional performance recorder for EHR access tokens, in both token storage modes. Turned on with
 * {@code ecr.ehr.token.metrics.enabled=true}; when off, every method returns immediately and
 * nothing is measured.
 *
 * <p>For every token request made by a job it records which way the token was obtained ({@link
 * Outcome}), how long the thread waited for the per-URL lock (in-memory mode), how long the call to
 * the EHR token endpoint took, and the total time. Every {@code
 * ecr.ehr.token.metrics.summary-interval-seconds} it logs a summary for the last interval and since
 * startup. With {@code ecr.ehr.token.metrics.log-each-call=true} it also logs one line per token
 * call that did more than read a valid token (cache hits are only counted).
 *
 * <p>Token values are never logged.
 */
@Component
public class EhrTokenMetrics {

  private static final Logger logger = LoggerFactory.getLogger(EhrTokenMetrics.class);

  /** How a token request was served. */
  public enum Outcome {
    MEMORY_CACHE_HIT("memory: valid token from the cache, no lock"),
    MEMORY_REFRESH_IN_PROGRESS_USED_CURRENT(
        "memory: another thread was refreshing; current token used without waiting"),
    MEMORY_REFRESHED_BY_OTHER_THREAD("memory: token refreshed by another thread just before"),
    MEMORY_REFRESHED_AHEAD("memory: this thread refreshed the token at half its lifetime"),
    MEMORY_REFRESH_FAILED_USED_CURRENT("memory: refresh failed; current valid token used"),
    MEMORY_WAITED_THEN_REUSED("memory: waited for the lock, then used another thread's token"),
    MEMORY_REQUESTED("memory: this thread requested the token from the EHR"),
    MEMORY_REQUESTED_AFTER_401("memory: this thread requested a new token after HTTP 401"),
    MEMORY_REQUEST_FAILED_USED_CURRENT("memory: request failed; current valid token used"),
    MEMORY_REQUEST_FAILED("memory: token request failed; job fails"),
    MEMORY_LOCK_TIMEOUT("memory: gave up waiting for the lock; job fails"),
    DATABASE_REUSED("database: valid token reused from the HealthcareSetting"),
    DATABASE_REQUESTED("database: new token requested and saved on the HealthcareSetting"),
    DATABASE_REQUESTED_AFTER_401("database: new token requested after HTTP 401"),
    DATABASE_RR_REQUESTED("database: new token requested to post an RR"),
    DATABASE_REQUEST_FAILED("database: token request failed");

    private final String description;

    Outcome(String description) {
      this.description = description;
    }

    public String description() {
      return description;
    }
  }

  /** What was timed. */
  public enum Timing {
    TOTAL,
    LOCK_WAIT,
    TOKEN_ENDPOINT
  }

  /** One token request by one thread. All methods do nothing on a disabled recorder. */
  public final class Sample {
    private final long startNanos;
    private long lockWaitNanos = -1;
    private long endpointNanos = -1;
    private Outcome outcome;

    private Sample(long startNanos) {
      this.startNanos = startNanos;
    }

    public void outcome(Outcome value) {
      this.outcome = value;
    }

    public void lockWait(long nanos) {
      this.lockWaitNanos = nanos;
    }

    public void endpoint(long nanos) {
      this.endpointNanos = nanos;
    }

    /** Records the sample. Call once, in a finally block. */
    public void finish() {
      if (this == noop) {
        return;
      }
      CURRENT.remove();
      record(this, System.nanoTime() - startNanos);
    }
  }

  private static final ThreadLocal<Sample> CURRENT = new ThreadLocal<>();

  /** Shared no-op sample of the disabled recorder. */
  private final Sample noop = new Sample(0);

  private static final long[] BUCKET_LIMITS_MS = {1, 10, 100, 1_000, 5_000, 30_000};
  private static final String[] BUCKET_LABELS = {
    "<1ms", "<10ms", "<100ms", "<1s", "<5s", "<30s", ">=30s"
  };

  /** Count, total, max and a coarse histogram of one timing, resettable per interval. */
  private static final class Stats {
    final LongAdder count = new LongAdder();
    final LongAdder totalNanos = new LongAdder();
    final AtomicLong maxNanos = new AtomicLong();
    final LongAdder[] buckets = new LongAdder[BUCKET_LABELS.length];

    Stats() {
      for (int i = 0; i < buckets.length; i++) {
        buckets[i] = new LongAdder();
      }
    }

    void add(long nanos) {
      count.increment();
      totalNanos.add(nanos);
      maxNanos.accumulateAndGet(nanos, Math::max);
      long ms = TimeUnit.NANOSECONDS.toMillis(nanos);
      int i = 0;
      while (i < BUCKET_LIMITS_MS.length && ms >= BUCKET_LIMITS_MS[i]) {
        i++;
      }
      buckets[i].increment();
    }
  }

  /** All counters of one period (the current interval, or since startup). */
  private static final class Period {
    final Map<Outcome, LongAdder> outcomes = new EnumMap<>(Outcome.class);
    final Map<Timing, Stats> timings = new EnumMap<>(Timing.class);
    final AtomicInteger maxThreadsWaiting = new AtomicInteger();

    Period() {
      for (Outcome o : Outcome.values()) {
        outcomes.put(o, new LongAdder());
      }
      for (Timing t : Timing.values()) {
        timings.put(t, new Stats());
      }
    }
  }

  private final boolean enabled;
  private final long summaryIntervalSeconds;
  private final boolean logEachCall;

  private volatile Period interval = new Period();
  private final Period sinceStart = new Period();
  private final AtomicInteger threadsWaiting = new AtomicInteger();
  private ScheduledExecutorService summaryLogger;

  @Autowired
  public EhrTokenMetrics(
      @Value("${ecr.ehr.token.metrics.enabled:false}") boolean enabled,
      @Value("${ecr.ehr.token.metrics.summary-interval-seconds:60}") long summaryIntervalSeconds,
      @Value("${ecr.ehr.token.metrics.log-each-call:false}") boolean logEachCall) {
    this.enabled = enabled;
    this.summaryIntervalSeconds = summaryIntervalSeconds;
    this.logEachCall = logEachCall;
  }

  /** A recorder that measures nothing, for code and tests created without Spring. */
  public static EhrTokenMetrics disabled() {
    return new EhrTokenMetrics(false, 0, false);
  }

  public boolean isEnabled() {
    return enabled;
  }

  @PostConstruct
  void startSummaryLogging() {
    if (!enabled) {
      return;
    }
    logger.info(
        "EHR token metrics enabled (summary every {} s, log each call: {})",
        summaryIntervalSeconds,
        logEachCall);
    if (summaryIntervalSeconds > 0) {
      summaryLogger =
          Executors.newSingleThreadScheduledExecutor(
              r -> {
                Thread t = new Thread(r, "ehr-token-metrics");
                t.setDaemon(true);
                return t;
              });
      summaryLogger.scheduleAtFixedRate(
          this::logSummary, summaryIntervalSeconds, summaryIntervalSeconds, TimeUnit.SECONDS);
    }
  }

  @PreDestroy
  void stop() {
    if (summaryLogger != null) {
      summaryLogger.shutdownNow();
    }
    if (enabled) {
      logSummary();
    }
  }

  /** Starts timing a token request on the current thread. */
  public Sample begin() {
    if (!enabled) {
      return noop;
    }
    Sample sample = new Sample(System.nanoTime());
    CURRENT.set(sample);
    return sample;
  }

  /** The sample of the token request running on this thread, or a no-op sample. */
  public Sample current() {
    if (!enabled) {
      return noop;
    }
    Sample sample = CURRENT.get();
    return sample != null ? sample : noop;
  }

  /** {@link System#nanoTime()} when enabled, 0 otherwise (avoids the call when off). */
  public long now() {
    return enabled ? System.nanoTime() : 0;
  }

  /** A thread starts waiting for a token lock. */
  public void waitingStarted() {
    if (enabled) {
      int waiting = threadsWaiting.incrementAndGet();
      interval.maxThreadsWaiting.accumulateAndGet(waiting, Math::max);
      sinceStart.maxThreadsWaiting.accumulateAndGet(waiting, Math::max);
    }
  }

  /** A thread stopped waiting for a token lock. */
  public void waitingEnded() {
    if (enabled) {
      threadsWaiting.decrementAndGet();
    }
  }

  private void record(Sample sample, long totalNanos) {
    Outcome outcome = sample.outcome;
    for (Period p : new Period[] {interval, sinceStart}) {
      if (outcome != null) {
        p.outcomes.get(outcome).increment();
      }
      p.timings.get(Timing.TOTAL).add(totalNanos);
      if (sample.lockWaitNanos >= 0) {
        p.timings.get(Timing.LOCK_WAIT).add(sample.lockWaitNanos);
      }
      if (sample.endpointNanos >= 0) {
        p.timings.get(Timing.TOKEN_ENDPOINT).add(sample.endpointNanos);
      }
    }
    if (logEachCall && outcome != Outcome.MEMORY_CACHE_HIT && outcome != Outcome.DATABASE_REUSED) {
      logger.info(
          "EHR token call: thread={}, outcome={}, lockWaitMs={}, tokenEndpointMs={}, totalMs={}",
          Thread.currentThread().getName(),
          outcome,
          ms(sample.lockWaitNanos),
          ms(sample.endpointNanos),
          ms(totalNanos));
    }
  }

  /** Logs the summary of the last interval and since startup, then starts a new interval. */
  public void logSummary() {
    Period last = interval;
    interval = new Period();
    logger.info("EHR token metrics, last {} s:{}", summaryIntervalSeconds, describe(last));
    logger.info("EHR token metrics, since startup:{}", describe(sinceStart));
  }

  /** The summary since startup, as text (also used by tests). */
  public String summarySinceStart() {
    return describe(sinceStart);
  }

  /** How many token calls had this outcome since startup. */
  public long count(Outcome outcome) {
    return sinceStart.outcomes.get(outcome).sum();
  }

  long timingCount(Timing timing) {
    return sinceStart.timings.get(timing).count.sum();
  }

  long timingMaxNanos(Timing timing) {
    return sinceStart.timings.get(timing).maxNanos.get();
  }

  int maxThreadsWaiting() {
    return sinceStart.maxThreadsWaiting.get();
  }

  private static String describe(Period p) {
    StringBuilder sb = new StringBuilder();
    long calls = p.timings.get(Timing.TOTAL).count.sum();
    sb.append("\n  token calls: ").append(calls);
    for (Outcome o : Outcome.values()) {
      long n = p.outcomes.get(o).sum();
      if (n > 0) {
        sb.append(
            String.format(
                Locale.ROOT,
                "%n    %-40s %8d (%5.1f%%)  %s",
                o.name(),
                n,
                calls == 0 ? 0.0 : 100.0 * n / calls,
                o.description()));
      }
    }
    for (Timing t : Timing.values()) {
      Stats s = p.timings.get(t);
      long n = s.count.sum();
      if (n == 0) {
        continue;
      }
      sb.append(
          String.format(
              Locale.ROOT,
              "%n  %-14s count=%d avg=%.2fms max=%.2fms  [",
              t.name(),
              n,
              s.totalNanos.sum() / 1e6 / n,
              s.maxNanos.get() / 1e6));
      for (int i = 0; i < BUCKET_LABELS.length; i++) {
        sb.append(i == 0 ? "" : " ")
            .append(BUCKET_LABELS[i])
            .append('=')
            .append(s.buckets[i].sum());
      }
      sb.append(']');
    }
    sb.append("\n  max threads waiting for a token lock at once: ")
        .append(p.maxThreadsWaiting.get());
    return sb.toString();
  }

  private static String ms(long nanos) {
    return nanos < 0 ? "-" : String.format(Locale.ROOT, "%.2f", nanos / 1e6);
  }
}
