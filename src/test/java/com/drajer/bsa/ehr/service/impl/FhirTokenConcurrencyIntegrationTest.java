package com.drajer.bsa.ehr.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.auth.EhrAccessTokenCache;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.ecrretry.RetryStatusCode;
import com.drajer.sof.utils.FhirContextInitializer;
import com.sun.net.httpserver.HttpServer;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Resource;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * Many scheduled jobs running in parallel against one EHR, end to end.
 *
 * <p>Uses the real EhrFhirR4QueryServiceImpl, FhirOperationExecutor, EhrAccessTokenCache,
 * FhirContextInitializer and retry template (enabled, as in production) against a multi-threaded
 * mock EHR on localhost. Only the token endpoint (AuthorizationUtils) is mocked; it answers slowly
 * so that threads really collide while a token is being obtained.
 *
 * <p>Every job loads its own HealthcareSetting copy, as a scheduled job does. Each test checks that
 * every job succeeds, how often the token endpoint was called, and that nothing at WARN or ERROR
 * level was logged except the expected 401 retry warning.
 */
public class FhirTokenConcurrencyIntegrationTest {

  private static final int THREADS = 20;
  private volatile long tokenEndpointDelayMillis = 200;

  // ----- mock EHR state -----
  private HttpServer server;
  private ExecutorService serverThreads;
  private String baseUrl;
  private final Map<String, Long> validTokens = new ConcurrentHashMap<>(); // token -> valid until
  private volatile long tokenLifetimeSeconds = 300;
  private volatile int revokeAllTokensAtRequest = -1;
  private volatile int failTokenCall = -1; // this token call fails, as on a network dropout
  private final AtomicInteger ehrRequests = new AtomicInteger();
  private final AtomicInteger ehrUnauthorized = new AtomicInteger();
  private final AtomicInteger tokenCalls = new AtomicInteger();
  private final List<Long> jobMillis = new CopyOnWriteArrayList<>();

  // ----- eCRNow objects under test -----
  private final FhirContext context = FhirContext.forR4();
  private EhrFhirR4QueryServiceImpl service;
  private EhrAccessTokenCache tokenCache;
  private FhirContextInitializer fhirContextInitializer;
  private AuthorizationUtils authorizationUtils;

  // ----- log capture -----
  private ch.qos.logback.classic.Logger rootLogger;
  private ListAppender<ILoggingEvent> logs;

  @Before
  public void setUp() throws Exception {
    startMockEhr();
    wireEcrNow();
    rootLogger =
        (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
    logs = new ListAppender<>();
    logs.start();
    rootLogger.addAppender(logs);
  }

  @After
  public void tearDown() {
    rootLogger.detachAppender(logs);
    server.stop(0);
    serverThreads.shutdownNow();
  }

  // ===================================== tests =====================================

  @Test
  public void parallelJobs_shareOneToken_withoutErrors() throws Exception {
    int jobs = 200;

    List<Throwable> failures = runJobs(jobs);

    assertEquals("failed jobs: " + failures, 0, failures.size());
    assertEquals("one token for all parallel jobs", 1, tokenCalls.get());
    assertEquals("the EHR never rejected a token", 0, ehrUnauthorized.get());
    assertEquals(jobs, ehrRequests.get());
    assertNoProblemsLogged(e -> false);
    assertEquals(1, countLogged("Obtained EHR access token"));
  }

  @Test
  public void tokenExpiresWhileJobsRun_isReplacedBeforeExpiry_withoutErrors() throws Exception {
    // 2 s tokens: the cache replaces each one after about 1 s (half its lifetime), so the EHR
    // never sees an expired token even though jobs run continuously for 4 s.
    tokenLifetimeSeconds = 2;

    int completed = runJobsFor(4_000);

    assertTrue("jobs completed: " + completed, completed > 0);
    assertEquals("the EHR never rejected a token", 0, ehrUnauthorized.get());
    assertTrue(
        "token calls: " + tokenCalls.get(), tokenCalls.get() >= 2 && tokenCalls.get() <= 8);
    assertNoProblemsLogged(e -> false);
  }

  @Test
  public void ehrRevokesTokenMidLoad_allJobsRecoverWithOneNewToken_withoutErrors()
      throws Exception {
    revokeAllTokensAtRequest = 50; // the EHR revokes every issued token at its 50th request
    int jobs = 200;

    List<Throwable> failures = runJobs(jobs);

    assertEquals("failed jobs: " + failures, 0, failures.size());
    assertTrue("some requests were rejected", ehrUnauthorized.get() > 0);
    assertEquals("initial token + one shared replacement", 2, tokenCalls.get());
    // The only acceptable warning is the executor announcing its one 401 retry.
    assertNoProblemsLogged(
        e ->
            e.getLevel() == Level.WARN
                && e.getLoggerName().endsWith("FhirOperationExecutor")
                && e.getFormattedMessage().startsWith("FHIR request returned 401"));
  }

  @Test
  public void firstTokenCallFails_onlyThatJobFails_allOtherJobsSucceed() throws Exception {
    failTokenCall = 1; // the very first token call hits a dropout; the endpoint then works
    int jobs = 200;

    List<Throwable> failures = runJobs(jobs);

    assertEquals("only the job whose token call failed: " + failures, 1, failures.size());
    assertEquals("the failed call, then one token shared by everyone else", 2, tokenCalls.get());
    assertEquals("the EHR never rejected a token", 0, ehrUnauthorized.get());
    assertNoProblemsLogged(e -> false);
  }

  @Test
  public void refreshFailsWhileTokenStillValid_noJobFails() throws Exception {
    // 2 s tokens are refreshed after about 1 s; the first refresh (call 2) hits a dropout. The
    // current token is still valid for about 1 s, so jobs keep using it and the next refresh works.
    tokenLifetimeSeconds = 2;
    failTokenCall = 2;

    int completed = runJobsFor(3_000);

    assertTrue("jobs completed: " + completed, completed > 0);
    assertEquals("the EHR never rejected a token", 0, ehrUnauthorized.get());
    // The one expected warning: the failed refresh, answered with the still-valid token.
    assertNoProblemsLogged(
        e ->
            e.getLevel() == Level.WARN
                && e.getLoggerName().endsWith("EhrAccessTokenCache")
                && e.getFormattedMessage().startsWith("Could not refresh the EHR access token"));
  }

  @Test
  public void slowTokenApi_refreshAheadDoesNotBlockOtherJobs_withoutErrors() throws Exception {
    // 8 s tokens and a token API taking 2 s. With a 1 s threshold the refresh starts after 4 s,
    // half the lifetime, while the current token is still valid for another 4 s.
    useTokenCache(1);
    tokenLifetimeSeconds = 8;
    tokenEndpointDelayMillis = 2_000;
    runOneJob(-1); // the first token: no token exists yet, so this job waits for it

    int completed = runJobsFor(6_000);

    long slowJobs = jobMillis.stream().filter(ms -> ms >= 1_500).count();
    assertTrue("jobs completed: " + completed, completed > 0);
    assertTrue("a refresh ahead of expiry happened: " + tokenCalls.get(), tokenCalls.get() >= 2);
    assertTrue(
        "only the job doing each refresh waits for the slow token API; slow jobs: "
            + slowJobs
            + ", refreshes: "
            + (tokenCalls.get() - 1),
        slowJobs <= tokenCalls.get() - 1);
    assertEquals("the EHR never rejected a token", 0, ehrUnauthorized.get());
    assertNoProblemsLogged(e -> false);
  }

  // ===================================== jobs =====================================

  /** One scheduled job: loads its own HealthcareSetting copy and reads the Patient. */
  private void runOneJob(int jobNumber) {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setFhirServerBaseURL(baseUrl);
    hs.setClientId("ecrnow-client");
    NotificationContext nc = new NotificationContext();
    nc.setxRequestId("job-" + jobNumber);
    KarProcessingData kd = new KarProcessingData();
    kd.setHealthcareSetting(hs);
    kd.setNotificationContext(nc);

    IGenericClient client = service.getClient(kd, context);
    Resource patient = service.getResourceById(client, kd, context, "Patient", "1", false);
    if (patient == null) {
      throw new AssertionError("job " + jobNumber + ": Patient was not returned");
    }
  }

  /** Runs the jobs on THREADS threads, all released at once; returns the failures. */
  private List<Throwable> runJobs(int jobs) throws Exception {
    ExecutorService pool = Executors.newFixedThreadPool(THREADS);
    CountDownLatch start = new CountDownLatch(1);
    List<Future<?>> results = new ArrayList<>();
    try {
      for (int i = 0; i < jobs; i++) {
        int jobNumber = i;
        results.add(
            pool.submit(
                () -> {
                  start.await();
                  runOneJob(jobNumber);
                  return null;
                }));
      }
      start.countDown();
      List<Throwable> failures = new ArrayList<>();
      for (Future<?> result : results) {
        try {
          result.get(60, TimeUnit.SECONDS);
        } catch (java.util.concurrent.ExecutionException e) {
          failures.add(e.getCause());
        }
      }
      return failures;
    } finally {
      pool.shutdownNow();
    }
  }

  /** Runs jobs continuously on THREADS threads for the given time; returns how many completed. */
  private int runJobsFor(long millis) throws Exception {
    ExecutorService pool = Executors.newFixedThreadPool(THREADS);
    AtomicInteger completed = new AtomicInteger();
    List<Throwable> failures = new CopyOnWriteArrayList<>();
    long deadline = System.currentTimeMillis() + millis;
    try {
      for (int t = 0; t < THREADS; t++) {
        pool.submit(
            () -> {
              while (System.currentTimeMillis() < deadline) {
                try {
                  long jobStart = System.nanoTime();
                  runOneJob(completed.get());
                  jobMillis.add(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - jobStart));
                  completed.incrementAndGet();
                } catch (Throwable e) {
                  failures.add(e);
                }
              }
            });
      }
      pool.shutdown();
      assertTrue(pool.awaitTermination(millis + 60_000, TimeUnit.MILLISECONDS));
    } finally {
      pool.shutdownNow();
    }
    assertEquals("failed jobs: " + failures, 0, failures.size());
    return completed.get();
  }

  // ===================================== logs =====================================

  private void assertNoProblemsLogged(Predicate<ILoggingEvent> expected) {
    List<String> problems =
        logs.list.stream()
            .filter(e -> e.getLevel().isGreaterOrEqual(Level.WARN))
            .filter(expected.negate())
            .map(e -> e.getLevel() + " " + e.getLoggerName() + ": " + e.getFormattedMessage())
            .collect(Collectors.toList());
    assertEquals("unexpected WARN/ERROR log lines: " + problems, 0, problems.size());
  }

  private long countLogged(String messageStart) {
    return logs.list.stream().filter(e -> e.getFormattedMessage().startsWith(messageStart)).count();
  }

  // ===================================== setup =====================================

  private void wireEcrNow() {
    FHIRRetryTemplateConfig.HttpMethodType get = new FHIRRetryTemplateConfig.HttpMethodType();
    get.setMaxRetries(3);
    get.setRetryWaitTimeInMillis(10);
    get.setRetryStatusCodes(List.of(408, 429, 502, 503, 504, 500));
    FHIRRetryTemplateConfig config = new FHIRRetryTemplateConfig();
    config.setMaxRetries(3);
    config.setRetryWaitTimeInMillis(10);
    config.setRetryStatusCodes(List.of(408, 429, 502, 503, 504, 500));
    config.setHttpMethodTypeMap(Map.of("GET", get));

    FHIRRetryTemplate retryTemplate =
        new FHIRRetryTemplate(new RetryStatusCode(config).configureRetryTemplate());
    ReflectionTestUtils.setField(retryTemplate, "isRetryEnabled", true);
    ReflectionTestUtils.setField(retryTemplate, "fhirRetryTemplateConfig", config);

    fhirContextInitializer = new FhirContextInitializer(retryTemplate);
    ReflectionTestUtils.setField(fhirContextInitializer, "socketTimeout", 10);
    ReflectionTestUtils.setField(fhirContextInitializer, "connectionTimeout", 10);
    ReflectionTestUtils.setField(fhirContextInitializer, "poolMaxPerRoute", 100);
    ReflectionTestUtils.setField(fhirContextInitializer, "poolMaxTotal", 100);
    ReflectionTestUtils.setField(fhirContextInitializer, "connectionReqTimeOut", 30);

    // Token endpoint: slow, issues token-1, token-2, ... and tells the mock EHR it is valid.
    authorizationUtils = mock(AuthorizationUtils.class);
    when(authorizationUtils.getToken(any()))
        .thenAnswer(
            inv -> {
              Thread.sleep(tokenEndpointDelayMillis);
              int call = tokenCalls.incrementAndGet();
              if (call == failTokenCall) {
                throw new IllegalStateException("simulated connection dropout to token endpoint");
              }
              String token = "token-" + call;
              long lifetime = tokenLifetimeSeconds;
              validTokens.put(token, System.currentTimeMillis() + lifetime * 1000);
              return new JSONObject().put("access_token", token).put("expires_in", lifetime);
            });

    // Production settings: 20 s refresh threshold, 45 s lock timeout.
    useTokenCache(20);
  }

  /** Builds the cache, executor and query service with the given token.refresh.threshold. */
  private void useTokenCache(long refreshThresholdSeconds) {
    tokenCache = new EhrAccessTokenCache(authorizationUtils, refreshThresholdSeconds, 45);
    FhirOperationExecutor executor = new FhirOperationExecutor(fhirContextInitializer, tokenCache);

    service = new EhrFhirR4QueryServiceImpl();
    ReflectionTestUtils.setField(service, "fhirOperationExecutor", executor);
    ReflectionTestUtils.setField(service, "fhirContextInitializer", fhirContextInitializer);
    ReflectionTestUtils.setField(service, "ehrAccessTokenCache", tokenCache);
  }

  private void startMockEhr() throws Exception {
    server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    serverThreads = Executors.newFixedThreadPool(THREADS);
    server.setExecutor(serverThreads);
    server.createContext(
        "/fhir",
        exchange -> {
          int n = ehrRequests.incrementAndGet();
          if (n == revokeAllTokensAtRequest) {
            validTokens.clear();
          }
          String auth = exchange.getRequestHeaders().getFirst("Authorization");
          String token = auth != null && auth.startsWith("Bearer ") ? auth.substring(7) : "";
          Long validUntil = validTokens.get(token);

          int status;
          String body;
          if (validUntil == null || System.currentTimeMillis() >= validUntil) {
            ehrUnauthorized.incrementAndGet();
            status = 401;
            body =
                "{\"resourceType\":\"OperationOutcome\",\"issue\":[{\"severity\":\"error\","
                    + "\"code\":\"login\",\"diagnostics\":\"Unauthorized\"}]}";
          } else {
            status = 200;
            body = "{\"resourceType\":\"Patient\",\"id\":\"1\"}";
          }

          byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
          exchange.getResponseHeaders().add("Content-Type", "application/fhir+json");
          exchange.sendResponseHeaders(status, bytes.length);
          try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
          }
        });
    server.start();
    baseUrl = "http://127.0.0.1:" + server.getAddress().getPort() + "/fhir";
  }
}
