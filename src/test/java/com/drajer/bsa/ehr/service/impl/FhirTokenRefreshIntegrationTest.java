package com.drajer.bsa.ehr.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
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
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import org.hl7.fhir.r4.model.Resource;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * End-to-end check of the 401 / token-refresh handling (GitHub issue #1153).
 *
 * <p>Uses the real FhirContextInitializer, retry template, EcrFhirRetryClient,
 * FhirOperationExecutor and EhrFhirR4QueryServiceImpl against a local HTTP server that plays the
 * EHR. Only the token endpoint (AuthorizationUtils) and the DAO are mocked.
 *
 * <p>Every test runs twice: with ecr.fhir.retry.enabled=true (production) and =false.
 */
@RunWith(Parameterized.class)
public class FhirTokenRefreshIntegrationTest {

  @Parameterized.Parameters(name = "retryEnabled={0}")
  public static Collection<Object[]> modes() {
    return Arrays.asList(new Object[][] {{true}, {false}});
  }

  private final boolean retryEnabled;

  public FhirTokenRefreshIntegrationTest(boolean retryEnabled) {
    this.retryEnabled = retryEnabled;
  }

  private static final String OLD_TOKEN = "old-token";
  private static final String NEW_TOKEN = "new-token";

  // ----- mock EHR state -----
  private HttpServer server;
  private String baseUrl;
  private volatile String acceptedToken;
  private volatile int forcedStatus; // when > 0 every request gets this status
  private volatile int expireOldTokenAfterRequest = -1;
  private final AtomicInteger requestCount = new AtomicInteger();
  private final AtomicInteger tokenCalls = new AtomicInteger();
  private final List<String> requestLog = new CopyOnWriteArrayList<>();

  // ----- eCRNow objects under test -----
  private final FhirContext context = FhirContext.forR4();
  private EhrFhirR4QueryServiceImpl service;
  private FhirOperationExecutor executor;
  private FhirContextInitializer fhirContextInitializer;
  private KarProcessingData kd;
  private HealthcareSetting hs;

  @Before
  public void setUp() throws Exception {
    startMockEhr();
    wireEcrNow();
  }

  @After
  public void tearDown() {
    server.stop(0);
  }

  // ===================================== tests =====================================

  @Test
  public void read_staleToken_refreshesOnceAndSucceeds() {
    acceptedToken = NEW_TOKEN; // the EHR has already revoked old-token

    Resource patient =
        service.getResourceById(originalClient(), kd, context, "Patient", "1", false);

    assertNotNull("Patient should be returned after the token refresh", patient);
    assertEquals(2, requestCount.get()); // 401, then 200
    assertEquals(1, tokenCalls.get());
    assertEquals(NEW_TOKEN, hs.getEhrAccessToken());
  }

  @Test
  public void search_staleToken_refreshesOnceAndReturnsAllPages() {
    acceptedToken = NEW_TOKEN;

    Set<Resource> results =
        service.fetchResources(originalClient(), kd, context, baseUrl + "/Observation?patient=1");

    assertEquals(4, results.size()); // o1..o4 across two pages
    assertEquals(1, tokenCalls.get());
  }

  @Test
  public void pagination_tokenExpiresAfterPage1_onlyPage2IsRetried() {
    acceptedToken = OLD_TOKEN;
    expireOldTokenAfterRequest = 1; // old-token stops working right after page 1

    Set<Resource> results =
        service.fetchResources(originalClient(), kd, context, baseUrl + "/Observation?patient=1");

    assertEquals("page 1 data must not be lost", 4, results.size());
    assertEquals(1, tokenCalls.get());
    long page1Calls = requestLog.stream().filter(r -> r.contains("/Observation")).count();
    assertEquals("page 1 must not be fetched again", 1, page1Calls);
  }

  @Test
  public void everyTokenRejected_throwsAfterExactlyOneRetry() {
    acceptedToken = "nothing-is-accepted";

    RuntimeException thrown =
        assertThrows(
            RuntimeException.class,
            () -> service.getResourceById(originalClient(), kd, context, "Patient", "1", false));

    assertTrue(FhirOperationExecutor.isUnauthorized(thrown));
    assertEquals(2, requestCount.get()); // original + one retry, no loop
    assertEquals(1, tokenCalls.get());
  }

  @Test
  public void notFound_returnsNullWithoutTokenRefresh() {
    acceptedToken = OLD_TOKEN;
    forcedStatus = 404;

    Resource result = service.getResourceById(originalClient(), kd, context, "Patient", "1", false);

    assertNull(result);
    assertEquals(1, requestCount.get());
    assertEquals(0, tokenCalls.get());
  }

  @Test
  public void serverError_isRetriedOnlyByRetryTemplate_noTokenRefresh() {
    acceptedToken = OLD_TOKEN;
    forcedStatus = 500;

    Resource result = service.getResourceById(originalClient(), kd, context, "Patient", "1", false);

    assertNull(result);
    assertEquals(0, tokenCalls.get());
    // maxRetries=3 is the total number of attempts when retry is enabled
    assertEquals(retryEnabled ? 3 : 1, requestCount.get());
  }

  // ------------------- ecr.fhir.auth.retry-on-401.max-attempts -------------------

  @Test
  public void maxAttemptsZero_failsOnFirst401WithoutCallingTheTokenEndpoint() {
    ReflectionTestUtils.setField(executor, "retryOn401MaxAttempts", 0);
    acceptedToken = NEW_TOKEN;

    RuntimeException thrown =
        assertThrows(
            RuntimeException.class,
            () -> service.getResourceById(originalClient(), kd, context, "Patient", "1", false));

    assertTrue(FhirOperationExecutor.isUnauthorized(thrown));
    assertEquals(1, requestCount.get());
    assertEquals(0, tokenCalls.get());
  }

  @Test
  public void maxAttemptsTwo_succeedsWhenOnlyTheSecondFreshTokenIsAccepted() {
    ReflectionTestUtils.setField(executor, "retryOn401MaxAttempts", 2);
    acceptedToken = NEW_TOKEN + "-2"; // the EHR rejects old-token and the first fresh token

    Resource patient =
        service.getResourceById(originalClient(), kd, context, "Patient", "1", false);

    assertNotNull(patient);
    assertEquals(3, requestCount.get()); // old-token 401, new-token 401, new-token-2 200
    assertEquals(2, tokenCalls.get());
    assertEquals(NEW_TOKEN + "-2", hs.getEhrAccessToken());
  }

  @Test
  public void defaultMaxAttempts_stopsAfterOneRetryEvenIfALaterTokenWouldWork() {
    acceptedToken = NEW_TOKEN + "-2";

    RuntimeException thrown =
        assertThrows(
            RuntimeException.class,
            () -> service.getResourceById(originalClient(), kd, context, "Patient", "1", false));

    assertTrue(FhirOperationExecutor.isUnauthorized(thrown));
    assertEquals(2, requestCount.get());
    assertEquals(1, tokenCalls.get());
  }

  // ===================================== setup =====================================

  private IGenericClient originalClient() {
    return fhirContextInitializer.createClient(context, baseUrl, OLD_TOKEN, "req-1", null);
  }

  private void wireEcrNow() {
    // Retry template configured like application.properties (shorter wait for the test).
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
    ReflectionTestUtils.setField(retryTemplate, "isRetryEnabled", retryEnabled);
    ReflectionTestUtils.setField(retryTemplate, "fhirRetryTemplateConfig", config);

    fhirContextInitializer = new FhirContextInitializer(retryTemplate);
    ReflectionTestUtils.setField(fhirContextInitializer, "socketTimeout", 5);
    ReflectionTestUtils.setField(fhirContextInitializer, "connectionTimeout", 5);
    ReflectionTestUtils.setField(fhirContextInitializer, "poolMaxPerRoute", 10);
    ReflectionTestUtils.setField(fhirContextInitializer, "poolMaxTotal", 100);
    ReflectionTestUtils.setField(fhirContextInitializer, "connectionReqTimeOut", 30);

    // Token endpoint: always issues NEW_TOKEN.
    AuthorizationUtils authorizationUtils = mock(AuthorizationUtils.class);
    when(authorizationUtils.getToken(any()))
        .thenAnswer(
            inv -> {
              int n = tokenCalls.incrementAndGet();
              String token = n == 1 ? NEW_TOKEN : NEW_TOKEN + "-" + n;
              return new JSONObject().put("access_token", token).put("expires_in", 300);
            });

    executor =
        new FhirOperationExecutor(
            fhirContextInitializer, authorizationUtils, mock(HealthcareSettingsDao.class));
    ReflectionTestUtils.setField(executor, "TOKEN_EXPIRY_BUFFER_SECONDS", 60L);

    service = new EhrFhirR4QueryServiceImpl();
    ReflectionTestUtils.setField(service, "fhirOperationExecutor", executor);
    ReflectionTestUtils.setField(service, "fhirContextInitializer", fhirContextInitializer);

    // eCRNow believes old-token is valid for another hour.
    hs = new HealthcareSetting();
    hs.setFhirServerBaseURL(baseUrl);
    hs.setEhrAccessToken(OLD_TOKEN);
    hs.setEhrAccessTokenExpirationTime(new Date(System.currentTimeMillis() + 3_600_000));
    NotificationContext nc = new NotificationContext();
    nc.setxRequestId("req-1");
    kd = new KarProcessingData();
    kd.setHealthcareSetting(hs);
    kd.setNotificationContext(nc);
  }

  private void startMockEhr() throws Exception {
    server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    server.createContext(
        "/fhir",
        exchange -> {
          int n = requestCount.incrementAndGet();
          String uri = exchange.getRequestURI().toString();
          requestLog.add(uri);
          String auth = exchange.getRequestHeaders().getFirst("Authorization");

          int status;
          String body;
          if (forcedStatus > 0) {
            status = forcedStatus;
            body = operationOutcome("forced " + forcedStatus);
          } else if (!("Bearer " + acceptedToken).equals(auth)) {
            status = 401;
            body = operationOutcome("Unauthorized");
          } else {
            status = 200;
            if (uri.contains("page=2")) {
              body = bundle(List.of("o3", "o4"), null);
            } else if (uri.contains("/Observation")) {
              body = bundle(List.of("o1", "o2"), baseUrl + "?page=2");
            } else {
              body = "{\"resourceType\":\"Patient\",\"id\":\"1\"}";
            }
          }

          if (n == expireOldTokenAfterRequest) {
            acceptedToken = NEW_TOKEN;
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

  private static String operationOutcome(String text) {
    return "{\"resourceType\":\"OperationOutcome\",\"issue\":[{\"severity\":\"error\","
        + "\"code\":\"processing\",\"diagnostics\":\""
        + text
        + "\"}]}";
  }

  private static String bundle(List<String> ids, String nextUrl) {
    StringBuilder sb = new StringBuilder("{\"resourceType\":\"Bundle\",\"type\":\"searchset\"");
    if (nextUrl != null) {
      sb.append(",\"link\":[{\"relation\":\"next\",\"url\":\"").append(nextUrl).append("\"}]");
    }
    sb.append(",\"entry\":[");
    for (int i = 0; i < ids.size(); i++) {
      if (i > 0) {
        sb.append(',');
      }
      sb.append("{\"resource\":{\"resourceType\":\"Observation\",\"id\":\"")
          .append(ids.get(i))
          .append("\",\"status\":\"final\",\"code\":{\"text\":\"test\"}}}");
    }
    return sb.append("]}").toString();
  }
}
