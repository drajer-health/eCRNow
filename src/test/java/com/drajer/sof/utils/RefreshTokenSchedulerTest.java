package com.drajer.sof.utils;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.*;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.test.util.TestUtils;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockClassRule;
import java.util.Date;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.http.MediaType;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.test.util.ReflectionTestUtils;

public class RefreshTokenSchedulerTest {

  @ClassRule
  public static WireMockClassRule wireMockRule =
      new WireMockClassRule(wireMockConfig().dynamicPort());

  @Rule public WireMockClassRule mockServer = wireMockRule;

  private ClientDetails clientDetails;
  private RefreshTokenScheduler token;
  private LaunchDetails launchDetails;
  private int wireMockPort = wireMockRule.port();
  private ThreadPoolTaskScheduler mockScheduler;

  @Before
  public void setup() {
    mockScheduler = mock(ThreadPoolTaskScheduler.class);
    token = newScheduler(mockScheduler);

    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDetail_IT_FullECR.json", ClientDetails.class);
    clientDetails.setTokenURL("http://localhost:" + wireMockPort + "/authorization");

    launchDetails = new LaunchDetails();
    launchDetails.setClientId("test-client");
    launchDetails.setExpiry(120); // 2 minutes expiry
    launchDetails.setTokenUrl("http://localhost:" + wireMockPort + "/authorization");
    launchDetails.setScope("system/Patient.read");
    launchDetails.setIsSystem(true);
    launchDetails.setIsUserAccountLaunch(false);
    launchDetails.setIsMultiTenantSystemLaunch(false);
  }

  @Test
  public void testgetSystemAccessToken_Succes() {

    try {

      String accesstoken =
          "{\"access_token\":\"eyJraWQiOiIy\",\"scope\":\"system\\/MedicationRequest.read\",\"token_type\":\"Bearer\",\"expires_in\":570}";

      MappingBuilder mappingBuilder = post(urlEqualTo("/authorization"));
      ResponseDefinitionBuilder response =
          aResponse()
              .withStatus(HttpStatus.SC_OK)
              .withBody(accesstoken)
              .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);

      stubFor(mappingBuilder.willReturn(response));

      // Test
      JSONObject authresponse = token.getAccessTokenUsingClientDetails(clientDetails);

      verify(postRequestedFor(urlEqualTo("/authorization")));
      assertEquals("eyJraWQiOiIy", authresponse.getString("access_token"));
      assertEquals(570, authresponse.getInt("expires_in"));

    } catch (Exception e) {

      fail(e.getMessage() + ": This exception is not expected, fix the test");
    }
  }

  @Test
  public void testgetSystemAccessToken_Unauthorized() {

    try {

      MappingBuilder mappingBuilder = post(urlEqualTo("/authorization"));
      ResponseDefinitionBuilder response = aResponse().withStatus(HttpStatus.SC_UNAUTHORIZED);
      stubFor(mappingBuilder.willReturn(response));

      // Test
      JSONObject authresponse = token.getAccessTokenUsingClientDetails(clientDetails);

      verify(postRequestedFor(urlEqualTo("/authorization")));
      assertNull(authresponse);

    } catch (Exception e) {

      fail(e.getMessage() + ": This exception is not expected, fix the test");
    }
  }

  @Test
  public void testScheduleJob_CronTrigger() {

    ThreadPoolTaskScheduler testMockScheduler = mock(ThreadPoolTaskScheduler.class);
    RefreshTokenScheduler testToken = newScheduler(testMockScheduler);
    when(testMockScheduler.schedule(any(Runnable.class), any(CronTrigger.class))).thenReturn(null);
    testToken.scheduleJob(launchDetails);
    ArgumentCaptor<CronTrigger> cronCaptor = ArgumentCaptor.forClass(CronTrigger.class);
    verify(testMockScheduler).schedule(any(Runnable.class), cronCaptor.capture());
    String cronExp = cronCaptor.getValue().getExpression();
    assertTrue("Cron expression should include 2 minutes", cronExp.contains("0/2"));
  }

  @Test
  public void testGetSystemAccessToken_Success() throws Exception {
    String accesstoken =
        "{\"access_token\":\"eyJraWQiOiIy\",\"scope\":\"system\\/MedicationRequest.read\",\"token_type\":\"Bearer\",\"expires_in\":570}";
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(HttpStatus.SC_OK)
                    .withBody(accesstoken)
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)));

    JSONObject authResponse = token.getAccessTokenUsingClientDetails(clientDetails);

    verify(postRequestedFor(urlEqualTo("/authorization")));
    assertNotNull(authResponse);
    assertEquals("eyJraWQiOiIy", authResponse.getString("access_token"));
    assertEquals(570, authResponse.getInt("expires_in"));
  }

  @Test
  public void testGetSystemAccessToken_Unauthorized() throws Exception {
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(aResponse().withStatus(HttpStatus.SC_UNAUTHORIZED)));

    JSONObject authResponse = token.getAccessTokenUsingClientDetails(clientDetails);

    verify(postRequestedFor(urlEqualTo("/authorization")));
    assertNull(authResponse);
  }

  @Test
  public void testGetAccessTokenUsingLaunchDetails_SystemLaunch() throws Exception {
    String accesstoken = "{\"access_token\":\"system-token\",\"expires_in\":600}";
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(HttpStatus.SC_OK)
                    .withBody(accesstoken)
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)));

    JSONObject response = token.getAccessTokenUsingLaunchDetails(launchDetails);

    assertNotNull(response);
    assertEquals("system-token", response.getString("access_token"));
    assertEquals(600, response.getInt("expires_in"));
  }

  @Test
  public void testRunnableTaskRun_RealData() throws Exception {
    String accesstoken = "{\"access_token\":\"real-token\",\"expires_in\":300}";
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody(accesstoken)
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)));
    LaunchDetails realLaunch = new LaunchDetails();
    realLaunch.setClientId("test-client");
    realLaunch.setClientSecret("test-secret");
    realLaunch.setTokenUrl("http://localhost:" + wireMockPort + "/authorization");
    realLaunch.setScope("system/Patient.read");
    realLaunch.setIsSystem(true);
    realLaunch.setExpiry(120);
    ThreadPoolTaskScheduler tempScheduler = mock(ThreadPoolTaskScheduler.class);
    RefreshTokenScheduler tempToken = newScheduler(tempScheduler);
    RefreshTokenScheduler.RunnableTask task = tempToken.new RunnableTask(realLaunch);
    task.run();
    JSONObject response = token.getAccessTokenUsingLaunchDetails(realLaunch);
    assertNotNull(response);
    assertEquals("real-token", response.getString("access_token"));
    assertEquals(300, response.getInt("expires_in"));
  }

  @Test
  public void testGetAccessToken_SystemClientWithAud() throws Exception {
    ClientDetails client = new ClientDetails();
    client.setClientId("aud-client");
    client.setClientSecret("secret");
    client.setScopes("system/Patient.read");
    client.setIsSystem(true);
    client.setRequireAud(true);
    client.setFhirServerBaseURL("http://fhir-server");
    client.setTokenURL("http://localhost:" + wireMockPort + "/authorization");
    client.setId(1);
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody("{\"access_token\":\"aud-token\",\"expires_in\":300}")
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)));
    ClientDetailsService clientService = mock(ClientDetailsService.class);
    when(clientService.getClientDetailsById(1)).thenReturn(client);
    ActionRepo.getInstance().setClientDetailsService(clientService);
    JSONObject response = token.getAccessTokenUsingClientDetails(client);
    assertNotNull(response);
    assertEquals("aud-token", response.getString("access_token"));
    assertEquals(300, response.getInt("expires_in"));
  }

  @Test
  public void testGetAccessToken_MultiTenantSystemLaunch() throws Exception {
    ClientDetails client = new ClientDetails();
    client.setClientId("mt-client");
    client.setClientSecret("secret");
    client.setScopes("system/Patient.read");
    client.setIsSystem(true);
    client.setIsMultiTenantSystemLaunch(true);
    client.setTokenURL("http://localhost:" + wireMockPort + "/authorization");
    client.setId(3);
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody("{\"access_token\":\"mt-token\",\"expires_in\":100}")
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)));

    ClientDetailsService clientService = mock(ClientDetailsService.class);
    when(clientService.getClientDetailsById(3)).thenReturn(client);
    ActionRepo.getInstance().setClientDetailsService(clientService);
    JSONObject response = token.getAccessTokenUsingClientDetails(client);
    assertNotNull(response);
    assertEquals("mt-token", response.getString("access_token"));
    assertEquals(100, response.getInt("expires_in"));
  }

  @Test
  public void testGetAccessTokenUsingLaunchDetails_RefreshTokenFlow() throws Exception {
    LaunchDetails normalLaunch = new LaunchDetails();
    normalLaunch.setClientId("user-client");
    normalLaunch.setTokenUrl("http://localhost:" + wireMockPort + "/authorization");
    normalLaunch.setRefreshToken("old-refresh-token");
    normalLaunch.setIsSystem(false);
    normalLaunch.setIsUserAccountLaunch(false);
    normalLaunch.setIsMultiTenantSystemLaunch(false);
    String refreshTokenResponse = "{\"access_token\":\"refreshed-token\",\"expires_in\":400}";
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(HttpStatus.SC_OK)
                    .withBody(refreshTokenResponse)
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)));
    JSONObject tokenResponse = token.getAccessTokenUsingLaunchDetails(normalLaunch);
    assertNotNull(tokenResponse);
    assertEquals("refreshed-token", tokenResponse.getString("access_token"));
    assertEquals(400, tokenResponse.getInt("expires_in"));
    verify(postRequestedFor(urlEqualTo("/authorization")));
  }

  @Test
  public void testGetAccessToken_RequireAud_SystemLaunch() throws Exception {
    LaunchDetails authDetails = new LaunchDetails();
    authDetails.setClientId("test-client");
    authDetails.setClientSecret("test-secret");
    authDetails.setTokenUrl("http://localhost:" + wireMockPort + "/authorization");
    authDetails.setScope("system/Patient.read");
    authDetails.setIsSystem(true);
    authDetails.setRequireAud(true);
    authDetails.setEhrServerURL("http://ehr-server");
    String accessTokenJson = "{\"access_token\":\"aud-token\",\"expires_in\":300}";
    stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(HttpStatus.SC_OK)
                    .withBody(accessTokenJson)
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)));

    JSONObject tokenResponse = token.getAccessTokenUsingLaunchDetails(authDetails);
    assertNotNull(tokenResponse);
    assertEquals("aud-token", tokenResponse.getString("access_token"));
    assertEquals(300, tokenResponse.getInt("expires_in"));
    verify(postRequestedFor(urlEqualTo("/authorization")));
  }

  @Test
  public void testGetAccessTokenUsingLaunchDetails_ExceptionCoverage() throws Exception {
    LaunchDetails badLaunch = new LaunchDetails();
    badLaunch.setClientId("bad-client");
    badLaunch.setTokenUrl("http://localhost:" + wireMockPort + "/authorization");
    badLaunch.setIsSystem(true);
    ClientDetailsService clientService = mock(ClientDetailsService.class);
    when(clientService.getClientDetailsById(anyInt()))
        .thenThrow(new RuntimeException("Simulated Exception"));
    ActionRepo.getInstance().setClientDetailsService(clientService);
    JSONObject tokenResponse = token.getAccessTokenUsingLaunchDetails(badLaunch);
    assertNull(tokenResponse);
  }

  private LaunchDetails userAccountLaunch() {
    LaunchDetails ld = new LaunchDetails();
    ld.setId(11);
    ld.setClientId("user-account-client");
    ld.setClientSecret("user-account-secret");
    ld.setScope("system/Patient.read");
    ld.setTokenUrl("http://localhost:" + wireMockPort + "/authorization");
    ld.setEhrServerURL("http://localhost:" + wireMockPort + "/fhir");
    ld.setIsSystem(false);
    ld.setIsUserAccountLaunch(true);
    ld.setIsMultiTenantSystemLaunch(false);
    return ld;
  }

  private LaunchService stubLaunchService(LaunchDetails stored) {
    LaunchService launchService = mock(LaunchService.class);
    when(launchService.getAuthDetailsById(any())).thenReturn(stored);
    ActionRepo.getInstance().setLaunchService(launchService);
    return launchService;
  }

  private ClientDetailsService stubClientDetailsService(ClientDetails byUrl, ClientDetails byId) {
    ClientDetailsService svc = mock(ClientDetailsService.class);
    when(svc.getClientDetailsByUrl(any())).thenReturn(byUrl);
    when(svc.getClientDetailsById(any())).thenReturn(byId);
    ActionRepo.getInstance().setClientDetailsService(svc);
    return svc;
  }

  private void stubTokenPost(String body) {
    mockServer.stubFor(
        post(urlEqualTo("/authorization"))
            .willReturn(
                aResponse()
                    .withStatus(HttpStatus.SC_OK)
                    .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .withBody(body)));
  }

  @Test
  public void testMultiTenantSystemLaunch_withNoClientDetailsRow() {
    stubTokenPost("{\"access_token\":\"mt-launch-token\",\"expires_in\":700}");

    LaunchDetails details = userAccountLaunch();
    details.setIsUserAccountLaunch(false);
    details.setIsMultiTenantSystemLaunch(true);

    ClientDetailsService svc = stubClientDetailsService(null, null);
    stubLaunchService(userAccountLaunch());

    assertNotNull(token.getAccessTokenUsingLaunchDetails(details));
    verify(svc, never()).saveOrUpdate(any());
  }

  @Test
  public void testGetAccessTokenForMultiTenantLaunch_lookupFailureReturnsNull() {
    ClientDetailsService svc = mock(ClientDetailsService.class);
    when(svc.getClientDetailsByUrl(any())).thenThrow(new RuntimeException("no client for url"));
    ActionRepo.getInstance().setClientDetailsService(svc);

    assertNull(token.getAccessTokenForMultiTenantLaunch(userAccountLaunch()));
  }

  @Test
  public void testDefaultConstructorLeavesSchedulerForSpringToInject() {
    // taskScheduler is now an @Autowired field: the default constructor no longer creates one.
    RefreshTokenScheduler standalone = new RefreshTokenScheduler();
    assertNull(standalone.taskScheduler);

    RefreshTokenScheduler wired = newScheduler(mockScheduler);
    assertSame(mockScheduler, wired.taskScheduler);
  }

  /**
   * RefreshTokenScheduler now uses @Autowired field injection (no-arg constructor only). The
   * taskScheduler field is package-private and this test is in the same package.
   */
  private static RefreshTokenScheduler newScheduler(ThreadPoolTaskScheduler scheduler) {
    RefreshTokenScheduler refreshTokenScheduler = new RefreshTokenScheduler();
    refreshTokenScheduler.taskScheduler = scheduler;
    return refreshTokenScheduler;
  }

  // =====================================================================================
  // New tests added for more coverage. The existing tests above are unchanged.
  // =====================================================================================

  private Object originalActionRepo;

  /**
   * ActionRepo is a JVM-wide singleton. In a full "mvn test" run another test class can leave a
   * mocked ActionRepo in its static "instance" field; then setLaunchService/setClientDetailsService
   * do nothing and getLaunchService()/getClientDetailsService() return null, so the token is never
   * stored. Each test here starts with a fresh, real ActionRepo and the previous one is put back
   * afterwards, so other test classes are not affected.
   */
  @Before
  public void useAFreshActionRepo() {
    originalActionRepo = ReflectionTestUtils.getField(ActionRepo.class, "instance");
    ReflectionTestUtils.setField(ActionRepo.class, "instance", null);
  }

  @After
  public void restoreActionRepo() {
    ReflectionTestUtils.setField(ActionRepo.class, "instance", originalActionRepo);
  }

  private String tokenUrl() {
    return "http://localhost:" + wireMockPort + "/authorization";
  }

  private void stubTokenGet(String body) {
    ResponseDefinitionBuilder response =
        aResponse()
            .withStatus(HttpStatus.SC_OK)
            .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);
    if (body != null) {
      response.withBody(body);
    }
    mockServer.stubFor(get(urlPathEqualTo("/authorization")).willReturn(response));
  }

  private static LaunchDetails launch(Boolean isSystem, Boolean isUserAccount, Boolean isMulti) {
    useTestEncryptionKey();
    LaunchDetails ld = new LaunchDetails();
    ld.setId(21);
    ld.setClientId("launch-client");
    ld.setClientSecret("launch-secret");
    ld.setScope("system/Patient.read");
    ld.setEhrServerURL("http://ehr-server");
    ld.setRefreshToken("refresh-token");
    ld.setIsSystem(isSystem);
    ld.setIsUserAccountLaunch(isUserAccount);
    ld.setIsMultiTenantSystemLaunch(isMulti);
    return ld;
  }

  private ClientDetails client(Boolean isSystem, Boolean isUserAccount, Boolean isMulti) {
    useTestEncryptionKey();
    ClientDetails cd = new ClientDetails();
    cd.setId(31);
    cd.setClientId("client-row");
    cd.setClientSecret("client-secret");
    cd.setScopes("system/Patient.read");
    cd.setTokenURL(tokenUrl());
    cd.setIsSystem(isSystem);
    cd.setIsUserAccountLaunch(isUserAccount);
    cd.setIsMultiTenantSystemLaunch(isMulti);
    cd.setRequireAud(false);
    return cd;
  }

  /**
   * setClientSecret() encrypts the secret with AESEncryption, whose secretKey is injected by
   * Spring. Without a key the secret is stored as null and setBasicAuth(clientId, null) fails with
   * "Password must not be null" in the user-account branches.
   */
  private static void useTestEncryptionKey() {
    ReflectionTestUtils.setField(AESEncryption.class, "secretKey", "test-secret-key");
  }

  // ---------------- RunnableTask ----------------

  @Test
  public void testRunnableTask_nullLaunchDetails_errorIsCaughtByRun() {
    // getAccessTokenUsingLaunchDetails(null) fails before its own try block,
    // so the exception reaches the catch inside RunnableTask.run().
    // run() calls Thread.currentThread().interrupt() only after a successful token call,
    // so on the error path the thread must not be left interrupted.
    Thread.interrupted(); // start with a clear interrupt flag
    RefreshTokenScheduler.RunnableTask task = token.new RunnableTask(null);

    task.run();

    assertFalse(
        "the error is caught in run(), so the success-path interrupt is skipped",
        Thread.interrupted());
  }

  // ---------------- getAccessTokenUsingLaunchDetails ----------------

  @Test
  public void testLaunchDetails_systemLaunch_updatesStoredLaunchDetails() {
    stubTokenPost("{\"access_token\":\"stored-token\",\"expires_in\":500}");
    LaunchDetails details = launch(true, false, false);
    details.setTokenUrl(tokenUrl());
    LaunchDetails stored = new LaunchDetails();
    LaunchService launchService = stubLaunchService(stored);

    JSONObject response = token.getAccessTokenUsingLaunchDetails(details);

    assertEquals("stored-token", response.getString("access_token"));
    assertEquals("stored-token", stored.getAccessToken());
    assertNull("no uuid in the response", stored.getProviderUUID());
    assertNotNull(stored.getTokenExpiryDateTime());
    verify(launchService).saveOrUpdate(stored);
  }

  @Test
  public void testLaunchDetails_userAccountLaunch_usesGetAndRenamesCamelCaseKeys() {
    stubTokenGet("{\"accessToken\":\"ua-token\",\"expiresIn\":900,\"uuid\":\"provider-1\"}");
    LaunchDetails details = launch(false, true, false);
    details.setTokenUrl(tokenUrl());
    LaunchDetails stored = new LaunchDetails();
    stubLaunchService(stored);

    JSONObject response = token.getAccessTokenUsingLaunchDetails(details);

    assertEquals("ua-token", response.getString("access_token"));
    assertEquals(900, response.getInt("expires_in"));
    assertEquals("ua-token", stored.getAccessToken());
    assertEquals("provider-1", stored.getProviderUUID());
    verify(
        getRequestedFor(urlPathEqualTo("/authorization"))
            .withQueryParam("grant_type", equalTo("client_credentials")));
  }

  @Test
  public void testLaunchDetails_userAccountLaunch_emptyBody_returnsNull() {
    stubTokenGet(null);
    LaunchDetails details = launch(false, true, false);
    details.setTokenUrl(tokenUrl());
    LaunchDetails stored = new LaunchDetails();
    stubLaunchService(stored);

    assertNull(token.getAccessTokenUsingLaunchDetails(details));
    // updateAccessToken fails on the null response and logs the error
    assertNull(stored.getAccessToken());
  }

  @Test
  public void testLaunchDetails_tokenWithoutExpiresIn_updateFailsButTokenIsReturned() {
    stubTokenGet("{\"accessToken\":\"no-expiry-token\"}");
    LaunchDetails details = launch(false, true, false);
    details.setTokenUrl(tokenUrl());
    LaunchService launchService = stubLaunchService(new LaunchDetails());

    JSONObject response = token.getAccessTokenUsingLaunchDetails(details);

    assertEquals("no-expiry-token", response.getString("access_token"));
    verify(launchService, never()).saveOrUpdate(any());
  }

  @Test
  public void testLaunchDetails_multiTenantLaunch_updatesClientDetailsWithoutAud() {
    stubTokenPost("{\"access_token\":\"mt-token\",\"expires_in\":800}");
    LaunchDetails details = launch(false, false, true);
    details.setTokenUrl(tokenUrl());
    details.setRequireAud(true); // aud is only added for isSystem launches
    ClientDetails clientRow = client(false, false, true);
    ClientDetailsService svc = stubClientDetailsService(clientRow, null);
    stubLaunchService(null);

    JSONObject response = token.getAccessTokenUsingLaunchDetails(details);

    assertEquals("mt-token", response.getString("access_token"));
    assertEquals("mt-token", clientRow.getAccessToken());
    verify(svc).saveOrUpdate(clientRow);
    verify(postRequestedFor(urlEqualTo("/authorization")).withRequestBody(notMatching(".*aud=.*")));
  }

  @Test
  public void testLaunchDetails_noMatchingLaunchType_returnsNull() {
    LaunchDetails details = launch(false, null, false);
    details.setTokenUrl(tokenUrl());
    stubLaunchService(new LaunchDetails());

    assertNull(token.getAccessTokenUsingLaunchDetails(details));
  }

  // ---------------- getAccessTokenForMultiTenantLaunch ----------------

  @Test
  public void testMultiTenantLaunch_tokenStillValid_returnsCachedToken() {
    ClientDetails clientRow = client(true, false, true);
    clientRow.setAccessToken("cached-token");
    clientRow.setTokenExpiry(3600);
    clientRow.setTokenExpiryDateTime(new Date(System.currentTimeMillis() + 3_600_000L));
    stubClientDetailsService(clientRow, clientRow);

    JSONObject response = token.getAccessTokenForMultiTenantLaunch(launch(true, false, true));

    assertEquals("cached-token", response.getString("access_token"));
    assertEquals(3600, response.getInt("expires_in"));
  }

  @Test
  public void testMultiTenantLaunch_tokenExpired_getsAndStoresNewToken() {
    stubTokenPost("{\"access_token\":\"new-mt-token\",\"expires_in\":1200}");
    ClientDetails clientRow = client(true, false, true);
    clientRow.setAccessToken("old-token");
    clientRow.setTokenExpiryDateTime(new Date(System.currentTimeMillis() - 3_600_000L));
    ClientDetailsService svc = stubClientDetailsService(clientRow, clientRow);

    JSONObject response = token.getAccessTokenForMultiTenantLaunch(launch(true, false, true));

    assertEquals("new-mt-token", response.getString("access_token"));
    assertEquals("new-mt-token", clientRow.getAccessToken());
    verify(svc).saveOrUpdate(clientRow);
  }

  // ---------------- getAccessTokenUsingClientDetails ----------------

  @Test
  public void testClientDetails_userAccountLaunch_usesGetAndDoesNotUpdateSingleTenantRow() {
    stubTokenGet("{\"accessToken\":\"ua-client-token\",\"expiresIn\":450}");
    ClientDetails clientRow = client(false, true, false);
    ClientDetailsService svc = stubClientDetailsService(null, clientRow);

    JSONObject response = token.getAccessTokenUsingClientDetails(clientRow);

    assertEquals("ua-client-token", response.getString("access_token"));
    assertEquals(450, response.getInt("expires_in"));
    verify(svc, never()).saveOrUpdate(any());
  }

  @Test
  public void testClientDetails_userAccountLaunch_emptyBody_returnsNull() {
    stubTokenGet(null);
    ClientDetails clientRow = client(false, true, false);
    stubClientDetailsService(null, clientRow);

    assertNull(token.getAccessTokenUsingClientDetails(clientRow));
  }

  @Test
  public void testClientDetails_noMatchingLaunchType_updateFailsOnNullResponse() {
    ClientDetails clientRow = client(false, false, false);
    ClientDetails storedMultiTenantRow = client(true, false, true);
    ClientDetailsService svc = stubClientDetailsService(null, storedMultiTenantRow);

    assertNull(token.getAccessTokenUsingClientDetails(clientRow));
    // updateAccessTokenInClientDetails fails on the null response and logs the error
    verify(svc, never()).saveOrUpdate(any());
  }
}
