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
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.test.util.TestUtils;
import com.github.tomakehurst.wiremock.client.MappingBuilder;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import com.github.tomakehurst.wiremock.junit.WireMockClassRule;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.http.MediaType;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;

public class RefreshTokenSchedulerTest {

  @ClassRule
  public static WireMockClassRule wireMockRule =
      new WireMockClassRule(wireMockConfig().dynamicPort());

  @Rule public WireMockClassRule mockServer = wireMockRule;

  private ClientDetails clientDetails;
  private RefreshTokenScheduler token = new RefreshTokenScheduler();
  private LaunchDetails launchDetails;
  private int wireMockPort = wireMockRule.port();

  @Before
  public void setup() {

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

    token = new RefreshTokenScheduler();
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

    ThreadPoolTaskScheduler mockScheduler = mock(ThreadPoolTaskScheduler.class);
    token.taskScheduler = mockScheduler;
    when(mockScheduler.schedule(any(Runnable.class), any(CronTrigger.class))).thenReturn(null);
    token.scheduleJob(launchDetails);
    ArgumentCaptor<CronTrigger> cronCaptor = ArgumentCaptor.forClass(CronTrigger.class);
    verify(mockScheduler).schedule(any(Runnable.class), cronCaptor.capture());
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
    RefreshTokenScheduler.RunnableTask task =
        new RefreshTokenScheduler().new RunnableTask(realLaunch);
    task.run();
    JSONObject response = new RefreshTokenScheduler().getAccessTokenUsingLaunchDetails(realLaunch);
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
}
