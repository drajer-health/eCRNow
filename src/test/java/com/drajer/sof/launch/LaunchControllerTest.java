package com.drajer.sof.launch;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.eca.model.EventTypes;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.sof.model.*;
import com.drajer.sof.service.*;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.RefreshTokenScheduler;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Period;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.*;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.web.server.ResponseStatusException;

@RunWith(MockitoJUnitRunner.class)
public class LaunchControllerTest {
  @Spy @InjectMocks private LaunchController launchController;

  @Mock private LaunchService authDetailsService;
  @Mock private RefreshTokenScheduler tokenScheduler;
  @Mock private WorkflowService workflowService;
  @Mock private TriggerQueryService triggerQueryService;
  @Mock private LoadingQueryService loadingQueryService;
  @Mock private ClientDetailsService clientDetailsService;
  @Mock private Authorization authorization;
  @Mock private HttpServletRequest request;
  @Mock private HttpServletResponse response;
  @Mock FhirContextInitializer fhirContextInitializer;

  private LaunchDetails launchDetails;
  private ClientDetails clientDetails;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    launchDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);

    launchDetails.setLastUpdated(new Date());

    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDetail_IT_FullECR.json", ClientDetails.class);

    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDetail_IT_FullECR.json", ClientDetails.class);

    clientDetails.setEncounterStartThreshold("1");
    clientDetails.setEncounterEndThreshold("2");
    launchDetails = new LaunchDetails();
    launchDetails.setId(1);
    launchDetails.setLaunchPatientId("patient-123");
  }

  @Test
  public void testGetLaunchDetailsById() {
    when(authDetailsService.getAuthDetailsById(1)).thenReturn(launchDetails);

    LaunchDetails result = launchController.getLaunchDetailsById(1);

    assertNotNull(result);
    assertEquals(Integer.valueOf(1), result.getId());

    verify(authDetailsService).getAuthDetailsById(1);
  }

  @Test
  public void testSaveLaunchDetails_NonMultiTenant() {

    launchDetails.setIsMultiTenantSystemLaunch(false);

    when(authDetailsService.saveOrUpdate(any())).thenReturn(launchDetails);

    LaunchDetails result = launchController.saveLaunchDetails(launchDetails);

    assertNotNull(result);
    assertFalse(result.getIsMultiTenantSystemLaunch());

    verify(tokenScheduler).scheduleJob(launchDetails);
    verify(workflowService)
        .handleWorkflowEvent(eq(EventTypes.WorkflowEvent.SOF_LAUNCH), eq(launchDetails));
  }

  @Test
  public void testSaveLaunchDetails_MultiTenant() {

    launchDetails.setIsMultiTenantSystemLaunch(true);

    when(authDetailsService.saveOrUpdate(any())).thenReturn(launchDetails);

    LaunchDetails result = launchController.saveLaunchDetails(launchDetails);

    assertNotNull(result);
    assertTrue(result.getIsMultiTenantSystemLaunch());

    verify(tokenScheduler, never()).scheduleJob(any());
    verify(workflowService)
        .handleWorkflowEvent(eq(EventTypes.WorkflowEvent.SOF_LAUNCH), eq(launchDetails));
  }

  @Test
  public void testTriggerDataFromEHR_Success() {

    when(authDetailsService.getAuthDetailsById(1)).thenReturn(launchDetails);

    String response = launchController.triggerDataFromEHR(1);

    assertEquals("Success", response);

    verify(triggerQueryService).getData(eq(launchDetails), any(Date.class), any(Date.class));
  }

  @Test
  public void testLoadingDataFromEHR_Success() {

    when(authDetailsService.getAuthDetailsById(1)).thenReturn(launchDetails);

    String response = launchController.loadingDataFromEHR(1);

    assertEquals("Success", response);

    verify(loadingQueryService).getData(eq(launchDetails), any(Date.class), any(Date.class));
  }

  @Test
  public void testInvokeSystemLaunch_Success() throws Exception {

    SystemLaunch systemLaunch = new SystemLaunch();
    systemLaunch.setPatientId("patient-1");
    systemLaunch.setEncounterId("enc-1");
    systemLaunch.setFhirServerURL("http://test");

    ClientDetails clientDetails = new ClientDetails();
    clientDetails.setClientId("cid");
    clientDetails.setScopes("scope");
    clientDetails.setAssigningAuthorityId("auth");
    clientDetails.setFhirServerBaseURL("http://base");
    clientDetails.setIsMultiTenantSystemLaunch(false);

    JSONObject metadata = new JSONObject();
    metadata.put("fhirVersion", "4.0.1");

    JSONArray restArray = new JSONArray();
    JSONObject restObj = new JSONObject();
    JSONObject security = new JSONObject();
    JSONArray extArray = new JSONArray();
    JSONObject extension = new JSONObject();
    extension.put("extension", new JSONArray());
    extArray.put(extension);
    security.put("extension", extArray);
    restObj.put("security", security);
    restArray.put(restObj);
    metadata.put("rest", restArray);

    JSONObject tokenResponse = new JSONObject();
    tokenResponse.put("access_token", "token123");
    tokenResponse.put("expires_in", 3600);

    when(clientDetailsService.getClientDetailsByUrl(any())).thenReturn(clientDetails);

    when(authorization.getMetadata(any())).thenReturn(metadata);

    when(tokenScheduler.getAccessTokenUsingClientDetails(any())).thenReturn(tokenResponse);

    when(request.getHeader("X-Request-ID")).thenReturn("REQ-1");

    doReturn(null).when(launchController).getEncounterById(any());

    doNothing().when(launchController).setStartAndEndDates(any(), any(), any());

    doReturn(new LaunchDetails()).when(launchController).saveLaunchDetails(any());

    String result = launchController.invokeSystemLaunch(systemLaunch, request, response);

    assertNotNull(result);
    assertEquals("App is launched successfully", result);

    verify(response).setStatus(HttpServletResponse.SC_ACCEPTED);
    verify(clientDetailsService).saveOrUpdate(any());
  }

  @Test(expected = ResponseStatusException.class)
  public void testInvokeSystemLaunch_ClientNotFound() throws Exception {

    SystemLaunch systemLaunch = new SystemLaunch();
    systemLaunch.setFhirServerURL("url");

    when(clientDetailsService.getClientDetailsByUrl(any())).thenReturn(null);

    launchController.invokeSystemLaunch(systemLaunch, request, response);
  }

  @Test(expected = ResponseStatusException.class)
  public void testInvokeSystemLaunch_Conflict() throws Exception {

    SystemLaunch systemLaunch = new SystemLaunch();
    systemLaunch.setPatientId("patient-1");
    systemLaunch.setEncounterId("enc-1");
    systemLaunch.setFhirServerURL("http://test");

    ClientDetails clientDetails = new ClientDetails();
    clientDetails.setIsMultiTenantSystemLaunch(false);

    JSONObject tokenResponse = new JSONObject();
    tokenResponse.put("access_token", "token123");
    tokenResponse.put("expires_in", 3600);

    when(clientDetailsService.getClientDetailsByUrl(any())).thenReturn(clientDetails);

    when(tokenScheduler.getAccessTokenUsingClientDetails(any())).thenReturn(tokenResponse);

    launchController.invokeSystemLaunch(systemLaunch, request, response);
  }

  @Test(expected = ResponseStatusException.class)
  public void testInvokeSystemLaunch_PatientNull() throws Exception {

    SystemLaunch systemLaunch = new SystemLaunch();
    systemLaunch.setPatientId(null);
    systemLaunch.setFhirServerURL("url");

    ClientDetails clientDetails = new ClientDetails();

    when(clientDetailsService.getClientDetailsByUrl(any())).thenReturn(clientDetails);

    JSONObject tokenResponse = new JSONObject();
    tokenResponse.put("access_token", "token123");
    tokenResponse.put("expires_in", 3600);

    when(tokenScheduler.getAccessTokenUsingClientDetails(any())).thenReturn(tokenResponse);

    launchController.invokeSystemLaunch(systemLaunch, request, response);
  }

  @Test
  public void testInvokeSystemLaunch_TokenNull() throws Exception {

    SystemLaunch systemLaunch = new SystemLaunch();
    systemLaunch.setPatientId("p1");
    systemLaunch.setEncounterId("e1");
    systemLaunch.setFhirServerURL("url");

    ClientDetails clientDetails = new ClientDetails();

    when(clientDetailsService.getClientDetailsByUrl(any())).thenReturn(clientDetails);

    when(tokenScheduler.getAccessTokenUsingClientDetails(any())).thenReturn(null);

    launchController.invokeSystemLaunch(systemLaunch, request, response);

    verify(response).sendError(HttpServletResponse.SC_BAD_REQUEST, "Error in Launching the App");
  }

  @Test
  public void testLaunchApp() throws Exception {

    String launch = "launch123";
    String iss = "http://fhirserver";

    when(request.getServerName()).thenReturn("localhost");
    when(request.getScheme()).thenReturn("https");
    when(request.getServerPort()).thenReturn(443);
    when(request.getContextPath()).thenReturn("/app");
    when(request.getHeader("X-Request-ID")).thenReturn("REQ-1");

    JSONObject metadata = new JSONObject();
    metadata.put("fhirVersion", "4.(.*).(.*)");

    JSONArray restArray = new JSONArray();
    JSONObject restObj = new JSONObject();
    JSONObject security = new JSONObject();
    JSONArray extArray = new JSONArray();

    JSONObject extension = new JSONObject();
    JSONArray innerExt = new JSONArray();

    JSONObject authExt = new JSONObject();
    authExt.put("url", "authorize");
    authExt.put("valueUri", "http://auth-url");

    JSONObject tokenExt = new JSONObject();
    tokenExt.put("url", "token");
    tokenExt.put("valueUri", "http://token-url");

    innerExt.put(authExt);
    innerExt.put(tokenExt);

    extension.put("extension", innerExt);
    extArray.put(extension);
    security.put("extension", extArray);
    restObj.put("security", security);
    restArray.put(restObj);
    metadata.put("rest", restArray);

    when(authorization.getMetadata(any())).thenReturn(metadata);

    ClientDetails clientDetails = new ClientDetails();
    clientDetails.setClientId("client123");
    clientDetails.setScopes("openid profile");
    clientDetails.setRequireAud(true);

    when(clientDetailsService.getClientDetailsByUrl(any())).thenReturn(clientDetails);

    when(authorization.createAuthUrl(any(), any(), anyInt()))
        .thenReturn("http://constructed-auth-url");

    doReturn(new LaunchDetails()).when(authDetailsService).saveOrUpdate(any());

    launchController.launchApp(launch, iss, request, response);

    verify(authorization).getMetadata(iss + "/metadata");

    verify(clientDetailsService).getClientDetailsByUrl(iss);

    verify(authorization).createAuthUrl(any(), eq(clientDetails), anyInt());

    verify(authDetailsService).saveOrUpdate(any());

    verify(response).sendRedirect("http://constructed-auth-url");
  }

  @Test
  public void testRedirectEndPoint_SuccessFlow() throws Exception {

    String code = "authCode123";
    String state = "1";

    LaunchDetails launchDetails = new LaunchDetails();
    launchDetails.setEhrServerURL("http://fhir");

    when(authDetailsService.getLaunchDetailsByState(1)).thenReturn(launchDetails);

    JSONObject tokenResponse = new JSONObject();
    tokenResponse.put("access_token", "token123");
    tokenResponse.put("expires_in", 3600);

    tokenResponse.put("patient", "patient1");
    tokenResponse.put("encounter", "enc1");

    when(authorization.getAccessToken(any())).thenReturn(tokenResponse);

    ClientDetails clientDetails = new ClientDetails();
    when(clientDetailsService.getClientDetailsByUrl(any())).thenReturn(clientDetails);

    doReturn(launchDetails).when(launchController).setLaunchDetails(any(), any(), any());

    doReturn(launchDetails).when(launchController).saveLaunchDetails(any());

    launchController.redirectEndPoint(code, state, request, response);

    verify(authDetailsService).getLaunchDetailsByState(1);
    verify(authorization).getAccessToken(any());
    verify(clientDetailsService).getClientDetailsByUrl(any());
    verify(launchController).saveLaunchDetails(any());
  }

  @Test(expected = Exception.class)
  public void testRedirectEndPoint_AccessTokenNull() throws Exception {

    when(authDetailsService.getLaunchDetailsByState(3)).thenReturn(new LaunchDetails());

    when(authorization.getAccessToken(any())).thenReturn(null);

    launchController.redirectEndPoint("code", "3", request, response);
  }

  @Test(expected = Exception.class)
  public void testRedirectEndPoint_LaunchDetailsNull() throws Exception {

    when(authDetailsService.getLaunchDetailsByState(4)).thenReturn(null);

    launchController.redirectEndPoint("code", "4", request, response);
  }

  @Test(expected = Exception.class)
  public void testRedirectEndPoint_CodeOrStateMissing() throws Exception {

    launchController.redirectEndPoint(null, null, request, response);
  }

  @Test
  public void testSetLaunchDetails() throws JSONException {

    LaunchDetails currentDetails = new LaunchDetails();

    JSONObject accessTokenObject = new JSONObject();
    accessTokenObject.put("access_token", "token123");
    accessTokenObject.put("refresh_token", "refresh123");
    accessTokenObject.put("user", "user1");
    accessTokenObject.put("expires_in", 3600);
    accessTokenObject.put("patient", "patient1");
    accessTokenObject.put("encounter", "enc1");

    accessTokenObject.put("PATIENT", "patient1");
    accessTokenObject.put("ENCOUNTER", "enc1");

    ClientDetails clientDetails = mock(ClientDetails.class);

    when(clientDetails.getAssigningAuthorityId()).thenReturn("authId");
    when(clientDetails.getDirectUser()).thenReturn("directUser");
    when(clientDetails.getDirectHost()).thenReturn("directHost");
    when(clientDetails.getDirectPwd()).thenReturn("directPwd"); // no encryption
    when(clientDetails.getDirectRecipientAddress()).thenReturn("recipient@test.com");
    when(clientDetails.getRestAPIURL()).thenReturn("http://rest");
    when(clientDetails.getIsCovid()).thenReturn(true);
    when(clientDetails.getDebugFhirQueryAndEicr()).thenReturn(true);

    LaunchController spyController = Mockito.spy(launchController);

    doReturn(null).when(spyController).getEncounterById(any());
    doNothing().when(spyController).setStartAndEndDates(any(), any(), any());

    LaunchDetails result =
        spyController.setLaunchDetails(currentDetails, accessTokenObject, clientDetails);

    assertNotNull(result);

    assertEquals("token123", result.getAccessToken());
    assertEquals("refresh123", result.getRefreshToken());
    assertEquals("user1", result.getUserId());
    assertEquals(3600, result.getExpiry().intValue());

    assertEquals("patient1", result.getLaunchPatientId());
    assertEquals("enc1", result.getEncounterId());

    assertEquals("authId", result.getAssigningAuthorityId());
    assertEquals("patient1|enc1", result.getSetId());
    assertEquals(3600, result.getExpiry().intValue());

    assertEquals("directUser", result.getDirectUser());
    assertEquals("directHost", result.getDirectHost());
    assertEquals("recipient@test.com", result.getDirectRecipient());
    assertEquals("http://rest", result.getRestAPIURL());

    assertTrue(result.getIsCovid());
    assertTrue(result.getDebugFhirQueryAndEicr());

    verify(spyController).getEncounterById(result);
    verify(spyController).setStartAndEndDates(eq(clientDetails), eq(result), isNull());
  }

  @Test
  public void testStartEndDateDSTU2_basedOnClientDetailThreshold()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    launchDetails.setFhirVersion("DSTU2");
    Date startDate =
        DateUtils.addHours(
            new Date(), Integer.parseInt(clientDetails.getEncounterStartThreshold()));
    Date endDate =
        DateUtils.addHours(new Date(), Integer.parseInt(clientDetails.getEncounterEndThreshold()));

    Encounter encounter = new Encounter();
    encounter.setPeriod(new PeriodDt());

    launchController.setStartAndEndDates(clientDetails, launchDetails, encounter);

    assertTrue(Math.abs(launchDetails.getStartDate().getTime() - startDate.getTime()) < 2000);
    assertTrue(Math.abs(launchDetails.getEndDate().getTime() - endDate.getTime()) < 2000);
  }

  @Test
  public void testStartEndDateDSTU2_basedOnEncounterPeriod()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    launchDetails.setFhirVersion("DSTU2");

    SimpleDateFormat sdf = new SimpleDateFormat("yyyMMddHHmmss");
    String startDateStr = "20200101010101";
    Date startDate = sdf.parse(startDateStr);
    String endDateStr = "20200102010101";
    Date endDate = sdf.parse(endDateStr);

    PeriodDt periodDt = new PeriodDt();
    periodDt.setStartWithSecondsPrecision(startDate);
    periodDt.setEndWithSecondsPrecision(endDate);
    Encounter encounter = new Encounter();
    encounter.setPeriod(periodDt);

    launchController.setStartAndEndDates(clientDetails, launchDetails, encounter);

    assertEquals(startDate, launchDetails.getStartDate());
    assertEquals(endDate, launchDetails.getEndDate());
  }

  @Test
  public void testStartEndDateR4_basedOnEncounterPeriod()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    launchDetails.setFhirVersion("R4");

    SimpleDateFormat sdf = new SimpleDateFormat("yyyMMddHHmmss");
    String startDateStr = "20200101010101";
    Date startDate = sdf.parse(startDateStr);
    String endDateStr = "20200102010101";
    Date endDate = sdf.parse(endDateStr);

    Period period = new Period();
    period.setStart(startDate);
    period.setEnd(endDate);
    org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(period);

    launchController.setStartAndEndDates(clientDetails, launchDetails, r4Encounter);

    assertEquals(launchDetails.getStartDate(), startDate);
    assertEquals(launchDetails.getEndDate(), endDate);
  }

  @Test
  public void testStartEndDateR4_basedOnClientDetailThreshold()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    launchDetails.setFhirVersion("R4");
    Date startDate =
        DateUtils.addHours(
            new Date(), Integer.parseInt(clientDetails.getEncounterStartThreshold()));
    Date endDate =
        DateUtils.addHours(new Date(), Integer.parseInt(clientDetails.getEncounterEndThreshold()));

    org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(new Period());

    launchController.setStartAndEndDates(clientDetails, launchDetails, r4Encounter);

    assertTrue(Math.abs(launchDetails.getStartDate().getTime() - startDate.getTime()) < 2000);
    assertTrue(Math.abs(launchDetails.getEndDate().getTime() - endDate.getTime()) < 2000);
  }

  @Test
  public void testStartEndDate_nullValue()
      throws JsonParseException, JsonMappingException, IOException, ParseException {

    launchDetails.setFhirVersion("DSTU3");
    org.hl7.fhir.r4.model.Encounter r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(new Period());

    launchController.setStartAndEndDates(clientDetails, launchDetails, r4Encounter);

    assertEquals(null, launchDetails.getStartDate());
    assertEquals(null, launchDetails.getEndDate());

    launchDetails.setFhirVersion("R4");
    r4Encounter = null;
    launchController.setStartAndEndDates(clientDetails, launchDetails, r4Encounter);

    assertEquals(null, launchDetails.getStartDate());
    assertEquals(null, launchDetails.getEndDate());

    launchDetails.setFhirVersion("DSTU3");
    r4Encounter = new org.hl7.fhir.r4.model.Encounter();
    r4Encounter.setPeriod(null);

    launchController.setStartAndEndDates(clientDetails, launchDetails, r4Encounter);

    assertEquals(null, launchDetails.getStartDate());
    assertEquals(null, launchDetails.getEndDate());
  }

  @Test
  public void testGetEncounterById() {

    LaunchDetails launchDetails = new LaunchDetails();

    FhirContext context = mock(FhirContext.class);
    IGenericClient client = mock(IGenericClient.class);
    IRead read = mock(IRead.class);
    IReadTyped<IBaseResource> readTyped = mock(IReadTyped.class);
    IReadExecutable<IBaseResource> readExec = mock(IReadExecutable.class);

    when(fhirContextInitializer.getFhirContext(any())).thenReturn(context);

    when(fhirContextInitializer.createClient(any(), any(), any())).thenReturn(client);

    launchDetails.setFhirVersion("R4");
    launchDetails.setEncounterId("enc1");

    org.hl7.fhir.r4.model.Encounter encounter = new org.hl7.fhir.r4.model.Encounter();
    encounter.setId("enc1");

    when(client.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readTyped);
    when(readTyped.withId("enc1")).thenReturn(readExec);
    when(readExec.execute()).thenReturn(encounter);

    IBaseResource result = launchController.getEncounterById(launchDetails);

    assertNotNull(result);
    assertEquals("enc1", result.getIdElement().getIdPart());

    launchDetails.setEncounterId(null);

    Bundle bundle = new Bundle();

    org.hl7.fhir.r4.model.Encounter e1 = new org.hl7.fhir.r4.model.Encounter();
    e1.setId("e1");
    e1.getMeta().setLastUpdated(new Date(System.currentTimeMillis() - 10000));

    org.hl7.fhir.r4.model.Encounter e2 = new org.hl7.fhir.r4.model.Encounter();
    e2.setId("e2");
    e2.getMeta().setLastUpdated(new Date());

    bundle.addEntry().setResource(e1);
    bundle.addEntry().setResource(e2);

    when(fhirContextInitializer.getResourceByPatientId(any(), any(), any(), eq("Encounter")))
        .thenReturn(bundle);

    result = launchController.getEncounterById(launchDetails);

    assertNotNull(result);
    assertEquals("e2", result.getIdElement().getIdPart());
    assertEquals("e2", launchDetails.getEncounterId());

    launchDetails.setFhirVersion("DSTU2");
    launchDetails.setEncounterId(null);

    ca.uhn.fhir.model.dstu2.resource.Bundle dstu2Bundle =
        new ca.uhn.fhir.model.dstu2.resource.Bundle();

    ca.uhn.fhir.model.dstu2.resource.Encounter d1 =
        new ca.uhn.fhir.model.dstu2.resource.Encounter();
    d1.setId("d1");
    d1.getMeta().setLastUpdated(new Date(System.currentTimeMillis() - 10000));

    ca.uhn.fhir.model.dstu2.resource.Encounter d2 =
        new ca.uhn.fhir.model.dstu2.resource.Encounter();
    d2.setId("d2");
    d2.getMeta().setLastUpdated(new Date());

    dstu2Bundle.addEntry().setResource(d1);
    dstu2Bundle.addEntry().setResource(d2);

    when(fhirContextInitializer.getResourceByPatientId(any(), any(), any(), eq("Encounter")))
        .thenReturn(dstu2Bundle);

    result = launchController.getEncounterById(launchDetails);

    assertNotNull(result);
    assertEquals("d2", result.getIdElement().getIdPart());
    assertEquals("d2", launchDetails.getEncounterId());

    launchDetails.setFhirVersion("R4");
    launchDetails.setEncounterId("encX");

    when(client.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readTyped);
    when(readTyped.withId("encX")).thenReturn(readExec);
    when(readExec.execute()).thenThrow(new ResourceNotFoundException("Not found"));

    try {
      launchController.getEncounterById(launchDetails);
      fail("Expected ResponseStatusException");
    } catch (ResponseStatusException ex) {
      assertEquals(404, ex.getStatusCode().value());
    }

    when(fhirContextInitializer.getFhirContext(any())).thenThrow(new RuntimeException("Boom"));

    try {
      launchController.getEncounterById(launchDetails);
      fail("Expected ResponseStatusException");
    } catch (ResponseStatusException ex) {
      assertEquals(500, ex.getStatusCode().value());
    }
  }
}
