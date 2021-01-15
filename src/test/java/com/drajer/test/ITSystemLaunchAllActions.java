package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static org.junit.Assert.*;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.*;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

/**
 * Description: This test class is created for testing all the actions. ersd.json for this test is
 * modified with actions as below. MatchTrigger - Will execute without any dependency. Create_EICR -
 * Will execute after 2 seconds of MatchTrigger action. Periodic_EICR - will execute after 10
 * seconds of MatchTrigger action. with frequency 1 with interval of 2 seconds. CloseOut_EICR - will
 * execute immediately after CreateEicr action is completed. with frequency 1 with interval of 2
 * seconds. Validate_EICR - will execute immediately after each EICR created with above action.
 * Submit_EICR - will execute immediately after ValidateEICR.
 */
@RunWith(Parameterized.class)
@TestPropertySource(properties = "ersd.file.location=src/test/resources/AppData/ersd.json")
public class ITSystemLaunchAllActions extends BaseIntegrationTest {

  private String testCaseId;
  private Map<String, String> testData;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;

  public ITSystemLaunchAllActions(
      String testCaseId,
      Map<String, String> testData,
      Map<String, ?> resourceMapping,
      Map<String, ?> otherMapping) {
    this.testCaseId = testCaseId;
    this.testData = testData;
    this.allResourceMapping = resourceMapping;
    this.allOtherMapping = otherMapping;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITSystemLaunchAllActions.class);
  private String systemLaunchPayload;
  private LaunchDetails launchDetails;
  private PatientExecutionState state;

  private Eicr createEicr;
  private Eicr closeOutEicr;
  private List<Eicr> periodicEicr;

  WireMockHelper stubHelper;

  @Before
  public void launchTestSetUp() throws IOException {

    logger.info("Executing Test {}: ", testCaseId);
    tx = session.beginTransaction();

    // Data Setup
    createClientDetails(testData.get("ClientDataToBeSaved"));
    systemLaunchPayload = getSystemLaunchPayload(testData.get("SystemLaunchPayload"));
    session.flush();
    tx.commit();

    // Setup wireMock and mock FHIR call as per yaml file.
    stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
    logger.info("Creating WireMock stubs..");
    stubHelper.stubResources(allResourceMapping);
    stubHelper.stubAuthAndMetadata(allOtherMapping);
  }

  @Parameters(name = "{0}")
  public static Collection<Object[]> data() {

    TestDataGenerator testDataGenerator =
        new TestDataGenerator("test-yaml/systemLaunchAllActionsTest.yaml");

    Set<String> testCaseSet = testDataGenerator.getAllTestCases();
    Object[][] data = new Object[testCaseSet.size()][4];

    int count = 0;
    for (String testCase : testCaseSet) {
      data[count][0] = testCase;
      data[count][1] = testDataGenerator.getTestCaseByID(testCase).getTestData();
      data[count][2] = testDataGenerator.getResourceMappings(testCase);
      data[count][3] = testDataGenerator.getOtherMappings(testCase);
      count++;
    }

    return Arrays.asList(data);
  }

  @Test
  public void testSystemLaunchAllActions() {

    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayload);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
    waitForEICR(50000);
    getLaunchDetailAndStatus();
    validateActionStatus();
  }

  @Test
  public void testSubmitEicrFromRestApi() {
    URL restApiUrl = null;
    try {
      restApiUrl = new URL(clientDetails.getRestAPIURL());
    } catch (MalformedURLException e) {
      fail(e.getMessage() + " This exception is not expected fix the test.");
    }

    // mock Authorization
    String accesstoken =
        "{\"access_token\":\"eyJraWQiOiIy\",\"scope\":\"system\\/MedicationRequest.read\",\"token_type\":\"Bearer\",\"expires_in\":570}";
    StringBuilder sb = new StringBuilder(200);
    sb.append("{\"client_id\":\"");
    sb.append(clientDetails.getClientId());
    sb.append("\",\"client_secret\":\"");
    sb.append(clientDetails.getClientSecret());
    sb.append("\"}");

    wireMockServer.stubFor(
        post(urlPathEqualTo(restApiUrl.getPath()))
            .withRequestBody(equalToJson(sb.toString()))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody(accesstoken)
                    .withHeader("Content-Type", "application/json; charset=utf-8")));

    // mock RESTAPI
    JSONObject jsonObject = new JSONObject(systemLaunchPayload);

    StringBuilder sb1 = new StringBuilder(200);
    sb1.append("{\"fhirServerURL\":\"");
    sb1.append(clientDetails.getFhirServerBaseURL());
    sb1.append("\",\"patientId\":\"");
    sb1.append(jsonObject.get("patientId"));
    sb1.append("\",\"encounterId\":\"");
    sb1.append(jsonObject.get("encounterId"));
    sb1.append("\",\"ecrRequestId\":\"");
    sb1.append(testCaseId);
    sb1.append("\"}");

    String restResponse = "{\"status\":\"success\"}";
    wireMockServer.stubFor(
        post(urlPathEqualTo(restApiUrl.getPath()))
            .withRequestBody(equalToJson(sb1.toString()))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody(restResponse)
                    .withHeader("Content-Type", "application/json; charset=utf-8")));

    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayload);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
    waitForEICR(50000);
    getLaunchDetailAndStatus();
    validateActionStatus();

    wireMockServer.verify(
        postRequestedFor(urlEqualTo(restApiUrl.getPath()))
            .withRequestBody(equalToJson(sb.toString())));
    wireMockServer.verify(
        postRequestedFor(urlPathEqualTo(restApiUrl.getPath()))
            .withRequestBody(equalToJson(sb.toString())));
  }

  @Test
  public void testNoEicrWhenMissingJurisdiction() throws InterruptedException {

    wireMockServer.resetMappings();
    Map<String, ?> modifiedMapping = new HashMap<>(allResourceMapping);
    modifiedMapping.remove("Organization");
    stubHelper.stubResources(modifiedMapping);
    stubHelper.stubAuthAndMetadata(allOtherMapping);

    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayload);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
    waitForEICR(50000);
    getLaunchDetailAndStatus();
    validateMatchedTriggerStatus(JobStatus.COMPLETED);
    validateCreateEICR(JobStatus.SCHEDULED, false);
    List<Eicr> allEICRDocuments = getAllEICRDocuments();
    assertEquals(0, allEICRDocuments.size());
  }

  private void getLaunchDetailAndStatus() {
    try {
      Criteria criteria = session.createCriteria(LaunchDetails.class);
      criteria.add(Restrictions.eq("xRequestId", testCaseId));
      launchDetails = (LaunchDetails) criteria.uniqueResult();

      state = mapper.readValue(launchDetails.getStatus(), PatientExecutionState.class);
      session.refresh(launchDetails);

    } catch (Exception e) {
      logger.error("Exception occurred retrieving launchDetail and status", e);
      fail("Something went wrong with launch status, check the log");
    }
  }

  private void validateActionStatus() {
    try {
      // MatchTrigger Action
      validateMatchedTriggerStatus(JobStatus.COMPLETED);

      // CreateEICR Action
      validateCreateEICR(JobStatus.COMPLETED, true);

      // Periodic EICR Action
      validatePeriodicEICR(JobStatus.COMPLETED, 1, true);

      // CloseOut EICR Action
      validateCloseOut(JobStatus.COMPLETED, true);

      // Validate EICR Action
      validateValidateStatus(JobStatus.COMPLETED, 3);

      // Submit EICR Action
      validateSubmitStatus(JobStatus.COMPLETED, 3);

    } catch (Exception e) {
      fail(e.getMessage() + "Error while retrieving action status");
    }
  }

  private void validateMatchedTriggerStatus(JobStatus status) {
    MatchTriggerStatus matchTriggerStatus = state.getMatchTriggerStatus();
    assertEquals(status, matchTriggerStatus.getJobStatus());
    assertNotNull(matchTriggerStatus.getMatchedCodes());
    assertTrue(matchTriggerStatus.getMatchedCodes().size() > 0);
  }

  private void validateCreateEICR(JobStatus status, Boolean validateEICR) {
    CreateEicrStatus createEicrStatus = state.getCreateEicrStatus();
    assertEquals(status, createEicrStatus.getJobStatus());
    if (validateEICR) {
      assertTrue(createEicrStatus.getEicrCreated());
      validateEICR(createEicrStatus.geteICRId());
    } else {
      assertFalse(createEicrStatus.getEicrCreated());
    }
  }

  private void validatePeriodicEICR(JobStatus status, int periodicEICRCount, Boolean validateEICR) {
    assertNotNull(state.getPeriodicUpdateStatus());
    assertEquals(periodicEICRCount, state.getPeriodicUpdateStatus().size());
    for (PeriodicUpdateEicrStatus periodicStatus : state.getPeriodicUpdateStatus()) {
      assertEquals(status, periodicStatus.getJobStatus());
      if (validateEICR) {
        assertTrue(periodicStatus.getEicrUpdated());
        validateEICR(periodicStatus.geteICRId());
      } else {
        assertFalse(periodicStatus.getEicrUpdated());
      }
    }
  }

  private void validateCloseOut(JobStatus status, Boolean validateEICR) {
    CloseOutEicrStatus closeEICRStatus = state.getCloseOutEicrStatus();
    assertEquals(status, closeEICRStatus.getJobStatus());
    if (validateEICR) {
      assertTrue(closeEICRStatus.getEicrClosed());
      validateEICR(closeEICRStatus.geteICRId());
    } else {
      assertFalse(closeEICRStatus.getEicrClosed());
    }
  }

  private void validateValidateStatus(JobStatus status, int validateEICRCount) {
    assertNotNull(state.getValidateEicrStatus());
    assertEquals(validateEICRCount, state.getValidateEicrStatus().size());
    for (ValidateEicrStatus valStatus : state.getValidateEicrStatus()) {
      assertEquals(status, valStatus.getJobStatus());
      assertTrue(valStatus.getEicrValidated());
    }
  }

  private void validateSubmitStatus(JobStatus status, int submitEICRCount) {
    assertNotNull(state.getSubmitEicrStatus());
    assertEquals(submitEICRCount, state.getSubmitEicrStatus().size());
    for (SubmitEicrStatus submitStatus : state.getSubmitEicrStatus()) {
      assertEquals(status, submitStatus.getJobStatus());
    }
  }

  private void validateEICR(String eICRId) {
    assertNotNull(eICRId);
    assertFalse(eICRId.isEmpty());
    Eicr eicr = getEICRDocument(eICRId);
    assertNotNull(eicr);
    assertNotNull(eicr.getEicrData());
    assertFalse(eicr.getEicrData().isEmpty());
  }

  private Eicr getEICRDocument(String eicrId) {
    try {
      return session.get(Eicr.class, Integer.parseInt(eicrId));
    } catch (Exception e) {
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }

  private List<Eicr> getAllEICRDocuments() {
    try {

      Criteria criteria = session.createCriteria(Eicr.class);
      return criteria.list();
    } catch (Exception e) {
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }

  private void waitForEICR(int interval) {
    try {
      Thread.sleep(interval);
    } catch (InterruptedException e) {
      logger.warn("Issue with thread sleep", e);
    }
  }
}
