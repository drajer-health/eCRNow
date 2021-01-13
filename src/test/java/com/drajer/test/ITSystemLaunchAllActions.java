package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static org.junit.Assert.*;

import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.eca.model.PeriodicUpdateEicrStatus;
import com.drajer.eca.model.SubmitEicrStatus;
import com.drajer.eca.model.ValidateEicrStatus;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
  public void testSystemLaunch() {

    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayload);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
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
    validateActionStatus();

    wireMockServer.verify(
        postRequestedFor(urlEqualTo(restApiUrl.getPath()))
            .withRequestBody(equalToJson(sb.toString())));
    wireMockServer.verify(
        postRequestedFor(urlPathEqualTo(restApiUrl.getPath()))
            .withRequestBody(equalToJson(sb.toString())));
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
      validateMatchedTriggerStatus();

      // CreateEicr Action
      validateCreateEicrStatus();

      // Periodic Eicr Action
      // validatePeriodicEicrStatus();

      // CloseOut Eicr Action
      validateCloseOutStatus();

      // Validate Eicr Action
      validateValidateStatus();

      // Submit Eicr Action
      validateSubmitStatus();

    } catch (Exception e) {
      fail(e.getMessage() + "Error while retrieving action status");
    }
  }

  private void validateMatchedTriggerStatus() {
    do {

      getLaunchDetailAndStatus();

    } while (state.getMatchTriggerStatus().getJobStatus() != JobStatus.COMPLETED);

    assertNotNull(state.getMatchTriggerStatus().getMatchedCodes());
    assertTrue(state.getMatchTriggerStatus().getMatchedCodes().size() > 0);
  }

  private void validateCreateEicrStatus() throws InterruptedException {
    do {

      Thread.sleep(2000);
      getLaunchDetailAndStatus();

    } while (state.getCreateEicrStatus().getJobStatus() != JobStatus.COMPLETED);
    assertNotNull(state.getCreateEicrStatus().geteICRId());
    assertFalse(state.getCreateEicrStatus().geteICRId().isEmpty());
    createEicr = getEicrDocument(state.getCreateEicrStatus().geteICRId());
    assertNotNull(createEicr.getEicrData());
    assertFalse(createEicr.getEicrData().isEmpty());
    assertTrue(state.getCreateEicrStatus().getEicrCreated());
  }

  private void validatePeriodicEicrStatus() throws InterruptedException {
    do {

      Thread.sleep(2000);
      getLaunchDetailAndStatus();

    } while (state.getPeriodicUpdateJobStatus() != JobStatus.COMPLETED);

    if (state.getPeriodicUpdateStatus() != null) {
      for (PeriodicUpdateEicrStatus periodicStatus : state.getPeriodicUpdateStatus()) {
        assertEquals(JobStatus.COMPLETED, periodicStatus.getJobStatus());
        assertTrue(periodicStatus.getEicrUpdated());
        assertNotNull(periodicStatus.geteICRId());
        assertFalse(periodicStatus.geteICRId().isEmpty());
        Eicr eicr = getEicrDocument(periodicStatus.geteICRId());
        assertNotNull(eicr.getEicrData());
        periodicEicr.add(eicr);
      }
    }
  }

  private void validateCloseOutStatus() throws InterruptedException {
    do {

      Thread.sleep(2000);
      getLaunchDetailAndStatus();

    } while (state.getCloseOutEicrStatus().getJobStatus() != JobStatus.COMPLETED);
    assertNotNull(state.getCloseOutEicrStatus().geteICRId());
    assertFalse(state.getCloseOutEicrStatus().geteICRId().isEmpty());
    closeOutEicr = getEicrDocument(state.getCloseOutEicrStatus().geteICRId());
    assertNotNull(closeOutEicr.getEicrData());
    assertFalse(closeOutEicr.getEicrData().isEmpty());
    assertTrue(state.getCloseOutEicrStatus().getEicrClosed());
  }

  private void validateValidateStatus() throws InterruptedException {
    Thread.sleep(4000);
    getLaunchDetailAndStatus();
    if (state.getValidateEicrStatus() != null) {
      for (ValidateEicrStatus valStatus : state.getValidateEicrStatus()) {
        assertEquals(valStatus.getJobStatus(), JobStatus.COMPLETED);
        assertTrue(valStatus.getEicrValidated());
      }
    }
  }

  private void validateSubmitStatus() throws InterruptedException {
    Thread.sleep(4000);
    getLaunchDetailAndStatus();
    if (state.getSubmitEicrStatus() != null) {
      for (SubmitEicrStatus submitStatus : state.getSubmitEicrStatus()) {
        assertEquals(submitStatus.getJobStatus(), JobStatus.COMPLETED);
      }
    }
  }

  private Eicr getEicrDocument(String eicrId) {
    try {
      return session.get(Eicr.class, Integer.parseInt(eicrId));
    } catch (Exception e) {
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }
}
