package com.drajer.ecr.it;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.stubFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.eca.model.PeriodicUpdateEicrStatus;
import com.drajer.eca.model.SubmitEicrStatus;
import com.drajer.eca.model.ValidateEicrStatus;
import com.drajer.ecr.it.common.BaseIntegrationTest;
import com.drajer.ecr.it.common.WireMockHelper;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;

@TestPropertySource(properties = "ersd.file.location=src/test/resources/AppData/ersd.json")
public class ITSystemLaunchAllActions extends BaseIntegrationTest {

  private String testCaseId;

  public ITSystemLaunchAllActions(String testCaseId) {
    this.testCaseId = testCaseId;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITSystemLaunchAllActions.class);

  private final String systemLaunchURI = "/api/systemLaunch";

  private static String systemLaunchInputData;
  private String patientId;
  private String encounterID;
  private String fhirServerUrl;

  static TestDataGenerator testDataGenerator;
  String clientDetailsFile;
  String systemLaunchFile;
  String expectedEICRFile;

  private LaunchDetails launchDetails;
  private PatientExecutionState state;

  private Eicr createEicr;
  private Eicr closeOutEicr;
  private List<Eicr> periodicEicr;

  List<String> validationSectionList;
  Map<String, List<String>> allResourceFiles;

  WireMockHelper stubHelper;

  @Before
  public void launchTestSetUp() throws IOException {
    logger.info("Executing Tests with TestCase: " + testCaseId);
    tx = session.beginTransaction();
    // Retrieve test data from TestSystemLaunchAllActions.yaml file
    clientDetailsFile = testDataGenerator.getTestFile(testCaseId, "ClientDataToBeSaved");
    systemLaunchFile = testDataGenerator.getTestFile(testCaseId, "SystemLaunchPayload");
    validationSectionList =
        Arrays.asList(testDataGenerator.getValidationSections(testCaseId).split("\\|"));
    allResourceFiles = testDataGenerator.getResourceFiles(testCaseId);

    // Data Setup
    createTestClientDetailsInDB(clientDetailsFile);
    systemLaunchInputData = getSystemLaunchInputData(systemLaunchFile);
    JSONObject jsonObject = new JSONObject(systemLaunchInputData);
    patientId = (String) jsonObject.get("patientId");
    encounterID = (String) jsonObject.get("encounterId");
    fhirServerUrl = (String) jsonObject.get("fhirServerURL");

    session.flush();
    tx.commit();

    // Setup wireMock and mock FHIR call as per yaml file.
    stubHelper = new WireMockHelper(baseUrl, wireMockHttpPort);
    logger.info("Creating wiremockstubs..");
    stubHelper.stubResources(testDataGenerator.getResourceMappings(testCaseId));
    stubHelper.stubAuthAndMetadata(testDataGenerator.getOtherMappings(testCaseId));
  }

  @After
  public void cleanUp() {
    if (stubHelper != null) {
      stubHelper.stopMockServer();
    }
  }

  @Parameters(name = "{index}: {0}")
  public static Collection<Object[]> data() {
    testDataGenerator = new TestDataGenerator("TestSystemLaunchAllActions.yaml");
    Set<String> testCaseSet = testDataGenerator.getAllTestCases();
    Object[][] data = new Object[testCaseSet.size()][1];
    int count = 0;
    for (String testCase : testCaseSet) {
      data[count][0] = testCase;
      count++;
    }

    return Arrays.asList(data);
  }

  @Test
  public void testSystemLaunch() throws Exception {

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(systemLaunchInputData, headers);
    logger.info("Invoking systemLaunch...");
    logger.info("Payload: \n" + systemLaunchInputData);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort(systemLaunchURI), HttpMethod.POST, entity, String.class);
    logger.info("Received Response. Waiting for EICR generation.....");

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    validateActionStatus();
  }

  // @Test
  public void testSubmitEicrFromRestApi() {

    // mock RESTAPI
    stubFor(
        post(urlEqualTo("/directurl"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody("Reportability Response recieved from AIMS")
                    .withHeader("Content-Type", "application/json; charset=utf-8")));
    // mock EHR AuthURl
    String accesstoken =
        "{\"access_token\":\"eyJraWQiOiIy\",\"scope\":\"system\\/MedicationRequest.read\",\"token_type\":\"Bearer\",\"expires_in\":570}";
    stubFor(
        post(urlEqualTo("/ehrauthurl"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody(accesstoken)
                    .withHeader("Content-Type", "application/json; charset=utf-8")));

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(systemLaunchInputData, headers);
    logger.info("Invoking systemLaunch...");
    logger.info("Payload: \n" + systemLaunchInputData);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort(systemLaunchURI), HttpMethod.POST, entity, String.class);
    logger.info("Received Response. Waiting for EICR generation.....");

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    validateActionStatus();

    verify(postRequestedFor(urlEqualTo("/ehrauthurl")));
    verify(postRequestedFor(urlPathEqualTo("/directurl")));
  }

  private void getLaunchDetailAndStatus() {

    try {
      Criteria criteria = session.createCriteria(LaunchDetails.class);
      criteria.add(Restrictions.eq("ehrServerURL", fhirServerUrl));
      criteria.add(Restrictions.eq("launchPatientId", patientId));
      criteria.add(Restrictions.eq("encounterId", encounterID));
      launchDetails = (LaunchDetails) criteria.uniqueResult();

      state = mapper.readValue(launchDetails.getStatus(), PatientExecutionState.class);
      session.refresh(launchDetails);

    } catch (Exception e) {

      fail(e.getMessage() + "Exception occured retreving launchdetail and status");
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
    assertTrue((state.getCreateEicrStatus().geteICRId() != ""));
    createEicr = getEicrDocument(state.getCreateEicrStatus().geteICRId());
    assertNotNull(createEicr.getEicrData());
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
        assertTrue((periodicStatus.geteICRId() != ""));
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
    assertTrue((state.getCloseOutEicrStatus().geteICRId() != ""));
    closeOutEicr = getEicrDocument(state.getCloseOutEicrStatus().geteICRId());
    assertNotNull(closeOutEicr.getEicrData());
    assertTrue(state.getCloseOutEicrStatus().getEicrClosed());
  }

  private void validateValidateStatus() throws InterruptedException {
    Thread.sleep(2000);
    getLaunchDetailAndStatus();
    if (state.getValidateEicrStatus() != null) {
      for (ValidateEicrStatus valStatus : state.getValidateEicrStatus()) {
        assertEquals(valStatus.getJobStatus(), JobStatus.COMPLETED);
        assertTrue(valStatus.getEicrValidated());
      }
    }
  }

  private void validateSubmitStatus() throws InterruptedException {
    Thread.sleep(2000);
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
      fail(e.getMessage() + "Error while retrieving EICR document");
    }
    return null;
  }
}
