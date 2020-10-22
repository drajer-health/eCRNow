package com.drajer.ecr.it;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecr.it.common.BaseIntegrationTest;
import com.drajer.ecr.it.common.WireMockHelper;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.EICRValidator;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.ValidationUtils;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.hl7.v3.POCDMT000040ClinicalDocument;
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

public class ITLaunchController extends BaseIntegrationTest {

  private String testCaseId;

  public ITLaunchController(String testCaseId) {
    this.testCaseId = testCaseId;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITLaunchController.class);

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

  List<String> validationSectionList;
  Map<String, List<String>> allResourceFiles;

  WireMockHelper stubHelper;

  @Before
  public void launchTestSetUp() throws IOException {
    logger.info("Executing Tests with TestCase: " + testCaseId);
    tx = session.beginTransaction();
    clientDetailsFile = testDataGenerator.getTestFile(testCaseId, "ClientDataToBeSaved");
    systemLaunchFile = testDataGenerator.getTestFile(testCaseId, "SystemLaunchPayload");
    expectedEICRFile = testDataGenerator.getTestFile(testCaseId, "ExpectedEICRFile");
    validationSectionList =
        Arrays.asList(testDataGenerator.getValidationSections(testCaseId).split("\\|"));
    allResourceFiles = testDataGenerator.getResourceFiles(testCaseId);

    // Data Setup
    createTestClientDetailsInDB(clientDetailsFile);
    systemLaunchInputData = getSystemLaunchInputData(systemLaunchFile);
    JSONObject jsonObject = new JSONObject(systemLaunchInputData);
    patientId = (String) jsonObject.get("patientId");
    if (jsonObject.get("encounterId") instanceof String) {
      encounterID = (String) jsonObject.get("encounterId");
    }
    fhirServerUrl = (String) jsonObject.get("fhirServerURL");

    session.flush();
    tx.commit();

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
    testDataGenerator = new TestDataGenerator("TestSystemLaunch.yaml");
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

    Eicr createEicr = getCreateEicrDocument();
    assertNotNull(createEicr.getData());

    getLaunchDetailAndStatus();
    ValidationUtils.setLaunchDetails(launchDetails);

    POCDMT000040ClinicalDocument clinicalDoc = TestUtils.getClinicalDocXml(createEicr);

    EICRValidator.validate(clinicalDoc, validationSectionList, allResourceFiles);
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

  private Eicr getCreateEicrDocument() {

    try {

      do {

        // Minimum 4 sec is required as App will execute
        // createEicr workflow after 3 sec as per eRSD.
        Thread.sleep(2000);
        getLaunchDetailAndStatus();

      } while (!state.getCreateEicrStatus().getEicrCreated());

      return (session.get(Eicr.class, Integer.parseInt(state.getCreateEicrStatus().geteICRId())));

    } catch (Exception e) {
      fail(e.getMessage() + "Error while retrieving EICR document");
    }
    return null;
  }
}
