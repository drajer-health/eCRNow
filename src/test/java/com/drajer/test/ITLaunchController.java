package com.drajer.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@RunWith(Parameterized.class)
public class ITLaunchController extends BaseIntegrationTest {

  private String testCaseId;

  public ITLaunchController(String testCaseId) {
    this.testCaseId = testCaseId;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITLaunchController.class);

  private final String systemLaunchURI = "/api/systemLaunch";

  private static String systemLaunchInputData;
  private static TestDataGenerator testDataGenerator;
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
    clientDetailsFile = testDataGenerator.getTestData(testCaseId, "ClientDataToBeSaved");
    systemLaunchFile = testDataGenerator.getTestData(testCaseId, "SystemLaunchPayload");

    createClientDetails(clientDetailsFile);
    systemLaunchInputData = getSystemLaunchInputData(systemLaunchFile);

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

  @Parameters(name = "{0}")
  public static Collection<Object[]> data() {
    testDataGenerator = new TestDataGenerator("test-yaml/launchControllerTest.yaml");
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
    headers.add("X-Request-ID", testCaseId);

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
    assertNotNull(createEicr.getEicrData());
  }

  private void getLaunchDetailAndStatus() {

    try {
      Criteria criteria = session.createCriteria(LaunchDetails.class);
      criteria.add(Restrictions.eq("xRequestId", testCaseId));
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

        // Minimum 2 sec is required as App will execute
        // createEicr workflow after 2 sec as per eRSD.
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
