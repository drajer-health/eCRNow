package com.drajer.test;

import static org.junit.Assert.*;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(Parameterized.class)
public class ITSystemLaunch extends BaseIntegrationTest {

  private String testCaseId;
  private Map<String, String> testData;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;

  public ITSystemLaunch(
      String testCaseId,
      Map<String, String> testData,
      Map<String, ?> resourceMapping,
      Map<String, ?> otherMapping) {
    this.testCaseId = testCaseId;
    this.testData = testData;
    this.allResourceMapping = resourceMapping;
    this.allOtherMapping = otherMapping;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITSystemLaunch.class);
  private String systemLaunchPayLoad;
  private LaunchDetails launchDetails;
  private PatientExecutionState state;
  WireMockHelper stubHelper;

  @Before
  public void launchTestSetUp() throws IOException {
    logger.info("Executing test: {}", testCaseId);
    tx = session.beginTransaction();

    // Data Setup
    createClientDetails(testData.get("ClientDataToBeSaved"));
    systemLaunchPayLoad = getSystemLaunchPayload(testData.get("SystemLaunchPayload"));
    session.flush();
    tx.commit();

    stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
    logger.info("Creating WireMock stubs..");
    stubHelper.stubResources(allResourceMapping);
    stubHelper.stubAuthAndMetadata(allOtherMapping);
  }

  @Parameters(name = "{0}")
  public static Collection<Object[]> data() {
    TestDataGenerator testDataGenerator = new TestDataGenerator("test-yaml/systemLaunchTest.yaml");

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
    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayLoad);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
    Eicr createEicr = getCreateEicrDocument();
    if (createEicr != null) {
      assertNotNull(createEicr.getEicrData());
      assertFalse(createEicr.getEicrData().isEmpty());
    }
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

  private Eicr getCreateEicrDocument() {
    try {
      do {
        // Minimum 2 sec is required as App will execute
        // createEicr workflow after 2 sec as per eRSD.
        Thread.sleep(2000);
        getLaunchDetailAndStatus();
      } while (!state.getCreateEicrStatus().getEicrCreated());

      return (session.get(
          Eicr.class,
          Integer.parseInt(
              state.getCreateEicrStatus() != null ? state.getCreateEicrStatus().geteICRId() : "")));

    } catch (Exception e) {
      logger.error("Exception occurred retrieving launchDetail or Eicr", e);
      fail("Something went wrong with Eicr creation, check the log");
    }
    return null;
  }
}
