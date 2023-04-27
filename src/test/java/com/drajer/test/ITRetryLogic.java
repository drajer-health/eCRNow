package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(Parameterized.class)
public class ITRetryLogic extends BaseIntegrationTest {

  private String testCaseId;
  private Map<String, String> testData;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;

  public ITRetryLogic(
      String testCaseId,
      Map<String, String> testData,
      Map<String, ?> resourceMapping,
      Map<String, ?> otherMapping) {
    this.testCaseId = testCaseId;
    this.testData = testData;
    this.allResourceMapping = resourceMapping;
    this.allOtherMapping = otherMapping;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITRetryLogic.class);
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
    mockRestApi();

    String response = TestUtils.getFileContentAsString("R4/Patient/Patient_12742571.json");
    wireMockServer.stubFor(
        post(urlEqualTo("http://localhost:9011/FHIR/Encounter?patient=12742571"))
            .atPriority(1)
            .willReturn(aResponse().withStatus(202).withBody(response)));
  }

  @Parameterized.Parameters(name = "{0}")
  public static Collection<Object[]> data() {
    TestDataGenerator testDataGenerator = new TestDataGenerator("test-yaml/fhirRetryReadTest.yaml");

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
  public void testFhirRetryUrl() {
    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayLoad);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    wireMockServer.verify(
        moreThanOrExactly(3), getRequestedFor(urlEqualTo("/FHIR/Encounter/97953900")));
  }
}
