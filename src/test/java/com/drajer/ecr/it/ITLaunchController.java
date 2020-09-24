package com.drajer.ecr.it;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecr.it.common.BaseIntegrationTest;
import com.drajer.ecr.it.common.WireMockHelper;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.hibernate.Criteria;
import org.hibernate.Query;
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
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

public class ITLaunchController extends BaseIntegrationTest {

  private String testCaseId;

  public ITLaunchController(String testCaseId) {
    this.testCaseId = testCaseId;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITLaunchController.class);

  static final Set<Integer> transactionalEntrySet = new HashSet<>(Arrays.asList(10, 13, 202, 244));

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

  WireMockHelper stubHelper;

  @Before
  public void launchTestSetUp() throws IOException {
    logger.info("Executing Tests with TestCase: " + testCaseId);
    tx = session.beginTransaction();
    clientDetailsFile = testDataGenerator.getTestFile(testCaseId, "ClientDataToBeSaved");
    systemLaunchFile = testDataGenerator.getTestFile(testCaseId, "SystemLaunchPayload");
    expectedEICRFile = testDataGenerator.getTestFile(testCaseId, "ExpectedEICRFile");

    // Data Setup
    createTestClientDetailsInDB(clientDetailsFile);
    systemLaunchInputData = getSystemLaunchInputData(systemLaunchFile);
    JSONObject jsonObject = new JSONObject(systemLaunchInputData);
    patientId = (String) jsonObject.get("patientId");
    encounterID = (String) jsonObject.get("encounterId");
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
    stubHelper.stopMockServer();
  }

  @Parameters(name = "{index}: Execute TestSystemLaunch with Test Case = {0}")
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

    // System.out.println("Port==" + wireMockHttpPort);

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(systemLaunchInputData, headers);
    logger.info("Invoking systemLaunch...");
    logger.info("Payload: \n" + systemLaunchInputData);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort(systemLaunchURI), HttpMethod.POST, entity, String.class);
    logger.info("Received Response. Waiting for EICR generation.....");
    Thread.sleep(100000);

    Query query = session.createQuery("from Eicr order by id DESC");
    query.setMaxResults(1);
    Eicr last = (Eicr) query.uniqueResult();
    Document expectedDoc = TestUtils.getXmlDocument(expectedEICRFile);
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document actualDoc = builder.parse(new InputSource(new StringReader(last.getData())));
    BufferedReader br1 =
        new BufferedReader(
            new InputStreamReader(
                classLoader.getResourceAsStream(expectedEICRFile), StandardCharsets.UTF_8));
    BufferedReader br2 = new BufferedReader(new StringReader(last.getData()));

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    assertEquals(
        expectedDoc.getDocumentElement().getTextContent(),
        actualDoc.getDocumentElement().getTextContent());

    if (((String) testDataGenerator.getOtherMappings(testCaseId).get("metadata")).contains("r4")) {
      assertTrue(TestUtils.compareStringBuffer(br1, br2, transactionalEntrySet));
    }
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

  private Document getCreateEicrDocument() {
    Document eicrDoc = null;
    try {

      if (state.getCreateEicrStatus().getEicrCreated()) {
        Eicr createEicr =
            session.get(Eicr.class, Integer.parseInt(state.getCreateEicrStatus().geteICRId()));
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        InputSource doc = new InputSource(createEicr.getData());
        eicrDoc = dBuilder.parse(doc);
      }
    } catch (Exception e) {
      fail(e.getMessage() + "Error while retrieving EICR document");
    }
    return eicrDoc;
  }
}
