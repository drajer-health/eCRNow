package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.moreThanOrExactly;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static org.junit.Assert.*;

import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.WireMockHelper;
import java.util.*;

import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import org.hibernate.query.Query;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@RunWith(Parameterized.class)
public class ITPageRetry extends BaseIntegrationTest {

  private String testCaseId;
  private Map<String, String> testData;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;
  private List<Map<String, String>> fieldsToValidate;

  public ITPageRetry(
      String testCaseId,
      Map<String, String> testData,
      List<Map<String, String>> validateFields,
      Map<String, ?> resourceMapping,
      Map<String, ?> otherMapping) {

    this.testCaseId = testCaseId;
    this.testData = testData;
    this.fieldsToValidate = validateFields;
    this.allResourceMapping = resourceMapping;
    this.allOtherMapping = otherMapping;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITPageRetry.class);
  private static String systemLaunchPayload;
  private LaunchDetails launchDetails;
  private PatientExecutionState state;
  WireMockHelper stubHelper;

  @Before
  public void launchTestSetUp() throws Throwable {
    logger.info("Executing Test: {}", testCaseId);
    tx = session.beginTransaction();

    // Data Setup
    createClientDetails(testData.get("ClientDataToBeSaved"));
    systemLaunchPayload = getSystemLaunchPayload(testData.get("SystemLaunchPayload"));
    session.flush();
    tx.commit();

    stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
    logger.info("Creating WireMock stubs..");
    stubHelper.stubResources(allResourceMapping);
    stubHelper.stubAuthAndMetadata(allOtherMapping);
    mockRestApi();
  }

  @Parameterized.Parameters(name = "{0}")
  public static Collection<Object[]> data() {

    List<TestDataGenerator> testDataGenerator = new ArrayList<>();
    testDataGenerator.add(new TestDataGenerator("test-yaml/fhirRetryPageTest.yaml"));

    int totalTestCount = 0;
    for (TestDataGenerator testData : testDataGenerator) {
      totalTestCount = totalTestCount + testData.getAllTestCases().size();
    }

    Object[][] data = new Object[totalTestCount][5];

    int count = 0;
    for (TestDataGenerator testData : testDataGenerator) {
      Set<String> testCaseSet = testData.getAllTestCases();
      for (String testCase : testCaseSet) {
        data[count][0] = testCase;
        data[count][1] = testData.getTestCaseByID(testCase).getTestData();
        data[count][2] = testData.getValidate(testCase);
        data[count][3] = testData.getResourceMappings(testCase);
        data[count][4] = testData.getOtherMappings(testCase);
        count++;
      }
    }
    return Arrays.asList(data);
  }

  @Test
  public void testPage() throws Exception {

    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayload);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
    Eicr createEicr = getCreateEicrDocument();
    wireMockServer.verify(
        moreThanOrExactly(9),
        getRequestedFor(
            urlEqualTo(
                "/FHIR/Observation?patient=12742571&category=laboratory&-pageContext=10065315_11316911_12742571_1_1&-pageDirection=NEXT")));
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
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }

  private void getLaunchDetailAndStatus() {
    try {
      EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
      CriteriaBuilder cb = em.getCriteriaBuilder();
      CriteriaQuery<LaunchDetails> cq = cb.createQuery(LaunchDetails.class);
      Root<LaunchDetails> root = cq.from(LaunchDetails.class);
      cq.where(cb.equal(root.get("xRequestId"), testCaseId));

      Query<LaunchDetails> q = getSession().createQuery(cq);

      launchDetails = q.uniqueResult();

      state = mapper.readValue(launchDetails.getStatus(), PatientExecutionState.class);
      session.refresh(launchDetails);

    } catch (Exception e) {
      logger.error("Exception occurred retrieving launchDetail and status", e);
      fail("Something went wrong with launch status, check the log");
    }
  }
}
