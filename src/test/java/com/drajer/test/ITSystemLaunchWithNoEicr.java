package com.drajer.test;

import static org.junit.Assert.*;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.*;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.WireMockHelper;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.io.IOException;
import java.util.*;
import org.hibernate.query.Query;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;

@RunWith(Parameterized.class)
public class ITSystemLaunchWithNoEicr extends BaseIntegrationTest {

  private String testCaseId;
  private Map<String, String> testData;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;

  public ITSystemLaunchWithNoEicr(
      String testCaseId,
      Map<String, String> testData,
      Map<String, ?> resourceMapping,
      Map<String, ?> otherMapping) {
    this.testCaseId = testCaseId;
    this.testData = testData;
    this.allResourceMapping = resourceMapping;
    this.allOtherMapping = otherMapping;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITSystemLaunchWithNoEicr.class);
  private String systemLaunchPayload;
  private LaunchDetails launchDetails;
  private PatientExecutionState state;

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
    mockRestApi();
  }

  @Parameters(name = "{0}")
  public static Collection<Object[]> data() {

    TestDataGenerator testDataGenerator =
        new TestDataGenerator("test-yaml/systemLaunchWithNoEcr.yaml");

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
  public void testNoEicrWhenNoTriggerCode() {

    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayload);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
    waitForCreateEicrCompletion();
    getLaunchDetailAndStatus();
    validateNoMatchedTriggerStatus(JobStatus.COMPLETED);
    validateCreateEICR(JobStatus.COMPLETED, false);
    List<Eicr> allEICRDocuments = getAllEICRDocuments();
    assertEquals(0, allEICRDocuments != null ? allEICRDocuments.size() : "");
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

  private void validateNoMatchedTriggerStatus(JobStatus status) {
    MatchTriggerStatus matchTriggerStatus = state.getMatchTriggerStatus();

    assertNotNull(matchTriggerStatus.getMatchedCodes());
    assertTrue(matchTriggerStatus.getMatchedCodes().size() == 0);
  }

  private void validateCreateEICR(JobStatus status, Boolean validateEICR) {
    CreateEicrStatus createEicrStatus = state.getCreateEicrStatus();

    if (validateEICR) {
      assertTrue(createEicrStatus.getEicrCreated());
      validateEICR(createEicrStatus.geteICRId());
    } else {
      assertFalse(createEicrStatus.getEicrCreated());
    }
  }

  private void validateEICR(String eICRId) {
    assertNotNull(eICRId);
    assertFalse(eICRId.isEmpty());
    Eicr eicr = getEICRDocument(eICRId);
    if (eicr != null) {
      assertNotNull(eicr);
      assertNotNull(eicr.getEicrData());
      assertFalse(eicr.getEicrData().isEmpty());
    } else {
      fail("Eicr Not Found");
    }
  }

  private Eicr getEICRDocument(String eicrId) {
    try {

      Eicr eicr = session.get(Eicr.class, Integer.parseInt(eicrId));
      if (eicr != null) {
        session.refresh(eicr);
        return eicr;
      }
    } catch (Exception e) {
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }

  private List<Eicr> getAllEICRDocuments() {
    try {

      EntityManager em = getSession().getEntityManagerFactory().createEntityManager();
      CriteriaBuilder cb = em.getCriteriaBuilder();
      CriteriaQuery<Eicr> cq = cb.createQuery(Eicr.class);

      Query<Eicr> q = getSession().createQuery(cq);

      return q.getResultList();
    } catch (Exception e) {
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }

  private void waitForCreateEicrCompletion() {
    try {
      do {
        // Minimum 2 sec is required as App will execute
        // createEicr workflow after 2 sec as per eRSD.
        Thread.sleep(2000);
        getLaunchDetailAndStatus();

      } while (state.getCreateEicrStatus().getJobStatus() != JobStatus.COMPLETED);
    } catch (InterruptedException e) {
      logger.warn("Issue with thread sleep", e);
      Thread.currentThread().interrupt();
    }
  }
}
