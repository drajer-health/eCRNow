package com.drajer.test;

import static org.junit.Assert.*;

import com.drajer.cda.utils.CdaValidatorUtil;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
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
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

@RunWith(Parameterized.class)
public class ITValidateEicrDoc extends BaseIntegrationTest {

  private String testCaseId;
  private Map<String, String> testData;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;
  private List<Map<String, String>> fieldsToValidate;

  public ITValidateEicrDoc(
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

  private static final Logger logger = LoggerFactory.getLogger(ITValidateEicrDoc.class);
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
  }

  @Parameters(name = "{0}")
  public static Collection<Object[]> data() {

    List<TestDataGenerator> testDataGenerator = new ArrayList<>();
    testDataGenerator.add(new TestDataGenerator("test-yaml/headerSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/problemSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/encounterSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/resultSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/medicationSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/immunizationSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/socialHistorySection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/planOfTreatmentSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/historyOfPresentIllnessSection.yaml"));
    testDataGenerator.add(new TestDataGenerator("test-yaml/reasonForVisitSection.yaml"));

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
  public void testEicrDocument() throws Exception {

    ResponseEntity<String> response = invokeSystemLaunch(testCaseId, systemLaunchPayload);

    assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
    assertTrue(response.getBody().contains("App is launched successfully"));

    logger.info("Received success response, waiting for EICR generation.....");
    Eicr createEicr = getCreateEicrDocument();

    if (createEicr != null) {
      String eICRXml = createEicr.getEicrData();
      assertNotNull(eICRXml);
      assertFalse(eICRXml.isEmpty());
      getLaunchDetailAndStatus();

      assertTrue(
          "Schema Validation Failed, check the logs",
          CdaValidatorUtil.validateEicrXMLData(eICRXml));
      assertTrue(
          "Schematron Validation Failed, check the logs",
          CdaValidatorUtil.validateEicrToSchematron(eICRXml));

      Document eicrXmlDoc = TestUtils.getXmlDocument(eICRXml);
      validateXml(eicrXmlDoc);
    } else {
      fail("Eicr Not found");
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
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }

  private void validateXml(Document eicrXml) {

    final XPath xPath = XPathFactory.newInstance().newXPath();
    final String baseXPath = testData.get("BaseXPath");

    if (fieldsToValidate != null) {
      for (Map<String, String> field : fieldsToValidate) {
        try {
          String xPathExp = baseXPath + field.get("xPath");
          if (field.containsKey("count")) {
            try {
              NodeList nodeList =
                  (NodeList) xPath.compile(xPathExp).evaluate(eicrXml, XPathConstants.NODESET);
              assertEquals(xPathExp, Integer.parseInt(field.get("count")), nodeList.getLength());
            } catch (XPathExpressionException e) {
              logger.error("Exception validating field:", e);
              fail(e.getMessage() + ": Failed to evaluate field " + xPathExp);
            }
          } else {
            for (Entry<String, String> set : field.entrySet()) {
              if (!set.getKey().equalsIgnoreCase("xPath")) {
                String xPathFullExp = xPathExp + set.getKey();
                try {
                  String fieldValue =
                      (String) xPath.compile(xPathFullExp).evaluate(eicrXml, XPathConstants.STRING);
                  assertEquals(xPathFullExp, set.getValue(), fieldValue);
                } catch (XPathExpressionException e) {
                  logger.error("Exception validating field:", e);
                  fail(e.getMessage() + ": Failed to evaluate field " + xPathExp);
                }
              }
            }
          }

        } catch (Exception e) {
          logger.error("Exception validating field:", e);
          fail(e.getMessage() + ": This exception is not expected fix the test");
        }
      }

    } else {
      fail("validate field is not configured in the test");
    }
  }
}
