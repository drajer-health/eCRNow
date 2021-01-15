package com.drajer.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.commons.lang3.time.DateUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

@RunWith(Parameterized.class)
public class ITLoadingQueryServiceTest extends BaseIntegrationTest {

  private String testCaseId;
  private String launchDetailsFile;
  private String startDate;
  private String endDate;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;
  private List<Map<String, String>> fieldsToValidate;

  public ITLoadingQueryServiceTest(
      String testCaseId,
      String launchDetails,
      String startDate,
      String endDate,
      List<Map<String, String>> validateFields,
      Map<String, ?> resourceMapping,
      Map<String, ?> otherMapping) {

    this.testCaseId = testCaseId;
    this.launchDetailsFile = launchDetails;
    this.startDate = startDate;
    this.endDate = endDate;
    this.fieldsToValidate = validateFields;
    this.allResourceMapping = resourceMapping;
    this.allOtherMapping = otherMapping;
  }

  private LaunchDetails launchDetails;

  @Autowired LoadingQueryService loadingQueryService;

  WireMockHelper stubHelper;

  private static final Logger logger = LoggerFactory.getLogger(ITLoadingQueryServiceTest.class);

  @Before
  public void laoadingQuerySetUp() {
    try {
      tx = session.beginTransaction();

      String launchDetailJson = TestUtils.getFileContentAsString(launchDetailsFile);
      launchDetailJson = launchDetailJson.replace("port", "" + wireMockHttpPort);
      launchDetails = mapper.readValue(launchDetailJson, LaunchDetails.class);

      stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
      logger.info("Creating WireMock stubs..");
      stubHelper.stubResources(allResourceMapping);
      stubHelper.stubAuthAndMetadata(allOtherMapping);

    } catch (IOException e) {
      logger.error("Exception setting up loadingQuery", e);
      fail(e.getMessage() + "This exception is not expected fix test");
    }
    session.flush();
    tx.commit();
  }

  @Parameters(name = "{0}")
  public static Collection<Object[]> data() {

    TestDataGenerator testDataGenerator =
        new TestDataGenerator("test-yaml/loadingQueryServiceTest.yaml");

    Set<String> testCaseSet = testDataGenerator.getAllTestCases();
    Object[][] data = new Object[testCaseSet.size()][7];
    int count = 0;
    for (String testCase : testCaseSet) {
      data[count][0] = testCase;
      data[count][1] = testDataGenerator.getTestData(testCase, "LaunchDetails");
      data[count][2] = testDataGenerator.getTestData(testCase, "StartDate");
      data[count][3] = testDataGenerator.getTestData(testCase, "EndDate");
      data[count][4] = testDataGenerator.getValidate(testCase);
      data[count][5] = testDataGenerator.getResourceMappings(testCase);
      data[count][6] = testDataGenerator.getOtherMappings(testCase);
      count++;
    }
    return Arrays.asList(data);
  }

  @Test
  public void loadingQueryServiceTest() {
    R4FhirData r4FhirData = null;
    try {
      r4FhirData =
          (R4FhirData)
              loadingQueryService.getData(
                  launchDetails,
                  DateUtils.parseDate(startDate, "yyyyMMdd"),
                  DateUtils.parseDate(endDate, "yyyyMMdd"));
    } catch (ParseException e) {
      logger.error("Exception parsing date:", e);
      fail(e.getMessage() + " Fix the test data to pass correct datetime");
    }
    assertNotNull("Failed to generate r4Data", r4FhirData);
    validateBundle(r4FhirData);
  }

  private void validateBundle(R4FhirData r4FhirData) {

    if (fieldsToValidate != null) {

      for (Map<String, String> field : fieldsToValidate) {
        for (Entry<String, String> set : field.entrySet()) {

          String resourceName = set.getKey();
          int resourceCount = Integer.parseInt(set.getValue());

          switch (resourceName) {
            case "Encounter":
              assertNotNull(r4FhirData.getEncounter());
              break;
            case "Practitioner":
              assertNotNull(r4FhirData.getPractitionersList());
              assertEquals(resourceName, resourceCount, r4FhirData.getPractitionersList().size());
              break;
            case "Organization":
              assertNotNull(r4FhirData.getOrganization());
              break;
            case "Condition":
              assertNotNull(r4FhirData.getConditions());
              assertEquals(resourceName, resourceCount, r4FhirData.getConditions().size());
              break;
            case "PregnancyCondition":
              assertNotNull(r4FhirData.getPregnancyConditions());
              assertEquals(resourceName, resourceCount, r4FhirData.getPregnancyConditions().size());
              break;
            case "PregnancyObservation":
              assertNotNull(r4FhirData.getPregnancyObs());
              assertEquals(resourceName, resourceCount, r4FhirData.getPregnancyObs().size());
              break;
            case "Observation":
              assertNotNull(r4FhirData.getLabResults());
              assertEquals(resourceName, resourceCount, r4FhirData.getLabResults().size());
              break;
            case "TravelObservation":
              assertNotNull(r4FhirData.getTravelObs());
              assertEquals(resourceName, resourceCount, r4FhirData.getTravelObs().size());
              break;
            case "OccupationObservation":
              assertNotNull(r4FhirData.getOccupationObs());
              assertEquals(resourceName, resourceCount, r4FhirData.getOccupationObs().size());
              break;
            case "Immunization":
              assertNotNull(r4FhirData.getImmunizations());
              assertEquals(resourceName, resourceCount, r4FhirData.getImmunizations().size());
              break;
            case "ServiceRequest":
              assertNotNull(r4FhirData.getServiceRequests());
              assertEquals(resourceName, resourceCount, r4FhirData.getServiceRequests().size());
              break;
            case "MedicationRequest":
              assertNotNull(r4FhirData.getMedicationRequests());
              assertEquals(resourceName, resourceCount, r4FhirData.getMedicationRequests().size());
              break;
            case "MedicationAdministration":
              assertNotNull(r4FhirData.getMedicationAdministrations());
              assertEquals(
                  resourceName, resourceCount, r4FhirData.getMedicationAdministrations().size());
              break;
            case "MedicationStatement":
              assertNotNull(r4FhirData.getMedications());
              assertEquals(resourceName, resourceCount, r4FhirData.getMedications().size());
              break;
          }
        }
      }
    } else {
      fail("Validate fields not configured for test " + testCaseId);
    }
  }
}
