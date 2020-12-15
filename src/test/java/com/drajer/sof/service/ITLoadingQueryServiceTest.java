package com.drajer.sof.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecr.it.common.BaseIntegrationTest;
import com.drajer.ecr.it.common.WireMockHelper;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import java.io.IOException;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.commons.lang3.time.DateUtils;
import org.junit.After;
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
  public void laoadingQuerySetUp() throws IOException {

    try {

      tx = session.beginTransaction();

      String launchDetailJson = TestUtils.getFileContentAsString(launchDetailsFile);
      launchDetailJson = launchDetailJson.replace("port", "" + wireMockHttpPort);
      launchDetails = mapper.readValue(launchDetailJson, LaunchDetails.class);

      stubHelper = new WireMockHelper(baseUrl, wireMockHttpPort);
      logger.info("Creating wiremockstubs..");
      stubHelper.stubResources(allResourceMapping);
      stubHelper.stubAuthAndMetadata(allOtherMapping);

    } catch (IOException e) {

      fail(e.getMessage() + "This exception is not expected fix test");
    }
    session.flush();
    tx.commit();
  }

  @After
  public void cleanUp() {

    if (stubHelper != null) {
      stubHelper.stopMockServer();
    }
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
      data[count][1] = testDataGenerator.getTestFile(testCase, "LaunchDetails");
      data[count][2] = testDataGenerator.getTestFile(testCase, "StartDate");
      data[count][3] = testDataGenerator.getTestFile(testCase, "EndDate");
      data[count][4] = testDataGenerator.getValidate(testCase);
      data[count][5] = testDataGenerator.getResourceMappings(testCase);
      data[count][6] = testDataGenerator.getOtherMappings(testCase);
      count++;
    }

    return Arrays.asList(data);
  }

  @Test
  public void loadingQueryServiceTest() throws IOException {

    R4FhirData r4FhirData = null;

    try {
      r4FhirData =
          (R4FhirData)
              loadingQueryService.getData(
                  launchDetails,
                  DateUtils.parseDate(startDate, "yyyyMMdd"),
                  DateUtils.parseDate(endDate, "yyyyMMdd"));
    } catch (ParseException e) {
      fail(e.getMessage() + " Fix the test data to pass correct datetime");
    }

    assertNotNull("Failed to generate r4Data", r4FhirData);
    // Bundle generatedBundle = r4FhirData.getData();
    // assertNotNull("Failed to generate bundle", generatedBundle);
    validateBundle(r4FhirData);
  }

  private void validateBundle(R4FhirData r4FhirData) {

    if (fieldsToValidate != null) {

      for (Map<String, String> field : fieldsToValidate) {
        for (Entry<String, String> set : field.entrySet()) {

          String resourceName = set.getKey();
          int resourceCount = Integer.parseInt(set.getValue());

          switch (resourceName) {
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
            case "PergnancyCondition":
              assertNotNull(r4FhirData.getPregnancyConditions());
              assertEquals(resourceName, resourceCount, r4FhirData.getPregnancyConditions().size());
              break;
            case "PergnancyObservation":
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
          }
        }
      }
    } else {
      fail("Validate fields not configured for test " + testCaseId);
    }

    /*    // Patient
    if (allResourceFiles.get("Patient") != null) {
      Patient patient = (Patient) TestUtils.getResourceFromBundle(r4Bundle, Patient.class);
      assertNotNull(patient);
    }
    // Encounter
    if (allResourceFiles.get("Encounter") != null) {
      Encounter encounter = (Encounter) TestUtils.getResourceFromBundle(r4Bundle, Encounter.class);
      assertNotNull(encounter);
    }
    // Conditions
    if (allResourceFiles.get("Condition") != null) {
      List<Resource> actualRsrc = TestUtils.getResourcesFromBundle(r4Bundle, Condition.class);
      Bundle conditionBundle =
          TestUtils.getR4BundleFromJson(allResourceFiles.get("Condition").get(0));
      assertNotNull(actualRsrc);
      assertEquals(conditionBundle.getEntry().size(), actualRsrc.size());
    }
    // Practitioner
    if (allResourceFiles.get("Practitioner") != null) {
      List<Resource> actualRsrc = TestUtils.getResourcesFromBundle(r4Bundle, Practitioner.class);
      assertNotNull(actualRsrc);
      // TODO - Bug in the code adding duplicate Practitioner in the bundle.
      // assertEquals(allResourceFiles.get("Practitioner").size(), actualRsrc.size());
    }
    // Observation
    if (allResourceFiles.get("Observation") != null) {
      List<Resource> actualRsrc = TestUtils.getResourcesFromBundle(r4Bundle, Observation.class);
      int obsBundleSize = 0;
      for (String filename : allResourceFiles.get("Observation")) {
        Bundle observationBundle = TestUtils.getR4BundleFromJson(filename);
        obsBundleSize = obsBundleSize + observationBundle.getEntry().size();
      }
      // assertNotNull(actualRsrc);
      // assertEquals(obsBundleSize, actualRsrc.size());
    }
    // Immunization
    if (allResourceFiles.get("Immunization") != null) {
      List<Resource> actualRsrc = TestUtils.getResourcesFromBundle(r4Bundle, Immunization.class);
      int imunBundleSize = 0;
      for (String filename : allResourceFiles.get("Immunization")) {
        Bundle immunBundle = TestUtils.getR4BundleFromJson(filename);
        imunBundleSize = imunBundleSize + immunBundle.getEntry().size();
      }
      assertNotNull(actualRsrc);
      assertEquals(imunBundleSize, actualRsrc.size());
    }
    // ServiceRequest
    if (allResourceFiles.get("ServiceRequest") != null) {
      List<Resource> actualRsrc = TestUtils.getResourcesFromBundle(r4Bundle, ServiceRequest.class);
      int srBundleSize = 0;
      for (String filename : allResourceFiles.get("ServiceRequest")) {
        Bundle srBundle = TestUtils.getR4BundleFromJson(filename);
        srBundleSize = srBundleSize + srBundle.getEntry().size();
      }
      assertNotNull(actualRsrc);
      assertEquals(srBundleSize, actualRsrc.size());
    }*/
  }
}
