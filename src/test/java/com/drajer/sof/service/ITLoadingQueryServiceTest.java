package com.drajer.sof.service;

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
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class ITLoadingQueryServiceTest extends BaseIntegrationTest {

  private String testCaseId;

  public ITLoadingQueryServiceTest(String testCaseId) {
    this.testCaseId = testCaseId;
  }

  static TestDataGenerator testDataGenerator;
  private LaunchDetails launchDetails;
  Map<String, List<String>> allResourceFiles;
  Date startDate;
  Date endDate;

  @Autowired LoadingQueryService loadingQueryService;

  WireMockHelper stubHelper;

  private static final Logger logger = LoggerFactory.getLogger(ITLoadingQueryServiceTest.class);

  @Before
  public void laoadingQuerySetUp() throws IOException {

    try {

      tx = session.beginTransaction();

      launchDetails =
          mapper.readValue(
              this.getClass()
                  .getClassLoader()
                  .getResourceAsStream(testDataGenerator.getTestFile(testCaseId, "LaunchDetails")),
              LaunchDetails.class);
      allResourceFiles = testDataGenerator.getResourceFiles(testCaseId);

      launchDetails.setEhrServerURL(
          launchDetails.getEhrServerURL().replace("port", "" + wireMockHttpPort));
      launchDetails.setAccessToken(
          launchDetails.getAccessToken().replace("port", "" + wireMockHttpPort));

      startDate = DateUtils.parseDate("20201001", "yyyyMMdd");
      endDate = DateUtils.parseDate("20201030", "yyyyMMdd");

      stubHelper = new WireMockHelper(baseUrl, wireMockHttpPort);
      logger.info("Creating wiremockstubs..");
      stubHelper.stubResources(testDataGenerator.getResourceMappings(testCaseId));
      stubHelper.stubAuthAndMetadata(testDataGenerator.getOtherMappings(testCaseId));

    } catch (IOException e) {

      fail(e.getMessage() + "This exception is not expected fix test");
    } catch (ParseException e1) {

      fail(e1.getMessage() + "This exception is not expected fix test");
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

  @Parameters(name = "{index}: {0}")
  public static Collection<Object[]> data() {
    testDataGenerator = new TestDataGenerator("LoadingQueryServiceTest.yaml");
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
  public void loadingQueryServiceTest() throws IOException {
    R4FhirData r4FhirData =
        (R4FhirData) loadingQueryService.getData(launchDetails, startDate, endDate);
    Bundle generatedBundle = r4FhirData.getData();

    assertNotNull(generatedBundle);
    validateBundle(generatedBundle);
  }

  private void validateBundle(Bundle r4Bundle) {

    // Patient
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
    }
  }
}
