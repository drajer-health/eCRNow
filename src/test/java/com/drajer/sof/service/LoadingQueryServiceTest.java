/*package com.drajer.sof.service;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecr.it.common.BaseIntegrationTest;
import com.drajer.ecr.it.common.WireMockHelper;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Set;
import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class LoadingQueryServiceTest extends BaseIntegrationTest {

  private String testCaseId;

  public LoadingQueryServiceTest(String testCaseId) {
    this.testCaseId = testCaseId;
  }

  static TestDataGenerator testDataGenerator;
  private LaunchDetails launchDetails;
  Date startDate;
  Date endDate;

  @Autowired LoadingQueryService loadingQueryService;

  WireMockHelper stubHelper;

  private static final Logger logger = LoggerFactory.getLogger(TriggerQueryServiceTest.class);

  @Before
  public void launchTestSetUp() throws IOException {
    tx = session.beginTransaction();

    try {
      launchDetails =
          mapper.readValue(
              this.getClass()
                  .getClassLoader()
                  .getResourceAsStream("R4/Misc/LaunchDetails/LaunchDetails.json"),
              LaunchDetails.class);
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    launchDetails.setEhrServerURL(
        launchDetails.getEhrServerURL().replace("port", "" + wireMockHttpPort));
    launchDetails.setAccessToken(
        launchDetails.getAccessToken().replace("port", "" + wireMockHttpPort));

    startDate = DateUtils.addHours(new Date(), 3);

    endDate = DateUtils.addHours(new Date(), 30);

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
    testDataGenerator = new TestDataGenerator("TriggerQueryServiceTest.yaml");
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
  public void getDataTest() throws IOException {
    R4FhirData r4FhirData =
        (R4FhirData) loadingQueryService.getData(launchDetails, startDate, endDate);
    Bundle bundle = r4FhirData.getData();

    assertNotNull(bundle);

    String generatedBundle = getGeneratedBundle();
    String expectedBundle =
        TestUtils.getFileContentAsString("TriggerQueryR4Bundle-12742571_expected.json");

    assertEquals(generatedBundle, expectedBundle);
  }

  public String getGeneratedBundle() throws IOException {
    String file =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/LoadingQueryR4Bundle-"
            + launchDetails.getLaunchPatientId()
            + ".json";

    BufferedReader reader = new BufferedReader(new FileReader(file));
    String currentLine = reader.readLine();
    reader.close();
    return currentLine;
  }
}*/
