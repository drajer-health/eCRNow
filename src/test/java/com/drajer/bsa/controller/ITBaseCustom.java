package com.drajer.bsa.controller;

import com.drajer.bsa.utils.StartupUtils;
import com.drajer.test.BaseIntegrationTest;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(Parameterized.class)
public class ITBaseCustom extends BaseIntegrationTest {
  String uri = "/api/launchPatient";

  private String testCaseId;
  private Map<String, String> testData;
  private Map<String, ?> allResourceMapping;
  private Map<String, ?> allOtherMapping;

  @Autowired private RestTemplateBuilder restTemplateBuilder;
  TestRestTemplate restTemplate;

  public ITBaseCustom(
      String testCaseId,
      Map<String, String> testData,
      Map<String, ?> resourceMapping,
      Map<String, ?> otherMapping) {
    this.testCaseId = testCaseId;
    this.testData = testData;
    this.allResourceMapping = resourceMapping;
    this.allOtherMapping = otherMapping;
  }

  private static final Logger logger = LoggerFactory.getLogger(ITBaseCustom.class);
  private String systemLaunch3PayLoad;
  WireMockHelper stubHelper;

  @Before
  public void launchTestSetUp() throws IOException {
    logger.info("Executing test: {}", testCaseId);
    restTemplate =
        new TestRestTemplate(
            restTemplateBuilder
                .setConnectTimeout(Duration.ofSeconds(1000)) // Set connection timeout
                .setReadTimeout(Duration.ofSeconds(60000)) // Set read timeout
            );
    tx = session.beginTransaction();
    ReflectionTestUtils.setField(StartupUtils.class, "startTime", Date.from(Instant.now()));
    // Data Setup
    saveHealtcareSetting(testData.get("HealcareSettingsFile"));
    saveKnowdlegeArtifact(testData.get("KnowledgeArtifactStatusFile"));

    systemLaunch3PayLoad = getSystemLaunch3Payload(testData.get("SystemLaunch3Payload"));
    session.flush();
    tx.commit();

    stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
    logger.info("Creating WireMock stubs..");
    stubHelper.stubResources(allResourceMapping);
    stubHelper.stubAuthAndMetadata(allOtherMapping);
    mockRestApi();
  }

  @Parameters(name = "{0}")
  public static Collection<Object[]> data() {
    TestDataGenerator testDataGenerator = new TestDataGenerator("test-yaml/customQueryTest.yaml");

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
  public void callApi() {

    HttpHeaders headers = new HttpHeaders();
    String requestId = "1234";
    String url = createURLWithPort(uri);
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add("X-Request-ID", requestId);
    HttpEntity<String> entity = new HttpEntity<>(systemLaunch3PayLoad, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(url, HttpMethod.POST, entity, String.class);
    response.getStatusCode();

    // wireMockServer.verify(moreThanOrExactly(1),
    // getRequestedFor(urlEqualTo("/FHIR/Encounter/97953900")));
  }

  private void saveHealtcareSetting(String healthCareSettingsFile) throws IOException {
    String healthcareSettingFile = TestUtils.getFileContentAsString(healthCareSettingsFile);
    headers.setContentType(MediaType.APPLICATION_JSON);
    String requestId = "5678";
    headers.add("X-Request-ID", requestId);
    HttpEntity<String> entity = new HttpEntity<>(healthcareSettingFile, headers);
    restTemplate.exchange(
        createURLWithPort("/api/healthcareSettings"), HttpMethod.POST, entity, String.class);
  }

  private void saveKnowdlegeArtifact(String knowledgeArtifactFile) throws IOException {
    headers.setContentType(MediaType.APPLICATION_JSON);
    HttpEntity<String> entity =
        new HttpEntity<>(TestUtils.getFileContentAsString(knowledgeArtifactFile), headers);
    restTemplate.exchange(
        createURLWithPort("/api/addKARStatus/"), HttpMethod.POST, entity, String.class);
  }
}
