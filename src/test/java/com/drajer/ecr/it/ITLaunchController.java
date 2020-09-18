package com.drajer.ecr.it;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.drajer.ecr.it.common.BaseIntegrationTest;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.test.util.TestUtils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.hibernate.Query;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
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

  private static final Logger logger = LoggerFactory.getLogger(ITLaunchController.class);

  static final Set<Integer> transactionalEntrySet = new HashSet<>(Arrays.asList(10, 13, 202, 244));

  private final String systemLaunchURI = "/api/systemLaunch";

  // For WireMock
  private String clientDetailsFile = "R4/Misc/ClientDetails/ClientDataEntry1.json";
  private String launchDetailsFile = "R4/Misc/LaunchDetails/LaunchDataEntry1.json";
  private String systemLaunchFile = "R4/Misc/SystemLaunchPayload/systemLaunchRequest.json";
  private String expectedEICRFile = "R4/Misc/ExpectedEICR/EICR_Expected.xml";

  // TestDataGenerator testDataGenerator = new
  // TestDataGenerator("LaunchTestData.yaml");

  @Before
  public void launchTestSetUp() throws IOException {
    tx = session.beginTransaction();

    // Data Setup
    createTestClientDetailsInDB(clientDetailsFile);
    getSystemLaunchInputData(systemLaunchFile);
    createTestLaunchDetailsInDB(launchDetailsFile);

    session.flush();
    tx.commit();

    boolean isQueryParam = true;
    boolean isPathParam = false;
    // stub all r4 URIs
    logger.info("Creating wiremockstubs..");
    stubResource("Patient", isPathParam, "12742571", "R4/Patient/Patient_12742571.json");
    stubResource("Encounter", isPathParam, "97953900", "R4/Encounter/Encounter_97953900.json");
    stubResource(
        "Encounter",
        isQueryParam,
        "patient=12742571",
        "R4/Encounter/EncounterBundle_97953900.json");

    stubResource(
        "Practitioner", isPathParam, "11817978", "R4/Practitioner/Practitioner_11817978.json");
    stubResource(
        "Practitioner", isPathParam, "4122622", "R4/Practitioner/Practitioner_4122622.json");
    stubResource(
        "Practitioner", isPathParam, "11938004", "R4/Practitioner/Practitioner_11938004.json");

    stubResource(
        "Condition",
        isQueryParam,
        "patient=12742571",
        "R4/Condition/ConditionBundle_d2572364249.json");

    stubResource(
        "Observation",
        isQueryParam,
        "patient=12742571&category=laboratory",
        "R4/Observation/ObservationBundle_1.json");
    stubResource(
        "Observation",
        isQueryParam,
        "patient=12742571&code=http://loinc.org|90767-5",
        "R4/Observation/ObservationBundle_2.json");
    stubResource(
        "Observation",
        isQueryParam,
        "patient=12742571&code=http://loinc.org|929762-2",
        "R4/Observation/ObservationBundle_3.json");
  }

  @After
  public void cleanUp() {
    tx = session.beginTransaction();
    dataCleanup();

    tx.commit();
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
    Thread.sleep(140000);

    Query query = session.createQuery("from Eicr order by id DESC");
    query.setMaxResults(1);
    Eicr last = (Eicr) query.uniqueResult();
    System.out.println(last.getData());
    Document expectedDoc = getExpectedXml(expectedEICRFile);
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

    assertTrue(TestUtils.compareStringBuffer(br1, br2, transactionalEntrySet));
  }
}
