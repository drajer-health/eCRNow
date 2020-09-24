package com.drajer.ecr.it;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecr.it.common.BaseIntegrationTest;
import com.drajer.sof.model.ClientDetails;
import com.drajer.test.util.TestDataGenerator;
import com.drajer.test.util.TestUtils;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
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

public class ITClientController extends BaseIntegrationTest {

  private String testCaseId;

  public ITClientController(String testCaseId) {
    this.testCaseId = testCaseId;
  }

  static int savedId;
  static String clientDetailString;
  static String testSaveClientData;

  static int testClientDetailsId;

  static TestDataGenerator testDataGenerator;
  String clientURI = "/api/clientDetails";

  String clientDetailsFile2;
  String clientDetailsFile1;

  private static final Logger logger = LoggerFactory.getLogger(ITClientController.class);

  @Before
  public void clientTestSetUp() throws IOException {
    logger.info("Executing Tests with TestCase: " + testCaseId);
    clientDetailsFile2 = testDataGenerator.getTestFile(testCaseId, "ClientDataToBeSaved");
    clientDetailsFile1 = testDataGenerator.getTestFile(testCaseId, "ClientDataToBeSaved2");
  }

  @After
  public void cleanUp() {
    session.close();
  }

  @Parameters(name = "{index}: Execute Test with TestCase= {0}")
  public static Collection<Object[]> data() {
    testDataGenerator = new TestDataGenerator("TestClientController.yaml");
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
  public void testSaveClient() throws IOException {
    createSaveClientInputData();

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(testSaveClientData, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(createURLWithPort(clientURI), HttpMethod.POST, entity, String.class);
    savedId = mapper.readValue(response.getBody(), ClientDetails.class).getId();
    ClientDetails expectedDetails = (ClientDetails) session.get(ClientDetails.class, savedId);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    assertEquals(TestUtils.toJson(expectedDetails), response.getBody());
  }

  @Test
  public void testSaveClient_dup() throws IOException {
    createSaveClientInputData();

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(testSaveClientData, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(createURLWithPort(clientURI), HttpMethod.POST, entity, String.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    // resend the same client details
    ResponseEntity<String> dupResponse =
        restTemplate.exchange(createURLWithPort(clientURI), HttpMethod.POST, entity, String.class);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, dupResponse.getStatusCode());
  }

  @Test
  public void testUpdateClient_new() throws IOException {
    createSaveClientInputData();
    headers.setContentType(MediaType.APPLICATION_JSON);

    StringBuilder str = new StringBuilder(testSaveClientData);
    str.replace(
        str.indexOf("test@ett.healthit.gov"),
        str.indexOf("test@ett.healthit.gov") + "test@ett.healthit.gov".length(),
        "updated-test@ett.healthit.gov");
    str.replace(
        str.indexOf("fhircreate"),
        str.indexOf("fhircreate") + "fhircreate".length(),
        "fhircreateUpd");

    // send the updated client details
    HttpEntity<String> updatedEntity = new HttpEntity<String>(str.toString(), headers);
    ResponseEntity<String> updateResponse =
        restTemplate.exchange(
            createURLWithPort(clientURI), HttpMethod.PUT, updatedEntity, String.class);
    assertEquals(HttpStatus.OK, updateResponse.getStatusCode());
    assertEquals(
        "updated-test@ett.healthit.gov",
        mapper.readValue(updateResponse.getBody(), ClientDetails.class).getDirectUser());
  }

  @Test
  public void testUpdateClient_existing() throws IOException {
    createSaveClientInputData();
    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(testSaveClientData, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(createURLWithPort(clientURI), HttpMethod.POST, entity, String.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    StringBuilder str = new StringBuilder(testSaveClientData);
    str.replace(
        str.indexOf("test@ett.healthit.gov"),
        str.indexOf("test@ett.healthit.gov") + "test@ett.healthit.gov".length(),
        "updated-test@ett.healthit.gov");

    // send the updated client details
    HttpEntity<String> updatedEntity = new HttpEntity<String>(str.toString(), headers);
    ResponseEntity<String> updateResponse =
        restTemplate.exchange(
            createURLWithPort(clientURI), HttpMethod.PUT, updatedEntity, String.class);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, updateResponse.getStatusCode());
  }

  @Test
  public void testGetClientDetailsById() throws IOException {
    createTestClientDetailsInDB();
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort(clientURI + "/" + testClientDetailsId),
            HttpMethod.GET,
            null,
            String.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    ClientDetails clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(
        mapper.readValue(clientDetailString, ClientDetails.class).getClientId(),
        clientDetails.getClientId());
  }

  @Test
  public void testGetClientDetailsByURL() throws IOException {
    createTestClientDetailsInDB();
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort(
                clientURI
                    + "?url=https://fhirsave-ehr.sandboxcerner.com/dstu2/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca"),
            HttpMethod.GET,
            null,
            String.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    ClientDetails clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(
        mapper.readValue(clientDetailString, ClientDetails.class).getClientId(),
        clientDetails.getClientId());
  }

  @Test
  public void testGetAllClientDetails() throws IOException {
    createTestClientDetailsInDB();
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort(clientURI + "/"), HttpMethod.GET, null, String.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    List<ClientDetails> clientList =
        (List<ClientDetails>) mapper.readValue(response.getBody(), List.class);

    assertEquals(1, clientList.size());
  }

  private void createSaveClientInputData() throws IOException {

    testSaveClientData = TestUtils.getFileContentAsString(clientDetailsFile1);
  }

  private void createTestClientDetailsInDB() throws IOException {
    session = sessionFactory.openSession();
    tx = session.beginTransaction();
    clientDetailString = TestUtils.getFileContentAsString(clientDetailsFile2);

    testClientDetailsId =
        (int) session.save(mapper.readValue(clientDetailString, ClientDetails.class));
    session.flush();
    tx.commit();
  }
}
