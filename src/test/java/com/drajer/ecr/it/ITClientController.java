package com.drajer.ecr.it;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.ClientDetails;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureMockMvc
@Transactional
@ActiveProfiles("test")
public class ITClientController {

  private static final Logger logger = LoggerFactory.getLogger(ITClientController.class);

  ClassLoader classLoader = this.getClass().getClassLoader();

  @LocalServerPort protected int port;

  @Autowired protected MockMvc mockMvc;

  @Autowired protected SessionFactory sessionFactory;
  protected Session session = null;
  protected Transaction tx = null;

  protected TestRestTemplate restTemplate = new TestRestTemplate();

  static int savedId;
  static String clientDetailString;
  static String testSaveClientData;

  static int testClientDetailsId;

  static List<ClientDetails> deleteClientList = new ArrayList<>();

  protected HttpHeaders headers = new HttpHeaders();

  protected static ObjectMapper mapper = new ObjectMapper();

  protected static final String URL = "http://localhost:";

  String clientDetailsFile2 = "R4/Misc/ClientDetails/ClientDataEntry2.json";
  String clientDetailsFile1 = "R4/Misc/ClientDetails/ClientDetails1.json";

  @Before
  public void clientTestSetUp() throws IOException {
    session = sessionFactory.openSession();
    tx = session.beginTransaction();
    createTestClientDetailsInDB();
    createSaveClientInputData();
    session.flush();
    tx.commit();
  }

  @After
  public void cleanUp() {
    tx = session.beginTransaction();
    dataCleanup();

    tx.commit();
  }

  @Test
  public void testSaveClient() throws IOException {

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(testSaveClientData, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails"), HttpMethod.POST, entity, String.class);
    savedId = mapper.readValue(response.getBody(), ClientDetails.class).getId();
    ClientDetails expectedDetails = (ClientDetails) session.get(ClientDetails.class, savedId);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    assertEquals(TestUtils.toJson(expectedDetails), response.getBody());

    // add for cleanup later
    deleteClientList.add(expectedDetails);
  }

  @Test
  public void testSaveClient_dup() throws IOException {

    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(testSaveClientData, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails"), HttpMethod.POST, entity, String.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    // resend the same client details
    ResponseEntity<String> dupResponse =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails"), HttpMethod.POST, entity, String.class);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, dupResponse.getStatusCode());

    // add for cleanup later
    savedId = mapper.readValue(response.getBody(), ClientDetails.class).getId();
    ClientDetails clientDetailsToBeDeleted =
        (ClientDetails) session.get(ClientDetails.class, savedId);

    deleteClientList.add(clientDetailsToBeDeleted);
  }

  @Test
  public void testUpdateClient_new() throws IOException {
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
            createURLWithPort("/api/clientDetails"), HttpMethod.PUT, updatedEntity, String.class);
    assertEquals(HttpStatus.OK, updateResponse.getStatusCode());
    assertEquals(
        "updated-test@ett.healthit.gov",
        mapper.readValue(updateResponse.getBody(), ClientDetails.class).getDirectUser());

    savedId = mapper.readValue(updateResponse.getBody(), ClientDetails.class).getId();
    ClientDetails clientDetailsToBeDeleted =
        (ClientDetails) session.get(ClientDetails.class, savedId);

    // add for cleanup later
    deleteClientList.add(clientDetailsToBeDeleted);
  }

  @Test
  public void testUpdateClient_existing() throws IOException {
    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> entity = new HttpEntity<String>(testSaveClientData, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails"), HttpMethod.POST, entity, String.class);
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
            createURLWithPort("/api/clientDetails"), HttpMethod.PUT, updatedEntity, String.class);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, updateResponse.getStatusCode());

    savedId = mapper.readValue(response.getBody(), ClientDetails.class).getId();
    ClientDetails clientDetailsToBeDeleted =
        (ClientDetails) session.get(ClientDetails.class, savedId);

    // add for cleanup later
    deleteClientList.add(clientDetailsToBeDeleted);
  }

  @Test
  public void testGetClientDetailsById() throws IOException {

    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails/" + testClientDetailsId),
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

    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort(
                "/api/clientDetails?url=https://fhirsave-ehr.sandboxcerner.com/dstu2/0b8a0111-e8e6-4c26-a91c-5069cbc6b1ca"),
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

    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails/"), HttpMethod.GET, null, String.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    List<ClientDetails> clientList =
        (List<ClientDetails>) mapper.readValue(response.getBody(), List.class);

    assertEquals(1, clientList.size());
  }

  private String createURLWithPort(String uri) {
    return URL + port + uri;
  }

  private void createSaveClientInputData() throws IOException {

    testSaveClientData = TestUtils.getFileContentAsString(clientDetailsFile1);
  }

  private void createTestClientDetailsInDB() throws IOException {

    clientDetailString = TestUtils.getFileContentAsString(clientDetailsFile2);

    testClientDetailsId =
        (int) session.save(mapper.readValue(clientDetailString, ClientDetails.class));

    ClientDetails clientDetailsToBeDeleted =
        (ClientDetails) session.get(ClientDetails.class, testClientDetailsId);
    deleteClientList.add(clientDetailsToBeDeleted);
  }

  private void dataCleanup() {
    for (ClientDetails clientDetails : deleteClientList) {
      session.load(ClientDetails.class, clientDetails.getId());
      session.delete(clientDetails);
    }
    deleteClientList.clear();
  }
}
