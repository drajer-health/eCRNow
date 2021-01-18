package com.drajer.test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.sof.model.ClientDetails;
import com.drajer.test.util.TestUtils;
import java.io.IOException;
import java.util.List;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

public class ITClientController extends BaseIntegrationTest {

  private static String clientDetailPayload;
  private static final String clientURI = "/api/clientDetails";

  private static ClientDetails expectedClientDetails;
  private String clientUrl;

  @BeforeClass
  public static void clientSetUp() throws IOException {
    clientDetailPayload =
        TestUtils.getFileContentAsString("R4/Misc/ClientDetails/ClientDataEntry1.json");
    expectedClientDetails = mapper.readValue(clientDetailPayload, ClientDetails.class);
  }

  @Before
  public void initTestData() {
    clientUrl = createURLWithPort(clientURI);
  }

  @Test
  public void testSaveClient() throws IOException {
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    ClientDetails actualClientDetails = mapper.readValue(response.getBody(), ClientDetails.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertClientDetails(expectedClientDetails, actualClientDetails);
  }

  @Test
  public void testSaveClient_duplicate() {
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Resend the same client details
    response = invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
  }

  @Test
  public void testUpdateClient_new() throws IOException {
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.PUT);
    ClientDetails actualClientDetails = mapper.readValue(response.getBody(), ClientDetails.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertClientDetails(expectedClientDetails, actualClientDetails);
  }

  @Test
  public void testUpdateClient_existing_withSameID() throws IOException {
    // Insert ClientDetail row
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    ClientDetails clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Update the same clientDetails inserted above
    clientDetails.setDirectUser("updatedUser@test.com");
    String updateClientDetails = mapper.writeValueAsString(clientDetails);

    // Send the updated client details
    response = invokeClientDetailAPI(updateClientDetails, clientUrl, HttpMethod.PUT);
    ClientDetails actualClientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(clientDetails.getId(), actualClientDetails.getId());
    assertEquals("updatedUser@test.com", actualClientDetails.getDirectUser());
    assertClientDetails(clientDetails, actualClientDetails);
  }

  @Test
  public void testUpdateClient_existing_withDifferentID() {
    // Insert ClientDetail row
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Update same client details above - this will act as duplicate due to different ID
    response = invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.PUT);
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
  }

  @Test
  public void testGetClientDetailsById() throws IOException {
    // Insert ClientDetail Row
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    ClientDetails clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Fetch the row
    String getUrl = clientUrl + "/" + clientDetails.getId();
    response = invokeClientDetailAPI(clientDetailPayload, getUrl, HttpMethod.GET);
    ClientDetails actualClientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertClientDetails(clientDetails, actualClientDetails);
  }

  @Test
  public void testGetClientDetailsByURL() throws IOException {
    // Insert ClientDetail Row
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    ClientDetails clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Fetch the row
    String getUrl = clientUrl + "?url=" + clientDetails.getFhirServerBaseURL();
    response = invokeClientDetailAPI(clientDetailPayload, getUrl, HttpMethod.GET);
    ClientDetails actualClientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertClientDetails(clientDetails, actualClientDetails);
  }

  @Test
  public void testGetAllClientDetails() throws IOException {
    // Insert first ClientDetail Row
    ResponseEntity<String> response =
        invokeClientDetailAPI(clientDetailPayload, clientUrl, HttpMethod.POST);
    ClientDetails clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Insert second ClientDetail Row
    clientDetails.setId(null);
    clientDetails.setFhirServerBaseURL("https://modified/second/fhir/url");
    clientDetails.setClientId("modifiedclientidfortest");
    String secondClientDetails = mapper.writeValueAsString(clientDetails);
    response = invokeClientDetailAPI(secondClientDetails, clientUrl, HttpMethod.POST);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Fetch the row
    String getUrl = clientUrl + "/";
    response = invokeClientDetailAPI(clientDetailPayload, getUrl, HttpMethod.GET);

    List<ClientDetails> clientList =
        (List<ClientDetails>) mapper.readValue(response.getBody(), List.class);

    assertEquals(2, clientList.size());
  }

  private ResponseEntity<String> invokeClientDetailAPI(
      String testSaveClientData, String url, HttpMethod post) {
    headers.setContentType(MediaType.APPLICATION_JSON);
    HttpEntity<String> entity = new HttpEntity<>(testSaveClientData, headers);
    return restTemplate.exchange(url, post, entity, String.class);
  }

  private void assertClientDetails(
      ClientDetails expectedClientDetails, ClientDetails actualClientDetails) {

    assertEquals(expectedClientDetails.getClientId(), actualClientDetails.getClientId());
    assertEquals(
        expectedClientDetails.getFhirServerBaseURL(), actualClientDetails.getFhirServerBaseURL());
    assertEquals(expectedClientDetails.getTokenURL(), actualClientDetails.getTokenURL());
    assertEquals(expectedClientDetails.getScopes(), actualClientDetails.getScopes());
    assertEquals(
        expectedClientDetails.getDirectRecipientAddress(),
        actualClientDetails.getDirectRecipientAddress());
    assertEquals(expectedClientDetails.getDirectUser(), actualClientDetails.getDirectUser());
    assertEquals(expectedClientDetails.getRestAPIURL(), actualClientDetails.getRestAPIURL());
    assertEquals(expectedClientDetails.getDirectHost(), actualClientDetails.getDirectHost());
    assertEquals(
        expectedClientDetails.getAssigningAuthorityId(),
        actualClientDetails.getAssigningAuthorityId());
    assertEquals(
        expectedClientDetails.getEncounterEndThreshold(),
        actualClientDetails.getEncounterEndThreshold());
    assertEquals(
        expectedClientDetails.getEncounterStartThreshold(),
        actualClientDetails.getEncounterStartThreshold());
    assertEquals(expectedClientDetails.getImapPort(), actualClientDetails.getImapPort());
    assertEquals(expectedClientDetails.getSmtpPort(), actualClientDetails.getSmtpPort());
    assertEquals(
        expectedClientDetails.getXdrRecipientAddress(),
        actualClientDetails.getXdrRecipientAddress());
    assertEquals(
        expectedClientDetails.getDebugFhirQueryAndEicr(),
        actualClientDetails.getDebugFhirQueryAndEicr());
    assertEquals(expectedClientDetails.getIsCovid(), actualClientDetails.getIsCovid());
    assertEquals(expectedClientDetails.getIsSystem(), actualClientDetails.getIsSystem());
    assertEquals(expectedClientDetails.getIsDirect(), actualClientDetails.getIsDirect());
    assertEquals(expectedClientDetails.getIsFullEcr(), actualClientDetails.getIsFullEcr());
    assertEquals(expectedClientDetails.getIsProvider(), actualClientDetails.getIsProvider());
    assertEquals(expectedClientDetails.getIsRestAPI(), actualClientDetails.getIsRestAPI());
    assertEquals(expectedClientDetails.getIsXdr(), actualClientDetails.getIsXdr());
  }
}
