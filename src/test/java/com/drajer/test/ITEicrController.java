package com.drajer.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.drajer.ecrapp.model.Eicr;
import com.fasterxml.jackson.core.JsonProcessingException;
import java.util.HashMap;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.springframework.http.*;

public class ITEicrController extends BaseIntegrationTest {

  private Eicr expectedEicr;

  @Before
  public void setUp() throws Throwable {
    super.setUp();
    tx = session.beginTransaction();
    createEicr();
    session.flush();
    tx.commit();
  }

  @Test
  public void testDeleteEicrByEicrDocID_Success() {

    String url = createURLWithPort("/api/eicr");
    url = url + "?eicrDocId=" + expectedEicr.getEicrDocId();

    ResponseEntity<String> response = invokeEicrAPI("", url, HttpMethod.DELETE);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals("Eicr deleted successfully", response.getBody());
  }

  @Test
  public void testDeleteEicrByEicrDocID_BadRequest() {

    String url = createURLWithPort("/api/eicr");
    url = url + "?eicrDocId=";

    ResponseEntity<String> response = invokeEicrAPI("", url, HttpMethod.DELETE);
    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Requested Eicr Doc Id is missing or empty", response.getBody());
  }

  @Test
  public void testDeleteEicrByEicrDocID_NotFound() {

    String url = createURLWithPort("/api/eicr");
    url = url + "?eicrDocId=" + "wrong-eicr-doc-id";

    ResponseEntity<String> response = invokeEicrAPI("", url, HttpMethod.DELETE);
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    assertEquals("Eicr Not found", response.getBody());
  }

  @Test
  public void testgetEicrByEicrDocID_Success() throws JsonProcessingException {

    String url = createURLWithPort("/api/eicr");
    url = url + "?eicrDocId=" + expectedEicr.getEicrDocId();

    ResponseEntity<String> response = invokeEicrAPI("", url, HttpMethod.GET);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    Eicr actualEicr = mapper.readValue(response.getBody(), Eicr.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEicr(expectedEicr, actualEicr);
  }

  @Test
  public void testgetEicrByEicrDocID_BadRequest() {

    String url = createURLWithPort("/api/eicr");
    url = url + "?eicrDocId=";

    ResponseEntity<String> response = invokeEicrAPI("", url, HttpMethod.GET);
    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Requested eicrDocId is missing or empty", response.getBody());
  }

  @Test
  public void testgetEicrByEicrDocID_NotFound() {

    String url = createURLWithPort("/api/eicr");
    url = url + "?eicrDocId=" + "wrong-eicr-doc-id";

    ResponseEntity<String> response = invokeEicrAPI("", url, HttpMethod.GET);
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    assertEquals("Failed to find EICR by EICR_DOC_ID: wrong-eicr-doc-id", response.getBody());
  }

  @Test
  public void testgetEicrAndRRByRequestId_Success() throws JsonProcessingException {

    String url = createURLWithPort("/api/eicrAndRRData");
    url = url + "?xRequestId=" + expectedEicr.getxRequestId();

    ResponseEntity<String> response = invokeEicrAPI("", url, HttpMethod.GET);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    List<Object> actualEicr = (List<Object>) mapper.readValue(response.getBody(), List.class);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertNotNull(actualEicr);
    assertEquals(1, actualEicr.size());
    assertNotNull(actualEicr.get(0));
    HashMap<String, String> result = (HashMap) actualEicr.get(0);
    assertEquals(expectedEicr.getEicrData(), result.get("eicrData"));
    assertEquals(expectedEicr.getResponseData(), result.get("responseData"));
  }

  private void createEicr() {
    expectedEicr = new Eicr();
    expectedEicr.setFhirServerUrl("https://dummy/fhir/url");
    expectedEicr.setLaunchPatientId("12345");
    expectedEicr.setEncounterId("67890");
    expectedEicr.setDocVersion(1);
    expectedEicr.setxRequestId("RRTESTXREQUESTID");
    expectedEicr.setSetId("12345|67890");
    expectedEicr.setxCorrelationId("RR-TEST-XCORRELATIONID");
    expectedEicr.setEicrDocId("junitest-eicr-doc-id-fortesting");
    expectedEicr.setEicrData("This is a dummy EICR for test");
    expectedEicr.setResponseData("This is a dummy Reportability Response");
    expectedEicr.setResponseDocId("junitest-rr-doc-id-fortesting");
    expectedEicr.setResponseType("RRSV1");
    expectedEicr.setResponseTypeDisplay("RRSV1");
    expectedEicr.setResponseXRequestId("Xrequest-id-for-rr-junitest");
    expectedEicr.setEhrDocRefId("ehrdoc-refid-for-junitest");

    session.saveOrUpdate(expectedEicr);
  }

  private ResponseEntity<String> invokeEicrAPI(String requestBody, String url, HttpMethod method) {
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add("X-Request-ID", "test-eicr-apis-junit-test");
    HttpEntity<String> entity;
    if (method == HttpMethod.GET) {
      entity = new HttpEntity<>(headers);
    } else {
      entity = new HttpEntity<>(requestBody, headers);
    }
    return restTemplate.exchange(url, method, entity, String.class);
  }

  private void assertEicr(Eicr expectedEicr, Eicr actualEicr) {
    assertEquals(expectedEicr.getEicrDocId(), actualEicr.getEicrDocId());
    assertEquals(expectedEicr.getEhrDocRefId(), actualEicr.getEhrDocRefId());
    assertEquals(expectedEicr.getEicrData(), actualEicr.getEicrData());
    assertEquals(expectedEicr.getEncounterId(), actualEicr.getEncounterId());
    assertEquals(expectedEicr.getLaunchPatientId(), actualEicr.getLaunchPatientId());
    assertEquals(expectedEicr.getFhirServerUrl(), actualEicr.getFhirServerUrl());
    assertEquals(expectedEicr.getResponseData(), actualEicr.getResponseData());
    assertEquals(expectedEicr.getResponseDocId(), actualEicr.getResponseDocId());
    assertEquals(expectedEicr.getResponseType(), actualEicr.getResponseType());
    assertEquals(expectedEicr.getResponseTypeDisplay(), actualEicr.getResponseTypeDisplay());
    assertEquals(expectedEicr.getResponseXRequestId(), actualEicr.getResponseXRequestId());
    assertEquals(expectedEicr.getxRequestId(), actualEicr.getxRequestId());
  }
}
