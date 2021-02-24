package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.WireMockHelper;
import com.fasterxml.jackson.core.JsonProcessingException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import org.apache.http.client.utils.URIBuilder;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;

public class ITRRReceiverServiceController extends BaseIntegrationTest {

  private static final Logger logger = LoggerFactory.getLogger(ITRRReceiverServiceController.class);
  WireMockHelper stubHelper;
  private Eicr eicr;

  @Before
  public void setUp() throws Throwable {
    try {
      super.setUp();
      tx = session.beginTransaction();
      createClientDetails("R4/Misc/ClientDetails/ClientDataEntry1.json");
      eicr = createEicr();
      session.flush();
      tx.commit();
    } catch (IOException e) {
      logger.error("Error setting up the test data:", e);
      fail("Fix the exception: " + e.getMessage());
    }

    Map<String, String> map = new HashMap<>();
    map.put("token", "R4/Misc/AccessToken.json");
    map.put("metadata", "R4/Misc/MetaData_r4.json");

    stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
    stubHelper.stubAuthAndMetadata(map);

    wireMockServer.stubFor(
        post(urlPathEqualTo(
                getURLPath(clientDetails.getFhirServerBaseURL()) + "/DocumentReference"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(201)
                    .withHeader("Content-Type", "application/json+fhir; charset=utf-8")));
  }

  @Test
  public void testRRReceiver_WithRR_Success() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    eicr = getEICRDocument(eicr.getId().toString());
    assertEquals("RRVS1", eicr != null ? eicr.getResponseType() : null);
    assertEquals(rr != null ? rr.getRrXml() : "", eicr != null ? eicr.getResponseData() : null);
    assertEquals("123456", eicr != null ? eicr.getResponseXRequestId() : null);
  }

  @Test
  public void testRRReceiver_WithRR_FailToPost() {
    // Mock FHIR Document Reference to return failure.
    wireMockServer.stubFor(
        post(urlPathEqualTo(
                getURLPath(clientDetails.getFhirServerBaseURL()) + "/DocumentReference"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(HttpStatus.BAD_REQUEST.value())
                    .withHeader("Content-Type", "application/json+fhir; charset=utf-8")));

    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void testRRReceiver_WithRR_WrongDocID() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    // Setting different DocID then in DB
    if (rr != null) {
      String rrXml =
          rr.getRrXml().replace("69550923-8b72-475c-b64b-5f7c44a78e4f", "WrongXCorrelationID");
      rr.setRrXml(rrXml);
      ResponseEntity<String> response = postReportabilityResponse(rr, eicr);
      assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
  }

  @Test
  public void testRRReceiver_WithEmptyRR() {
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setResponseType("RR");
    rr.setRrXml("");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);
    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
  }

  @Test
  public void testRRReceiver_WithMDN() {
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setResponseType("MDN");
    rr.setRrXml("");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);
    assertEquals(HttpStatus.OK, response.getStatusCode());

    eicr = getEICRDocument(eicr.getId().toString());
    assertEquals("FAILURE_MDN", eicr != null ? eicr.getResponseType() : null);
    assertEquals(rr.getRrXml(), eicr != null ? eicr.getResponseData() : "");
    assertEquals("123456", eicr != null ? eicr.getResponseXRequestId() : "");
  }

  @Test
  public void testRRReceiver_WithMDN_WrongCorrelationID() {
    ReportabilityResponse rr = new ReportabilityResponse();
    rr.setResponseType("MDN");
    rr.setRrXml("");
    // Setting different CorrelationID then DB
    eicr.setxCoorrelationId("WrongXCorrelationID");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);
    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
  }

  @Test
  public void testRRReceiver_With_EmptyDocId() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    if (rr != null) {
      String rrXml = rr.getRrXml().replace("RR-TEST-XCORRELATIONID", "");
      rr.setRrXml(rrXml);
      ResponseEntity<String> response = postReportabilityResponse(rr, eicr);

      assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }
  }

  private String getURLPath(String url) {
    java.net.URL fullUrl = null;

    try {
      fullUrl = new URL(url);
      if (fullUrl != null) {
        return fullUrl.getPath();
      }
    } catch (MalformedURLException e) {
      fail(e.getMessage() + " This exception is not expected fix the test.");
    }
    return null;
  }

  private Eicr createEicr() {
    eicr = new Eicr();
    eicr.setFhirServerUrl(clientDetails.getFhirServerBaseURL());
    eicr.setLaunchPatientId("12345");
    eicr.setEncounterId("67890");
    eicr.setDocVersion(1);
    eicr.setxRequestId("RRTESTXREQUESTID");
    eicr.setSetId("12345|67890");
    eicr.setxCoorrelationId("RR-TEST-XCORRELATIONID");
    eicr.setEicrDocId("69550923-8b72-475c-b64b-5f7c44a78e4f");
    eicr.setEicrData("This is a dummy EICR for test");

    session.saveOrUpdate(eicr);

    return eicr;
  }

  private Eicr getEICRDocument(String eicrId) {
    try {
      Eicr eicr = session.get(Eicr.class, Integer.parseInt(eicrId));
      if (eicr != null) {
        session.refresh(eicr);
        return eicr;
      }
    } catch (Exception e) {
      logger.error("Exception retrieving EICR ", e);
      fail("Something went wrong retrieving EICR, check the log");
    }
    return null;
  }

  private ReportabilityResponse getReportabilityResponse(String filename) {
    String rrResponse = TestUtils.getFileContentAsString(filename);
    try {
      if (rrResponse != null) {
        return mapper.readValue(rrResponse, ReportabilityResponse.class);
      }
    } catch (JsonProcessingException e) {
      fail("This exception is not expected, fix the test");
    }
    return null;
  }

  private ResponseEntity<String> postReportabilityResponse(ReportabilityResponse rr, Eicr eicr) {

    headers.clear();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add("X-Request-ID", "123456");
    headers.add("X-Correlation-ID", eicr.getxCoorrelationId());

    URIBuilder ub;
    try {
      ub = new URIBuilder(createURLWithPort("/api/rrReceiver"));

      HttpEntity<ReportabilityResponse> entity = new HttpEntity<>(rr, headers);
      return restTemplate.exchange(ub.toString(), HttpMethod.POST, entity, String.class);

    } catch (URISyntaxException e) {
      logger.error("Error building the URL", e);
      fail("Fix the exception: " + e.getMessage());
    }

    return null;
  }
}
