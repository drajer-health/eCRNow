package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.ecrapp.service.impl.EicrServiceImpl;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;

public class ITRRReceiverServiceController extends BaseIntegrationTest {

  private static final String FHIR_DOCREF_URL = "/FHIR/DocumentReference?_pretty=true";

  private static final Logger logger = LoggerFactory.getLogger(ITRRReceiverServiceController.class);
  WireMockHelper stubHelper;
  private Eicr eicr;
  private Eicr eicrReSubmit;

  @Autowired private EicrServiceImpl eicrService;

  @Before
  public void setUp() throws Throwable {
    try {
      super.setUp();
      tx = session.beginTransaction();
      createClientDetails("R4/Misc/ClientDetails/ClientDetail_IT_FullECR.json");
      eicr = createEicr();
      eicrReSubmit = createEicrForReSubmit();
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

    String response =
        TestUtils.getFileContentAsString("R4/DocumentReference/DocumentReference.json");

    wireMockServer.stubFor(
        post(urlEqualTo(FHIR_DOCREF_URL))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(201)
                    .withBody(response)
                    .withHeader("Content-Type", "application/json+fhir; charset=utf-8")
                    .withHeader(
                        "location",
                        "http://localhost:"
                            + wireMockHttpPort
                            + "/r4/ec2458f2-1e24-41c8-b71b-0e701af7583d/DocumentReference/197477086")
                    .withHeader("x-request-id", "32034a8e-07ff-4bfb-a686-de8a956fbda9")
                    .withHeader("Cache-Control", "no-cache")));
  }

  @Test
  public void testRRReceiver_WithRR_Success() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Mock FHIR called.
    wireMockServer.verify(moreThanOrExactly(1), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

    eicr = getEICRDocument(eicr.getId().toString());
    assertEquals("RRVS1", eicr != null ? eicr.getResponseType() : null);
    assertEquals(rr != null ? rr.getRrXml() : "", eicr != null ? eicr.getResponseData() : null);
    assertEquals("123456", eicr != null ? eicr.getResponseXRequestId() : null);
    assertEquals("197477086", eicr.getEhrDocRefId());
  }

  @Test
  public void testRRReceiver_WithRRVS2_Success() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest_RRVS2.json");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Mock FHIR called.
    wireMockServer.verify(moreThanOrExactly(1), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

    eicr = getEICRDocument(eicr.getId().toString());
    assertEquals("RRVS2", eicr != null ? eicr.getResponseType() : null);
    assertEquals(rr != null ? rr.getRrXml() : "", eicr != null ? eicr.getResponseData() : null);
    assertEquals("123456", eicr != null ? eicr.getResponseXRequestId() : null);
    assertEquals("197477086", eicr.getEhrDocRefId());
  }

  @Test
  public void testRRReceiver_WithRRVS3() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest_RRVS2.json");
    rr.setRrXml(rr.getRrXml().replace("RRVS2", "RRVS3"));
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    // Mock FHIR not called (not reportable condition).
    wireMockServer.verify(moreThanOrExactly(0), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

    eicr = getEICRDocument(eicr.getId().toString());
    assertEquals("RRVS3", eicr != null ? eicr.getResponseType() : null);
    assertEquals(rr != null ? rr.getRrXml() : "", eicr != null ? eicr.getResponseData() : null);
    assertEquals("123456", eicr != null ? eicr.getResponseXRequestId() : null);
    assertNull(eicr.getEhrDocRefId());
  }

  @Test
  public void testRRReceiver_WithRRVS4() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest_RRVS2.json");
    rr.setRrXml(rr.getRrXml().replace("RRVS2", "RRVS4"));
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr);

    // Mock FHIR not called (not reportable condition).
    wireMockServer.verify(moreThanOrExactly(0), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

    assertEquals(HttpStatus.OK, response.getStatusCode());

    eicr = getEICRDocument(eicr.getId().toString());
    assertEquals("RRVS4", eicr != null ? eicr.getResponseType() : null);
    assertEquals(rr != null ? rr.getRrXml() : "", eicr != null ? eicr.getResponseData() : null);
    assertEquals("123456", eicr != null ? eicr.getResponseXRequestId() : null);
    assertNull(eicr.getEhrDocRefId());
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

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
  }

  @Test
  public void testRRReceiver_WithRR_NoSaveToEHR() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    ResponseEntity<String> response = postReportabilityResponse(rr, eicr, false);

    // Mock FHIR not called (reportable condition, but saveToEhr = false).
    wireMockServer.verify(moreThanOrExactly(0), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void testReSubmitRR_Success() {

    ResponseEntity<String> response = reSubmitRR(eicrReSubmit);

    // Mock FHIR called.
    wireMockServer.verify(moreThanOrExactly(1), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  public void testReSubmitRR_Failure() {
    // Mock FHIR Document Reference to return failure.
    wireMockServer.stubFor(
        post(urlPathEqualTo(
                getURLPath(clientDetails.getFhirServerBaseURL()) + "/DocumentReference"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(HttpStatus.BAD_REQUEST.value())
                    .withHeader("Content-Type", "application/json+fhir; charset=utf-8")));

    ResponseEntity<String> response = reSubmitRR(eicrReSubmit);

    // Mock FHIR called.
    wireMockServer.verify(moreThanOrExactly(1), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
  }

  @Test
  public void testReSubmitRR_OrphanRR() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest_RRVS2.json");
    eicrService.setProcessOrphanRr(true);
    // Setting different DocID then in DB
    if (rr != null) {
      String rrXml =
          rr.getRrXml().replace("69550923-8b72-475c-b64b-5f7c44a78e4f", "WrongXCorrelationID");
      rr.setRrXml(rrXml);
      String fhirUrl = "http://localhost:" + wireMockHttpPort + "/FHIR";
      rr.setFhirUrl(fhirUrl);
      ResponseEntity<String> response = postReportabilityResponse(rr, eicr);
      wireMockServer.verify(moreThanOrExactly(0), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));
      assertEquals(HttpStatus.OK, response.getStatusCode());

      Eicr eicr = new Eicr();
      eicr.setEicrDocId("WrongXCorrelationID");

      response = reSubmitRR(eicr);
      // Mock FHIR called.
      wireMockServer.verify(moreThanOrExactly(1), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));

      assertEquals(HttpStatus.OK, response.getStatusCode());
    }
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
    eicr.setxCorrelationId("WrongXCorrelationID");
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

  @Test
  public void testRRReceiver_OrphanRR_WithSetId() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    eicrService.setProcessOrphanRr(true);
    // Setting different DocID then in DB
    if (rr != null) {
      String rrXml =
          rr.getRrXml().replace("69550923-8b72-475c-b64b-5f7c44a78e4f", "WrongXCorrelationID");
      rr.setRrXml(rrXml);
      String fhirUrl = "http://localhost:" + wireMockHttpPort + "/FHIR";
      rr.setFhirUrl(fhirUrl);
      ResponseEntity<String> response = postReportabilityResponse(rr, eicr);
      wireMockServer.verify(moreThanOrExactly(0), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));
      assertEquals(HttpStatus.OK, response.getStatusCode());
    }
  }

  @Test
  public void testRRReceiver_OrphanRR_WithoutSetId() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest_RRVS2.json");
    eicrService.setProcessOrphanRr(true);
    // Setting different DocID then in DB
    if (rr != null) {
      String rrXml =
          rr.getRrXml().replace("69550923-8b72-475c-b64b-5f7c44a78e4f", "WrongXCorrelationID");
      rr.setRrXml(rrXml);
      String fhirUrl = "http://localhost:" + wireMockHttpPort + "/FHIR";
      rr.setFhirUrl(fhirUrl);
      ResponseEntity<String> response = postReportabilityResponse(rr, eicr);
      wireMockServer.verify(moreThanOrExactly(0), postRequestedFor(urlEqualTo(FHIR_DOCREF_URL)));
      assertEquals(HttpStatus.OK, response.getStatusCode());
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
    eicr.setxCorrelationId("RR-TEST-XCORRELATIONID");
    eicr.setEicrDocId("69550923-8b72-475c-b64b-5f7c44a78e4f");
    eicr.setEicrData("This is a dummy EICR for test");

    session.saveOrUpdate(eicr);

    return eicr;
  }

  private Eicr createEicrForReSubmit() {
    ReportabilityResponse rr = getReportabilityResponse("R4/Misc/rrTest.json");
    eicrReSubmit = new Eicr();
    eicrReSubmit.setFhirServerUrl(clientDetails.getFhirServerBaseURL());
    eicrReSubmit.setLaunchPatientId("123456");
    eicrReSubmit.setEncounterId("567890");
    eicrReSubmit.setDocVersion(2);
    eicrReSubmit.setxRequestId("RRRESUBMITTESTXREQUESTID");
    eicrReSubmit.setSetId("123456|567890");
    eicrReSubmit.setxCorrelationId("RR-RESUBMIT-TEST-XCORRELATIONID");
    eicrReSubmit.setEicrDocId("69550923-8b72-475c-b64b-5f7c44a78e4");
    eicrReSubmit.setEicrData("This is a dummy EICR for test");
    eicrReSubmit.setResponseData(rr != null ? rr.getRrXml() : "");
    eicrReSubmit.setResponseType(rr != null ? rr.getResponseType() : "");

    session.saveOrUpdate(eicrReSubmit);
    return eicrReSubmit;
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
    return postReportabilityResponse(rr, eicr, true);
  }

  private ResponseEntity<String> postReportabilityResponse(
      ReportabilityResponse rr, Eicr eicr, boolean saveToEhr) {

    headers.clear();
    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add("X-Request-ID", "123456");
    headers.add("X-Correlation-ID", eicr.getxCorrelationId());

    URIBuilder ub;
    try {
      ub = new URIBuilder(createURLWithPort("/api/rrReceiver"));
      ub.addParameter("saveToEhr", Boolean.toString(saveToEhr));

      HttpEntity<ReportabilityResponse> entity = new HttpEntity<>(rr, headers);
      return restTemplate.exchange(ub.toString(), HttpMethod.POST, entity, String.class);

    } catch (URISyntaxException e) {
      logger.error("Error building the URL", e);
      fail("Fix the exception: " + e.getMessage());
    }

    return null;
  }

  private ResponseEntity<String> reSubmitRR(Eicr eicr) {
    headers.clear();

    URIBuilder ub;
    try {
      ub = new URIBuilder(createURLWithPort("/api/reSubmitRR"));
      if (eicr.getId() != null) {
        ub.addParameter("eicrId", String.valueOf(eicr.getId()));
      }
      ub.addParameter("eicrDocId", eicr.getEicrDocId());
      logger.info("Constructed URL:::::" + ub.toString());
      return restTemplate.postForEntity(ub.toString(), null, String.class);

    } catch (URISyntaxException e) {
      logger.error("Error building the URL", e);
      fail("Fix the exception: " + e.getMessage());
    }

    return null;
  }
}
