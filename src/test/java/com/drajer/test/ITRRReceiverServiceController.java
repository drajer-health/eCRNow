package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.drajer.test.util.TestUtils;
import com.drajer.test.util.WireMockHelper;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import org.apache.http.client.utils.URIBuilder;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;

public class ITRRReceiverServiceController extends BaseIntegrationTest {

  private static final Logger logger = LoggerFactory.getLogger(ITRRReceiverServiceController.class);
  WireMockHelper stubHelper;

  @Before
  public void setUp() throws Throwable {

    try {
      super.setUp();
      tx = session.beginTransaction();
      createClientDetails("R4/Misc/ClientDetails/ClientDataEntry1.json");
      session.flush();
      tx.commit();
    } catch (IOException e) {
      logger.error("Error setting up the test data:", e);
      fail("Fix the exception: " + e.getMessage());
    }

    Map<String, String> map = new HashMap<>();
    map.put("token", "R4/Misc/AccessToken.json");
    map.put("metaData", "R4/Misc/MetaData_r4.json");

    stubHelper = new WireMockHelper(wireMockServer, wireMockHttpPort);
    stubHelper.stubAuthAndMetadata(map);

    stubFor(
        post(urlEqualTo(clientDetails.getFhirServerBaseURL() + "/DocumentReference"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/fhir+json; charset=utf-8")));
  }

  @Test
  @Ignore
  public void testRRReceiver() {

    headers.setContentType(MediaType.APPLICATION_JSON);

    URIBuilder ub;
    try {
      ub = new URIBuilder(createURLWithPort("/api/rrReceiver"));
      ub.addParameter("type", "RR");
      ub.addParameter("xRequestIdHttpHeaderValue", "testRRReceiver");
      ub.addParameter("fhirServerURL", clientDetails.getFhirServerBaseURL());
      ub.addParameter("patientId", "12345");
      ub.addParameter("encounterId", "67890");

      String rrResponse = TestUtils.getFileContentAsString("R4/Misc/reportabilityResponse.json");

      HttpEntity<String> entity = new HttpEntity<>(rrResponse, headers);
      ResponseEntity<String> response =
          restTemplate.exchange(ub.toString(), HttpMethod.POST, entity, String.class);
      assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());

    } catch (URISyntaxException e) {
      logger.error("Error building the URL", e);
      fail("Fix the exception: " + e.getMessage());
    }
  }
}
