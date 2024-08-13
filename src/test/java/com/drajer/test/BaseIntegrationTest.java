package com.drajer.test;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static org.junit.Assert.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.sof.model.ClientDetails;
import com.drajer.test.util.TestUtils;
import com.drajer.test.util.WireMockHandle;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import java.io.IOException;
import java.util.TimeZone;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.*;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.rules.SpringClassRule;
import org.springframework.test.context.junit4.rules.SpringMethodRule;
import org.springframework.transaction.annotation.Transactional;

@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = SpringConfiguration.class)
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
@AutoConfigureMockMvc
@Transactional
@ActiveProfiles("test")
public abstract class BaseIntegrationTest {

  private static final Logger logger = LoggerFactory.getLogger(BaseIntegrationTest.class);

  @ClassRule public static final SpringClassRule SPRING_CLASS_RULE = new SpringClassRule();

  @Rule public final SpringMethodRule springMethodRule = new SpringMethodRule();

  @LocalServerPort protected int port;

  @Autowired protected SessionFactory sessionFactory;
  protected Session session = null;
  protected Transaction tx = null;
  protected HttpHeaders headers = new HttpHeaders();

  protected static final TestRestTemplate restTemplate = new TestRestTemplate();
  protected static final ObjectMapper mapper = new ObjectMapper();

  protected static final String URL = "http://localhost:";
  protected static final String fhirBaseUrl = "/FHIR";

  protected static final int wireMockHttpPort = 9011;
  protected WireMockServer wireMockServer;
  protected ClientDetails clientDetails;

  @Before
  public void setUp() throws Throwable {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    session = sessionFactory.openSession();
    wireMockServer = WireMockHandle.getInstance().getWireMockServer(wireMockHttpPort);
    wireMockServer.resetMappings();
    headers.clear();
  }

  @After
  public void tearDown() {
    if (session != null) {
      session.close();
    }
  }

  protected String getSystemLaunchPayload(String systemLaunchFile) throws IOException {
    try {

      String systemLaunchPayload = TestUtils.getFileContentAsString(systemLaunchFile);

      // Hardcode FHIR URL to match clientDetail to avoid mistakes in test data file.
      String fhirUrl =
          clientDetails != null
              ? clientDetails.getFhirServerBaseURL()
              : URL + wireMockHttpPort + fhirBaseUrl;
      JSONObject jsonObject = new JSONObject(systemLaunchPayload);
      jsonObject.put("fhirServerURL", fhirUrl);
      systemLaunchPayload = jsonObject.toString();

      return systemLaunchPayload;
    } catch (JSONException e) {
      throw new IOException(e);
    }
  }

  protected String getSystemLaunch3Payload(String systemLaunchFile) {

    return TestUtils.getFileContentAsString(systemLaunchFile);
  }

  protected void createClientDetails(String clientDetailsFile) throws IOException, JSONException {

    String clientDetailString = TestUtils.getFileContentAsString(clientDetailsFile);

    // Hardcode FHIR & Token URL to avoid mistakes in test data file.
    String fhirUrl = URL + wireMockHttpPort + fhirBaseUrl;
    String authUrl = fhirUrl + "/token";
    String directUrl = URL + wireMockHttpPort + "/directurl";
    JSONObject jsonObject = new JSONObject(clientDetailString);
    jsonObject.put("fhirServerBaseURL", fhirUrl);
    jsonObject.put("tokenURL", authUrl);
    jsonObject.put("restAPIURL", directUrl);
    clientDetailString = jsonObject.toString();

    // Insert ClientDetails Row
    headers.setContentType(MediaType.APPLICATION_JSON);
    HttpEntity<String> entity = new HttpEntity<>(clientDetailString, headers);
    ResponseEntity<String> response =
        restTemplate.exchange(
            createURLWithPort("/api/clientDetails"), HttpMethod.POST, entity, String.class);
    clientDetails = mapper.readValue(response.getBody(), ClientDetails.class);

    assertEquals("Failed to save client details", HttpStatus.OK, response.getStatusCode());
  }

  protected ResponseEntity<String> invokeSystemLaunch(
      String requestId, String systemLaunchPayload) {

    logger.info("Invoking systemLaunch with payload: {}", systemLaunchPayload);

    headers.setContentType(MediaType.APPLICATION_JSON);
    headers.add("X-Request-ID", requestId);
    HttpEntity<String> entity = new HttpEntity<>(systemLaunchPayload, headers);
    return restTemplate.exchange(
        createURLWithPort("/api/systemLaunch"), HttpMethod.POST, entity, String.class);
  }

  protected String createURLWithPort(String uri) {
    return URL + port + uri;
  }

  protected void mockRestApi() {

    String restResponse = "{\"status\":\"success\"}";

    wireMockServer.stubFor(
        post(urlPathEqualTo("/directurl"))
            .atPriority(1)
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withBody(restResponse)
                    .withHeader("Content-Type", "application/json; charset=utf-8")));
  }
}
