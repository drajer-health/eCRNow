package com.drajer.routing;

import static org.assertj.core.api.Fail.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.security.AuthorizationService;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpServer;
import java.io.File;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpHeaders;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

public class RestApiSenderTest {
  private HttpServer server;
  private String url;

  @Before
  public void startServer() throws Exception {

    server = HttpServer.create(new InetSocketAddress(0), 0);
    url = "http://localhost:" + server.getAddress().getPort() + "/echo";
    server.createContext(
        "/echo",
        exchange -> {
          String responseJson = "{\"status\":200,\"result\":\"success\"}";
          exchange.getResponseHeaders().add("Content-Type", "application/json");
          exchange.sendResponseHeaders(200, responseJson.getBytes().length);
          OutputStream os = exchange.getResponseBody();
          os.write(responseJson.getBytes());
          os.close();
        });
    server.start();
  }

  /**
   * RestApiSender now uses @Autowired field injection (no-arg constructor only) and both fields are
   * private, so they are set through reflection.
   */
  private static RestApiSender newSender(
      AuthorizationService authService, RestTemplate restTemplate) {
    RestApiSender sender = new RestApiSender();
    ReflectionTestUtils.setField(sender, "authorizationService", authService);
    ReflectionTestUtils.setField(sender, "restTemplate", restTemplate);
    return sender;
  }

  @Test
  @Ignore
  public void testSendEicrXmlDocument_realData() throws Exception {
    LaunchDetails launchDetails = new LaunchDetails();
    launchDetails.setRestAPIURL(url);
    Eicr ecr = new Eicr();
    ecr.setFhirServerUrl("https://fhir.example.com");
    ecr.setLaunchPatientId("patient-123");
    ecr.setEncounterId("enc-456");
    ecr.setSetId("set-789");
    ecr.setxRequestId("req-001");
    ecr.setxCorrelationId("corr-001");
    String eicrXml = "<ClinicalDocument><id root='123'/></ClinicalDocument>";

    AuthorizationService authService = mock(AuthorizationService.class);
    RestTemplate restTemplate = new RestTemplate();
    RestApiSender sender = newSender(authService, restTemplate);

    JSONObject response = sender.sendEicrXmlDocument(launchDetails, eicrXml, ecr);
    assertEquals(200, response.getInt("status"));
    assertEquals("success", response.getString("result"));
  }

  @Test
  @Ignore
  public void testSendEicrXmlDocument_exceptionPath() throws Exception {
    LaunchDetails launchDetails = new LaunchDetails();
    launchDetails.setRestAPIURL("http://invalid-url");
    Eicr ecr = new Eicr();
    ecr.setFhirServerUrl("https://fhir.example.com");
    ecr.setLaunchPatientId("patient-123");
    ecr.setEncounterId("enc-456");
    ecr.setSetId("set-789");
    ecr.setxRequestId("req-001");
    ecr.setxCorrelationId("corr-001");
    String eicrXml = "<ClinicalDocument><id root='123'/></ClinicalDocument>";

    AuthorizationService authService = mock(AuthorizationService.class);
    RestTemplate restTemplate = new RestTemplate();
    RestApiSender sender = newSender(authService, restTemplate);

    try {
      sender.sendEicrXmlDocument(launchDetails, eicrXml, ecr);

      fail("Expected RuntimeException to be thrown for invalid URL");

    } catch (RuntimeException ex) {

      assertTrue(
          "Exception message should contain error details",
          ex.getMessage() != null && ex.getMessage().length() > 0);
    }
  }

  @Test
  public void sendEicrXmlDocument_postsTheRealEicrToTheEndpointAsTriggerJson() throws Exception {
    Eicr ecr =
        new ObjectMapper().readValue(new File("src/test/resources/R4/Misc/eicr.json"), Eicr.class);
    String eicrXml =
        new String(
            Files.readAllBytes(new ClassPathResource("Bsa/report/ecir_r11.xml").getFile().toPath()),
            StandardCharsets.UTF_8);
    AtomicReference<String> receivedBody = new AtomicReference<>();
    AtomicReference<Headers> receivedHeaders = new AtomicReference<>();
    server.createContext(
        "/capture",
        exchange -> {
          receivedBody.set(
              new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
          receivedHeaders.set(exchange.getRequestHeaders());
          byte[] reply = "{\"result\":\"success\"}".getBytes(StandardCharsets.UTF_8);
          exchange.getResponseHeaders().add("Content-Type", "application/json");
          exchange.sendResponseHeaders(200, reply.length);
          try (OutputStream os = exchange.getResponseBody()) {
            os.write(reply);
          }
        });
    LaunchDetails launchDetails = new LaunchDetails();
    launchDetails.setRestAPIURL(url.replace("/echo", "/capture"));
    HttpHeaders authorization = new HttpHeaders();
    authorization.add("Authorization", "Bearer ehr-access-token");
    AuthorizationService authService = mock(AuthorizationService.class);
    when(authService.getAuthorizationHeader(launchDetails)).thenReturn(authorization);

    JSONObject response =
        newSender(authService, new RestTemplate()).sendEicrXmlDocument(launchDetails, eicrXml, ecr);

    assertEquals(200, response.getInt("status"));
    assertEquals("success", response.getString("result"));
    JSONObject trigger = new JSONObject(receivedBody.get());
    assertEquals(ecr.getFhirServerUrl(), trigger.getString("fhirServerURL"));
    assertEquals(ecr.getLaunchPatientId(), trigger.getString("patientId"));
    assertEquals(ecr.getEncounterId(), trigger.getString("encounterId"));
    assertEquals(ecr.getSetId(), trigger.getString("eicrSetId"));
    assertEquals(eicrXml, trigger.getString("eicrXml"));
    assertEquals("Bearer ehr-access-token", receivedHeaders.get().getFirst("Authorization"));
    assertEquals(ecr.getxCorrelationId(), receivedHeaders.get().getFirst("X-Correlation-ID"));
    String newRequestId = receivedHeaders.get().getFirst("X-Request-ID");
    assertEquals(newRequestId, UUID.fromString(newRequestId).toString());
    assertNotEquals(ecr.getxRequestId(), newRequestId);
  }

  @Test
  public void sendEicrXmlDocument_whenTheEndpointFails_throwsWithTheHttpStatus() throws Exception {
    Eicr ecr =
        new ObjectMapper().readValue(new File("src/test/resources/R4/Misc/eicr.json"), Eicr.class);
    server.createContext(
        "/fail",
        exchange -> {
          exchange.sendResponseHeaders(500, -1);
          exchange.close();
        });
    LaunchDetails launchDetails = new LaunchDetails();
    launchDetails.setRestAPIURL(url.replace("/echo", "/fail"));
    RestApiSender withoutAuthorization = newSender(null, new RestTemplate());

    RuntimeException thrown =
        assertThrows(
            RuntimeException.class,
            () ->
                withoutAuthorization.sendEicrXmlDocument(
                    launchDetails, "<ClinicalDocument/>", ecr));

    assertTrue(thrown.getMessage().contains("500"));
  }
}
