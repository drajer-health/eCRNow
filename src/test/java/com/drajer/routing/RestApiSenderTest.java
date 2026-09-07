package com.drajer.routing;

import static org.assertj.core.api.Fail.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.security.AuthorizationService;
import com.drajer.sof.model.LaunchDetails;
import com.sun.net.httpserver.HttpServer;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
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
    RestApiSender sender = new RestApiSender(authService, restTemplate);

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
    RestApiSender sender = new RestApiSender(authService, restTemplate);

    try {
      sender.sendEicrXmlDocument(launchDetails, eicrXml, ecr);

      fail("Expected RuntimeException to be thrown for invalid URL");

    } catch (RuntimeException ex) {

      assertTrue(
          "Exception message should contain error details",
          ex.getMessage() != null && ex.getMessage().length() > 0);
    }
  }
}
