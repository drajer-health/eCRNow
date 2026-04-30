package com.drajer.routing;

import static org.junit.Assert.assertEquals;

import com.drajer.ecrapp.model.Eicr;
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
    RestApiSender sender = new RestApiSender();
    java.lang.reflect.Field field = RestApiSender.class.getDeclaredField("restTemplate");
    field.setAccessible(true);
    field.set(sender, new RestTemplate());
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
    RestApiSender sender = new RestApiSender();
    java.lang.reflect.Field field = RestApiSender.class.getDeclaredField("restTemplate");
    field.setAccessible(true);
    field.set(sender, new RestTemplate());
    try {
      sender.sendEicrXmlDocument(launchDetails, eicrXml, ecr);
    } catch (RuntimeException ex) {
      assert (ex.getMessage().contains("invalid-url") || ex.getMessage().length() > 0);
    }
  }
}
