package com.drajer.sof.utils;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.Response;
import com.drajer.sof.service.ClientDetailsService;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.web.client.RestTemplate;

@RunWith(MockitoJUnitRunner.Silent.class)
public class AuthorizationTest {

  private Authorization authorization;

  @Mock private ClientDetailsService clientDetailsService;

  @Mock private RestTemplate restTemplate;

  private HttpServer server;

  @Before
  public void setUp() {
    authorization = new Authorization();
  }

  @After
  public void stopServer() {
    if (server != null) {
      server.stop(0);
    }
  }

  @Test
  public void testGetMetadata_exception() {
    String url = "http://error/metadata";

    when(restTemplate.exchange(
            eq(url), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
        .thenThrow(new RuntimeException("Connection error"));

    JSONObject result = authorization.getMetadata(url);

    assertNull(result);
  }

  @Test
  public void testCreateAuthUrl() {
    LaunchDetails launch = mock(LaunchDetails.class);
    ClientDetails client = mock(ClientDetails.class);

    when(launch.getAuthUrl()).thenReturn("http://auth");
    when(launch.getRedirectURI()).thenReturn("http://redirect");
    when(launch.getLaunchId()).thenReturn("launch123");
    when(launch.getEhrServerURL()).thenReturn("http://ehr");
    when(client.getClientId()).thenReturn("client1");
    when(client.getScopes()).thenReturn("openid fhirUser");

    String result = authorization.createAuthUrl(launch, client, 123);

    assertNotNull(result);
    assertTrue(result.startsWith("http://auth?"));
    assertTrue(result.contains("response_type=code"));
    assertTrue(result.contains("client_id=client1"));
    assertTrue(result.contains("redirect_uri=http://redirect"));
    assertTrue(result.contains("launch=launch123"));
    assertTrue(result.contains("state=123"));
    assertTrue(result.contains("scope=openid fhirUser"));
    assertTrue(result.contains("aud=http://ehr"));
  }

  @Test
  public void testGetAccessToken_exception() {
    LaunchDetails launch = mock(LaunchDetails.class);

    when(launch.getClientId()).thenReturn("client1");
    when(launch.getTokenUrl()).thenReturn("http://token");

    when(restTemplate.exchange(
            eq("http://token"), eq(HttpMethod.POST), any(HttpEntity.class), eq(Response.class)))
        .thenThrow(new RuntimeException("Token error"));

    JSONObject result = authorization.getAccessToken(launch);

    assertNull(result);
  }

  @Test
  public void getMetadata_success_returnsTheJsonBody() throws Exception {
    String url =
        startServer(
            "/fhir/metadata",
            200,
            "{\"resourceType\":\"CapabilityStatement\",\"fhirVersion\":\"4.0.1\"}");

    JSONObject result = authorization.getMetadata(url);

    assertNotNull(result);
    assertEquals("CapabilityStatement", result.getString("resourceType"));
    assertEquals("4.0.1", result.getString("fhirVersion"));
  }

  @Test
  public void getMetadata_bodyIsNotJson_returnsNull() throws Exception {
    String url = startServer("/fhir/metadata", 200, "not json");

    assertNull(authorization.getMetadata(url));
  }

  @Test
  public void getMetadata_serverError_returnsNull() throws Exception {
    String url = startServer("/fhir/metadata", 500, "{\"error\":\"server_error\"}");

    assertNull(authorization.getMetadata(url));
  }

  @Test
  public void getAccessToken_success_returnsTheTokenResponse() throws Exception {
    String url =
        startServer(
            "/token",
            200,
            "{\"access_token\":\"token-123\",\"expires_in\":3600,\"patient\":\"patient-1\"}");
    LaunchDetails launch = mock(LaunchDetails.class);
    when(launch.getClientId()).thenReturn("client1");
    when(launch.getAuthorizationCode()).thenReturn("auth-code");
    when(launch.getRedirectURI()).thenReturn("http://redirect");
    when(launch.getTokenUrl()).thenReturn(url);

    JSONObject result = authorization.getAccessToken(launch);

    assertNotNull(result);
    assertTrue(result.toString().contains("token-123"));
    assertTrue(result.toString().contains("patient-1"));
  }

  @Test
  public void getAccessToken_serverError_returnsNull() throws Exception {
    String url = startServer("/token", 500, "{\"error\":\"server_error\"}");
    LaunchDetails launch = mock(LaunchDetails.class);
    when(launch.getClientId()).thenReturn("client1");
    when(launch.getTokenUrl()).thenReturn(url);

    assertNull(authorization.getAccessToken(launch));
  }

  @Test
  public void formatQueryParams_emptyMap_returnsEmptyString() {
    assertEquals("", authorization.formatQueryParams(new HashMap<>()));
  }

  @Test
  public void formatQueryParams_singleParam() {
    assertEquals("?only=one", authorization.formatQueryParams(Map.of("only", "one")));
  }

  @Test
  public void formatQueryParams_joinsParamsWithAmpersand() {
    Map<String, String> params = new LinkedHashMap<>();
    params.put("a", "1");
    params.put("b", "2");

    assertEquals("?a=1&b=2", authorization.formatQueryParams(params));
  }

  private String startServer(String path, int status, String body) throws IOException {
    server = HttpServer.create(new InetSocketAddress("localhost", 0), 0);
    server.createContext(
        path,
        exchange -> {
          exchange.getRequestBody().readAllBytes();
          byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
          exchange.getResponseHeaders().add("Content-Type", "application/json");
          exchange.sendResponseHeaders(status, bytes.length);
          try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
          }
        });
    server.start();
    return "http://localhost:" + server.getAddress().getPort() + path;
  }
}
