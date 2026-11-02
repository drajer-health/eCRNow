package com.drajer.ecrapp.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.drajer.ecrapp.exceptions.KeycloakCredentialsException;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import okhttp3.OkHttpClient;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.util.ReflectionTestUtils;

public class KeyCloakTokenValidationClientHttpTest {

  private static final String TOKEN_RESPONSE =
      "{\"access_token\":\"eyJhbGciOiJSUzI1NiJ9\",\"expires_in\":300,\"token_type\":\"Bearer\"}";

  private HttpServer keycloak;
  private final AtomicReference<String> receivedPath = new AtomicReference<>();
  private final AtomicReference<String> receivedBody = new AtomicReference<>();
  private final AtomicReference<String> receivedContentType = new AtomicReference<>();
  private final AtomicInteger responseStatus = new AtomicInteger(200);
  private final AtomicReference<String> responseBody = new AtomicReference<>(TOKEN_RESPONSE);
  private MockEnvironment environment;
  private KeyCloakTokenValidationClient client;

  @Before
  public void startKeycloak() throws IOException {
    keycloak = HttpServer.create(new InetSocketAddress(0), 0);
    keycloak.createContext(
        "/",
        exchange -> {
          receivedPath.set(exchange.getRequestURI().getPath());
          receivedContentType.set(exchange.getRequestHeaders().getFirst("Content-Type"));
          receivedBody.set(
              new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8));
          byte[] reply = responseBody.get().getBytes(StandardCharsets.UTF_8);
          exchange.sendResponseHeaders(responseStatus.get(), reply.length);
          try (OutputStream out = exchange.getResponseBody()) {
            out.write(reply);
          }
        });
    keycloak.start();
    int port = keycloak.getAddress().getPort();
    environment =
        new MockEnvironment()
            .withProperty("keycloak.auth.server", "http://localhost:" + port)
            .withProperty("keycloak.realm", "ecr")
            .withProperty("keycloak.client.id", "ecr-client")
            .withProperty("keycloak.client.secret", "s3cr3t");
    client = newClient(environment);
  }

  /**
   * KeyCloakTokenValidationClient now uses @Autowired field injection (no-arg constructor only), so
   * the OkHttpClient and Environment are set on its fields directly.
   */
  private static KeyCloakTokenValidationClient newClient(MockEnvironment env) {
    KeyCloakTokenValidationClient c = new KeyCloakTokenValidationClient();
    ReflectionTestUtils.setField(c, "client", new OkHttpClient());
    ReflectionTestUtils.setField(c, "environment", env);
    return c;
  }

  @After
  public void stopKeycloak() {
    keycloak.stop(0);
  }

  @Test
  public void validateToken_returnsFalseWhenKeycloakRejectsTheRequest() {
    responseStatus.set(401);
    MockHttpServletRequest request = new MockHttpServletRequest();
    request.addHeader("Authorization", "Bearer abc.def.ghi");

    assertFalse(client.validateToken(request));
  }

  @Test
  public void generateToken_returnsTheKeycloakTokenWithTheSuccessFlag() throws Exception {
    Map<String, Object> details = new HashMap<>();
    details.put("grant_type", "client_credentials");

    JSONObject token = (JSONObject) client.generateToken(details);

    assertEquals("/realms/ecr/protocol/openid-connect/token", receivedPath.get());
    assertEquals("grant_type=client_credentials", receivedBody.get());
    assertEquals("eyJhbGciOiJSUzI1NiJ9", token.getString("access_token"));
    assertEquals(300, token.getInt("expires_in"));
    assertTrue(token.getBoolean("isSuccess"));
  }

  @Test
  public void generateToken_marksTheResponseUnsuccessfulWhenKeycloakFails() throws Exception {
    responseStatus.set(400);
    responseBody.set("{\"error\":\"invalid_grant\"}");

    JSONObject token = (JSONObject) client.generateToken(new HashMap<>(Map.of("a", "b")));

    assertFalse(token.getBoolean("isSuccess"));
    assertEquals("invalid_grant", token.getString("error"));
  }

  @Test
  public void generateToken_failsWhenKeycloakIsUnreachableOrReturnsInvalidJson() {
    responseBody.set("<html>not json</html>");
    Map<String, Object> details = new HashMap<>(Map.of("a", "b"));

    KeycloakCredentialsException invalid =
        assertThrows(KeycloakCredentialsException.class, () -> client.generateToken(details));
    environment.setProperty("keycloak.auth.server", "http://localhost:1");
    KeyCloakTokenValidationClient offline = newClient(environment);
    IOException unreachable = assertThrows(IOException.class, () -> offline.generateToken(details));

    assertTrue(invalid.getMessage().contains("generate Method"));
    assertTrue(unreachable.getMessage().contains("validateToken"));
  }

  @Test
  public void generateUserAuthToken_usesThePasswordGrantWithoutARefreshToken() throws Exception {
    Map<String, Object> details = new HashMap<>();
    details.put("username", "clinician");
    details.put("password", "pw");

    JSONObject token = (JSONObject) client.generateUserAuthToken(details);

    assertTrue(token.getBoolean("isSuccess"));
    assertTrue(receivedBody.get().contains("grant_type=password"));
    assertTrue(receivedBody.get().contains("client_id=ecr-client"));
    assertTrue(receivedBody.get().contains("client_secret=s3cr3t"));
    assertTrue(receivedBody.get().contains("username=clinician"));
  }

  @Test
  public void generateUserAuthToken_reportsFailuresOfKeycloak() throws Exception {
    responseStatus.set(401);
    responseBody.set("{\"error\":\"invalid_token\"}");
    Map<String, Object> details = new HashMap<>(Map.of("refresh_token", "old"));

    JSONObject token = (JSONObject) client.generateUserAuthToken(details);
    responseStatus.set(200);
    responseBody.set("not json");

    assertFalse(token.getBoolean("isSuccess"));
    assertTrue(receivedBody.get().contains("grant_type=refresh_token"));
    assertThrows(
        KeycloakCredentialsException.class,
        () -> client.generateUserAuthToken(new HashMap<>(Map.of("refresh_token", "old"))));
    environment.setProperty("keycloak.auth.server", "http://localhost:1");
    KeyCloakTokenValidationClient offline = newClient(environment);
    assertThrows(
        IOException.class,
        () -> offline.generateUserAuthToken(new HashMap<>(Map.of("refresh_token", "old"))));
  }

  @Test
  public void fetchAndValidateKeyCredentials_rejectsMissingSettingsAndCachesValidOnes() {
    KeyCloakTokenValidationClient incomplete =
        newClient(new MockEnvironment().withProperty("keycloak.realm", "ecr"));

    assertThrows(KeycloakCredentialsException.class, incomplete::fetchAndValidateKeyCredentials);
    client.fetchAndValidateKeyCredentials();
    environment.setProperty("keycloak.realm", "changed");
    client.fetchAndValidateKeyCredentials();

    MockHttpServletRequest request = new MockHttpServletRequest();
    request.addHeader("Authorization", "Bearer abc");
    responseBody.set("{\"active\":true}");
    assertTrue(client.validateToken(request));
    assertEquals("/realms/ecr/protocol/openid-connect/token/introspect", receivedPath.get());
  }
}
