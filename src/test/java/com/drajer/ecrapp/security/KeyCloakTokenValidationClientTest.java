package com.drajer.ecrapp.security;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.ecrapp.exceptions.KeycloakCredentialsException;
import com.sun.net.httpserver.HttpServer;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;
import okhttp3.Call;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.core.env.Environment;

public class KeyCloakTokenValidationClientTest {

  @InjectMocks private KeyCloakTokenValidationClient keyCloakClient;

  @Mock private HttpServletRequest request;

  @Mock private Environment environment;

  @Before
  public void setup() {
    MockitoAnnotations.initMocks(this);
    when(environment.getProperty("keycloak.auth.server", ""))
        .thenReturn("http://localhost:8080/auth");
    when(environment.getProperty("keycloak.realm", "")).thenReturn("testrealm");
    when(environment.getProperty("keycloak.client.id", "")).thenReturn("testclient");
    when(environment.getProperty("keycloak.client.secret", "")).thenReturn("secret");

    keyCloakClient.fetchAndValidateKeyCredentials();
    keyCloakClient.client = new OkHttpClient();
  }

  private HttpServer startMockServer(int port, int statusCode, String response) throws Exception {
    HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
    server.createContext(
        "/auth/realms/testrealm/protocol/openid-connect/token/introspect",
        exchange -> {
          exchange.sendResponseHeaders(statusCode, response.getBytes().length);
          OutputStream os = exchange.getResponseBody();
          os.write(response.getBytes());
          os.close();
        });
    server.start();
    return server;
  }

  private HttpServer startTokenServer(int port, int statusCode, String response) throws Exception {
    HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
    server.createContext(
        "/auth/realms/testrealm/protocol/openid-connect/token",
        exchange -> {
          exchange.sendResponseHeaders(statusCode, response.getBytes().length);
          OutputStream os = exchange.getResponseBody();
          os.write(response.getBytes());
          os.close();
        });
    server.start();
    return server;
  }

  @Test
  public void testValidateToken_missingAuthorizationHeader() {
    when(request.getHeader("Authorization")).thenReturn(null);
    boolean result = keyCloakClient.validateToken(request);
    assertFalse(result);
  }

  @Test
  public void testValidateToken_invalidAuthorizationHeader() {
    when(request.getHeader("Authorization")).thenReturn("Basic abcdef");
    boolean result = keyCloakClient.validateToken(request);
    assertFalse(result);
  }

  @Test
  public void testValidateToken_ioException() throws Exception {
    when(request.getHeader("Authorization")).thenReturn("Bearer validtoken");
    OkHttpClient mockClient = mock(OkHttpClient.class);
    keyCloakClient.client = mockClient;
    Call mockCall = mock(Call.class);
    when(mockClient.newCall(any(Request.class))).thenReturn(mockCall);
    when(mockCall.execute()).thenThrow(new IOException("Network error"));
    boolean result = keyCloakClient.validateToken(request);
    assertFalse(result);
  }

  @Test(expected = KeycloakCredentialsException.class)
  public void testGenerateToken_runtimeException() throws Exception {
    Map<String, Object> authDetails = new HashMap<>();
    authDetails.put("username", "user");
    authDetails.put("password", "pass");
    OkHttpClient mockClient = mock(OkHttpClient.class);
    keyCloakClient.client = mockClient;
    Call mockCall = mock(Call.class);
    when(mockClient.newCall(any(Request.class))).thenReturn(mockCall);
    when(mockCall.execute()).thenThrow(new RuntimeException("Some error"));
    keyCloakClient.generateToken(authDetails);
    assertTrue(authDetails.containsKey("username"));
    assertTrue(authDetails.containsKey("password"));
  }

  @Test
  public void testGenerateToken_emptyDetails() throws Exception {
    Map<String, Object> authDetails = new HashMap<>();
    OkHttpClient mockClient = mock(OkHttpClient.class);
    keyCloakClient.client = mockClient;
    Call mockCall = mock(Call.class);
    when(mockClient.newCall(any(Request.class))).thenReturn(mockCall);
    when(mockCall.execute()).thenThrow(new IOException("Network error"));
    try {
      keyCloakClient.generateToken(authDetails);
      fail("Expected IOException");
    } catch (IOException e) {
      assertEquals(
          "Exception - validateToken Method in KeyCloakTokenValidationClient",
          e.getMessage().split(":")[0]);
    }
  }

  @Test(expected = IOException.class)
  public void testGenerateUserAuthToken_noRefreshToken_ioException() throws Exception {
    Map<String, Object> authDetails = new HashMap<>();
    authDetails.put("username", "user");
    authDetails.put("password", "pass");
    assertFalse(authDetails.isEmpty());
    assertEquals(2, authDetails.size());
    OkHttpClient mockClient = mock(OkHttpClient.class);
    keyCloakClient.client = mockClient;
    Call mockCall = mock(Call.class);
    when(mockClient.newCall(any(Request.class))).thenReturn(mockCall);
    when(mockCall.execute()).thenThrow(new IOException("Network error"));
    keyCloakClient.generateUserAuthToken(authDetails);
  }

  @Test(expected = KeycloakCredentialsException.class)
  public void testGenerateUserAuthToken_withRefreshToken_runtimeException() throws Exception {
    Map<String, Object> authDetails = new HashMap<>();
    authDetails.put("username", "user");
    authDetails.put("password", "pass");
    authDetails.put("refresh_token", "some-refresh-token");
    OkHttpClient mockClient = mock(OkHttpClient.class);
    keyCloakClient.client = mockClient;
    Call mockCall = mock(Call.class);
    when(mockClient.newCall(any(Request.class))).thenReturn(mockCall);
    when(mockCall.execute()).thenThrow(new RuntimeException("Some error"));
    keyCloakClient.generateUserAuthToken(authDetails);
    assertTrue(authDetails.containsKey("refresh_token"));
  }

  @Test
  public void testBuildRequestBody() throws Exception {
    String token = "sampleToken";
    String clientId = "testClient";
    String clientSecret = "secret123";
    java.lang.reflect.Method method =
        KeyCloakTokenValidationClient.class.getDeclaredMethod(
            "buildRequestBody", String.class, String.class, String.class);
    method.setAccessible(true);
    RequestBody requestBody =
        (RequestBody) method.invoke(keyCloakClient, token, clientId, clientSecret);
    assertNotNull(requestBody);
    okio.Buffer buffer = new okio.Buffer();
    requestBody.writeTo(buffer);
    String bodyString = buffer.readUtf8();
    assertEquals("token=sampleToken&client_id=testClient&client_secret=secret123", bodyString);
  }

  @Test
  public void testFetchAndValidateKeyCredentials_missingProperties_withAssert() throws Exception {
    when(environment.getProperty("keycloak.auth.server", "")).thenReturn("");
    when(environment.getProperty("keycloak.realm", "")).thenReturn("");
    when(environment.getProperty("keycloak.client.id", "")).thenReturn("");
    when(environment.getProperty("keycloak.client.secret", "")).thenReturn("");
    java.lang.reflect.Field field =
        KeyCloakTokenValidationClient.class.getDeclaredField("credentialsFetched");
    field.setAccessible(true);
    field.set(keyCloakClient, false);
    try {
      keyCloakClient.fetchAndValidateKeyCredentials();
      fail("Expected KeycloakCredentialsException was not thrown");
    } catch (KeycloakCredentialsException e) {
      assertEquals("One or more Keycloak credentials are missing or invalid.", e.getMessage());
    }
  }

  @Test
  public void testIsValidCredentials_nullAuthUrl() throws Exception {
    java.lang.reflect.Method method =
        KeyCloakTokenValidationClient.class.getDeclaredMethod(
            "isValidCredentials", String.class, String.class, String.class, String.class);
    method.setAccessible(true);
    boolean result =
        (boolean) method.invoke(keyCloakClient, null, "testrealm", "testclient", "secret");
    assertFalse(result);
  }

  @Test
  public void testIsValidCredentials_emptyRealm() throws Exception {
    java.lang.reflect.Method method =
        KeyCloakTokenValidationClient.class.getDeclaredMethod(
            "isValidCredentials", String.class, String.class, String.class, String.class);
    method.setAccessible(true);
    boolean result =
        (boolean)
            method.invoke(keyCloakClient, "http://localhost:8080/auth", "", "testclient", "secret");
    assertFalse(result);
  }

  @Test
  public void testIsValidCredentials_nullClientId() throws Exception {
    java.lang.reflect.Method method =
        KeyCloakTokenValidationClient.class.getDeclaredMethod(
            "isValidCredentials", String.class, String.class, String.class, String.class);
    method.setAccessible(true);
    boolean result =
        (boolean)
            method.invoke(
                keyCloakClient, "http://localhost:8080/auth", "testrealm", null, "secret");
    assertFalse(result);
  }

  @Test
  public void testValidateToken_activeTrue_realHttp() throws Exception {
    HttpServer server = startMockServer(8080, 200, "{\"active\":true}");
    when(request.getHeader("Authorization")).thenReturn("Bearer validtoken");
    boolean result = keyCloakClient.validateToken(request);
    assertTrue(result);
    server.stop(0);
  }

  @Test
  public void testGenerateToken_success_realHttp() throws Exception {
    HttpServer server = startTokenServer(8080, 200, "{\"access_token\":\"abc\"}");
    Map<String, Object> authDetails = new HashMap<>();
    authDetails.put("username", "user");
    authDetails.put("password", "pass");
    Object result = keyCloakClient.generateToken(authDetails);
    assertNotNull(result);
    assertTrue(result instanceof org.json.JSONObject);
    assertTrue(((org.json.JSONObject) result).getBoolean("isSuccess"));
    server.stop(0);
  }

  @Test
  public void testValidateToken_unsuccessfulHttpResponse() throws Exception {
    HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
    server.createContext(
        "/realms/testrealm/protocol/openid-connect/token/introspect",
        exchange -> {
          exchange.sendResponseHeaders(401, 0); // ❗ unsuccessful
          exchange.close();
        });
    server.start();
    int port = server.getAddress().getPort();
    when(environment.getProperty("keycloak.auth.server", ""))
        .thenReturn("http://localhost:" + port);
    java.lang.reflect.Field f =
        KeyCloakTokenValidationClient.class.getDeclaredField("credentialsFetched");
    f.setAccessible(true);
    f.set(keyCloakClient, false);
    when(request.getHeader("Authorization")).thenReturn("Bearer sometoken");
    boolean result = keyCloakClient.validateToken(request);
    assertFalse(result);
    server.stop(0);
  }

  @Test
  public void testGenerateToken_unsuccessfulHttp_setsIsSuccessFalse() throws Exception {
    HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
    server.createContext(
        "/realms/testrealm/protocol/openid-connect/token",
        exchange -> {
          exchange.sendResponseHeaders(400, 0); // ❗ unsuccessful
          exchange.getResponseBody().write("{}".getBytes());
          exchange.close();
        });
    server.start();
    int port = server.getAddress().getPort();
    when(environment.getProperty("keycloak.auth.server", ""))
        .thenReturn("http://localhost:" + port);
    java.lang.reflect.Field f =
        KeyCloakTokenValidationClient.class.getDeclaredField("credentialsFetched");
    f.setAccessible(true);
    f.set(keyCloakClient, false);
    Map<String, Object> auth = new HashMap<>();
    auth.put("username", "u");
    auth.put("password", "p");
    Object result = keyCloakClient.generateToken(auth);
    assertTrue(result instanceof org.json.JSONObject);
    org.json.JSONObject json = (org.json.JSONObject) result;
    assertFalse(json.getBoolean("isSuccess"));
    server.stop(0);
  }
}
