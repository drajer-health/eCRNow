package com.drajer.ecrapp.security;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import okhttp3.*;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.core.env.Environment;

public class KeyCloakTokenValidationClientTest {

  @InjectMocks private KeyCloakTokenValidationClient keycloakClient;

  @Mock private OkHttpClient okHttpClient;

  @Mock private Environment environment;

  @Mock private HttpServletRequest request;

  @Mock private Call call;

  @Before
  public void setup() {
    MockitoAnnotations.initMocks(this);

    when(environment.getProperty("keycloak.auth.server", "")).thenReturn("http://localhost:8080");
    when(environment.getProperty("keycloak.realm", "")).thenReturn("realm");
    when(environment.getProperty("keycloak.client.id", "")).thenReturn("client");
    when(environment.getProperty("keycloak.client.secret", "")).thenReturn("secret");
  }

  // ======================================================
  // validateToken - SUCCESS
  // ======================================================
  @Test
  public void testValidateTokenSuccess() throws Exception {

    when(request.getHeader("Authorization")).thenReturn("Bearer token123");

    Response response = buildResponse(200, "{\"active\":true}");

    when(okHttpClient.newCall(any())).thenReturn(call);
    when(call.execute()).thenReturn(response);

    boolean result = keycloakClient.validateToken(request);

    assertTrue(result);
  }

  // ======================================================
  // validateToken - INACTIVE TOKEN
  // ======================================================
  @Test
  public void testValidateTokenInactive() throws Exception {

    when(request.getHeader("Authorization")).thenReturn("Bearer token123");

    Response response = buildResponse(200, "{\"active\":false}");

    when(okHttpClient.newCall(any())).thenReturn(call);
    when(call.execute()).thenReturn(response);

    boolean result = keycloakClient.validateToken(request);

    assertFalse(result);
  }

  // ======================================================
  // validateToken - HEADER MISSING
  // ======================================================
  @Test
  public void testValidateTokenNoHeader() {

    when(request.getHeader("Authorization")).thenReturn(null);

    boolean result = keycloakClient.validateToken(request);

    assertFalse(result);
  }

  // ======================================================
  // validateToken - IOException
  // ======================================================
  @Test
  public void testValidateTokenException() throws Exception {

    when(request.getHeader("Authorization")).thenReturn("Bearer token123");

    when(okHttpClient.newCall(any())).thenReturn(call);
    when(call.execute()).thenThrow(new IOException());

    boolean result = keycloakClient.validateToken(request);

    assertFalse(result);
  }

  // ======================================================
  // generateToken SUCCESS
  // ======================================================
  @Test
  public void testGenerateTokenSuccess() throws Exception {

    Map<String, Object> map = new HashMap<>();
    map.put("grant_type", "client_credentials");

    Response response = buildResponse(200, "{\"access_token\":\"abc\"}");

    when(okHttpClient.newCall(any())).thenReturn(call);
    when(call.execute()).thenReturn(response);

    JSONObject json = (JSONObject) keycloakClient.generateToken(map);

    assertTrue(json.getBoolean("isSuccess"));
  }

  // ======================================================
  // generateUserAuthToken REFRESH TOKEN
  // ======================================================
  @Test
  public void testGenerateUserAuthTokenRefresh() throws Exception {

    Map<String, Object> map = new HashMap<>();
    map.put("refresh_token", "refresh123");

    Response response = buildResponse(200, "{\"access_token\":\"abc\"}");

    when(okHttpClient.newCall(any())).thenReturn(call);
    when(call.execute()).thenReturn(response);

    JSONObject json = (JSONObject) keycloakClient.generateUserAuthToken(map);

    assertTrue(json.getBoolean("isSuccess"));
  }

  // ======================================================
  // Helper to create Mock Response
  // ======================================================
  private Response buildResponse(int code, String body) {

    ResponseBody responseBody = ResponseBody.create(MediaType.parse("application/json"), body);

    return new Response.Builder()
        .request(new Request.Builder().url("http://localhost").build())
        .protocol(Protocol.HTTP_1_1)
        .code(code)
        .message("OK")
        .body(responseBody)
        .build();
  }
}
