package com.drajer.ecrapp.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.drajer.ecrapp.security.KeyCloakTokenValidationClient;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@RunWith(MockitoJUnitRunner.class)
public class KeycloakTokenControllerTest {
  @Mock private KeyCloakTokenValidationClient keyCloakTokenValidationClient;

  @InjectMocks private KeycloakTokenController keycloakTokenController;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this); // Deprecated but still works in JUnit 4
  }

  @Test
  public void testGenerateToken_Success() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("client_id", "test_client_id");
    Object tokenResponse = new Object();

    when(keyCloakTokenValidationClient.generateToken(tokenDetails)).thenReturn(tokenResponse);

    ResponseEntity<Object> response = keycloakTokenController.generateToken(tokenDetails);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(tokenResponse.toString(), response.getBody());
    assertNotNull(response.getBody());
    assertEquals(MediaType.APPLICATION_JSON, response.getHeaders().getContentType());
    verify(keyCloakTokenValidationClient, times(1)).generateToken(tokenDetails);
  }

  @Test
  public void testGenerateToken_BadRequest() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();

    ResponseEntity<Object> response = keycloakTokenController.generateToken(tokenDetails);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Token details are required.", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(response.getBody().toString().contains("Token details are required."));
    verify(keyCloakTokenValidationClient, never()).generateToken(any());
  }

  @Test
  public void testGenerateToken_Unauthorized() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("client_id", "test_client_id");

    when(keyCloakTokenValidationClient.generateToken(tokenDetails)).thenReturn(null);

    ResponseEntity<Object> response = keycloakTokenController.generateToken(tokenDetails);

    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals("Token validation failed.", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(response.getBody().toString().contains("Token validation failed."));
    verify(keyCloakTokenValidationClient, times(1)).generateToken(tokenDetails);
  }

  @Test
  public void testGenerateToken_InternalServerError() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("client_id", "test_client_id");

    when(keyCloakTokenValidationClient.generateToken(tokenDetails))
        .thenThrow(new RuntimeException("Error"));

    ResponseEntity<Object> response = keycloakTokenController.generateToken(tokenDetails);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("An error occurred while validating the token: Error", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(
        response
            .getBody()
            .toString()
            .contains("An error occurred while validating the token: Error"));
    verify(keyCloakTokenValidationClient, times(1)).generateToken(tokenDetails);
  }

  @Test
  public void testGenerateUserAuthToken_Success() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("client_id", "test_client_id");
    JSONObject tokenResponse = new JSONObject();
    tokenResponse.put("isSuccess", true);

    when(keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails))
        .thenReturn(tokenResponse);

    ResponseEntity<Object> response = keycloakTokenController.generateUserAuthToken(tokenDetails);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(tokenResponse.toString(), response.getBody());
    assertNotNull(response.getBody());
    assertEquals(MediaType.APPLICATION_JSON, response.getHeaders().getContentType());
    verify(keyCloakTokenValidationClient, times(1)).generateUserAuthToken(tokenDetails);
  }

  @Test
  public void testGenerateUserAuthToken_BadRequest() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();

    ResponseEntity<Object> response = keycloakTokenController.generateUserAuthToken(tokenDetails);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Token details are required.", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(response.getBody().toString().contains("Token details are required."));
    verify(keyCloakTokenValidationClient, never()).generateUserAuthToken(any());
  }

  @Test
  public void testGenerateUserAuthToken_InternalServerError() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("client_id", "test_client_id");

    when(keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails))
        .thenThrow(new RuntimeException("Error"));

    ResponseEntity<Object> response = keycloakTokenController.generateUserAuthToken(tokenDetails);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("An error occurred while validating the token: Error", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(
        response
            .getBody()
            .toString()
            .contains("An error occurred while validating the token: Error"));
    verify(keyCloakTokenValidationClient, times(1)).generateUserAuthToken(tokenDetails);
  }

  @Test
  public void testGenerateUserAuthToken_NullResponse() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("client_id", "test_client_id");

    when(keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails)).thenReturn(null);

    ResponseEntity<Object> response = keycloakTokenController.generateUserAuthToken(tokenDetails);

    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals("Token validation failed.", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(response.getBody().toString().contains("Token validation failed."));
    verify(keyCloakTokenValidationClient, times(1)).generateUserAuthToken(tokenDetails);
  }

  @Test
  public void testRefreshToken_Success() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("refresh_token", "valid_refresh_token");

    JSONObject tokenResponse = new JSONObject();
    tokenResponse.put("isSuccess", true);
    tokenResponse.put("access_token", "new_access_token");

    when(keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails))
        .thenReturn(tokenResponse);

    ResponseEntity<Object> response = keycloakTokenController.refreshToken(tokenDetails);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(tokenResponse.toString(), response.getBody());
    assertNotNull(response.getBody());
    assertEquals(MediaType.APPLICATION_JSON, response.getHeaders().getContentType());
    verify(keyCloakTokenValidationClient, times(1)).generateUserAuthToken(tokenDetails);
  }

  @Test
  public void testRefreshToken_BadRequest_MissingToken() throws IOException {
    Map<String, Object> tokenDetails = new HashMap<>();

    ResponseEntity<Object> response = keycloakTokenController.refreshToken(tokenDetails);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Token details are required.", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(response.getBody().toString().contains("Token details are required."));
    verify(keyCloakTokenValidationClient, never()).generateUserAuthToken(any());
  }

  @Test
  public void testRefreshToken_Unauthorized_NullResponse() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("refresh_token", "valid_refresh_token");

    when(keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails)).thenReturn(null);

    ResponseEntity<Object> response = keycloakTokenController.refreshToken(tokenDetails);

    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals("Token validation failed.", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(response.getBody().toString().contains("Token validation failed."));
    verify(keyCloakTokenValidationClient, times(1)).generateUserAuthToken(tokenDetails);
  }

  @Test
  public void testRefreshToken_Unauthorized_isSuccessFalse() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("refresh_token", "valid_refresh_token");

    JSONObject tokenResponse = new JSONObject();
    tokenResponse.put("isSuccess", false);

    when(keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails))
        .thenReturn(tokenResponse);

    ResponseEntity<Object> response = keycloakTokenController.refreshToken(tokenDetails);

    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals(tokenResponse.toString(), response.getBody());
    assertNotNull(response.getBody());
    assertEquals(MediaType.APPLICATION_JSON, response.getHeaders().getContentType());
    verify(keyCloakTokenValidationClient, times(1)).generateUserAuthToken(tokenDetails);
  }

  @Test
  public void testRefreshToken_InternalServerError() throws Exception {
    Map<String, Object> tokenDetails = new HashMap<>();
    tokenDetails.put("refresh_token", "valid_refresh_token");

    when(keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails))
        .thenThrow(new RuntimeException("Error"));

    ResponseEntity<Object> response = keycloakTokenController.refreshToken(tokenDetails);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("An error occurred while validating the token: Error", response.getBody());
    assertNotNull(response.getBody());
    assertTrue(
        response
            .getBody()
            .toString()
            .contains("An error occurred while validating the token: Error"));
    verify(keyCloakTokenValidationClient, times(1)).generateUserAuthToken(tokenDetails);
  }
}
