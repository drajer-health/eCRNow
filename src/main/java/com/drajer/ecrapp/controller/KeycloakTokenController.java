package com.drajer.ecrapp.controller;

import com.drajer.ecrapp.security.KeyCloakTokenValidationClient;
import java.util.Map;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/auth")
public class KeycloakTokenController {

  @Autowired private KeyCloakTokenValidationClient keyCloakTokenValidationClient;
  private static final String TOKEN_DETAILS_REQUIRED = "Token details are required.";
  private static final String TOKEN_VALIDATION_FAILED = "Token validation failed.";
  private static final String IS_SUCCESS = "isSuccess";
  private static final String ERROR_VALIDATING_TOKEN =
      "An error occurred while validating the token: ";

  /**
   * Endpoint to generate and validate a token.
   *
   * @param tokenDetails Map containing token details like client_id, client_secret, etc.
   * @return ResponseEntity indicating success or failure of token generation.
   */
  @CrossOrigin
  @PostMapping("/generate-token")
  public ResponseEntity<Object> generateToken(@RequestParam Map<String, Object> tokenDetails) {
    if (tokenDetails == null || tokenDetails.isEmpty()) {
      return ResponseEntity.badRequest().body(TOKEN_DETAILS_REQUIRED);
    }

    try {
      JSONObject tokenResponse =
          (JSONObject) keyCloakTokenValidationClient.generateToken(tokenDetails);
      if (tokenResponse == null) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(TOKEN_VALIDATION_FAILED);
      }

      boolean isSuccess = Boolean.parseBoolean(String.valueOf(tokenResponse.get(IS_SUCCESS)));
      HttpStatus status = isSuccess ? HttpStatus.OK : HttpStatus.UNAUTHORIZED;
      return ResponseEntity.status(status)
          .contentType(MediaType.APPLICATION_JSON)
          .body(tokenResponse.toString());

    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body(ERROR_VALIDATING_TOKEN + e.getMessage());
    }
  }

  /**
   * Endpoint to generate and validate a token.
   *
   * @param tokenDetails Map containing token details like client_id, client_secret, etc.
   * @return ResponseEntity indicating success or failure of token generation.
   */
  @CrossOrigin
  @PostMapping("/generateAuthToken")
  public ResponseEntity<Object> generateUserAuthToken(
      @RequestParam Map<String, Object> tokenDetails) {
    if (tokenDetails == null || tokenDetails.isEmpty()) {
      return ResponseEntity.badRequest().body(TOKEN_DETAILS_REQUIRED);
    }

    try {
      JSONObject tokenResponse =
          (JSONObject) keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails);

      if (tokenResponse == null) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(TOKEN_VALIDATION_FAILED);
      }

      boolean isSuccess = Boolean.parseBoolean(String.valueOf(tokenResponse.get(IS_SUCCESS)));
      HttpStatus status = isSuccess ? HttpStatus.OK : HttpStatus.UNAUTHORIZED;
      return ResponseEntity.status(status)
          .contentType(MediaType.APPLICATION_JSON)
          .body(tokenResponse.toString());

    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body(ERROR_VALIDATING_TOKEN + e.getMessage());
    }
  }

  /**
   * Endpoint to generate and validate a token.
   *
   * @param tokenDetails Map containing token details like client_id, client_secret, etc.
   * @return ResponseEntity indicating success or failure of token generation.
   */
  @CrossOrigin
  @PostMapping("/refresh-token")
  public ResponseEntity<Object> refreshToken(@RequestParam Map<String, Object> tokenDetails) {
    if (tokenDetails == null
        || tokenDetails.isEmpty()
        || !(tokenDetails.containsKey("refresh_token"))) {
      return ResponseEntity.badRequest().body(TOKEN_DETAILS_REQUIRED);
    }

    try {
      JSONObject tokenResponse =
          (JSONObject) keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails);

      if (tokenResponse == null) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(TOKEN_VALIDATION_FAILED);
      }

      boolean isSuccess = Boolean.parseBoolean(String.valueOf(tokenResponse.get(IS_SUCCESS)));
      HttpStatus status = isSuccess ? HttpStatus.OK : HttpStatus.UNAUTHORIZED;
      return ResponseEntity.status(status)
          .contentType(MediaType.APPLICATION_JSON)
          .body(tokenResponse.toString());

    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body(ERROR_VALIDATING_TOKEN + e.getMessage());
    }
  }
}
