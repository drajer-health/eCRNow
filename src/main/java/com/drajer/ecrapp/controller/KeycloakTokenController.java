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
      return ResponseEntity.badRequest().body("Token details are required.");
    }

    try {
      Object tokenResponse = keyCloakTokenValidationClient.generateToken(tokenDetails);
      if (tokenResponse != null) {
        return ResponseEntity.ok()
            .contentType(MediaType.APPLICATION_JSON)
            .body(tokenResponse.toString());

      } else {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Token validation failed.");
      }
    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("An error occurred while validating the token: " + e.getMessage());
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
      return ResponseEntity.badRequest().body("Token details are required.");
    }

    try {
      JSONObject tokenResponse =
          (JSONObject) keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails);

      if (tokenResponse == null) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Token validation failed.");
      }

      boolean isSuccess = Boolean.parseBoolean(String.valueOf(tokenResponse.get("isSuccess")));
      HttpStatus status = isSuccess ? HttpStatus.OK : HttpStatus.UNAUTHORIZED;
      return ResponseEntity.status(status)
          .contentType(MediaType.APPLICATION_JSON)
          .body(tokenResponse.toString());

    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("An error occurred while validating the token: " + e.getMessage());
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
      return ResponseEntity.badRequest().body("Token details are required.");
    }

    try {
      JSONObject tokenResponse =
          (JSONObject) keyCloakTokenValidationClient.generateUserAuthToken(tokenDetails);

      if (tokenResponse == null) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Token validation failed.");
      }

      boolean isSuccess = Boolean.parseBoolean(String.valueOf(tokenResponse.get("isSuccess")));
      HttpStatus status = isSuccess ? HttpStatus.OK : HttpStatus.UNAUTHORIZED;
      return ResponseEntity.status(status)
          .contentType(MediaType.APPLICATION_JSON)
          .body(tokenResponse.toString());

    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("An error occurred while validating the token: " + e.getMessage());
    }
  }
}
