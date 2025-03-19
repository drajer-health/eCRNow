package com.drajer.ecrapp.security;

import com.drajer.ecrapp.exceptions.KeycloakCredentialsException;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Map;
import okhttp3.*;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

@Component
public class KeyCloakTokenValidationClient {

  private static final Logger LOGGER = LoggerFactory.getLogger(KeyCloakTokenValidationClient.class);

  private static final String APPLICATION_URL_FORM_ENCODED = "application/x-www-form-urlencoded";
  private static final String AUTHORIZATION_HEADER = "Authorization";
  private static final String BEARER_PREFIX = "Bearer ";
  private static final String KEYCLOAK_AUTH_SERVER = "keycloak.auth.server";
  private static final String KEYCLOAK_REALM = "keycloak.realm";
  private static final String KEYCLOAK_CLIENT_ID = "keycloak.client.id";
  private static final String KEYCLOAK_CLIENT_SECRET = "keycloak.client.secret";
  private String authUrl;
  private String realm;
  private String clientId;
  private String clientSecret;

  @Autowired OkHttpClient client;

  @Autowired private Environment environment;
  private boolean credentialsFetched = false;

  public boolean validateToken(HttpServletRequest request) {
    LOGGER.info("Entry - validateToken Method in KeyCloakTokenValidationClient");

    String token = getTokenFromRequest(request);
    if (token == null) {
      LOGGER.error("Authorization token is missing or invalid.");
      return false;
    }

    fetchAndValidateKeyCredentials();

    String url =
        String.format("%s/realms/%s/protocol/openid-connect/token/introspect", authUrl, realm);

    MediaType mediaType = MediaType.parse(APPLICATION_URL_FORM_ENCODED);

    RequestBody body =
        RequestBody.create(
            mediaType,
            "token=" + token + "&client_id=" + clientId + "&client_secret=" + clientSecret);

    Request requestOne =
        new Request.Builder()
            .url(url)
            .method("POST", body)
            .addHeader("Content-Type", "application/x-www-form-urlencoded")
            .build();

    try (Response response = client.newCall(requestOne).execute()) {

      if (!response.isSuccessful()) {
        LOGGER.error("Failed to authenticate: {}", response.message());
        return false;
      }
      System.out.println(response);

      String responseBody = response.body() != null ? response.body().string() : "{}";
      System.out.println(responseBody);
      JSONObject jsonObj = new JSONObject(responseBody);
      System.out.println(responseBody);
      boolean validationResponse = jsonObj.optBoolean("active", false);
      LOGGER.info("Access Token Validation Status: {}", validationResponse);
      return validationResponse;
    } catch (IOException e) {
      LOGGER.error("Exception - validateToken Method in KeyCloakTokenValidationClient", e);
      return false;
    } finally {
      LOGGER.info("Exit - validateToken Method in KeyCloakTokenValidationClient");
    }
  }

  public Object generateToken(Map<String, Object> authenticationTokenDetails) throws IOException {
    LOGGER.info("Entry - validateToken Method in KeyCloakTokenValidationClient");

    fetchAndValidateKeyCredentials();

    String url = String.format("%s/realms/%s/protocol/openid-connect/token", authUrl, realm);

    MediaType mediaType = MediaType.parse(APPLICATION_URL_FORM_ENCODED);

    StringBuilder authRequestBody = new StringBuilder();
    for (Map.Entry<String, Object> authEntry : authenticationTokenDetails.entrySet()) {
      authRequestBody
          .append(authEntry.getKey())
          .append("=")
          .append(authEntry.getValue())
          .append("&");
    }
    if (authRequestBody.length() > 0) {
      authRequestBody.setLength(authRequestBody.length() - 1);
    }
    RequestBody body = RequestBody.create(mediaType, authRequestBody.toString());

    Request requestOne =
        new Request.Builder()
            .url(url)
            .method("POST", body)
            .addHeader("Content-Type", "application/x-www-form-urlencoded")
            .build();

    try (Response response = client.newCall(requestOne).execute()) {

      if (!response.isSuccessful()) {
        LOGGER.error("Failed to authenticate: {}", response.message());
        return false;
      }
      System.out.println(response);

      String responseBody = response.body() != null ? response.body().string() : "{}";
      System.out.println(responseBody);
      return new JSONObject(responseBody);

    } catch (IOException e) {
      LOGGER.error("Exception - validateToken Method in KeyCloakTokenValidationClient", e);
      throw new IOException("Exception - validateToken Method in KeyCloakTokenValidationClient", e);
    } catch (Exception e) {

      throw new KeycloakCredentialsException(
          "Exception - generate Method in KeyCloakTokenValidationClient" + e.getMessage());
    } finally {
      LOGGER.info("Exit - validateToken Method in KeyCloakTokenValidationClient");
    }
  }

  public Object generateUserAuthToken(Map<String, Object> authenticationTokenDetails)
      throws IOException {
    LOGGER.info("Entry - Generating Token Method in KeyCloakTokenValidationClient");

    fetchAndValidateKeyCredentials();

    String url = String.format("%s/realms/%s/protocol/openid-connect/token", authUrl, realm);

    MediaType mediaType = MediaType.parse(APPLICATION_URL_FORM_ENCODED);

    StringBuilder authRequestBody = new StringBuilder();

    String refreshToken = (String) authenticationTokenDetails.get("refresh_token");

    authenticationTokenDetails.put("client_id", clientId);
    authenticationTokenDetails.put("client_secret", clientSecret);
    if (refreshToken == null) {
      authenticationTokenDetails.put("grant_type", "password");

    } else {
      authenticationTokenDetails.put("grant_type", "refresh_token");
      authenticationTokenDetails.put("refresh_token", refreshToken);
    }
    for (Map.Entry<String, Object> authEntry : authenticationTokenDetails.entrySet()) {
      authRequestBody
          .append(authEntry.getKey())
          .append("=")
          .append(authEntry.getValue())
          .append("&");
    }
    if (authRequestBody.length() > 0) {

      authRequestBody.setLength(authRequestBody.length() - 1);
    }
    RequestBody body = RequestBody.create(mediaType, authRequestBody.toString());

    Request requestOne =
        new Request.Builder()
            .url(url)
            .method("POST", body)
            .addHeader("Content-Type", "application/x-www-form-urlencoded")
            .build();

    try (Response response = client.newCall(requestOne).execute()) {

      String responseBody = response.body() != null ? response.body().string() : "{}";
      JSONObject keyCloakResponse = new JSONObject(responseBody);

      if (!response.isSuccessful()) {
        LOGGER.error("Failed to authenticate: {}", response.message());
        keyCloakResponse.put("isSuccess", false);
        return keyCloakResponse;
      }
      keyCloakResponse.put("isSuccess", true);
      return keyCloakResponse;

    } catch (IOException e) {
      LOGGER.error("Exception - validateToken Method in KeyCloakTokenValidationClient", e);
      throw new IOException("Exception - validateToken Method in KeyCloakTokenValidationClient", e);
    } catch (Exception e) {

      throw new KeycloakCredentialsException(
          "Exception - generate Method in KeyCloakTokenValidationClient" + e.getMessage());
    } finally {
      LOGGER.info("Exit - validateToken Method in KeyCloakTokenValidationClient");
    }
  }

  private String getTokenFromRequest(HttpServletRequest request) {
    String authorizationHeaderValue = request.getHeader(AUTHORIZATION_HEADER);
    if (authorizationHeaderValue != null && authorizationHeaderValue.startsWith(BEARER_PREFIX)) {
      return authorizationHeaderValue.substring(BEARER_PREFIX.length());
    }
    return null;
  }

  private String getProperty(String propertyKey) {
    // Retrieve the property from Environment, falling back to default values if needed
    return environment.getProperty(propertyKey, "");
  }

  private RequestBody buildRequestBody(String token, String clientId, String clientSecret) {
    String formBody =
        String.format("token=%s&client_id=%s&client_secret=%s", token, clientId, clientSecret);
    return RequestBody.create(MediaType.parse(APPLICATION_URL_FORM_ENCODED), formBody);
  }

  public void fetchAndValidateKeyCredentials() {
    if (credentialsFetched) {
      LOGGER.info("Keycloak credentials have already been fetched and validated.");
      return;
    }

    authUrl = getProperty(KEYCLOAK_AUTH_SERVER);
    realm = getProperty(KEYCLOAK_REALM);
    clientId = getProperty(KEYCLOAK_CLIENT_ID);
    clientSecret = getProperty(KEYCLOAK_CLIENT_SECRET);

    if (isValidCredentials(authUrl, realm, clientId, clientSecret)) {
      credentialsFetched = true;
      LOGGER.info("Keycloak credentials fetched and validated successfully.");
    } else {
      String errorMessage = "One or more Keycloak credentials are missing or invalid.";
      LOGGER.error(errorMessage);
      throw new KeycloakCredentialsException(errorMessage);
    }
  }

  private boolean isValidCredentials(
      String authUrl, String realm, String clientId, String clientSecret) {
    return authUrl != null
        && !authUrl.isEmpty()
        && realm != null
        && !realm.isEmpty()
        && clientId != null
        && !clientId.isEmpty()
        && clientSecret != null
        && !clientSecret.isEmpty();
  }
}
