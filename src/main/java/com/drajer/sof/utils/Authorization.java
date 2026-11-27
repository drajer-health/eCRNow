package com.drajer.sof.utils;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.Response;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

@Component
public class Authorization {

  private final Logger logger = LoggerFactory.getLogger(Authorization.class);

  public JSONObject getMetadata(String serverURL) {
    String validatedUrl = validateServerUrl(serverURL);
    if (validatedUrl == null) {
      logger.error(
          "Not retrieving metadata, the FHIR server URL is not a valid http(s) URL: {}",
          StringEscapeUtils.escapeJava(serverURL));
      return null;
    }
    HttpHeaders headers = new HttpHeaders();
    headers.set("Accept", MediaType.APPLICATION_JSON_VALUE);
    HttpEntity<?> entity = new HttpEntity(headers);
    RestTemplate restTemplate = new RestTemplate();
    ResponseEntity<String> response = null;
    JSONObject metadata = null;
    try {
      logger.info(
          "Getting Metadata information from URL::::: {}", StringEscapeUtils.escapeJava(serverURL));
      response = restTemplate.exchange(validatedUrl, HttpMethod.GET, entity, String.class);
      metadata = new JSONObject(response.getBody());
      logger.info(
          "Received Metadata Information from URL::::: {}",
          StringEscapeUtils.escapeJava(serverURL));
    } catch (Exception e) {
      logger.error(
          "Error in getting Metadata information for URL::::: {}",
          StringEscapeUtils.escapeJava(serverURL),
          e);
    }
    return metadata;
  }

  /**
   * SSRF mitigation: the FHIR server base URL is supplied by administrators when provisioning
   * HealthcareSettings and can legitimately point at internal EHR servers, so it cannot be
   * allow-listed in code. Instead the URL is parsed and rebuilt from its validated components
   * before this class issues a request to it: only http/https schemes with a host and without
   * embedded credentials are accepted, and the request uses the reconstructed URL, never the raw
   * input. Returns null when the URL is not acceptable.
   */
  private static String validateServerUrl(String serverURL) {
    if (serverURL == null) {
      return null;
    }
    try {
      URI uri = new URI(serverURL.trim());
      boolean httpScheme =
          "https".equalsIgnoreCase(uri.getScheme()) || "http".equalsIgnoreCase(uri.getScheme());
      if (!httpScheme || uri.getHost() == null || uri.getUserInfo() != null) {
        return null;
      }
      URI rebuilt =
          new URI(
              uri.getScheme(),
              null,
              uri.getHost(),
              uri.getPort(),
              uri.getPath(),
              uri.getQuery(),
              null);
      return rebuilt.toString();
    } catch (URISyntaxException e) {
      return null;
    }
  }

  public String createAuthUrl(
      LaunchDetails authDetailsObject, ClientDetails clientDetails, Integer state) {
    String authUrl = authDetailsObject.getAuthUrl();
    Map<String, String> params = new HashMap<>();
    params.put("response_type", "code");
    params.put("client_id", clientDetails.getClientId());
    params.put("redirect_uri", authDetailsObject.getRedirectURI());
    params.put("launch", authDetailsObject.getLaunchId());
    params.put("state", Integer.toString(state));
    params.put("scope", clientDetails.getScopes());
    params.put("aud", authDetailsObject.getEhrServerURL());
    String queryParams = formatQueryParams(params);
    return authUrl + queryParams;
  }

  protected String formatQueryParams(Map<String, String> params) {
    return params.entrySet().stream()
        .map(p -> p.getKey() + "=" + p.getValue())
        .reduce((p1, p2) -> p1 + "&" + p2)
        .map(s -> "?" + s)
        .orElse("");
  }

  public JSONObject getAccessToken(LaunchDetails tokenDetails) {
    JSONObject tokenResponse = null;
    logger.info(
        "Getting AccessToken for Client: {}",
        StringEscapeUtils.escapeJava(tokenDetails.getClientId()));
    try {
      RestTemplate restTemplate = new RestTemplate();
      HttpHeaders headers = new HttpHeaders();
      headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
      headers.add("Accept", MediaType.APPLICATION_JSON_VALUE);
      MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
      map.add("grant_type", "authorization_code");
      map.add("code", tokenDetails.getAuthorizationCode());
      map.add("redirect_uri", tokenDetails.getRedirectURI());
      map.add("client_id", tokenDetails.getClientId());
      HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
      ResponseEntity<?> response =
          restTemplate.exchange(
              tokenDetails.getTokenUrl(), HttpMethod.POST, entity, Response.class);
      tokenResponse = new JSONObject(response.getBody());
      logger.info(
          "Received AccessToken for Client: {}",
          StringEscapeUtils.escapeJava(tokenDetails.getClientId()));
      logger.info("Received AccessToken: {}", tokenResponse);

    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the client: {}",
          StringEscapeUtils.escapeJava(tokenDetails.getClientId()),
          e);
    }
    return tokenResponse;
  }
}
