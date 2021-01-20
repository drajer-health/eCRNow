package com.drajer.ecrapp.security;

import com.drajer.sof.model.LaunchDetails;
import java.security.SecureRandom;
import java.util.Base64;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SampleAuthorizationServiceImpl implements SampleAuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(SampleAuthorizationServiceImpl.class);

  private static final SecureRandom secureRandom = new SecureRandom();

  private static final Base64.Encoder base64Encoder = Base64.getUrlEncoder();

  public String getAuthorizationHeader(LaunchDetails launchDetails) {
    logger.info("Generating the new AccessToken");
    /*
     * RestTemplate restTemplate = new RestTemplate(); JSONObject requestBody = new
     * JSONObject(); requestBody.put("client_id", launchDetails.getClientId());
     * requestBody.put("client_secret", launchDetails.getClientSecret());
     * HttpHeaders headers = new HttpHeaders(); headers.add("Content-Type",
     * MediaType.APPLICATION_JSON_VALUE); HttpEntity<String> request = new
     * HttpEntity<>(requestBody.toString(), headers);
     *
     * logger.info("Sending Authorization request to Endpoint::::: {}",
     * launchDetails.getRestAPIURL()); ResponseEntity<String> response =
     * restTemplate.exchange( launchDetails.getRestAPIURL(), HttpMethod.POST,
     * request, String.class);
     *
     * JSONObject responseObj = new JSONObject(response.getBody()); accessToken =
     * responseObj.getString("access_token");
     * logger.info("received Access_token====> {}", accessToken);
     */
    byte[] randomBytes = new byte[24];
    secureRandom.nextBytes(randomBytes);
    return base64Encoder.encodeToString(randomBytes);
  }
}
