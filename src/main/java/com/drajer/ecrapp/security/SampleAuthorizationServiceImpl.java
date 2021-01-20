package com.drajer.ecrapp.security;

import com.drajer.sof.model.LaunchDetails;
import java.security.SecureRandom;
import java.util.Base64;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;

public class SampleAuthorizationServiceImpl implements AuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(SampleAuthorizationServiceImpl.class);

  private static final SecureRandom secureRandom = new SecureRandom();

  private static final Base64.Encoder base64Encoder = Base64.getUrlEncoder();

  public HttpHeaders getAuthorizationHeader(LaunchDetails launchDetails) {
    HttpHeaders headers = new HttpHeaders();
    logger.info("Generating the new AccessToken");
    byte[] randomBytes = new byte[24];
    secureRandom.nextBytes(randomBytes);
    // Set Authorization Header
    headers.add("Authorization", "Bearer " + base64Encoder.encodeToString(randomBytes));
    return headers;
  }
}
