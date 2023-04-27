package com.drajer.bsa.auth.impl;

import com.drajer.bsa.auth.RestApiAuthorizationHeaderIf;
import com.drajer.bsa.model.KarProcessingData;
import java.security.SecureRandom;
import java.util.Base64;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;

@Service
public class SampleRestApiAuthorizer implements RestApiAuthorizationHeaderIf {

  private final Logger logger = LoggerFactory.getLogger(SampleRestApiAuthorizer.class);

  private static final SecureRandom secureRandom = new SecureRandom();

  private static final Base64.Encoder base64Encoder = Base64.getUrlEncoder();

  @Override
  public HttpHeaders getAuthorizationHeader(KarProcessingData data) {

    HttpHeaders headers = new HttpHeaders();
    logger.info("Generating the new AccessToken");
    byte[] randomBytes = new byte[24];
    secureRandom.nextBytes(randomBytes);

    // Set Authorization Header
    headers.add("Authorization", "Bearer " + base64Encoder.encodeToString(randomBytes));
    return headers;
  }
}
