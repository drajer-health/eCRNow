package com.drajer.bsa.auth;

import com.drajer.bsa.model.FhirServerDetails;
import java.util.HashMap;
import java.util.Map;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class AuthorizationUtils {

  private final Logger logger = LoggerFactory.getLogger(AuthorizationUtils.class);

  /** The EHR Authorization Service class enables the BSA to get an access token. */
  @Qualifier("backendauth")
  @Autowired
  AuthorizationService backendAuthorizationService;

  @Qualifier("ehrauth")
  @Autowired
  AuthorizationService ehrAuthorizationService;

  @Qualifier("passwordauth")
  @Autowired
  AuthorizationService passwordAuthorizationService;

  public JSONObject getToken(FhirServerDetails fsd) {
    String password = fsd.getPassword();
    String secret = fsd.getClientSecret();
    String client_id = fsd.getClientId();
    JSONObject token;
    if (client_id != null && client_id.equalsIgnoreCase("NO_AUTH_REQUIRED")) {
      Map<String, Object> tokenParams = new HashMap<>();
      tokenParams.put("access_token", "admin");
      tokenParams.put("expires_in", 60 * 60 * 24);
      token = new JSONObject(tokenParams);
      logger.info("Hacked no auth token");
    } else if (password != null && !password.isEmpty()) {
      logger.info("Getting passsword auth token");
      token = passwordAuthorizationService.getAuthorizationToken(fsd);
    } else if (secret == null || secret.isEmpty()) {
      logger.info("Getting backend auth token");
      token = backendAuthorizationService.getAuthorizationToken(fsd);
    } else {
      logger.info("Getting EHR auth token");
      token = ehrAuthorizationService.getAuthorizationToken(fsd);
    }
    logger.info("Returning token {}", token);
    return token;
  }
}
