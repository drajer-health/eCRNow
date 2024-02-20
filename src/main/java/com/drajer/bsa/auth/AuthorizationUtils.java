package com.drajer.bsa.auth;

import com.drajer.bsa.model.BsaTypes;
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
    JSONObject token;
    BsaTypes.AuthenticationType authTYpe = BsaTypes.getAuthenticationType(fsd.getAuthType());
    switch (authTYpe) {
      case USER_NAME_PWD:
        token = passwordAuthorizationService.getAuthorizationToken(fsd);
        break;
      case SOF_BACKEND:
      case SOF_PROVIDER:
        token = backendAuthorizationService.getAuthorizationToken(fsd);
        break;
      case SYSTEM:
      case MULTI_TENANT_SYSTEM_LAUNCH:
        token = ehrAuthorizationService.getAuthorizationToken(fsd);
        break;
      case UNKNOWN:
      default:
        Map<String, Object> tokenParams = new HashMap<>();
        tokenParams.put("expires_in", 60 * 60 * 24);
        tokenParams.put("access_token", "");
        token = new JSONObject(tokenParams);
        break;
    }

    logger.debug("Returning token {}", token);
    return token;
  }
}
