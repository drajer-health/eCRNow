package com.drajer.bsa.auth;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.FhirServerDetails;
import java.util.HashMap;
import java.util.Map;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class AuthorizationUtils {

  private final Logger logger = LoggerFactory.getLogger(AuthorizationUtils.class);
  private final AuthorizationService backendAuthorizationService;
  private final AuthorizationService ehrAuthorizationService;
  private final AuthorizationService passwordAuthorizationService;

  /**
   * Instantiates a new authorization utils with required authorization services.
   *
   * @param backendAuthorizationService the backend authorization service
   * @param ehrAuthorizationService the EHR authorization service
   * @param passwordAuthorizationService the password authorization service
   */
  public AuthorizationUtils(
      @Qualifier("backendauth") AuthorizationService backendAuthorizationService,
      @Qualifier("ehrauth") AuthorizationService ehrAuthorizationService,
      @Qualifier("passwordauth") AuthorizationService passwordAuthorizationService) {
    this.backendAuthorizationService = backendAuthorizationService;
    this.ehrAuthorizationService = ehrAuthorizationService;
    this.passwordAuthorizationService = passwordAuthorizationService;
  }

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
      case SOF_SYSTEM:
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
