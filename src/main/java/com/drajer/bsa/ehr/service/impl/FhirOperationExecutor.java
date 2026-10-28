package com.drajer.bsa.ehr.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.sof.utils.FhirContextInitializer;
import java.time.Instant;
import java.util.Date;
import java.util.function.Function;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class FhirOperationExecutor {

  private static final Logger logger = LoggerFactory.getLogger(FhirOperationExecutor.class);

  private static final int HTTP_UNAUTHORIZED = 401;

  @Value("${token.refresh.threshold:60}")
  private long TOKEN_EXPIRY_BUFFER_SECONDS;

  private static final String ACCESS_TOKEN = "access_token";
  private static final String EXPIRES_IN = "expires_in";
  private static final String PROVIDER_ID = "uuid";

  private final FhirContextInitializer fhirContextInitializer;
  private final AuthorizationUtils authorizationUtils;
  private final HealthcareSettingsDao healthcareSettingsDao;

  public FhirOperationExecutor(
      FhirContextInitializer fhirContextInitializer,
      AuthorizationUtils authorizationUtils,
      HealthcareSettingsDao healthcareSettingsDao) {

    this.fhirContextInitializer = fhirContextInitializer;
    this.authorizationUtils = authorizationUtils;
    this.healthcareSettingsDao = healthcareSettingsDao;
  }

  public <T> T execute(
      KarProcessingData data, FhirContext context, Function<IGenericClient, T> operation) {

    ensureValidAccessToken(data);

    IGenericClient client = createClient(data, context);

    try {
      return operation.apply(client);

    } catch (RuntimeException exception) {

      if (!isUnauthorized(exception)) {
        throw exception;
      }

      logger.warn(
          "FHIR request returned 401. Generating a fresh access token and retrying once. "
              + "Request ID: {}",
          data.getNotificationContext().getxRequestId());

      refreshAccessToken(data);

      /*
       * The existing client still has BearerTokenAuthInterceptor
       * configured with the old token, so a new client is mandatory.
       */
      IGenericClient refreshedClient = createClient(data, context);

      try {
        return operation.apply(refreshedClient);

      } catch (RuntimeException retryException) {

        if (isUnauthorized(retryException)) {
          logger.error(
              "FHIR request returned 401 after generating a fresh token. "
                  + "No further authentication retry will be attempted. Request ID: {}",
              data.getNotificationContext().getxRequestId());
        }

        throw retryException;
      }
    }
  }

//  /**
//   * Detects a 401 Unauthorized response regardless of how the FHIR client surfaced it. When the
//   * retryable client is enabled, HAPI's {@link BaseServerResponseException} is wrapped in a {@link
//   * RetryableException}; when it is disabled, the {@link BaseServerResponseException} is thrown
//   * directly.
//   */
 private boolean isUnauthorized(RuntimeException exception) {

  if (exception instanceof RetryableException) {
     return ((RetryableException) exception).getHttpResponseStatusCode() == HTTP_UNAUTHORIZED;
   }

    if (exception instanceof BaseServerResponseException) {
      return ((BaseServerResponseException) exception).getStatusCode() == HTTP_UNAUTHORIZED;
    }

   return false;
  }

  private IGenericClient createClient(KarProcessingData data, FhirContext context) {

    return fhirContextInitializer.createClient(
        context,
        data.getHealthcareSetting().getFhirServerBaseURL(),
        data.getHealthcareSetting().getEhrAccessToken(),
        data.getNotificationContext().getxRequestId(),
        data.getNotificationContext().getEhrLaunchContext());
  }

  private void refreshAccessToken(KarProcessingData data) {

    String previousAccessToken = data.getHealthcareSetting().getEhrAccessToken();

    JSONObject tokenResponse = authorizationUtils.getToken(data.getHealthcareSetting());

    String freshAccessToken = tokenResponse.getString(ACCESS_TOKEN);

    long expiresInSeconds = tokenResponse.getLong(EXPIRES_IN);

    Date expirationTime = Date.from(Instant.now().plusSeconds(expiresInSeconds));

    data.getHealthcareSetting().setEhrAccessToken(freshAccessToken);

    data.getHealthcareSetting().setEhrAccessTokenExpiryDuration(Math.toIntExact(expiresInSeconds));

    data.getHealthcareSetting().setEhrAccessTokenExpirationTime(expirationTime);

    if (tokenResponse.has(PROVIDER_ID)) {
      data.getHealthcareSetting().setDefaultProviderId(tokenResponse.getString(PROVIDER_ID));
    }

    healthcareSettingsDao.saveOrUpdate(data.getHealthcareSetting());

    logger.info(
        "Generated fresh EHR access token. old={}, new={}, changed={}, Expiration time: {}",
        fingerprintToken(previousAccessToken),
        fingerprintToken(freshAccessToken),
        !java.util.Objects.equals(previousAccessToken, freshAccessToken),
        expirationTime);
  }

  /**
   * Produces a non-sensitive fingerprint of an access token for logging. Never logs the full token;
   * only its length and last few characters so a rotation is visible in the logs.
   */
  private static String fingerprintToken(String token) {

    if (token == null || token.isBlank()) {
      return "none";
    }

    String suffix = token.length() <= 4 ? token : token.substring(token.length() - 4);

    return "len=" + token.length() + ",…" + suffix;
  }

  private void ensureValidAccessToken(KarProcessingData data) {

    if (hasUsableAccessToken(data)) {
      return;
    }

    logger.info("EHR access token is missing or approaching expiration. Generating a fresh token.");

    refreshAccessToken(data);
  }

  private boolean hasUsableAccessToken(KarProcessingData data) {

    String accessToken = data.getHealthcareSetting().getEhrAccessToken();

    Date expirationTime = data.getHealthcareSetting().getEhrAccessTokenExpirationTime();

    if (accessToken == null || accessToken.isBlank() || expirationTime == null) {
      return false;
    }

    Instant effectiveExpirationTime =
        expirationTime.toInstant().minusSeconds(TOKEN_EXPIRY_BUFFER_SECONDS);

    return Instant.now().isBefore(effectiveExpirationTime);
  }
}
