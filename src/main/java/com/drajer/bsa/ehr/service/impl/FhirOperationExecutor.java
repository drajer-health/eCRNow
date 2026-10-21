package com.drajer.bsa.ehr.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.KarProcessingData;
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

    } catch (BaseServerResponseException exception) {

      if (exception.getStatusCode() != HTTP_UNAUTHORIZED) {
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

      } catch (BaseServerResponseException retryException) {

        if (retryException.getStatusCode() == HTTP_UNAUTHORIZED) {
          logger.error(
              "FHIR request returned 401 after generating a fresh token. "
                  + "No further authentication retry will be attempted. Request ID: {}",
              data.getNotificationContext().getxRequestId());
        }

        throw retryException;
      }
    }
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

    logger.info("Generated fresh EHR access token. Expiration time: {}", expirationTime);
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
