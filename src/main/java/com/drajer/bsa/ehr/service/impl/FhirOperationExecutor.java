package com.drajer.bsa.ehr.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.auth.EhrAccessTokenCache;
import com.drajer.bsa.auth.EhrTokenMetrics;
import com.drajer.bsa.auth.EhrTokenMetrics.Outcome;
import com.drajer.bsa.auth.EhrTokenStorageMode;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.sof.utils.FhirContextInitializer;
import jakarta.annotation.PostConstruct;
import java.time.Instant;
import java.util.Date;
import java.util.function.Function;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class FhirOperationExecutor {

  private static final Logger logger = LoggerFactory.getLogger(FhirOperationExecutor.class);

  private static final int HTTP_UNAUTHORIZED = 401;

  @Value("${token.refresh.threshold:60}")
  private long TOKEN_EXPIRY_BUFFER_SECONDS;

  /** Default number of token refresh + retry attempts after a 401. */
  static final int DEFAULT_RETRY_ON_401_MAX_ATTEMPTS = 1;

  /** Upper limit, so a misconfigured value cannot flood the EHR token endpoint. */
  static final int MAX_ALLOWED_RETRY_ON_401_ATTEMPTS = 3;

  /**
   * How many times a request that gets HTTP 401 is retried with a freshly generated token. 0
   * disables the retry (the first 401 is thrown). Values are limited to 0..3.
   */
  @Value("${ecr.fhir.auth.retry-on-401.max-attempts:" + DEFAULT_RETRY_ON_401_MAX_ATTEMPTS + "}")
  private int retryOn401MaxAttempts = DEFAULT_RETRY_ON_401_MAX_ATTEMPTS;

  private static final String ACCESS_TOKEN = "access_token";
  private static final String EXPIRES_IN = "expires_in";
  private static final String PROVIDER_ID = "uuid";

  private final FhirContextInitializer fhirContextInitializer;
  private final AuthorizationUtils authorizationUtils;
  private final HealthcareSettingsDao healthcareSettingsDao;
  private final EhrAccessTokenCache tokenCache;
  private final EhrTokenStorageMode tokenStorage;
  private final EhrTokenMetrics tokenMetrics;

  @Autowired
  public FhirOperationExecutor(
      FhirContextInitializer fhirContextInitializer,
      AuthorizationUtils authorizationUtils,
      HealthcareSettingsDao healthcareSettingsDao,
      EhrAccessTokenCache tokenCache,
      EhrTokenStorageMode tokenStorage,
      EhrTokenMetrics tokenMetrics) {

    this.fhirContextInitializer = fhirContextInitializer;
    this.authorizationUtils = authorizationUtils;
    this.healthcareSettingsDao = healthcareSettingsDao;
    this.tokenCache = tokenCache;
    this.tokenStorage = tokenStorage;
    this.tokenMetrics = tokenMetrics == null ? EhrTokenMetrics.disabled() : tokenMetrics;
  }

  /** Without performance metrics. */
  public FhirOperationExecutor(
      FhirContextInitializer fhirContextInitializer,
      AuthorizationUtils authorizationUtils,
      HealthcareSettingsDao healthcareSettingsDao,
      EhrAccessTokenCache tokenCache,
      EhrTokenStorageMode tokenStorage) {
    this(
        fhirContextInitializer,
        authorizationUtils,
        healthcareSettingsDao,
        tokenCache,
        tokenStorage,
        EhrTokenMetrics.disabled());
  }

  /** In-memory mode: tokens only through the cache. */
  public FhirOperationExecutor(
      FhirContextInitializer fhirContextInitializer, EhrAccessTokenCache tokenCache) {
    this(fhirContextInitializer, null, null, tokenCache, EhrTokenStorageMode.memory());
  }

  /** Database mode: the original implementation, with the token saved on the HealthcareSetting. */
  public FhirOperationExecutor(
      FhirContextInitializer fhirContextInitializer,
      AuthorizationUtils authorizationUtils,
      HealthcareSettingsDao healthcareSettingsDao) {
    this(
        fhirContextInitializer,
        authorizationUtils,
        healthcareSettingsDao,
        null,
        EhrTokenStorageMode.database());
  }

  @PostConstruct
  void logRetryOn401Configuration() {
    int effective = getEffectiveRetryOn401MaxAttempts();
    if (effective != retryOn401MaxAttempts) {
      logger.warn(
          "ecr.fhir.auth.retry-on-401.max-attempts={} is outside the allowed range 0..{}; using {}",
          retryOn401MaxAttempts,
          MAX_ALLOWED_RETRY_ON_401_ATTEMPTS,
          effective);
    }
    logger.info("FHIR token refresh retries on HTTP 401: {}", effective);
  }

  /** The configured retry count limited to 0..MAX_ALLOWED_RETRY_ON_401_ATTEMPTS. */
  int getEffectiveRetryOn401MaxAttempts() {
    return Math.max(0, Math.min(retryOn401MaxAttempts, MAX_ALLOWED_RETRY_ON_401_ATTEMPTS));
  }

  public <T> T execute(
      KarProcessingData data, FhirContext context, Function<IGenericClient, T> operation) {

    String accessToken = currentAccessToken(data);

    IGenericClient client = createClient(data, context, accessToken);

    int maxAttempts = getEffectiveRetryOn401MaxAttempts();
    int attempt = 0;

    while (true) {
      try {
        return operation.apply(client);

      } catch (RuntimeException exception) {

        // With ecr.fhir.retry.enabled=true the EcrFhirRetryClient wraps every HAPI exception in a
        // RetryableException, so a 401 does not arrive as a BaseServerResponseException.
        if (!isUnauthorized(exception)) {
          throw exception;
        }

        String requestId = data.getNotificationContext().getxRequestId();

        if (attempt >= maxAttempts) {
          if (maxAttempts == 0) {
            logger.error(
                "FHIR request returned 401 and token retry is disabled "
                    + "(ecr.fhir.auth.retry-on-401.max-attempts=0). Request ID: {}",
                requestId);
          } else {
            logger.error(
                "FHIR request still returned 401 after {} token refresh attempt(s). "
                    + "No further authentication retry will be attempted. Request ID: {}",
                maxAttempts,
                requestId);
          }
          throw exception;
        }

        attempt++;
        logger.warn(
            "FHIR request returned 401. Generating a fresh access token and retrying "
                + "(attempt {} of {}). Request ID: {}",
            attempt,
            maxAttempts,
            requestId);

        accessToken = refreshedAccessToken(data, accessToken);

        // The previous client still carries the old bearer token, so a new client is required.
        client = createClient(data, context, accessToken);
      }
    }
  }

  /**
   * Returns true when the exception represents an HTTP 401, whether it is thrown directly by HAPI
   * (BaseServerResponseException) or wrapped by the eCRNow retry client (RetryableException). The
   * cause chain is checked as well.
   */
  public static boolean isUnauthorized(Throwable throwable) {

    Throwable current = throwable;
    int depth = 0;

    while (current != null && depth < 10) {

      if (current instanceof RetryableException
          && ((RetryableException) current).getHttpResponseStatusCode() == HTTP_UNAUTHORIZED) {
        return true;
      }

      if (current instanceof BaseServerResponseException
          && ((BaseServerResponseException) current).getStatusCode() == HTTP_UNAUTHORIZED) {
        return true;
      }

      if (current.getCause() == current) {
        break;
      }
      current = current.getCause();
      depth++;
    }

    return false;
  }

  private IGenericClient createClient(
      KarProcessingData data, FhirContext context, String accessToken) {

    return fhirContextInitializer.createClient(
        context,
        data.getHealthcareSetting().getFhirServerBaseURL(),
        accessToken,
        data.getNotificationContext().getxRequestId(),
        data.getNotificationContext().getEhrLaunchContext());
  }

  /** The access token to start with, from the configured token storage. */
  private String currentAccessToken(KarProcessingData data) {
    if (tokenStorage.isInMemory()) {
      EhrTokenStorageMode.keepTokenChangesOutOfDatabase(
          healthcareSettingsDao, data.getHealthcareSetting());
      return tokenCache.getToken(data.getHealthcareSetting()).accessToken();
    }
    // Database mode (the original implementation); the metrics only measure it.
    EhrTokenMetrics.Sample sample = tokenMetrics.begin();
    try {
      boolean reuse = tokenMetrics.isEnabled() && hasUsableAccessToken(data);
      long requestStarted = tokenMetrics.now();
      try {
        ensureValidAccessToken(data);
      } catch (RuntimeException e) {
        sample.outcome(Outcome.DATABASE_REQUEST_FAILED);
        throw e;
      } finally {
        if (!reuse) {
          sample.endpoint(tokenMetrics.now() - requestStarted);
        }
      }
      sample.outcome(reuse ? Outcome.DATABASE_REUSED : Outcome.DATABASE_REQUESTED);
      return data.getHealthcareSetting().getEhrAccessToken();
    } finally {
      sample.finish();
    }
  }

  /** A new access token after the EHR rejected {@code rejectedAccessToken} with HTTP 401. */
  private String refreshedAccessToken(KarProcessingData data, String rejectedAccessToken) {
    if (tokenStorage.isInMemory()) {
      return tokenCache
          .refreshAfterUnauthorized(data.getHealthcareSetting(), rejectedAccessToken)
          .accessToken();
    }
    EhrTokenMetrics.Sample sample = tokenMetrics.begin();
    long requestStarted = tokenMetrics.now();
    try {
      refreshAccessToken(data);
      sample.outcome(Outcome.DATABASE_REQUESTED_AFTER_401);
    } catch (RuntimeException e) {
      sample.outcome(Outcome.DATABASE_REQUEST_FAILED);
      throw e;
    } finally {
      sample.endpoint(tokenMetrics.now() - requestStarted);
      sample.finish();
    }
    return data.getHealthcareSetting().getEhrAccessToken();
  }

  // ---- Database mode: the original implementation ----

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
