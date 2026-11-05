package com.drajer.bsa.ehr.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import com.drajer.bsa.auth.EhrAccessTokenCache;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.sof.utils.FhirContextInitializer;
import jakarta.annotation.PostConstruct;
import java.util.function.Function;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class FhirOperationExecutor {

  private static final Logger logger = LoggerFactory.getLogger(FhirOperationExecutor.class);

  private static final int HTTP_UNAUTHORIZED = 401;

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

  private final FhirContextInitializer fhirContextInitializer;
  private final EhrAccessTokenCache tokenCache;

  public FhirOperationExecutor(
      FhirContextInitializer fhirContextInitializer, EhrAccessTokenCache tokenCache) {

    this.fhirContextInitializer = fhirContextInitializer;
    this.tokenCache = tokenCache;
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

    String accessToken = tokenCache.getToken(data.getHealthcareSetting()).accessToken();

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

        accessToken =
            tokenCache
                .refreshAfterUnauthorized(data.getHealthcareSetting(), accessToken)
                .accessToken();

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
}
