package com.drajer.ecrapp.config;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.exceptions.FhirClientConnectionException;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import ca.uhn.fhir.rest.server.exceptions.*;
import java.util.HashMap;
import java.util.Map;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.retry.backoff.FixedBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.web.client.HttpServerErrorException;

@Configuration
@ComponentScan(
    basePackages = {
      "com.drajer.ecrapp",
      "com.drajer.ersd",
      "com.drajer.sof",
      "com.drajer.eca",
      "com.drajer.routing"
    })
public class SpringConfiguration {

  public static final String ERSD_FHIR_BASE_SERVER = "https://ersd.aimsplatform.org/api/fhir";

  public static final String AUTHORIZATION_TOKEN =
      "d94874a5b6d848ae921e75b9bf202feb97905791ff890a6e189614053a8032c6f298662299dea42df6cef59fde";

  public static final FhirContext ctx = FhirContext.forR4();

  @Value("${ecr.fhir.retry.maxRetryCount}")
  private int maxRetryCount;

  public void setMaxRetryCount(int maxRetryCount) {
    this.maxRetryCount = maxRetryCount;
  }

  @Bean(name = "esrdGenericClient")
  public IGenericClient getEsrdFhirContext() {
    BearerTokenAuthInterceptor authInterceptor =
        new BearerTokenAuthInterceptor(AUTHORIZATION_TOKEN);
    IGenericClient genericClient = ctx.newRestfulGenericClient(ERSD_FHIR_BASE_SERVER);
    genericClient.registerInterceptor(authInterceptor);
    return genericClient;
  }

  @Bean(name = "jsonParser")
  public IParser getEsrdJsonParser() {
    return ctx.newJsonParser().setPrettyPrint(true);
  }

  @Bean(name = "FhirRetryTemplate")
  public RetryTemplate retryTemplate() {

    FixedBackOffPolicy backOffPolicy = new FixedBackOffPolicy();

    backOffPolicy.setBackOffPeriod(3000);

    RetryTemplate template = new RetryTemplate();

    Map<Class<? extends Throwable>, Boolean> retryableExceptions = new HashMap<>();
    retryableExceptions.put(FhirClientConnectionException.class, true);
    retryableExceptions.put(HttpServerErrorException.class, true);
    retryableExceptions.put(AuthenticationException.class, true);
    retryableExceptions.put(ForbiddenOperationException.class, true);
    retryableExceptions.put(InternalErrorException.class, true);
    retryableExceptions.put(InvalidRequestException.class, true);
    retryableExceptions.put(MethodNotAllowedException.class, true);
    retryableExceptions.put(NotImplementedOperationException.class, true);
    retryableExceptions.put(NotModifiedException.class, true);
    retryableExceptions.put(PayloadTooLargeException.class, true);
    retryableExceptions.put(PreconditionFailedException.class, true);
    retryableExceptions.put(ResourceGoneException.class, true);
    retryableExceptions.put(ResourceVersionConflictException.class, true);
    retryableExceptions.put(ResourceVersionNotSpecifiedException.class, true);
    retryableExceptions.put(UnclassifiedServerFailureException.class, true);
    retryableExceptions.put(UnprocessableEntityException.class, true);
    template.setRetryPolicy(new SimpleRetryPolicy(maxRetryCount, retryableExceptions));

    template.setBackOffPolicy(backOffPolicy);

    return template;
  }
}
