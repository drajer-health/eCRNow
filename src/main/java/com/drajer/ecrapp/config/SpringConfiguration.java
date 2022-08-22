package com.drajer.ecrapp.config;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import java.util.HashMap;
import java.util.Map;
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
    retryableExceptions.put(HttpServerErrorException.class, true);
    template.setRetryPolicy(new SimpleRetryPolicy(5, retryableExceptions));

    template.setBackOffPolicy(backOffPolicy);

    return template;
  }
}
