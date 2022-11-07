package com.drajer.ecrapp.fhir.utils.ecrretry;

import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import org.springframework.classify.Classifier;
import org.springframework.retry.RetryPolicy;
import org.springframework.retry.backoff.FixedBackOffPolicy;
import org.springframework.retry.policy.ExceptionClassifierRetryPolicy;
import org.springframework.retry.policy.NeverRetryPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Component;

@Component
public class RetryStatusCode {

  private SimpleRetryPolicy simpleRetryPolicy;
  private FHIRRetryTemplateConfig fhirRetryTemplateConfig;
  private RetryTemplate retryTemplate;
  private FixedBackOffPolicy backOffPolicy;

  public RetryStatusCode(FHIRRetryTemplateConfig fhirRetryTemplateConfig) {
    this.fhirRetryTemplateConfig = fhirRetryTemplateConfig;
  }

  public RetryTemplate configureRetryTemplate() {
    retryTemplate = new RetryTemplate();
    backOffPolicy = new FixedBackOffPolicy();
    backOffPolicy.setBackOffPeriod(fhirRetryTemplateConfig.getRetryWaitTime());
    ExceptionClassifierRetryPolicy policy = new ExceptionClassifierRetryPolicy();
    policy.setExceptionClassifier(configureStatusCodeBasedRetryPolicy());
    retryTemplate.setBackOffPolicy(backOffPolicy);
    retryTemplate.setRetryPolicy(policy);
    return retryTemplate;
  }

  public Classifier<Throwable, RetryPolicy> configureStatusCodeBasedRetryPolicy() {
    simpleRetryPolicy = new SimpleRetryPolicy(fhirRetryTemplateConfig.getMaxRetries());
    return throwable -> {
      if (throwable instanceof RetryableException) {
        return getRetryPolicyForException(throwable);
      }
      return new NeverRetryPolicy();
    };
  }

  public RetryPolicy getRetryPolicyForException(Throwable throwable) {

    int httpStatusCode = ((RetryableException) throwable).getHttpResponseStatusCode();
    String methodName = ((RetryableException) throwable).getMethodName();
    if (fhirRetryTemplateConfig.getHttpMethodTypeMap().containsKey(methodName)) {
      FHIRRetryTemplateConfig.HttpMethodType httpMethodType =
          fhirRetryTemplateConfig.getHttpMethodTypeMap().get(methodName);
      if (httpMethodType.getRetryStatusCodes().contains(httpStatusCode)) {
        simpleRetryPolicy.setMaxAttempts(httpMethodType.getMaxRetries());
        backOffPolicy.setBackOffPeriod(httpMethodType.getRetryWaitTime());
        return simpleRetryPolicy;
      }
    }

    if (methodName == null && fhirRetryTemplateConfig.getRetryStatusCodes().contains(httpStatusCode)) {
        return simpleRetryPolicy;
    }
    return new NeverRetryPolicy();
  }
}