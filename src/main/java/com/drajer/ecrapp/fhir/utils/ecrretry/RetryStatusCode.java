package com.drajer.ecrapp.fhir.utils.ecrretry;

import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import org.springframework.classify.Classifier;
import org.springframework.retry.RetryPolicy;
import org.springframework.retry.policy.NeverRetryPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.stereotype.Component;

@Component
public class RetryStatusCode {

  private SimpleRetryPolicy simpleRetryPolicy;
  private FHIRRetryTemplateConfig fhirRetryTemplateConfig;

  public RetryStatusCode(FHIRRetryTemplateConfig fhirRetryTemplateConfig) {
    this.fhirRetryTemplateConfig = fhirRetryTemplateConfig;
  }

  public Classifier<Throwable, RetryPolicy> configureStatusCodeBasedRetryPolicy() {
    for (FHIRRetryTemplateConfig.HttpMethodType map :
        fhirRetryTemplateConfig.getHttpMethodTypeMap().values()) {
      simpleRetryPolicy = new SimpleRetryPolicy(map.getMaxRetries());
    }
    return throwable -> {
      if (throwable instanceof BaseServerResponseException) {
        return getRetryPolicyForStatus((((BaseServerResponseException) throwable).getStatusCode()));
      }
      return new NeverRetryPolicy();
    };
  }

  public RetryPolicy getRetryPolicyForStatus(int httpStatus) {
    if (fhirRetryTemplateConfig.verifyStatusCode(httpStatus)) return simpleRetryPolicy;
    else return new NeverRetryPolicy();
  }
}
