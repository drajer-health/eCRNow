package com.drajer.ecrapp.fhir.utils;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.retry.RecoveryCallback;
import org.springframework.retry.RetryCallback;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Component;

@Component
public class FHIRRetryTemplate implements GenericRetryTemplate, InitializingBean {

  private static FHIRRetryTemplate fhirRetryTemplate;
  // To make this class more generic, configure this retry template inclass
  @Autowired
  @Qualifier("FhirRetryTemplate")
  private RetryTemplate retryTemplate;

  @Autowired FHIRRetryTemplateConfig fhirRetryTemplateConfig;

  public FHIRRetryTemplateConfig getFhirRetryTemplateConfig() {
    return fhirRetryTemplateConfig;
  }

  public void setRetryTemplate(RetryTemplate retryTemplate) {
    this.retryTemplate = retryTemplate;
  }

  @Value("${ecr.fhir.retry.enabled}")
  protected Boolean isRetryEnabled;

  public boolean isRetryEnabled() {
    return isRetryEnabled;
  }

  public void setRetryEnabled(boolean retryEnabled) {
    this.isRetryEnabled = retryEnabled;
  }

  @Override
  public final <T, E extends Throwable> T execute(
      final RetryCallback<T, E> retryCallback, final RecoveryCallback<T> recoveryCallback)
      throws E {
    return retryTemplate.execute(retryCallback, recoveryCallback);
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    fhirRetryTemplate = this;
  }

  public static FHIRRetryTemplate getInstance() {
    return fhirRetryTemplate;
  }
}
