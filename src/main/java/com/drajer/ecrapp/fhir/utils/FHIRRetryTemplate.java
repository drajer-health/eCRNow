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


  // To make this class more generic, configure this retry template inclass
  @Autowired
  @Qualifier("ECRRetryTemplate")

  private RetryTemplate retryTemplate;

  @Autowired FHIRRetryTemplateConfig fhirRetryTemplateConfig;


  @Value("${ecr.fhir.retry.enabled:false}")
  protected Boolean isRetryEnabled;

  private static FHIRRetryTemplate fhirRetryTemplate;

  public FHIRRetryTemplate(RetryTemplate retryTemplate) {
    this.retryTemplate = retryTemplate;
  }

  public FHIRRetryTemplateConfig getFhirRetryTemplateConfig() {
    return fhirRetryTemplateConfig;
  }


  public boolean isRetryEnabled() {
    return isRetryEnabled;
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
