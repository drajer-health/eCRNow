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

  private final RetryTemplate retryTemplate;
  private final FHIRRetryTemplateConfig fhirRetryTemplateConfig;
  private final Boolean isRetryEnabled;

  private static FHIRRetryTemplate fhirRetryTemplate;

  /**
   * Instantiates a new FHIR retry template.
   *
   * @param retryTemplate the ECR retry template
   * @param fhirRetryTemplateConfig the FHIR retry template configuration
   * @param isRetryEnabled whether retry is enabled from properties
   */
  @Autowired
  public FHIRRetryTemplate(
      @Qualifier("ECRRetryTemplate") RetryTemplate retryTemplate,
      FHIRRetryTemplateConfig fhirRetryTemplateConfig,
      @Value("${ecr.fhir.retry.enabled:false}") Boolean isRetryEnabled) {
    this.retryTemplate = retryTemplate;
    this.fhirRetryTemplateConfig = fhirRetryTemplateConfig;
    this.isRetryEnabled = isRetryEnabled;
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
