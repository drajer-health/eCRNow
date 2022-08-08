package com.drajer.ecrapp.fhir.utils.ecrretry;

import org.springframework.beans.factory.annotation.Value;

public class EcrFhirRetryConfig {

  @Value("${ecr.fhir.retry.enables}")
  protected boolean retryEnabled;

  public boolean isRetryEnabled() {
    return retryEnabled;
  }

  public void setRetryEnabled(boolean retryEnabled) {
    this.retryEnabled = retryEnabled;
  }
}
