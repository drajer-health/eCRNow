package com.drajer.ecrapp.fhir.utils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.retry.RecoveryCallback;
import org.springframework.retry.RetryCallback;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Component;

@Component
public class FHIRRetryTemplate implements GenericRetryTemplate {

  // To make this class more generic, configure this rettry template inclass
  @Autowired
  @Qualifier("FhirRetryTemplate")
  private RetryTemplate retryTemplate;

  @Override
  public final <T, E extends Throwable> T execute(
      final RetryCallback<T, E> retryCallback, final RecoveryCallback<T> recoveryCallback)
      throws E {
    return retryTemplate.execute(retryCallback, recoveryCallback, null);
  }
}
