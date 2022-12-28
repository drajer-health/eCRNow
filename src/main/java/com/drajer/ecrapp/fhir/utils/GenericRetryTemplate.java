package com.drajer.ecrapp.fhir.utils;

import org.springframework.retry.RecoveryCallback;
import org.springframework.retry.RetryCallback;
import org.springframework.stereotype.Component;

@Component
public interface GenericRetryTemplate {

  // This functional interface is to enforce the return type and parameter type in its
  // implementations

  public <T, E extends Throwable> T execute(
      final RetryCallback<T, E> retryCallback, final RecoveryCallback<T> recoveryCallback) throws E;
}
