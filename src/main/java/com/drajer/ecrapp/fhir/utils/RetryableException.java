package com.drajer.ecrapp.fhir.utils;

import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;

public class RetryableException extends BaseServerResponseException {
  private final String methodName;
  private final int httpResponseStatusCode;

  public RetryableException(
      final String message, final int httpResponseStatusCode, final String methodName) {
    super(httpResponseStatusCode);
    this.methodName = methodName;
    this.httpResponseStatusCode = httpResponseStatusCode;
  }

  public RetryableException(
      final Throwable cause, final int httpResponseStatusCode, final String methodName) {
    super(httpResponseStatusCode, cause);
    this.methodName = methodName;
    this.httpResponseStatusCode = httpResponseStatusCode;
  }

  public RetryableException(
      final String message,
      final Throwable cause,
      final int httpResponseStatusCode,
      final String methodName) {
    super(httpResponseStatusCode, cause);
    this.methodName = methodName;
    this.httpResponseStatusCode = httpResponseStatusCode;
  }

  public String getMethodName() {
    return methodName;
  }

  public int getHttpResponseStatusCode() {
    return httpResponseStatusCode;
  }
}
