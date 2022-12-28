package com.drajer.ecrapp.fhir.utils;

public class RetryableException extends RuntimeException {
  private final String methodName;
  private final int httpResponseStatusCode;

  public RetryableException(
      final String message, final int httpResponseStatusCode, final String methodName) {
    super(message);
    this.methodName = methodName;
    this.httpResponseStatusCode = httpResponseStatusCode;
  }

  public RetryableException(
      final Throwable cause, final int httpResponseStatusCode, final String methodName) {
    super(cause);
    this.methodName = methodName;
    this.httpResponseStatusCode = httpResponseStatusCode;
  }

  public RetryableException(
      final String message,
      final Throwable cause,
      final int httpResponseStatusCode,
      final String methodName) {
    super(message, cause);
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
