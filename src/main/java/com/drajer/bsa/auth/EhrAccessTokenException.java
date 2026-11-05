package com.drajer.bsa.auth;

/**
 * Thrown when an EHR access token cannot be obtained: the token endpoint failed or returned an
 * unusable response, a recent attempt failed and is still in its back-off period, or the wait for
 * another thread's refresh timed out.
 */
public class EhrAccessTokenException extends RuntimeException {

  private static final long serialVersionUID = 1L;

  public EhrAccessTokenException(String message) {
    super(message);
  }

  public EhrAccessTokenException(String message, Throwable cause) {
    super(message, cause);
  }
}
