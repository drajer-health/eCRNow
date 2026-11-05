package com.drajer.bsa.auth;

import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

/**
 * HTTP timeouts for calls to EHR token and SMART configuration endpoints.
 *
 * <p>Without timeouts, a token endpoint that stops responding would block the calling thread
 * indefinitely. Because {@link EhrAccessTokenCache} lets only one thread at a time request a token
 * for a FHIR server, that would also block every other job for the same server. One token request
 * makes at most two calls (SMART configuration discovery and the token call), so with a responsive
 * or unreachable server it ends within 2 x (connect + read) = 40 s, below the default {@code
 * ecr.ehr.token.lock-timeout-seconds} of 45 s. The read timeout applies to each socket read, so a
 * server that keeps sending data very slowly can take longer; waiting jobs then fail after the lock
 * timeout instead of blocking.
 */
public final class TokenEndpointHttp {

  static final int CONNECT_TIMEOUT_MILLIS = 15_000;
  static final int READ_TIMEOUT_MILLIS = 30_000;

  private TokenEndpointHttp() {}

  /** Applies the token endpoint timeouts to the RestTemplate and returns it. */
  public static RestTemplate withTimeouts(RestTemplate restTemplate) {
    SimpleClientHttpRequestFactory factory = new SimpleClientHttpRequestFactory();
    factory.setConnectTimeout(CONNECT_TIMEOUT_MILLIS);
    factory.setReadTimeout(READ_TIMEOUT_MILLIS);
    restTemplate.setRequestFactory(factory);
    return restTemplate;
  }
}
