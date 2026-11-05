package com.drajer.bsa.ehr.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.AuthenticationException;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import ca.uhn.fhir.rest.server.exceptions.InternalErrorException;
import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.bsa.auth.EhrAccessTokenCache;
import com.drajer.bsa.auth.EhrAccessTokenCache.EhrAccessToken;
import com.drajer.bsa.auth.EhrAccessTokenException;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.sof.utils.FhirContextInitializer;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * Unit tests for {@link FhirOperationExecutor}. Token caching and expiry are covered by
 * EhrAccessTokenCacheTest; here the cache is mocked.
 */
@RunWith(MockitoJUnitRunner.Silent.class)
public class FhirOperationExecutorTest {

  private static final String BASE_URL = "http://ehr.example.org/fhir";
  private static final String REQUEST_ID = "req-001";
  private static final String EHR_CONTEXT = "ehr-launch-context";

  @Mock private FhirContextInitializer fhirContextInitializer;
  @Mock private EhrAccessTokenCache tokenCache;

  @Mock private KarProcessingData data;
  @Mock private HealthcareSetting healthcareSetting;
  @Mock private NotificationContext notificationContext;
  @Mock private FhirContext fhirContext;

  @Mock private IGenericClient firstClient;
  @Mock private IGenericClient retryClient;

  private FhirOperationExecutor executor;

  @Before
  public void setUp() {
    executor = new FhirOperationExecutor(fhirContextInitializer, tokenCache);

    when(data.getHealthcareSetting()).thenReturn(healthcareSetting);
    when(data.getNotificationContext()).thenReturn(notificationContext);
    when(healthcareSetting.getFhirServerBaseURL()).thenReturn(BASE_URL);
    when(notificationContext.getxRequestId()).thenReturn(REQUEST_ID);
    when(notificationContext.getEhrLaunchContext()).thenReturn(EHR_CONTEXT);

    // first createClient call -> firstClient, any later call -> retryClient
    when(fhirContextInitializer.createClient(any(), any(), any(), any(), any()))
        .thenReturn(firstClient, retryClient);

    when(tokenCache.getToken(healthcareSetting)).thenReturn(token("valid-token"));
    // Every refresh after a 401 returns a different token: fresh-1, fresh-2, fresh-3.
    when(tokenCache.refreshAfterUnauthorized(eq(healthcareSetting), anyString()))
        .thenReturn(token("fresh-1"), token("fresh-2"), token("fresh-3"));
  }

  // ---------------- token from the cache ----------------

  @Test
  public void execute_usesTheCachedTokenWithoutRefreshing() {
    String result = executor.execute(data, fhirContext, client -> "result-" + name(client));

    assertEquals("result-first", result);
    verify(tokenCache, times(1)).getToken(healthcareSetting);
    verify(tokenCache, never()).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer)
        .createClient(fhirContext, BASE_URL, "valid-token", REQUEST_ID, EHR_CONTEXT);
  }

  @Test
  public void execute_tokenCannotBeObtained_throwsWithoutCallingTheEhr() {
    EhrAccessTokenException failure = new EhrAccessTokenException("token endpoint down");
    when(tokenCache.getToken(healthcareSetting)).thenThrow(failure);

    EhrAccessTokenException thrown =
        assertThrows(
            EhrAccessTokenException.class,
            () -> executor.execute(data, fhirContext, client -> "never"));

    assertSame(failure, thrown);
    verify(fhirContextInitializer, never()).createClient(any(), any(), any(), any(), any());
  }

  // ---------------- execute: error handling and the 401 retry ----------------

  @Test
  public void execute_non401ServerError_isRethrownWithoutRetry() {
    InternalErrorException serverError = new InternalErrorException("server error");

    InternalErrorException thrown =
        assertThrows(
            InternalErrorException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      throw serverError;
                    }));

    assertSame(serverError, thrown);
    verify(tokenCache, never()).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_otherRuntimeException_isNotCaught() {
    assertThrows(
        IllegalStateException.class,
        () ->
            executor.execute(
                data,
                fhirContext,
                client -> {
                  throw new IllegalStateException("socket closed");
                }));

    verify(tokenCache, never()).refreshAfterUnauthorized(any(), any());
  }

  @Test
  public void execute_401ThenSuccess_refreshesTokenAndRetriesWithANewClient() {
    List<IGenericClient> usedClients = new ArrayList<>();

    String result =
        executor.execute(
            data,
            fhirContext,
            client -> {
              usedClients.add(client);
              if (client == firstClient) {
                throw new AuthenticationException("expired token");
              }
              return "result-" + name(client);
            });

    assertEquals("result-retry", result);
    assertEquals(List.of(firstClient, retryClient), usedClients);
    verify(tokenCache, times(1)).refreshAfterUnauthorized(healthcareSetting, "valid-token");

    InOrder order = inOrder(fhirContextInitializer);
    order
        .verify(fhirContextInitializer)
        .createClient(fhirContext, BASE_URL, "valid-token", REQUEST_ID, EHR_CONTEXT);
    order
        .verify(fhirContextInitializer)
        .createClient(fhirContext, BASE_URL, "fresh-1", REQUEST_ID, EHR_CONTEXT);
  }

  @Test
  public void execute_401TwiceInARow_rethrowsTheRetryErrorAndRefreshesOnlyOnce() {
    AuthenticationException retryError = new AuthenticationException("still unauthorized");

    BaseServerResponseException thrown =
        assertThrows(
            BaseServerResponseException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      if (client == firstClient) {
                        throw new AuthenticationException("expired token");
                      }
                      throw retryError;
                    }));

    assertSame(retryError, thrown);
    assertEquals(401, thrown.getStatusCode());
    verify(tokenCache, times(1)).refreshAfterUnauthorized(healthcareSetting, "valid-token");
    verify(fhirContextInitializer, times(2)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_401ThenServerError_rethrowsTheServerError() {
    InternalErrorException retryError = new InternalErrorException("server error on retry");

    BaseServerResponseException thrown =
        assertThrows(
            BaseServerResponseException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      if (client == firstClient) {
                        throw new AuthenticationException("expired token");
                      }
                      throw retryError;
                    }));

    assertSame(retryError, thrown);
    assertEquals(500, thrown.getStatusCode());
    verify(tokenCache, times(1)).refreshAfterUnauthorized(any(), any());
  }

  @Test
  public void execute_401AndTokenRefreshFails_throwsTheTokenError() {
    EhrAccessTokenException failure = new EhrAccessTokenException("token endpoint down");
    when(tokenCache.refreshAfterUnauthorized(eq(healthcareSetting), anyString()))
        .thenThrow(failure);

    EhrAccessTokenException thrown =
        assertThrows(
            EhrAccessTokenException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      throw new AuthenticationException("expired token");
                    }));

    assertSame(failure, thrown);
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  // ------- ecr.fhir.retry.enabled=true: EcrFhirRetryClient wraps errors in RetryableException
  // -------

  @Test
  public void execute_wrapped401ThenSuccess_refreshesTokenAndRetriesWithANewClient() {
    List<IGenericClient> usedClients = new ArrayList<>();

    String result =
        executor.execute(
            data,
            fhirContext,
            client -> {
              usedClients.add(client);
              if (client == firstClient) {
                throw wrapped401(); // what EcrFhirRetryClient throws in production
              }
              return "result-" + name(client);
            });

    assertEquals("result-retry", result);
    assertEquals(List.of(firstClient, retryClient), usedClients);
    verify(tokenCache, times(1)).refreshAfterUnauthorized(healthcareSetting, "valid-token");
    verify(fhirContextInitializer, times(2)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_wrapped401TwiceInARow_rethrowsTheRetryErrorAndRefreshesOnlyOnce() {
    RetryableException retryError = wrapped401();

    RetryableException thrown =
        assertThrows(
            RetryableException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      throw client == firstClient ? wrapped401() : retryError;
                    }));

    assertSame(retryError, thrown); // original status and cause are preserved
    assertEquals(401, thrown.getHttpResponseStatusCode());
    verify(tokenCache, times(1)).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(2)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_wrappedNon401ServerError_isRethrownWithoutRetry() {
    RetryableException serverError =
        new RetryableException(new InternalErrorException("server error"), 500, "GET");

    RetryableException thrown =
        assertThrows(
            RetryableException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      throw serverError;
                    }));

    assertSame(serverError, thrown);
    verify(tokenCache, never()).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  // ---------------- isUnauthorized ----------------

  @Test
  public void isUnauthorized_recognisesDirectWrappedAndNested401() {
    assertTrue(FhirOperationExecutor.isUnauthorized(new AuthenticationException("401")));
    assertTrue(FhirOperationExecutor.isUnauthorized(wrapped401()));
    assertTrue(
        FhirOperationExecutor.isUnauthorized(
            new RuntimeException(new AuthenticationException("401"))));
    // Status 0 on the wrapper but a 401 cause (handleException copies status only for HAPI errors)
    assertTrue(
        FhirOperationExecutor.isUnauthorized(
            new RetryableException(new AuthenticationException("401"), 0, "GET")));
  }

  @Test
  public void isUnauthorized_isFalseForOtherErrors() {
    assertFalse(FhirOperationExecutor.isUnauthorized(null));
    assertFalse(FhirOperationExecutor.isUnauthorized(new ResourceNotFoundException("404")));
    assertFalse(
        FhirOperationExecutor.isUnauthorized(
            new RetryableException(new ResourceNotFoundException("404"), 404, "GET")));
    assertFalse(
        FhirOperationExecutor.isUnauthorized(
            new RetryableException(new InternalErrorException("500"), 500, "GET")));
    assertFalse(FhirOperationExecutor.isUnauthorized(new IllegalStateException("x")));
  }

  // ---------------- ecr.fhir.auth.retry-on-401.max-attempts ----------------

  @Test
  public void retryOn401_defaultIsOneAttempt() {
    assertEquals(1, executor.getEffectiveRetryOn401MaxAttempts());
  }

  @Test
  public void retryOn401_zero_throwsFirst401WithoutRefreshing() {
    setMaxAttempts(0);
    RetryableException first401 = wrapped401();

    RetryableException thrown =
        assertThrows(
            RetryableException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      throw first401;
                    }));

    assertSame(first401, thrown);
    verify(tokenCache, never()).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_two_succeedsOnTheSecondRetry() {
    setMaxAttempts(2);
    List<Integer> calls = new ArrayList<>();

    String result =
        executor.execute(
            data,
            fhirContext,
            client -> {
              calls.add(calls.size() + 1);
              if (calls.size() < 3) {
                throw wrapped401(); // original call and first retry are rejected
              }
              return "ok";
            });

    assertEquals("ok", result);
    assertEquals(3, calls.size());
    // Each refresh reports the token that was just rejected.
    InOrder order = inOrder(tokenCache);
    order.verify(tokenCache).refreshAfterUnauthorized(healthcareSetting, "valid-token");
    order.verify(tokenCache).refreshAfterUnauthorized(healthcareSetting, "fresh-1");
    verify(tokenCache, times(2)).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(3)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_two_always401_refreshesTwiceThenThrowsTheLastError() {
    setMaxAttempts(2);
    List<RetryableException> raised = new ArrayList<>();

    RetryableException thrown =
        assertThrows(
            RetryableException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      RetryableException e = wrapped401();
                      raised.add(e);
                      throw e;
                    }));

    assertEquals(3, raised.size());
    assertSame(raised.get(2), thrown);
    verify(tokenCache, times(2)).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(3)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_aboveMaximum_isLimitedToThree() {
    setMaxAttempts(10);
    assertEquals(3, executor.getEffectiveRetryOn401MaxAttempts());

    assertThrows(
        RetryableException.class,
        () ->
            executor.execute(
                data,
                fhirContext,
                client -> {
                  throw wrapped401();
                }));

    verify(tokenCache, times(3)).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(4)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_negative_isTreatedAsZero() {
    setMaxAttempts(-1);
    assertEquals(0, executor.getEffectiveRetryOn401MaxAttempts());

    assertThrows(
        AuthenticationException.class,
        () ->
            executor.execute(
                data,
                fhirContext,
                client -> {
                  throw new AuthenticationException("expired token");
                }));

    verify(tokenCache, never()).refreshAfterUnauthorized(any(), any());
  }

  @Test
  public void retryOn401_doesNotAffectNon401Errors() {
    setMaxAttempts(3);
    InternalErrorException serverError = new InternalErrorException("server error");

    InternalErrorException thrown =
        assertThrows(
            InternalErrorException.class,
            () ->
                executor.execute(
                    data,
                    fhirContext,
                    client -> {
                      throw serverError;
                    }));

    assertSame(serverError, thrown);
    verify(tokenCache, never()).refreshAfterUnauthorized(any(), any());
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  // ---------------- helpers ----------------

  private void setMaxAttempts(int value) {
    ReflectionTestUtils.setField(executor, "retryOn401MaxAttempts", value);
  }

  private static RetryableException wrapped401() {
    return new RetryableException(new AuthenticationException("HTTP 401 Unauthorized"), 401, "GET");
  }

  private static EhrAccessToken token(String value) {
    return new EhrAccessToken(value, Instant.now().plusSeconds(3600), null);
  }

  private String name(IGenericClient client) {
    return client == firstClient ? "first" : "retry";
  }
}
