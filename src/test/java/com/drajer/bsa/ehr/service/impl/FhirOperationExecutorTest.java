package com.drajer.bsa.ehr.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.sof.utils.FhirContextInitializer;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

/** Unit tests for {@link FhirOperationExecutor}. */
@RunWith(MockitoJUnitRunner.Silent.class)
public class FhirOperationExecutorTest {

  private static final String BASE_URL = "http://ehr.example.org/fhir";
  private static final String REQUEST_ID = "req-001";
  private static final String EHR_CONTEXT = "ehr-launch-context";

  @Mock private FhirContextInitializer fhirContextInitializer;
  @Mock private AuthorizationUtils authorizationUtils;
  @Mock private HealthcareSettingsDao healthcareSettingsDao;

  @Mock private KarProcessingData data;
  @Mock private HealthcareSetting healthcareSetting;
  @Mock private NotificationContext notificationContext;
  @Mock private FhirContext fhirContext;

  @Mock private IGenericClient firstClient;
  @Mock private IGenericClient retryClient;

  private FhirOperationExecutor executor;

  @Before
  public void setUp() {
    executor =
        new FhirOperationExecutor(
            fhirContextInitializer, authorizationUtils, healthcareSettingsDao);
    // @Value("${token.refresh.threshold:60}") is not injected outside Spring.
    ReflectionTestUtils.setField(executor, "TOKEN_EXPIRY_BUFFER_SECONDS", 60L);

    when(data.getHealthcareSetting()).thenReturn(healthcareSetting);
    when(data.getNotificationContext()).thenReturn(notificationContext);
    when(healthcareSetting.getFhirServerBaseURL()).thenReturn(BASE_URL);
    when(notificationContext.getxRequestId()).thenReturn(REQUEST_ID);
    when(notificationContext.getEhrLaunchContext()).thenReturn(EHR_CONTEXT);

    // first createClient call -> firstClient, any later call -> retryClient
    when(fhirContextInitializer.createClient(any(), any(), any(), any(), any()))
        .thenReturn(firstClient, retryClient);

    when(authorizationUtils.getToken(healthcareSetting))
        .thenReturn(tokenResponse("fresh-token", 3600, "provider-123"));
  }

  // ---------------- ensureValidAccessToken / hasUsableAccessToken ----------------

  @Test
  public void execute_validToken_runsOperationWithoutRefreshing() {
    validToken();

    String result = executor.execute(data, fhirContext, client -> "result-" + name(client));

    assertEquals("result-first", result);
    verify(authorizationUtils, never()).getToken(any());
    verify(healthcareSettingsDao, never()).saveOrUpdate(any());
    verify(fhirContextInitializer)
        .createClient(fhirContext, BASE_URL, "valid-token", REQUEST_ID, EHR_CONTEXT);
  }

  @Test
  public void execute_missingToken_refreshesBeforeRunning() {
    when(healthcareSetting.getEhrAccessToken()).thenReturn(null);
    when(healthcareSetting.getEhrAccessTokenExpirationTime()).thenReturn(inSeconds(3600));

    assertEquals("ok", executor.execute(data, fhirContext, client -> "ok"));

    verifyTokenRefreshedOnce(true);
  }

  @Test
  public void execute_blankToken_refreshesBeforeRunning() {
    when(healthcareSetting.getEhrAccessToken()).thenReturn("   ");
    when(healthcareSetting.getEhrAccessTokenExpirationTime()).thenReturn(inSeconds(3600));

    executor.execute(data, fhirContext, client -> "ok");

    verifyTokenRefreshedOnce(true);
  }

  @Test
  public void execute_missingExpirationTime_refreshesBeforeRunning() {
    when(healthcareSetting.getEhrAccessToken()).thenReturn("valid-token");
    when(healthcareSetting.getEhrAccessTokenExpirationTime()).thenReturn(null);

    executor.execute(data, fhirContext, client -> "ok");

    verifyTokenRefreshedOnce(true);
  }

  @Test
  public void execute_tokenExpiringWithinTheBuffer_refreshesBeforeRunning() {
    when(healthcareSetting.getEhrAccessToken()).thenReturn("valid-token");
    // 30 seconds left, but the buffer is 60 seconds -> treated as expired
    when(healthcareSetting.getEhrAccessTokenExpirationTime()).thenReturn(inSeconds(30));

    executor.execute(data, fhirContext, client -> "ok");

    verifyTokenRefreshedOnce(true);
  }

  @Test
  public void execute_tokenResponseWithoutProviderId_doesNotSetDefaultProvider() {
    when(healthcareSetting.getEhrAccessToken()).thenReturn(null);
    when(authorizationUtils.getToken(healthcareSetting))
        .thenReturn(tokenResponse("fresh-token", 3600, null));

    executor.execute(data, fhirContext, client -> "ok");

    verifyTokenRefreshedOnce(false);
  }

  // ---------------- execute: error handling and the single 401 retry ----------------

  @Test
  public void execute_non401ServerError_isRethrownWithoutRetry() {
    validToken();
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
    verify(authorizationUtils, never()).getToken(any());
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_otherRuntimeException_isNotCaught() {
    validToken();

    assertThrows(
        IllegalStateException.class,
        () ->
            executor.execute(
                data,
                fhirContext,
                client -> {
                  throw new IllegalStateException("socket closed");
                }));

    verify(authorizationUtils, never()).getToken(any());
  }

  @Test
  public void execute_401ThenSuccess_refreshesTokenAndRetriesWithANewClient() {
    validToken();
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
    verifyTokenRefreshedOnce(true);
    verify(fhirContextInitializer, times(2)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_401TwiceInARow_rethrowsTheRetryErrorAndRefreshesOnlyOnce() {
    validToken();
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
    verifyTokenRefreshedOnce(true);
    verify(fhirContextInitializer, times(2)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_401ThenServerError_rethrowsTheServerError() {
    validToken();
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
    verifyTokenRefreshedOnce(true);
  }

  // ------- ecr.fhir.retry.enabled=true: EcrFhirRetryClient wraps errors in RetryableException
  // -------

  @Test
  public void execute_wrapped401ThenSuccess_refreshesTokenAndRetriesWithANewClient() {
    validToken();
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
    verifyTokenRefreshedOnce(true);
    verify(fhirContextInitializer, times(2)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_wrapped401TwiceInARow_rethrowsTheRetryErrorAndRefreshesOnlyOnce() {
    validToken();
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
    verifyTokenRefreshedOnce(true);
    verify(fhirContextInitializer, times(2)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void execute_wrappedNon401ServerError_isRethrownWithoutRetry() {
    validToken();
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
    verify(authorizationUtils, never()).getToken(any());
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
    validToken();
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
    verify(authorizationUtils, never()).getToken(any());
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_two_succeedsOnTheSecondRetry() {
    validToken();
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
    verify(authorizationUtils, times(2)).getToken(healthcareSetting);
    verify(healthcareSettingsDao, times(2)).saveOrUpdate(healthcareSetting);
    verify(fhirContextInitializer, times(3)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_two_always401_refreshesTwiceThenThrowsTheLastError() {
    validToken();
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
    verify(authorizationUtils, times(2)).getToken(any());
    verify(fhirContextInitializer, times(3)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_aboveMaximum_isLimitedToThree() {
    validToken();
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

    verify(authorizationUtils, times(3)).getToken(any());
    verify(fhirContextInitializer, times(4)).createClient(any(), any(), any(), any(), any());
  }

  @Test
  public void retryOn401_negative_isTreatedAsZero() {
    validToken();
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

    verify(authorizationUtils, never()).getToken(any());
  }

  @Test
  public void retryOn401_doesNotAffectNon401Errors() {
    validToken();
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
    verify(authorizationUtils, never()).getToken(any());
    verify(fhirContextInitializer, times(1)).createClient(any(), any(), any(), any(), any());
  }

  // ---------------- helpers ----------------

  private void setMaxAttempts(int value) {
    ReflectionTestUtils.setField(executor, "retryOn401MaxAttempts", value);
  }

  private static RetryableException wrapped401() {
    return new RetryableException(new AuthenticationException("HTTP 401 Unauthorized"), 401, "GET");
  }

  private void validToken() {
    when(healthcareSetting.getEhrAccessToken()).thenReturn("valid-token");
    when(healthcareSetting.getEhrAccessTokenExpirationTime()).thenReturn(inSeconds(3600));
  }

  private void verifyTokenRefreshedOnce(boolean withProviderId) {
    verify(authorizationUtils, times(1)).getToken(healthcareSetting);
    verify(healthcareSetting).setEhrAccessToken("fresh-token");
    verify(healthcareSetting).setEhrAccessTokenExpiryDuration(3600);

    ArgumentCaptor<Date> expiration = ArgumentCaptor.forClass(Date.class);
    verify(healthcareSetting).setEhrAccessTokenExpirationTime(expiration.capture());
    long secondsLeft = (expiration.getValue().getTime() - System.currentTimeMillis()) / 1000L;
    assertTrue("expiry about one hour ahead, was " + secondsLeft, secondsLeft > 3500);

    if (withProviderId) {
      verify(healthcareSetting).setDefaultProviderId("provider-123");
    } else {
      verify(healthcareSetting, never()).setDefaultProviderId(anyString());
    }
    verify(healthcareSettingsDao, times(1)).saveOrUpdate(healthcareSetting);
  }

  private static JSONObject tokenResponse(String token, long expiresIn, String providerId) {
    JSONObject json = new JSONObject();
    json.put("access_token", token);
    json.put("expires_in", expiresIn);
    if (providerId != null) {
      json.put("uuid", providerId);
    }
    return json;
  }

  private static Date inSeconds(long seconds) {
    return Date.from(Instant.now().plusSeconds(seconds));
  }

  private String name(IGenericClient client) {
    return client == firstClient ? "first" : "retry";
  }
}
