package com.drajer.bsa.auth;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.bsa.auth.EhrAccessTokenCache.EhrAccessToken;
import com.drajer.bsa.model.HealthcareSetting;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

public class EhrAccessTokenCacheTest {

  private static final String URL = "https://ehr.example.com/fhir";
  private static final long THRESHOLD = 60;

  /** A clock the tests move forward explicitly. */
  private static final class MutableClock extends Clock {
    private volatile Instant now = Instant.parse("2026-01-01T00:00:00Z");

    void advanceSeconds(long seconds) {
      now = now.plusSeconds(seconds);
    }

    @Override
    public ZoneId getZone() {
      return ZoneOffset.UTC;
    }

    @Override
    public Clock withZone(ZoneId zone) {
      return this;
    }

    @Override
    public Instant instant() {
      return now;
    }
  }

  private AuthorizationUtils authUtils;
  private MutableClock clock;
  private EhrAccessTokenCache cache;

  @Before
  public void setUp() {
    authUtils = mock(AuthorizationUtils.class);
    clock = new MutableClock();
    cache = new EhrAccessTokenCache(authUtils, clock, THRESHOLD, 5);
  }

  private static HealthcareSetting hs(String url) {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setFhirServerBaseURL(url);
    hs.setClientId("client-1");
    hs.setTokenUrl("https://ehr.example.com/token");
    hs.setAuthType("SofSystem");
    return hs;
  }

  private static JSONObject tokenResponse(String token, long expiresIn) {
    return new JSONObject().put("access_token", token).put("expires_in", expiresIn);
  }

  // ==================== reuse and expiry ====================

  @Test
  public void firstCall_requestsToken_laterCallsReuseIt() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600));
    HealthcareSetting hs = hs(URL);

    EhrAccessToken first = cache.getToken(hs);
    clock.advanceSeconds(1700); // still in the first half of its lifetime
    EhrAccessToken second = cache.getToken(hs);

    assertEquals("t1", first.accessToken());
    assertSame(first, second);
    verify(authUtils, times(1)).getToken(any());
  }

  @Test
  public void tokenWithinRefreshThreshold_isReplaced() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(tokenResponse("t2", 3600));
    HealthcareSetting hs = hs(URL);

    cache.getToken(hs);
    clock.advanceSeconds(3600 - THRESHOLD + 1);

    assertEquals("t2", cache.getToken(hs).accessToken());
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void refreshStartsAtHalfLifetime_callingThreadGetsTheNewToken() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(tokenResponse("t2", 3600));
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);

    clock.advanceSeconds(1799);
    assertEquals("t1", cache.getToken(hs).accessToken());
    verify(authUtils, times(1)).getToken(any());

    clock.advanceSeconds(2);
    assertEquals("t2", cache.getToken(hs).accessToken());
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void refreshAheadFails_currentTokenIsReturned_nextCallTriesAgain() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(null)
        .thenReturn(tokenResponse("t2", 3600));
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);
    clock.advanceSeconds(2000); // past half its lifetime, far from expiry

    assertEquals("t1", cache.getToken(hs).accessToken()); // no exception, no failed job
    assertEquals("t2", cache.getToken(hs).accessToken());
    verify(authUtils, times(3)).getToken(any());
  }

  @Test
  public void shortLivedToken_isReusedForHalfItsLifetime_notRequestedOnEveryCall() {
    // 40 s lifetime with a 60 s threshold: the buffer is capped at half the lifetime (20 s).
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 40))
        .thenReturn(tokenResponse("t2", 40));
    HealthcareSetting hs = hs(URL);

    cache.getToken(hs);
    clock.advanceSeconds(19);
    assertEquals("t1", cache.getToken(hs).accessToken());

    clock.advanceSeconds(2);
    assertEquals("t2", cache.getToken(hs).accessToken());
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void missingExpiresIn_assumesFiveMinutes() {
    when(authUtils.getToken(any())).thenReturn(new JSONObject().put("access_token", "t1"));

    EhrAccessToken token = cache.getToken(hs(URL));

    assertEquals(clock.instant().plusSeconds(300), token.expiresAt());
  }

  @Test
  public void expiresInAsString_isAccepted() {
    when(authUtils.getToken(any()))
        .thenReturn(new JSONObject().put("access_token", "t1").put("expires_in", "120"));

    assertEquals(clock.instant().plusSeconds(120), cache.getToken(hs(URL)).expiresAt());
  }

  @Test
  public void emptyAccessToken_isAccepted_forServersWithoutAuthorization() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("", 86400));
    HealthcareSetting hs = hs(URL);

    assertEquals("", cache.getToken(hs).accessToken());
    assertEquals("", cache.getToken(hs).accessToken());
    verify(authUtils, times(1)).getToken(any());
  }

  // ==================== keys ====================

  @Test
  public void urlsDifferingOnlyInHostCaseOrTrailingSlash_shareOneToken() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600));

    cache.getToken(hs("https://EHR.Example.com/fhir/"));
    cache.getToken(hs("https://ehr.example.com/fhir"));

    verify(authUtils, times(1)).getToken(any());
  }

  @Test
  public void differentServers_getIndependentTokens() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("a", 3600))
        .thenReturn(tokenResponse("b", 3600));

    assertEquals("a", cache.getToken(hs("https://ehr-a.example.com/fhir")).accessToken());
    assertEquals("b", cache.getToken(hs("https://ehr-b.example.com/fhir")).accessToken());
    assertEquals("a", cache.getToken(hs("https://ehr-a.example.com/fhir")).accessToken());
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void cacheKey_normalizesSchemeHostAndTrailingSlash_keepsPathCase() {
    assertEquals(
        "https://ehr.example.com/fhir/R4",
        EhrAccessTokenCache.cacheKey(" HTTPS://EHR.Example.COM/fhir/R4// "));
    assertNotEquals(
        EhrAccessTokenCache.cacheKey("https://ehr.example.com/FHIR"),
        EhrAccessTokenCache.cacheKey("https://ehr.example.com/fhir"));
  }

  @Test
  public void missingFhirServerUrl_throws() {
    assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs(null)));
    assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs("  ")));
    verify(authUtils, never()).getToken(any());
  }

  // ==================== configuration changes ====================

  @Test
  public void changedAuthorizationSettings_requestNewToken() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(tokenResponse("t2", 3600));
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);

    HealthcareSetting updated = hs(URL);
    updated.setClientId("client-2");

    assertEquals("t2", cache.getToken(updated).accessToken());
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void unrelatedSettingChange_keepsToken() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600));
    cache.getToken(hs(URL));

    HealthcareSetting updated = hs(URL);
    updated.setDefaultProviderId("someone-else");

    assertEquals("t1", cache.getToken(updated).accessToken());
    verify(authUtils, times(1)).getToken(any());
  }

  // ==================== HTTP 401 ====================

  @Test
  public void refreshAfterUnauthorized_rejectedTokenIsCurrent_requestsNewToken() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(tokenResponse("t2", 3600));
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);

    assertEquals("t2", cache.refreshAfterUnauthorized(hs, "t1").accessToken());
    assertEquals("t2", cache.getToken(hs).accessToken());
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void refreshAfterUnauthorized_tokenAlreadyReplaced_returnsItWithoutRequest() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(tokenResponse("t2", 3600));
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);
    cache.refreshAfterUnauthorized(hs, "t1"); // first thread to see the 401 gets t2

    // A second thread that was also rejected with t1 reuses t2.
    assertEquals("t2", cache.refreshAfterUnauthorized(hs, "t1").accessToken());
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void refreshAfterUnauthorized_withEmptyCache_requestsToken() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600));

    assertEquals("t1", cache.refreshAfterUnauthorized(hs(URL), null).accessToken());
    verify(authUtils, times(1)).getToken(any());
  }

  // ==================== failures ====================

  @Test
  public void nullTokenResponse_throws_andIsNotCached() {
    when(authUtils.getToken(any())).thenReturn(null);

    assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs(URL)));
  }

  @Test
  public void responseWithoutAccessToken_throws() {
    when(authUtils.getToken(any())).thenReturn(new JSONObject().put("expires_in", 3600));

    assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs(URL)));
  }

  @Test
  public void tokenEndpointException_isWrapped() {
    RuntimeException cause = new RuntimeException("connection refused");
    when(authUtils.getToken(any())).thenThrow(cause);

    EhrAccessTokenException e =
        assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs(URL)));
    assertSame(cause, e.getCause());
  }

  @Test
  public void afterFailure_nextCallMakesItsOwnRequest() {
    when(authUtils.getToken(any())).thenReturn(null).thenReturn(tokenResponse("t1", 3600));
    HealthcareSetting hs = hs(URL);

    assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs));
    assertEquals("t1", cache.getToken(hs).accessToken()); // no waiting period, no shared failure
    verify(authUtils, times(2)).getToken(any());
  }

  @Test
  public void refreshFailsWhileCurrentTokenStillValid_currentTokenIsUsed() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(null) // the early refresh fails
        .thenReturn(tokenResponse("t2", 3600));
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);

    clock.advanceSeconds(3600 - THRESHOLD + 1); // inside the early-refresh window, not expired
    assertEquals("t1", cache.getToken(hs).accessToken());

    // The next call tries the refresh again, and it now works.
    assertEquals("t2", cache.getToken(hs).accessToken());
    verify(authUtils, times(3)).getToken(any());
  }

  @Test
  public void refreshFailsAfterCurrentTokenExpired_throws() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600)).thenReturn(null);
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);

    clock.advanceSeconds(3601);

    assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs));
  }

  @Test
  public void refreshFailsAfter401_rejectedTokenIsNotReturned() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600)).thenReturn(null);
    HealthcareSetting hs = hs(URL);
    cache.getToken(hs);

    assertThrows(EhrAccessTokenException.class, () -> cache.refreshAfterUnauthorized(hs, "t1"));
  }

  @Test
  public void refreshFailsWithChangedSettings_oldSettingsTokenIsNotReturned() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600)).thenReturn(null);
    cache.getToken(hs(URL));

    HealthcareSetting changed = hs(URL);
    changed.setClientId("client-2");

    assertThrows(EhrAccessTokenException.class, () -> cache.getToken(changed));
  }

  @Test
  public void evict_forcesNewRequest() {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenReturn(tokenResponse("t2", 3600));
    cache.getToken(hs(URL));

    cache.evict(URL + "/");

    assertEquals("t2", cache.getToken(hs(URL)).accessToken());
  }

  // ==================== HealthcareSetting is not used as token storage ====================

  @Test
  public void tokenIsNeverStoredOnTheHealthcareSetting() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600));
    HealthcareSetting hs = hs(URL);

    cache.getToken(hs);

    assertNull(hs.getEhrAccessToken());
    assertNull(hs.getEhrAccessTokenExpirationTime());
  }

  @Test
  public void providerIdFromTokenResponse_isSetOnEveryHealthcareSettingInstance() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600).put("uuid", "prov-1"));

    HealthcareSetting first = hs(URL);
    cache.getToken(first);
    HealthcareSetting loadedLater = hs(URL); // e.g. a later job loading the row from the database
    cache.getToken(loadedLater);

    assertEquals("prov-1", first.getDefaultProviderId());
    assertEquals("prov-1", loadedLater.getDefaultProviderId());
    verify(authUtils, times(1)).getToken(any());
  }

  @Test
  public void withoutProviderIdInResponse_configuredProviderIdIsKept() {
    when(authUtils.getToken(any())).thenReturn(tokenResponse("t1", 3600));
    HealthcareSetting hs = hs(URL);
    hs.setDefaultProviderId("configured");

    cache.getToken(hs);

    assertEquals("configured", hs.getDefaultProviderId());
  }

  @Test
  public void toString_doesNotRevealTheToken() {
    EhrAccessToken token = new EhrAccessToken("secret-token", Instant.EPOCH, null);

    assertFalse(token.toString().contains("secret-token"));
  }

  // ==================== concurrency ====================

  @Test
  public void concurrentRequests_forOneServer_callTheTokenEndpointOnce() throws Exception {
    AtomicInteger calls = new AtomicInteger();
    when(authUtils.getToken(any()))
        .thenAnswer(
            inv -> {
              calls.incrementAndGet();
              Thread.sleep(200); // slow token endpoint, so all threads pile up behind the lock
              return tokenResponse("t1", 3600);
            });

    int threads = 20;
    ExecutorService pool = Executors.newFixedThreadPool(threads);
    CountDownLatch start = new CountDownLatch(1);
    Set<String> tokensSeen = ConcurrentHashMap.newKeySet();
    List<Future<?>> results = new ArrayList<>();
    try {
      for (int i = 0; i < threads; i++) {
        results.add(
            pool.submit(
                () -> {
                  start.await();
                  tokensSeen.add(cache.getToken(hs(URL)).accessToken());
                  return null;
                }));
      }
      start.countDown();
      for (Future<?> result : results) {
        result.get(10, TimeUnit.SECONDS);
      }
    } finally {
      pool.shutdownNow();
    }

    assertEquals(1, calls.get());
    assertEquals(Set.of("t1"), tokensSeen);
  }

  @Test
  public void concurrent401s_withTheSameRejectedToken_requestOneNewToken() throws Exception {
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenAnswer(
            inv -> {
              Thread.sleep(200);
              return tokenResponse("t2", 3600);
            });
    cache.getToken(hs(URL));

    int threads = 10;
    ExecutorService pool = Executors.newFixedThreadPool(threads);
    CountDownLatch start = new CountDownLatch(1);
    Set<String> tokensSeen = ConcurrentHashMap.newKeySet();
    List<Future<?>> results = new ArrayList<>();
    try {
      for (int i = 0; i < threads; i++) {
        results.add(
            pool.submit(
                () -> {
                  start.await();
                  tokensSeen.add(cache.refreshAfterUnauthorized(hs(URL), "t1").accessToken());
                  return null;
                }));
      }
      start.countDown();
      for (Future<?> result : results) {
        result.get(10, TimeUnit.SECONDS);
      }
    } finally {
      pool.shutdownNow();
    }

    assertEquals(Set.of("t2"), tokensSeen);
    verify(authUtils, times(2)).getToken(any()); // t1 once, t2 once
  }

  @Test
  public void firstTokenCallFails_onlyThatThreadFails_othersGetTheirOwnToken() throws Exception {
    AtomicInteger calls = new AtomicInteger();
    when(authUtils.getToken(any()))
        .thenAnswer(
            inv -> {
              Thread.sleep(200);
              return calls.incrementAndGet() == 1 ? null : tokenResponse("t2", 3600);
            });

    int threads = 10;
    ExecutorService pool = Executors.newFixedThreadPool(threads);
    CountDownLatch start = new CountDownLatch(1);
    List<Future<String>> results = new ArrayList<>();
    try {
      for (int i = 0; i < threads; i++) {
        results.add(
            pool.submit(
                () -> {
                  start.await();
                  return cache.getToken(hs(URL)).accessToken();
                }));
      }
      start.countDown();
      int failed = 0;
      Set<String> tokensSeen = ConcurrentHashMap.newKeySet();
      for (Future<String> result : results) {
        try {
          tokensSeen.add(result.get(10, TimeUnit.SECONDS));
        } catch (java.util.concurrent.ExecutionException e) {
          assertTrue(e.getCause() instanceof EhrAccessTokenException);
          failed++;
        }
      }

      assertEquals("only the thread whose call failed", 1, failed);
      assertEquals(Set.of("t2"), tokensSeen);
      assertEquals("the failed call, then one successful call shared by the rest", 2, calls.get());
    } finally {
      pool.shutdownNow();
    }
  }

  @Test
  public void slowRefreshAhead_otherThreadsKeepUsingCurrentToken_withoutWaiting() throws Exception {
    CountDownLatch endpointEntered = new CountDownLatch(1);
    CountDownLatch releaseEndpoint = new CountDownLatch(1);
    when(authUtils.getToken(any()))
        .thenReturn(tokenResponse("t1", 3600))
        .thenAnswer(
            inv -> {
              endpointEntered.countDown();
              releaseEndpoint.await(10, TimeUnit.SECONDS); // a token endpoint taking very long
              return tokenResponse("t2", 3600);
            });
    cache.getToken(hs(URL));
    clock.advanceSeconds(2000); // past half its lifetime: the next call starts a refresh

    ExecutorService pool = Executors.newSingleThreadExecutor();
    try {
      Future<EhrAccessToken> refreshing = pool.submit(() -> cache.getToken(hs(URL)));
      assertTrue(endpointEntered.await(5, TimeUnit.SECONDS));

      // While that refresh is still running, other threads get the current token immediately.
      long started = System.nanoTime();
      for (int i = 0; i < 10; i++) {
        assertEquals("t1", cache.getToken(hs(URL)).accessToken());
      }
      long waitedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - started);
      assertTrue("waited " + waitedMillis + " ms", waitedMillis < 1000);

      releaseEndpoint.countDown();
      assertEquals("t2", refreshing.get(10, TimeUnit.SECONDS).accessToken());
      assertEquals("t2", cache.getToken(hs(URL)).accessToken());
      verify(authUtils, times(2)).getToken(any());
    } finally {
      releaseEndpoint.countDown();
      pool.shutdownNow();
    }
  }

  @Test
  public void waitingLongerThanLockTimeout_failsInsteadOfBlockingForever() throws Exception {
    cache = new EhrAccessTokenCache(authUtils, clock, THRESHOLD, 1);
    CountDownLatch endpointEntered = new CountDownLatch(1);
    CountDownLatch releaseEndpoint = new CountDownLatch(1);
    when(authUtils.getToken(any()))
        .thenAnswer(
            inv -> {
              endpointEntered.countDown();
              releaseEndpoint.await(10, TimeUnit.SECONDS); // a hung token endpoint
              return tokenResponse("t1", 3600);
            });

    ExecutorService pool = Executors.newSingleThreadExecutor();
    try {
      Future<EhrAccessToken> slow = pool.submit(() -> cache.getToken(hs(URL)));
      assertTrue(endpointEntered.await(5, TimeUnit.SECONDS));

      long started = System.nanoTime();
      EhrAccessTokenException e =
          assertThrows(EhrAccessTokenException.class, () -> cache.getToken(hs(URL)));
      long waitedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - started);

      assertTrue(e.getMessage().contains("Timed out"));
      assertTrue("waited " + waitedMillis + " ms", waitedMillis >= 900 && waitedMillis < 5000);

      releaseEndpoint.countDown();
      assertEquals("t1", slow.get(10, TimeUnit.SECONDS).accessToken());
    } finally {
      releaseEndpoint.countDown();
      pool.shutdownNow();
    }
  }
}
