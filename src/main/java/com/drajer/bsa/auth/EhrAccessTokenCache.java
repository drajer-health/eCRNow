package com.drajer.bsa.auth;

import com.drajer.bsa.model.HealthcareSetting;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.HexFormat;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * In-memory cache of EHR access tokens, one entry per HealthcareSetting FHIR server URL.
 *
 * <p>Access tokens are short-lived runtime state (SMART Backend Services tokens should not live
 * longer than five minutes, and no refresh token is issued), so they are kept in memory only and
 * are never written to the database. Each application instance keeps its own tokens; instances do
 * not share or lock anything, so the cache works the same with one pod or many.
 *
 * <ul>
 *   <li>A cached token is used without any locking for the first half of its lifetime.
 *   <li>From half its lifetime, one thread refreshes it ahead of expiry. Other threads that find
 *       the refresh in progress keep using the current token and do not wait, so a slow token
 *       endpoint delays only the thread doing the refresh.
 *   <li>A token is not handed out in its last {@code token.refresh.threshold} seconds (at most half
 *       its lifetime), so it never expires during a request.
 *   <li>Only when there is no usable token (first use, full expiry, changed settings, HTTP 401) do
 *       threads wait: at most one thread per FHIR server URL calls the token endpoint, and the
 *       others wait for that call and reuse its result.
 *   <li>After an HTTP 401, {@link #refreshAfterUnauthorized} gets a new token unless another thread
 *       has already replaced the rejected one.
 *   <li>A change to the HealthcareSetting's authorization settings (for example a new client id or
 *       token URL saved from eCRNow-UI) is detected on the next request on every instance, and a new
 *       token is requested.
 *   <li>A failed token request affects only the thread that made it. The next thread that needs
 *       the token makes its own request, so one failed call never fails other jobs.
 *   <li>If a refresh fails while the current token has not yet expired, the current token is still
 *       returned, so a short token-endpoint outage does not fail any job.
 * </ul>
 *
 * <p>Access token values are never logged.
 */
@Component
public class EhrAccessTokenCache {

  private static final Logger logger = LoggerFactory.getLogger(EhrAccessTokenCache.class);

  static final String ACCESS_TOKEN = "access_token";
  static final String EXPIRES_IN = "expires_in";

  /** Some EHRs return the default provider id with the token response. */
  static final String PROVIDER_ID = "uuid";

  /** Lifetime assumed when the token response has no usable expires_in (the SMART maximum). */
  static final long DEFAULT_EXPIRES_IN_SECONDS = 300;

  /** An EHR access token. {@link #toString()} does not include the token value. */
  public record EhrAccessToken(String accessToken, Instant expiresAt, String providerId) {

    @Override
    public String toString() {
      return "EhrAccessToken[expiresAt=" + expiresAt + ", providerId=" + providerId + "]";
    }
  }

  /**
   * @param refreshAt from here (half the lifetime) one thread refreshes the token ahead of expiry
   * @param usableUntil after this the token is no longer handed out
   */
  private record CachedToken(
      EhrAccessToken token, Instant refreshAt, Instant usableUntil, String configFingerprint) {}

  private final AuthorizationUtils authorizationUtils;
  private final Clock clock;
  private final long refreshThresholdSeconds;
  private final long lockTimeoutSeconds;

  private final ConcurrentHashMap<String, CachedToken> tokens = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, ReentrantLock> locks = new ConcurrentHashMap<>();

  @Autowired
  public EhrAccessTokenCache(
      AuthorizationUtils authorizationUtils,
      @Value("${token.refresh.threshold:60}") long refreshThresholdSeconds,
      @Value("${ecr.ehr.token.lock-timeout-seconds:45}") long lockTimeoutSeconds) {
    this(authorizationUtils, Clock.systemUTC(), refreshThresholdSeconds, lockTimeoutSeconds);
  }

  EhrAccessTokenCache(
      AuthorizationUtils authorizationUtils,
      Clock clock,
      long refreshThresholdSeconds,
      long lockTimeoutSeconds) {
    this.authorizationUtils = Objects.requireNonNull(authorizationUtils, "authorizationUtils");
    this.clock = Objects.requireNonNull(clock, "clock");
    this.refreshThresholdSeconds = Math.max(0, refreshThresholdSeconds);
    this.lockTimeoutSeconds = Math.max(1, lockTimeoutSeconds);
  }

  /**
   * Returns a valid access token for the HealthcareSetting, from the cache when possible.
   *
   * <p>If the token response carried a provider id, it is set on {@code hs} (in memory) so that
   * callers reading {@link HealthcareSetting#getDefaultProviderId()} see it.
   *
   * @throws EhrAccessTokenException if no token can be obtained.
   */
  public EhrAccessToken getToken(HealthcareSetting hs) {
    Objects.requireNonNull(hs, "HealthcareSetting");
    String key = cacheKey(hs.getFhirServerBaseURL());
    String fingerprint = configFingerprint(hs);

    CachedToken cached = tokens.get(key);
    if (isFresh(cached, fingerprint)) {
      return applyProviderId(hs, cached.token());
    }
    if (isUsable(cached, fingerprint)) {
      return applyProviderId(hs, refreshAhead(hs, key, fingerprint, cached));
    }
    return applyProviderId(hs, obtainToken(hs, key, fingerprint, null));
  }

  /**
   * Called after the EHR rejected {@code rejectedAccessToken} with HTTP 401. Returns a newer token
   * if another thread has already obtained one; otherwise requests a new token.
   *
   * @throws EhrAccessTokenException if no token can be obtained.
   */
  public EhrAccessToken refreshAfterUnauthorized(
      HealthcareSetting hs, String rejectedAccessToken) {
    Objects.requireNonNull(hs, "HealthcareSetting");
    String key = cacheKey(hs.getFhirServerBaseURL());
    String fingerprint = configFingerprint(hs);
    String rejected = rejectedAccessToken == null ? "" : rejectedAccessToken;
    return applyProviderId(hs, obtainToken(hs, key, fingerprint, rejected));
  }

  /** Removes the cached token for the FHIR server URL, if any. */
  public void evict(String fhirServerBaseUrl) {
    String key = cacheKey(fhirServerBaseUrl);
    tokens.remove(key);
  }

  /**
   * The cached token is past half its lifetime but still usable. One thread refreshes it; a thread
   * that finds a refresh already in progress keeps using the current token instead of waiting. If
   * the refresh fails, the current token is still returned and a later call tries again.
   */
  private EhrAccessToken refreshAhead(
      HealthcareSetting hs, String key, String fingerprint, CachedToken cached) {

    ReentrantLock lock = lockFor(key);
    if (!lock.tryLock()) {
      return cached.token(); // another thread is refreshing: do not wait for it
    }
    try {
      CachedToken current = tokens.get(key);
      if (isFresh(current, fingerprint)) {
        return current.token(); // refreshed just before this thread got the lock
      }
      try {
        EhrAccessToken token = requestToken(hs, key);
        tokens.put(key, newEntry(token, fingerprint));
        return token;
      } catch (EhrAccessTokenException e) {
        CachedToken usable = isUsable(current, fingerprint) ? current : cached;
        logger.warn(
            "Could not refresh the EHR access token for {} ahead of expiry; using the current"
                + " token, valid until {}. Cause: {}",
            key,
            usable.token().expiresAt(),
            String.valueOf(e.getCause() != null ? e.getCause() : e.getMessage()));
        return usable.token();
      }
    } finally {
      lock.unlock();
    }
  }

  /**
   * Requests a token from the endpoint unless another thread already has a suitable one. Only one
   * thread per key runs this at a time.
   *
   * @param rejectedToken null for a normal refresh; the token that got HTTP 401 otherwise.
   */
  private EhrAccessToken obtainToken(
      HealthcareSetting hs, String key, String fingerprint, String rejectedToken) {

    ReentrantLock lock = lockFor(key);
    acquire(lock, key);
    try {
      // Another thread may have obtained a token while this one was waiting for the lock.
      CachedToken cached = tokens.get(key);
      if (isUsable(cached, fingerprint)
          && (rejectedToken == null || !rejectedToken.equals(cached.token().accessToken()))) {
        return cached.token();
      }

      EhrAccessToken token;
      try {
        token = requestToken(hs, key);
      } catch (EhrAccessTokenException e) {
        // Only this thread's request failed: nothing is recorded, so the next thread that needs a
        // token makes its own request. During the early-refresh window the current token is
        // still valid, so keep using it instead of failing the job.
        if (rejectedToken == null
            && cached != null
            && cached.configFingerprint().equals(fingerprint)
            && now().isBefore(cached.token().expiresAt())) {
          logger.warn(
              "Could not refresh the EHR access token for {}; using the current token, valid until"
                  + " {}. Cause: {}",
              key,
              cached.token().expiresAt(),
              String.valueOf(e.getCause() != null ? e.getCause() : e.getMessage()));
          return cached.token();
        }
        throw e;
      }

      tokens.put(key, newEntry(token, fingerprint));
      return token;

    } finally {
      lock.unlock();
    }
  }

  private EhrAccessToken requestToken(HealthcareSetting hs, String key) {
    long started = System.nanoTime();
    JSONObject response;
    try {
      response = authorizationUtils.getToken(hs);
    } catch (RuntimeException e) {
      throw new EhrAccessTokenException("EHR access token request failed for " + key, e);
    }

    if (response == null) {
      throw new EhrAccessTokenException("EHR token endpoint returned no response for " + key);
    }

    // An empty access token is valid: AuthorizationUtils returns one for servers without auth.
    String accessToken = response.optString(ACCESS_TOKEN, null);
    if (accessToken == null) {
      throw new EhrAccessTokenException(
          "EHR token response for " + key + " does not contain " + ACCESS_TOKEN);
    }

    long expiresIn = response.optLong(EXPIRES_IN, -1);
    if (expiresIn <= 0) {
      logger.warn(
          "EHR token response for {} has no valid {}; assuming {} seconds",
          key,
          EXPIRES_IN,
          DEFAULT_EXPIRES_IN_SECONDS);
      expiresIn = DEFAULT_EXPIRES_IN_SECONDS;
    }

    String providerId = response.optString(PROVIDER_ID, null);
    Instant expiresAt = now().plusSeconds(expiresIn);

    logger.info(
        "Obtained EHR access token for {} (expires in {} s, at {}; request took {} ms)",
        key,
        expiresIn,
        expiresAt,
        TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - started));

    return new EhrAccessToken(accessToken, expiresAt, providerId);
  }

  /**
   * A refresh starts at half the token's lifetime, as MSAL does, so that even a slow token endpoint
   * has plenty of time. The token stops being handed out {@code token.refresh.threshold} seconds
   * before expiry (at most half its lifetime, so a short-lived token is not requested on every
   * call).
   */
  private CachedToken newEntry(EhrAccessToken token, String fingerprint) {
    long lifetimeSeconds = Math.max(0, Duration.between(now(), token.expiresAt()).getSeconds());
    long half = lifetimeSeconds / 2;
    Instant refreshAt = token.expiresAt().minusSeconds(lifetimeSeconds - half);
    Instant usableUntil = token.expiresAt().minusSeconds(Math.min(refreshThresholdSeconds, half));
    return new CachedToken(token, refreshAt, usableUntil, fingerprint);
  }

  /** Within the first half of its lifetime: used without any locking. */
  private boolean isFresh(CachedToken cached, String fingerprint) {
    return cached != null
        && cached.configFingerprint().equals(fingerprint)
        && now().isBefore(cached.refreshAt());
  }

  /** Still safe to hand out (not in its last {@code token.refresh.threshold} seconds). */
  private boolean isUsable(CachedToken cached, String fingerprint) {
    return cached != null
        && cached.configFingerprint().equals(fingerprint)
        && now().isBefore(cached.usableUntil());
  }

  private ReentrantLock lockFor(String key) {
    return locks.computeIfAbsent(key, k -> new ReentrantLock());
  }

  private void acquire(ReentrantLock lock, String key) {
    try {
      if (!lock.tryLock(lockTimeoutSeconds, TimeUnit.SECONDS)) {
        throw new EhrAccessTokenException(
            "Timed out after "
                + lockTimeoutSeconds
                + " s waiting for another thread to obtain the EHR access token for "
                + key);
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new EhrAccessTokenException(
          "Interrupted while waiting for the EHR access token for " + key, e);
    }
  }

  private static EhrAccessToken applyProviderId(HealthcareSetting hs, EhrAccessToken token) {
    String providerId = token.providerId();
    if (providerId != null && !providerId.equals(hs.getDefaultProviderId())) {
      hs.setDefaultProviderId(providerId);
    }
    return token;
  }

  private Instant now() {
    return clock.instant();
  }

  /**
   * The cache key for a FHIR server URL: scheme and host in lower case, trailing slashes removed.
   * The path is kept as is, because FHIR server paths can be case-sensitive.
   */
  static String cacheKey(String fhirServerBaseUrl) {
    if (fhirServerBaseUrl == null || fhirServerBaseUrl.isBlank()) {
      throw new EhrAccessTokenException("HealthcareSetting has no FHIR server URL");
    }
    String url = fhirServerBaseUrl.trim();
    while (url.endsWith("/")) {
      url = url.substring(0, url.length() - 1);
    }
    try {
      URI uri = URI.create(url);
      if (uri.getScheme() != null && uri.getRawAuthority() != null) {
        return uri.getScheme().toLowerCase(Locale.ROOT)
            + "://"
            + uri.getRawAuthority().toLowerCase(Locale.ROOT)
            + (uri.getRawPath() == null ? "" : uri.getRawPath())
            + (uri.getRawQuery() == null ? "" : "?" + uri.getRawQuery());
      }
    } catch (IllegalArgumentException e) {
      // Not a parseable URI; use the trimmed value as is.
    }
    return url;
  }

  /**
   * A hash of the settings that determine which token the endpoint issues. If any of them changes,
   * the cached token is no longer used.
   */
  static String configFingerprint(HealthcareSetting hs) {
    String material =
        String.join(
            "\u0000",
            String.valueOf(hs.getFhirServerBaseURL()),
            String.valueOf(hs.getAuthType()),
            String.valueOf(hs.getTokenUrl()),
            String.valueOf(hs.getClientId()),
            String.valueOf(hs.getClientSecret()),
            String.valueOf(hs.getScopes()),
            String.valueOf(hs.getUsername()),
            String.valueOf(hs.getPassword()),
            String.valueOf(hs.getBackendAuthKeyAlias()),
            String.valueOf(hs.getBackendAuthAlg()),
            String.valueOf(hs.getBackendAuthKid()),
            String.valueOf(hs.getRequireAud()));
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      return HexFormat.of().formatHex(digest.digest(material.getBytes(StandardCharsets.UTF_8)));
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("SHA-256 is not available", e);
    }
  }
}
