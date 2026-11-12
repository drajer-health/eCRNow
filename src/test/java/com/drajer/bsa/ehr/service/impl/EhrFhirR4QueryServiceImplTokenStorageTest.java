package com.drajer.bsa.ehr.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.auth.EhrAccessTokenCache;
import com.drajer.bsa.auth.EhrAccessTokenCache.EhrAccessToken;
import com.drajer.bsa.auth.EhrTokenStorageMode;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.sof.utils.FhirContextInitializer;
import java.time.Instant;
import java.util.Date;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

/** getClient in each EHR access token storage mode (ecr.ehr.token.store). */
public class EhrFhirR4QueryServiceImplTokenStorageTest {

  private static final String URL = "https://ehr.example.com/fhir";

  private EhrFhirR4QueryServiceImpl service;
  private AuthorizationUtils authUtils;
  private HealthcareSettingsDao hsDao;
  private EhrAccessTokenCache cache;
  private FhirContextInitializer fhirContextInitializer;
  private FhirContext context;
  private IGenericClient client;
  private HealthcareSetting hs;
  private KarProcessingData kd;

  @Before
  public void setUp() {
    authUtils = mock(AuthorizationUtils.class);
    hsDao = mock(HealthcareSettingsDao.class);
    cache = mock(EhrAccessTokenCache.class);
    fhirContextInitializer = mock(FhirContextInitializer.class);
    context = mock(FhirContext.class);
    client = mock(IGenericClient.class);
    when(fhirContextInitializer.createClient(any(), any(), any(), any(), any())).thenReturn(client);

    service = new EhrFhirR4QueryServiceImpl();
    ReflectionTestUtils.setField(service, "authUtils", authUtils);
    ReflectionTestUtils.setField(service, "hsDao", hsDao);
    ReflectionTestUtils.setField(service, "ehrAccessTokenCache", cache);
    ReflectionTestUtils.setField(service, "fhirContextInitializer", fhirContextInitializer);

    hs = new HealthcareSetting();
    hs.setFhirServerBaseURL(URL);
    kd = new KarProcessingData();
    kd.setHealthcareSetting(hs);
    kd.setNotificationContext(new NotificationContext());
    kd.setTokenRefreshThreshold(20);
  }

  private void mode(EhrTokenStorageMode mode) {
    ReflectionTestUtils.setField(service, "tokenStorage", mode);
  }

  // ==================== memory ====================

  @Test
  public void memory_tokenComesFromTheCache_andNothingIsReadOrSavedOnTheSetting() {
    mode(EhrTokenStorageMode.memory());
    when(cache.getToken(hs))
        .thenReturn(new EhrAccessToken("cached", Instant.now().plusSeconds(300), null));

    assertSame(client, service.getClient(kd, context));

    verify(fhirContextInitializer).createClient(eq(context), eq(URL), eq("cached"), any(), any());
    verifyNoInteractions(authUtils);
    verify(hsDao, never()).saveOrUpdate(any());
    assertEquals("the token is never put on the HealthcareSetting", null, hs.getEhrAccessToken());
  }

  // ==================== database (original implementation) ====================

  @Test
  public void database_validTokenOnTheSetting_isReused_cacheNotUsed() {
    mode(EhrTokenStorageMode.database());
    hs.setEhrAccessToken("stored");
    hs.setEhrAccessTokenExpirationTime(Date.from(Instant.now().plusSeconds(600)));

    service.getClient(kd, context);

    verify(fhirContextInitializer).createClient(eq(context), eq(URL), eq("stored"), any(), any());
    verifyNoInteractions(cache, authUtils);
    verify(hsDao, never()).saveOrUpdate(any());
  }

  @Test
  public void database_expiredToken_newTokenSetOnTheSetting_andSavedWithSaveOrUpdate() {
    mode(EhrTokenStorageMode.database());
    hs.setEhrAccessToken("old");
    hs.setEhrAccessTokenExpirationTime(Date.from(Instant.now().minusSeconds(5)));
    when(authUtils.getToken(hs))
        .thenReturn(
            new JSONObject().put("access_token", "fresh").put("expires_in", 300).put("uuid", "p1"));

    service.getClient(kd, context);

    assertEquals("fresh", hs.getEhrAccessToken());
    assertEquals(300, hs.getEhrAccessTokenExpiryDuration());
    assertEquals("p1", hs.getDefaultProviderId());
    verify(hsDao).saveOrUpdate(hs);
    verify(fhirContextInitializer).createClient(eq(context), eq(URL), eq("fresh"), any(), any());
    verifyNoInteractions(cache);
  }

  @Test
  public void database_metricsCountReuseAndRequest_withoutChangingBehaviour() {
    mode(EhrTokenStorageMode.database());
    com.drajer.bsa.auth.EhrTokenMetrics metrics =
        new com.drajer.bsa.auth.EhrTokenMetrics(true, 0, false);
    ReflectionTestUtils.setField(service, "tokenMetrics", metrics);
    hs.setEhrAccessTokenExpirationTime(Date.from(Instant.now().minusSeconds(5)));
    when(authUtils.getToken(hs))
        .thenReturn(new JSONObject().put("access_token", "fresh").put("expires_in", 300));

    service.getClient(kd, context); // expired: requested and saved
    service.getClient(kd, context); // now valid: reused

    assertEquals(1, metrics.count(com.drajer.bsa.auth.EhrTokenMetrics.Outcome.DATABASE_REQUESTED));
    assertEquals(1, metrics.count(com.drajer.bsa.auth.EhrTokenMetrics.Outcome.DATABASE_REUSED));
    verify(hsDao).saveOrUpdate(hs);
    verifyNoInteractions(cache);
  }
}
