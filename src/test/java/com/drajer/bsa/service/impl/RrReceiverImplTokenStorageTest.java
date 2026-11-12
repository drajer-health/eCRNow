package com.drajer.bsa.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.bsa.auth.EhrAccessTokenCache;
import com.drajer.bsa.auth.EhrAccessTokenCache.EhrAccessToken;
import com.drajer.bsa.auth.EhrTokenStorageMode;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.cda.parser.CdaRrModel;
import com.drajer.sof.utils.FhirContextInitializer;
import java.time.Instant;
import org.hl7.fhir.instance.model.api.IIdType;
import org.hl7.fhir.r4.model.DocumentReference;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ResponseStatusException;

/** Posting the RR to the EHR in each EHR access token storage mode (ecr.ehr.token.store). */
public class RrReceiverImplTokenStorageTest {

  private RrReceiverImpl rrReceiver;
  private EhrQueryService ehrService;
  private EhrAccessTokenCache cache;
  private HealthcareSettingsDao hsDao;
  private FhirContextInitializer fhirContextInitializer;
  private HealthcareSetting hs;
  private PublicHealthMessage phm;
  private DocumentReference docRef;

  @Before
  public void setUp() {
    ehrService = mock(EhrQueryService.class);
    cache = mock(EhrAccessTokenCache.class);
    hsDao = mock(HealthcareSettingsDao.class);
    fhirContextInitializer = mock(FhirContextInitializer.class);

    rrReceiver = new RrReceiverImpl();
    ReflectionTestUtils.setField(rrReceiver, "ehrService", ehrService);
    ReflectionTestUtils.setField(rrReceiver, "ehrAccessTokenCache", cache);
    ReflectionTestUtils.setField(rrReceiver, "hsDao", hsDao);
    ReflectionTestUtils.setField(rrReceiver, "ncDao", mock(NotificationContextDao.class));
    ReflectionTestUtils.setField(rrReceiver, "fhirContextInitializer", fhirContextInitializer);

    hs = new HealthcareSetting();
    hs.setFhirServerBaseURL("http://fhirserver.com");
    phm = new PublicHealthMessage();
    phm.setxRequestId("req-1");
    docRef = new DocumentReference();

    MethodOutcome created = new MethodOutcome();
    created.setCreated(true);
    IIdType id = mock(IIdType.class);
    when(id.getIdPart()).thenReturn("doc-1");
    created.setId(id);
    when(fhirContextInitializer.getFhirContext(anyString())).thenReturn(mock(FhirContext.class));
    when(fhirContextInitializer.createClient(any(), any(), any(), any(), any()))
        .thenReturn(mock(IGenericClient.class));
    when(fhirContextInitializer.submitResource(any(), any())).thenReturn(created);
    when(fhirContextInitializer.submitResourceRethrowingUnauthorized(any(), any()))
        .thenReturn(created);
  }

  @Test
  public void database_tokenFromTheEhrForThisRr_cacheNotUsed() {
    ReflectionTestUtils.setField(rrReceiver, "tokenStorage", EhrTokenStorageMode.database());
    when(ehrService.getAuthorizationToken(hs))
        .thenReturn(new JSONObject().put("access_token", "rr-token"));

    rrReceiver.postDocRefToEhr(docRef, phm, hs, new CdaRrModel());

    verify(fhirContextInitializer)
        .createClient(any(), eq("http://fhirserver.com"), eq("rr-token"), eq("req-1"), any());
    verify(fhirContextInitializer).submitResource(any(), eq(docRef));
    verifyNoInteractions(cache);
    assertEquals("doc-1", phm.getResponseEhrDocRefId());
  }

  @Test
  public void database_noTokenResponse_isUnauthorized() {
    ReflectionTestUtils.setField(rrReceiver, "tokenStorage", EhrTokenStorageMode.database());
    when(ehrService.getAuthorizationToken(hs)).thenReturn(null);

    ResponseStatusException e =
        assertThrows(
            ResponseStatusException.class,
            () -> rrReceiver.postDocRefToEhr(docRef, phm, hs, new CdaRrModel()));

    assertEquals(401, e.getStatusCode().value());
    verifyNoInteractions(cache);
  }

  @Test
  public void memory_tokenFromTheCache_ehrTokenEndpointNotCalledDirectly() {
    ReflectionTestUtils.setField(rrReceiver, "tokenStorage", EhrTokenStorageMode.memory());
    when(cache.getToken(hs))
        .thenReturn(new EhrAccessToken("cached", Instant.now().plusSeconds(300), null));

    rrReceiver.postDocRefToEhr(docRef, phm, hs, new CdaRrModel());

    verify(fhirContextInitializer)
        .createClient(any(), eq("http://fhirserver.com"), eq("cached"), eq("req-1"), any());
    verify(ehrService, never()).getAuthorizationToken(any());
    verify(hsDao, never()).saveOrUpdate(any());
  }
}
