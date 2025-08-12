package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.PublicHealthAuthorityDao;
import com.drajer.bsa.model.PublicHealthAuthority;
import java.util.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PublicHealthAuthorityServiceImplTest {
  @Mock private PublicHealthAuthorityDao phaDao;

  @InjectMocks private PublicHealthAuthorityServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this); // Initialize mocks for JUnit 4
  }

  @Test
  public void testSaveOrUpdate() {
    PublicHealthAuthority pha = new PublicHealthAuthority();
    when(phaDao.saveOrUpdate(pha)).thenReturn(pha);

    PublicHealthAuthority result = service.saveOrUpdate(pha);

    assertNotNull(result);
    assertEquals(pha, result);
    verify(phaDao, times(1)).saveOrUpdate(pha);
  }

  @Test
  public void testGetPublicHealthAuthorityById1() {
    Integer id = 1;
    PublicHealthAuthority pha = new PublicHealthAuthority();
    pha.setId(id);
    pha.setClientId("client123");

    when(phaDao.getPublicHealthAuthorityById(id)).thenReturn(pha);

    PublicHealthAuthority result = service.getPublicHealthAuthorityById(id);

    assertNotNull(result);
    assertEquals(id, result.getId());
    assertEquals("client123", result.getClientId());
    verify(phaDao, times(1)).getPublicHealthAuthorityById(id);
  }

  @Test
  public void testGetPublicHealthAuthorityByUrl() {
    PublicHealthAuthority pha = new PublicHealthAuthority();
    String url = "http://hosphealth.com";

    when(phaDao.getPublicHealthAuthorityByUrl(url)).thenReturn(pha);

    PublicHealthAuthority result = service.getPublicHealthAuthorityByUrl(url);

    assertNotNull(result);
    assertEquals(pha, result);
    verify(phaDao, times(1)).getPublicHealthAuthorityByUrl(url);
  }

  @Test
  public void testGetPublicHealthAuthorityByUrl1() {
    PublicHealthAuthority pha = new PublicHealthAuthority();
    String url = "http://hosphealth.com";
    pha.setFhirServerBaseURL(url);
    pha.setTokenUrl(url + "/token");

    when(phaDao.getPublicHealthAuthorityByUrl(url)).thenReturn(pha);

    PublicHealthAuthority result = service.getPublicHealthAuthorityByUrl(url);

    assertNotNull(result);
    assertEquals(pha, result);
    assertEquals(url, result.getFhirServerBaseURL());
    assertEquals(url + "/token", result.getTokenUrl());
    verify(phaDao, times(1)).getPublicHealthAuthorityByUrl(url);
  }

  @Test
  public void testGetAllPublicHealthAuthority() {
    List<PublicHealthAuthority> phaList = new ArrayList<>();

    PublicHealthAuthority pha1 = new PublicHealthAuthority();
    pha1.setId(1);
    pha1.setClientId("client-001");
    pha1.setClientSecret("secret1");
    pha1.setUsername("user1");
    pha1.setPassword("pass1");
    pha1.setFhirServerBaseURL("https://fhir.server1.com");
    pha1.setFhirVersion("4.0.1");
    pha1.setTokenUrl("https://fhir.server1.com/token");
    pha1.setScopes("openid profile");
    pha1.setRequireAud(true);
    pha1.setAuthType("client_credentials");
    pha1.setBackendAuthKeyAlias("alias1");
    pha1.setBackendAuthAlg("RS256");
    pha1.setBackendAuthKid("kid1");
    pha1.setLastUpdated(new Date());

    PublicHealthAuthority pha2 = new PublicHealthAuthority();
    pha2.setId(2);
    pha2.setClientId("client-002");
    pha2.setClientSecret("secret2");
    pha2.setUsername("user2");
    pha2.setPassword("pass2");
    pha2.setFhirServerBaseURL("https://fhir.server2.com");
    pha2.setFhirVersion("4.0.1");
    pha2.setTokenUrl("https://fhir.server2.com/token");
    pha2.setScopes("openid profile");
    pha2.setRequireAud(false);
    pha2.setAuthType("password");
    pha2.setBackendAuthKeyAlias("alias2");
    pha2.setBackendAuthAlg("ES256");
    pha2.setBackendAuthKid("kid2");
    pha2.setLastUpdated(new Date());

    phaList.add(pha1);
    phaList.add(pha2);

    when(phaDao.getAllPublicHealthAuthority()).thenReturn(phaList);

    List<PublicHealthAuthority> result = service.getAllPublicHealthAuthority();

    assertNotNull(result);
    assertFalse(result.isEmpty());
    assertEquals(2, result.size());

    PublicHealthAuthority resultPha1 = result.get(0);
    assertEquals("client-001", resultPha1.getClientId());
    assertEquals("secret1", resultPha1.getClientSecret());
    assertEquals("user1", resultPha1.getUsername());
    assertEquals("pass1", resultPha1.getPassword());
    assertEquals("https://fhir.server1.com", resultPha1.getFhirServerBaseURL());
    assertEquals("4.0.1", resultPha1.getFhirVersion());
    assertEquals("https://fhir.server1.com/token", resultPha1.getTokenUrl());
    assertEquals("openid profile", resultPha1.getScopes());
    assertTrue(resultPha1.getRequireAud());
    assertEquals("client_credentials", resultPha1.getAuthType());
    assertEquals("alias1", resultPha1.getBackendAuthKeyAlias());
    assertEquals("RS256", resultPha1.getBackendAuthAlg());
    assertEquals("kid1", resultPha1.getBackendAuthKid());

    PublicHealthAuthority resultPha2 = result.get(1);
    assertEquals("client-002", resultPha2.getClientId());
    assertEquals("secret2", resultPha2.getClientSecret());
    assertEquals("user2", resultPha2.getUsername());
    assertEquals("pass2", resultPha2.getPassword());
    assertEquals("https://fhir.server2.com", resultPha2.getFhirServerBaseURL());
    assertEquals("4.0.1", resultPha2.getFhirVersion());
    assertEquals("https://fhir.server2.com/token", resultPha2.getTokenUrl());
    assertEquals("openid profile", resultPha2.getScopes());
    assertFalse(resultPha2.getRequireAud());
    assertEquals("password", resultPha2.getAuthType());
    assertEquals("alias2", resultPha2.getBackendAuthKeyAlias());
    assertEquals("ES256", resultPha2.getBackendAuthAlg());
    assertEquals("kid2", resultPha2.getBackendAuthKid());

    verify(phaDao, times(1)).getAllPublicHealthAuthority();
  }
}
