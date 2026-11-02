package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.dao.PublicHealthAuthorityDao;
import com.drajer.bsa.model.PublicHealthAuthority;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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

    assertPublicHealthAuthorityFields(
        result.get(0),
        "client-001",
        "secret1",
        "user1",
        "pass1",
        "https://fhir.server1.com",
        "https://fhir.server1.com/token",
        true,
        "client_credentials",
        "alias1",
        "RS256",
        "kid1");

    assertPublicHealthAuthorityFields(
        result.get(1),
        "client-002",
        "secret2",
        "user2",
        "pass2",
        "https://fhir.server2.com",
        "https://fhir.server2.com/token",
        false,
        "password",
        "alias2",
        "ES256",
        "kid2");

    verify(phaDao, times(1)).getAllPublicHealthAuthority();
  }

  private void assertPublicHealthAuthorityFields(
      PublicHealthAuthority resultPha,
      String clientId,
      String clientSecret,
      String username,
      String password,
      String fhirServerBaseURL,
      String tokenUrl,
      boolean requireAud,
      String authType,
      String backendAuthKeyAlias,
      String backendAuthAlg,
      String backendAuthKid) {
    assertEquals(clientId, resultPha.getClientId());
    assertEquals(clientSecret, resultPha.getClientSecret());
    assertEquals(username, resultPha.getUsername());
    assertEquals(password, resultPha.getPassword());
    assertEquals(fhirServerBaseURL, resultPha.getFhirServerBaseURL());
    assertEquals("4.0.1", resultPha.getFhirVersion());
    assertEquals(tokenUrl, resultPha.getTokenUrl());
    assertEquals("openid profile", resultPha.getScopes());
    assertEquals(requireAud, resultPha.getRequireAud());
    assertEquals(authType, resultPha.getAuthType());
    assertEquals(backendAuthKeyAlias, resultPha.getBackendAuthKeyAlias());
    assertEquals(backendAuthAlg, resultPha.getBackendAuthAlg());
    assertEquals(backendAuthKid, resultPha.getBackendAuthKid());
  }
}
