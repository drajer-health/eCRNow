package com.drajer.bsa.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.HealthcareSetting;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;

public class HealthcareSettingsDaoImplTest {

  @Mock private SessionFactory sessionFactory;
  @Mock private Session session;
  @Mock private Criteria criteria;
  @Mock private KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;
  @Mock private Logger logger;
  @Mock private ObjectMapper objectMapper;

  @InjectMocks private HealthcareSettingsDaoImpl healthcareSettingsDaoImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(sessionFactory.getCurrentSession()).thenReturn(session);
  }

  @Test
  public void testSaveOrUpdate() throws JsonProcessingException {
    HealthcareSetting hsd = new HealthcareSetting();
    hsd.setId(1);
    hsd.setClientId("client-001");
    hsd.setClientSecret("secret-key");
    hsd.setFhirServerBaseURL("https://fhir.test.com");
    hsd.setFhirVersion("R4");
    hsd.setTokenUrl("https://auth.test.com/token");
    hsd.setDefaultProviderId("provider-123");
    hsd.setIsDirect(true);
    hsd.setIsRestAPI(true);
    hsd.setDirectHost("direct.test.com");
    hsd.setDirectUser("user@direct.test.com");
    hsd.setSmtpUrl("smtp.test.com");
    hsd.setLastUpdated(new Date());

    HealthcareSettingOperationalKnowledgeArtifacts karsObj =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    hsd.setKars(karsObj);

    ObjectMapper mapper = new ObjectMapper();
    String kars = mapper.writeValueAsString(hsd.getKars());

    healthcareSettingsDaoImpl.saveOrUpdate(hsd);

    verify(session, times(1)).saveOrUpdate(hsd);
    assertEquals(kars, hsd.getKarsActive());
    assertNotNull(hsd.getKars());
    assertEquals("client-001", hsd.getClientId());
    assertEquals("https://fhir.test.com", hsd.getFhirServerBaseURL());
  }

  @Test
  public void testGetHealthcareSettingById() {
    Integer id = 1;
    HealthcareSetting hs = new HealthcareSetting();
    hs.setId(id);
    hs.setClientId("client-123");
    hs.setFhirServerBaseURL("https://fhir.example.com");
    hs.setDirectHost("direct.example.com");

    when(session.get(HealthcareSetting.class, id)).thenReturn(hs);
    when(session.createCriteria(KnowledgeArtifactStatus.class)).thenReturn(criteria);
    when(criteria.add(Restrictions.eq("hsId", id))).thenReturn(criteria);
    when(criteria.list()).thenReturn(Arrays.asList(new KnowledgeArtifactStatus()));

    HealthcareSetting result = healthcareSettingsDaoImpl.getHealthcareSettingById(id);

    assertNotNull(result);
    assertEquals(id, result.getId());
    assertEquals("client-123", result.getClientId());
    assertEquals("https://fhir.example.com", result.getFhirServerBaseURL());
    assertEquals("direct.example.com", result.getDirectHost());
  }

  @Test
  public void testGetHealthcareSettingByUrl() {
    String url = "http://hosp.com";
    HealthcareSetting hs = new HealthcareSetting();
    hs.setId(10);
    hs.setFhirServerBaseURL(url);
    hs.setClientId("client-xyz");
    hs.setDirectHost("direct.hosp.com");
    hs.setTokenUrl("https://auth.hosp.com/token");

    when(session.createCriteria(HealthcareSetting.class)).thenReturn(criteria);
    when(criteria.add(Restrictions.eq("fhirServerBaseURL", url))).thenReturn(criteria);
    when(criteria.uniqueResult()).thenReturn(hs);
    when(session.createCriteria(KnowledgeArtifactStatus.class)).thenReturn(criteria);
    when(criteria.add(Restrictions.eq("hsId", hs.getId()))).thenReturn(criteria);
    when(criteria.list()).thenReturn(Arrays.asList(new KnowledgeArtifactStatus()));

    HealthcareSetting result = healthcareSettingsDaoImpl.getHealthcareSettingByUrl(url);

    assertNotNull(result);
    assertEquals(url, result.getFhirServerBaseURL());
    assertEquals("client-xyz", result.getClientId());
    assertEquals("direct.hosp.com", result.getDirectHost());
    assertEquals("https://auth.hosp.com/token", result.getTokenUrl());
  }

  @Test
  public void testGetAllHealthcareSettings() {
    HealthcareSetting hs1 = new HealthcareSetting();
    hs1.setId(1);
    hs1.setClientId("client-a");
    hs1.setFhirServerBaseURL("https://fhir.clienta.com");

    HealthcareSetting hs2 = new HealthcareSetting();
    hs2.setId(2);
    hs2.setClientId("client-b");
    hs2.setFhirServerBaseURL("https://fhir.clientb.com");

    List<HealthcareSetting> hsList = Arrays.asList(hs1, hs2);

    when(session.createCriteria(HealthcareSetting.class)).thenReturn(criteria);
    when(criteria.addOrder(any(Order.class))).thenReturn(criteria);
    when(criteria.list()).thenReturn(hsList);

    List<HealthcareSetting> result = healthcareSettingsDaoImpl.getAllHealthcareSettings();

    assertNotNull(result);
    assertEquals(2, result.size());
    assertFalse(result.isEmpty());
    assertEquals("client-a", result.get(0).getClientId());
    assertEquals("client-b", result.get(1).getClientId());
    assertEquals("https://fhir.clientb.com", result.get(1).getFhirServerBaseURL());
    assertEquals("https://fhir.clienta.com", result.get(0).getFhirServerBaseURL());
  }

  @Test
  public void testDelete() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setId(5);
    hs.setClientId("client-del");
    hs.setFhirServerBaseURL("https://delete.example.com");

    healthcareSettingsDaoImpl.delete(hs);

    verify(session, times(1)).delete(hs);
  }
}
