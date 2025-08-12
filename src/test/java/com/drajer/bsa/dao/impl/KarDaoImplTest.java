package com.drajer.bsa.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import java.util.*;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Restrictions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class KarDaoImplTest {

  @Mock private SessionFactory sessionFactory;
  @Mock private Session session;
  @Mock private Criteria criteria;
  @InjectMocks private KarDaoImpl karDaoImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(sessionFactory.getCurrentSession()).thenReturn(session);
  }

  @Test
  public void testSaveOrUpdate() {
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setRepoStatus(true);
    kar.setFhirServerURL("http://example.com");

    karDaoImpl.saveOrUpdate(kar);
    verify(session, times(1)).saveOrUpdate(kar);
    assertEquals("http://example.com", kar.getFhirServerURL());
    assertTrue(kar.getRepoStatus());
  }

  @Test
  public void testGetKARById() {
    Integer id = 1;
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setId(id);

    when(session.get(KnowledgeArtifactRepository.class, id)).thenReturn(kar);
    KnowledgeArtifactRepository result = karDaoImpl.getKARById(id);

    assertNotNull(result);
    assertEquals(id, result.getId());
  }

  @Test
  public void testGetKARByUrl() {
    String url = "http://example.com";
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setFhirServerURL(url);

    when(session.createCriteria(KnowledgeArtifactRepository.class)).thenReturn(criteria);
    when(criteria.add(Restrictions.eq("fhirServerURL", url))).thenReturn(criteria);
    when(criteria.uniqueResult()).thenReturn(kar);

    KnowledgeArtifactRepository result = karDaoImpl.getKARByUrl(url);

    assertNotNull(result);
    assertEquals(url, result.getFhirServerURL());
  }

  @Test
  public void testGetAllKARs() {
    List<KnowledgeArtifactRepository> kars = new ArrayList<>();
    kars.add(new KnowledgeArtifactRepository());
    kars.add(new KnowledgeArtifactRepository());

    when(session.createCriteria(KnowledgeArtifactRepository.class, "repos")).thenReturn(criteria);
    when(criteria.add(Restrictions.eq("repoStatus", true))).thenReturn(criteria);
    when(criteria.addOrder(any())).thenReturn(criteria);
    when(criteria.list()).thenReturn(kars);

    List<KnowledgeArtifactRepository> result = karDaoImpl.getAllKARs();

    assertNotNull(result);
    assertEquals(2, result.size());
  }

  @Test
  public void testSaveOrUpdateKARStatus() {
    KnowledgeArtifactStatus karStatus = new KnowledgeArtifactStatus();
    karStatus.setKarId("kar123");
    karStatus.setKarVersion("1.0");
    karStatus.setVersionUniqueKarId("kar123|1.0");
    karStatus.setIsActive(true);
    karStatus.setSubscriptionsEnabled(false);
    karStatus.setCovidOnly(true);
    karStatus.setLastActivationDate(new Date());
    Set<String> subscriptions = new HashSet<>();
    subscriptions.add("subscription1");
    subscriptions.add("subscription2");
    karStatus.setSubscriptions(subscriptions);

    karDaoImpl.saveOrUpdateKARStatus(karStatus);

    verify(session, times(1)).saveOrUpdate(karStatus);
    assertTrue(karStatus.getIsActive());
    assertFalse(karStatus.getSubscriptionsEnabled());
    assertTrue(karStatus.getCovidOnly());
    assertNotNull(karStatus.getLastActivationDate());
    assertEquals(2, karStatus.getSubscriptions().size());
  }

  @Test
  public void testGetKARStatusByHsId() {
    Integer hsId = 1;
    List<KnowledgeArtifactStatus> kars = new ArrayList<>();
    kars.add(new KnowledgeArtifactStatus());

    when(session.createCriteria(KnowledgeArtifactStatus.class)).thenReturn(criteria);
    when(criteria.add(Restrictions.eq("hsId", hsId))).thenReturn(criteria);
    when(criteria.list()).thenReturn(kars);

    List<KnowledgeArtifactStatus> result = karDaoImpl.getKARStatusByHsId(hsId);

    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testGetKarStausByKarIdAndKarVersion() {
    String karId = "karId";
    String karVersion = "karVersion";
    Integer hsId = 1;
    KnowledgeArtifactStatus karStatus = new KnowledgeArtifactStatus();
    karStatus.setKarId(karId);
    karStatus.setKarVersion(karVersion);
    karStatus.setVersionUniqueKarId(karId + "|" + karVersion);
    karStatus.setIsActive(true);

    when(session.createCriteria(KnowledgeArtifactStatus.class)).thenReturn(criteria);
    when(criteria.add(Restrictions.eq("versionUniqueKarId", karId + "|" + karVersion)))
        .thenReturn(criteria);
    when(criteria.add(Restrictions.eq("hsId", hsId))).thenReturn(criteria);
    when(criteria.uniqueResult()).thenReturn(karStatus);

    KnowledgeArtifactStatus result =
        karDaoImpl.getKarStausByKarIdAndKarVersion(karId, karVersion, hsId);

    assertNotNull(result);
    assertEquals(karId, result.getKarId());
    assertEquals(karVersion, result.getKarVersion());
    assertTrue(result.getIsActive());
  }
}
