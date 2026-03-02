package com.drajer.bsa.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.KnowledgeArtifactRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.hibernate.query.criteria.HibernateCriteriaBuilder;
import org.hibernate.query.criteria.JpaCriteriaQuery;
import org.hibernate.query.criteria.JpaRoot;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class KarDaoImplTest {

  @Mock private SessionFactory sessionFactory;
  @Mock private Session session;

  @Mock private EntityManagerFactory entityManagerFactory;
  @Mock private EntityManager entityManager;

  @Mock private CriteriaBuilder criteriaBuilder;
  @Mock private HibernateCriteriaBuilder hibernateCriteriaBuilder;
  @Mock private CriteriaQuery<KnowledgeArtifactRepository> karRepoQuery;
  @Mock private JpaCriteriaQuery<KnowledgeArtifactRepository> jpakarRepoQuery;

  @Mock private CriteriaQuery<KnowledgeArtifactStatus> karStatusQuery;
  @Mock private JpaCriteriaQuery<KnowledgeArtifactStatus> jpakarStatusQuery;

  @Mock private Root<KnowledgeArtifactRepository> karRepoRoot;
  @Mock private JpaRoot<KnowledgeArtifactRepository> JpakarRepoRoot;
  @Mock private Root<KnowledgeArtifactStatus> karStatusRoot;
  @Mock private JpaRoot<KnowledgeArtifactStatus> jpakarStatusRoot;

  @Mock private Query<KnowledgeArtifactRepository> karRepoHibernateQuery;
  @Mock private Query<KnowledgeArtifactStatus> karStatusHibernateQuery;

  @InjectMocks private KarDaoImpl karDaoImpl;

  @Test
  public void testSaveOrUpdate() {
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setRepoStatus(true);
    kar.setFhirServerURL("http://example.com");
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    KnowledgeArtifactRepository result = karDaoImpl.saveOrUpdate(kar);
    verify(session).saveOrUpdate(kar);
    assertEquals(kar, result);
  }

  @Test
  public void testGetKARById() {
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setId(1);
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.get(KnowledgeArtifactRepository.class, 1)).thenReturn(kar);
    KnowledgeArtifactRepository result = karDaoImpl.getKARById(1);
    assertNotNull(result);
    assertEquals(Integer.valueOf(1), result.getId());
  }

  @Test
  public void testGetKARByUrl() {

    String url = "http://example.com";
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);

    when(hibernateCriteriaBuilder.createQuery(KnowledgeArtifactRepository.class))
        .thenReturn(jpakarRepoQuery);

    when(jpakarRepoQuery.from(KnowledgeArtifactRepository.class)).thenReturn(JpakarRepoRoot);
    when(session.createQuery(jpakarRepoQuery)).thenReturn(karRepoHibernateQuery);
    KnowledgeArtifactRepository kar = new KnowledgeArtifactRepository();
    kar.setFhirServerURL(url);
    when(karRepoHibernateQuery.uniqueResult()).thenReturn(kar);
    KnowledgeArtifactRepository result = karDaoImpl.getKARByUrl(url);
    assertNotNull(result);
    assertEquals(url, result.getFhirServerURL());
  }

  @Test
  public void testGetAllKARs() {
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);
    when(hibernateCriteriaBuilder.createQuery(KnowledgeArtifactRepository.class))
        .thenReturn(jpakarRepoQuery);
    when(jpakarRepoQuery.from(KnowledgeArtifactRepository.class)).thenReturn(JpakarRepoRoot);

    when(session.createQuery(jpakarRepoQuery)).thenReturn(karRepoHibernateQuery);

    List<KnowledgeArtifactRepository> list =
        Arrays.asList(new KnowledgeArtifactRepository(), new KnowledgeArtifactRepository());

    when(karRepoHibernateQuery.getResultList()).thenReturn(list);

    List<KnowledgeArtifactRepository> result = karDaoImpl.getAllKARs();

    assertEquals(2, result.size());
  }

  @Test
  public void testSaveOrUpdateKARStatus() {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    karDaoImpl.saveOrUpdateKARStatus(status);

    verify(session).saveOrUpdate(status);
  }

  @Test
  public void testGetKARStatusByHsId() {

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);
    when(hibernateCriteriaBuilder.createQuery(KnowledgeArtifactStatus.class))
        .thenReturn(jpakarStatusQuery);
    when(jpakarStatusQuery.from(KnowledgeArtifactStatus.class)).thenReturn(jpakarStatusRoot);
    when(jpakarStatusQuery.select(jpakarStatusRoot)).thenReturn(jpakarStatusQuery);
    when(jpakarStatusQuery.where(hibernateCriteriaBuilder.equal(jpakarStatusRoot.get("hsId"), 1)))
        .thenReturn(jpakarStatusQuery);

    when(session.createQuery(jpakarStatusQuery)).thenReturn(karStatusHibernateQuery);
    List<KnowledgeArtifactStatus> mockList = new ArrayList<>();
    mockList.add(new KnowledgeArtifactStatus());
    when(karStatusHibernateQuery.getResultList()).thenReturn(mockList);
    List<KnowledgeArtifactStatus> result = karDaoImpl.getKARStatusByHsId(1);
    assertEquals(1, result.size());
  }

  @Test
  public void testGetKarStausByKarIdAndKarVersion() {

    String karId = "kar1";
    String version = "1.0";
    Integer hsId = 1;
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(entityManagerFactory);
    when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
    when(criteriaBuilder.createQuery(KnowledgeArtifactStatus.class)).thenReturn(karStatusQuery);
    when(karStatusQuery.from(KnowledgeArtifactStatus.class)).thenReturn(karStatusRoot);

    when(session.createQuery(karStatusQuery)).thenReturn(karStatusHibernateQuery);

    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setKarId("kar1");
    status.setKarVersion("1.0");

    when(karStatusHibernateQuery.uniqueResult()).thenReturn(status);

    // call
    KnowledgeArtifactStatus result =
        karDaoImpl.getKarStausByKarIdAndKarVersion(karId, version, hsId);

    // verify
    assertNotNull(result);
    assertEquals("kar1", result.getKarId());
  }
}
