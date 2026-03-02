package com.drajer.bsa.dao.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.HealthcareSetting;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.Arrays;
import java.util.List;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class HealthcareSettingsDaoImplTest {

  @Mock private SessionFactory sessionFactory;
  @Mock private Session session;

  @Mock private EntityManagerFactory entityManagerFactory;
  @Mock private EntityManager entityManager;

  @Mock private CriteriaBuilder criteriaBuilder;
  @Mock private CriteriaQuery<HealthcareSetting> healthcareSettingQuery;
  @Mock private CriteriaQuery<KnowledgeArtifactStatus> karsQuery;

  @Mock private Root<HealthcareSetting> healthcareSettingRoot;
  @Mock private Root<KnowledgeArtifactStatus> karsRoot;

  @Mock private Query<HealthcareSetting> healthcareSettingHibernateQuery;
  @Mock private Query<KnowledgeArtifactStatus> karsHibernateQuery;

  @InjectMocks private HealthcareSettingsDaoImpl dao;

  @Before
  public void setUp() {
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(entityManagerFactory);
    when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);

    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
  }

  @Test
  public void testSaveOrUpdate() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setId(1);
    hs.setClientId("client-1");
    hs.setKars(new HealthcareSettingOperationalKnowledgeArtifacts());

    dao.saveOrUpdate(hs);

    verify(session).saveOrUpdate(hs);
    assertNotNull(hs.getKarsActive());
  }

  @Test
  public void testGetHealthcareSettingById() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setId(10);

    when(session.get(HealthcareSetting.class, 10)).thenReturn(hs);

    when(criteriaBuilder.createQuery(KnowledgeArtifactStatus.class)).thenReturn(karsQuery);
    when(karsQuery.from(KnowledgeArtifactStatus.class)).thenReturn(karsRoot);
    when(session.createQuery(karsQuery)).thenReturn(karsHibernateQuery);
    when(karsHibernateQuery.getResultList())
        .thenReturn(Arrays.asList(new KnowledgeArtifactStatus()));

    HealthcareSetting result = dao.getHealthcareSettingById(10);

    assertNotNull(result);
    assertEquals(Integer.valueOf(10), result.getId());
    assertNotNull(result.getKars());
  }

  @Test
  public void testGetHealthcareSettingByUrl() {
    String url = "https://fhir.test.com";

    when(criteriaBuilder.createQuery(HealthcareSetting.class)).thenReturn(healthcareSettingQuery);
    when(healthcareSettingQuery.from(HealthcareSetting.class)).thenReturn(healthcareSettingRoot);

    when(criteriaBuilder.equal(any(), eq(url)))
        .thenReturn(mock(jakarta.persistence.criteria.Predicate.class));

    when(session.createQuery(healthcareSettingQuery)).thenReturn(healthcareSettingHibernateQuery);

    HealthcareSetting hs = new HealthcareSetting();
    hs.setId(5);
    hs.setFhirServerBaseURL(url);

    when(healthcareSettingHibernateQuery.uniqueResult()).thenReturn(hs);

    when(criteriaBuilder.createQuery(KnowledgeArtifactStatus.class)).thenReturn(karsQuery);
    when(karsQuery.from(KnowledgeArtifactStatus.class)).thenReturn(karsRoot);
    when(session.createQuery(karsQuery)).thenReturn(karsHibernateQuery);
    when(karsHibernateQuery.getResultList())
        .thenReturn(Arrays.asList(new KnowledgeArtifactStatus()));

    HealthcareSetting result = dao.getHealthcareSettingByUrl(url);

    assertNotNull(result);
    assertEquals(url, result.getFhirServerBaseURL());
    assertNotNull(result.getKars());
  }

  @Test
  public void testGetAllHealthcareSettings() {
    when(criteriaBuilder.createQuery(HealthcareSetting.class)).thenReturn(healthcareSettingQuery);
    when(healthcareSettingQuery.from(HealthcareSetting.class)).thenReturn(healthcareSettingRoot);

    when(session.createQuery(healthcareSettingQuery)).thenReturn(healthcareSettingHibernateQuery);

    List<HealthcareSetting> list = Arrays.asList(new HealthcareSetting(), new HealthcareSetting());

    when(healthcareSettingHibernateQuery.getResultList()).thenReturn(list);

    List<HealthcareSetting> result = dao.getAllHealthcareSettings();

    assertEquals(2, result.size());
  }

  @Test
  public void testGetKarsActiveByHsId() {
    when(criteriaBuilder.createQuery(KnowledgeArtifactStatus.class)).thenReturn(karsQuery);
    when(karsQuery.from(KnowledgeArtifactStatus.class)).thenReturn(karsRoot);

    when(session.createQuery(karsQuery)).thenReturn(karsHibernateQuery);

    when(karsHibernateQuery.getResultList())
        .thenReturn(Arrays.asList(new KnowledgeArtifactStatus()));

    List<KnowledgeArtifactStatus> result = dao.getKarsActiveByHsId(1);

    assertEquals(1, result.size());
  }

  @Test
  public void testDelete() {
    HealthcareSetting hs = new HealthcareSetting();
    dao.delete(hs);
    verify(session).delete(hs);
  }
}
