package com.drajer.bsa.dao.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.NotificationContext;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.*;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.hibernate.query.criteria.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
public class NotificationContextDaoImplTest {

  @Mock private Session session;

  @Mock private SessionFactory sessionFactory;

  @Mock private EntityManagerFactory entityManagerFactory;

  @Mock private EntityManager entityManager;

  @Mock private CriteriaBuilder criteriaBuilder;

  @Mock private HibernateCriteriaBuilder hibernateCriteriaBuilder;

  @Mock private CriteriaQuery<NotificationContext> criteriaQuery;

  @Mock private JpaCriteriaQuery<NotificationContext> jpaCriteriaQuery;
  @Mock private Query<NotificationContext> query;

  @Mock private JpaSubQuery<Long> subQuery;

  @Mock private JpaPredicate jpaPredicate;

  @Mock private JpaPath jpaPath;

  @Mock private JpaOrder jpaOrder;

  @Mock private Root<NotificationContext> root;

  @Mock private JpaRoot<NotificationContext> jpaRoot;

  @Mock private JpaRoot<NotificationContext> subQueryRoot;

  @Mock private Query<NotificationContext> notificationContextQuery;

  @InjectMocks private NotificationContextDaoImpl dao;

  private NotificationContext notificationContext;

  @Before
  public void setUp() {

    notificationContext = new NotificationContext();
    notificationContext.setId(UUID.randomUUID());
    notificationContext.setPatientId("patient-123");
    notificationContext.setNotificationResourceId("resource-123");
    notificationContext.setFhirServerBaseUrl("http://example.com");
    notificationContext.setNotificationResourceType("mockResource");

    when(sessionFactory.getCurrentSession()).thenReturn(session);
  }

  @Test
  public void testSaveOrUpdate_NewNotificationContext() {

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    doNothing().when(session).saveOrUpdate(notificationContext);
    NotificationContext result = dao.saveOrUpdate(notificationContext);

    assertEquals("patient-123", result.getPatientId());
    verify(sessionFactory).getCurrentSession();
  }

  @Test
  public void testGetNotificationContextByUrl() {
    when(session.getEntityManagerFactory()).thenReturn(entityManagerFactory);
    when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);

    when(criteriaBuilder.createQuery(NotificationContext.class)).thenReturn(criteriaQuery);
    when(criteriaQuery.from(NotificationContext.class)).thenReturn(root);

    when(session.createQuery(criteriaQuery)).thenReturn(query);
    when(query.uniqueResult()).thenReturn(notificationContext);

    NotificationContext result = dao.getNotificationContextByUrl("http://example.com");
    assertNotNull(result);
    assertEquals("patient-123", result.getPatientId());
    assertEquals("resource-123", result.getNotificationResourceId());
    assertEquals("http://example.com", result.getFhirServerBaseUrl());

    verify(session).getEntityManagerFactory();
    verify(entityManagerFactory).createEntityManager();
    verify(entityManager).getCriteriaBuilder();
    verify(session).createQuery(criteriaQuery);
    verify(query).uniqueResult();
  }

  @Test
  public void getNotificationContextById_Success() {
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.get(NotificationContext.class, notificationContext.getId()))
        .thenReturn(notificationContext);

    NotificationContext result = dao.getNotificationContextById(notificationContext.getId());

    assertNotNull(result);
    assertEquals(notificationContext.getId(), result.getId());

    verify(sessionFactory).getCurrentSession();
  }

  @Test
  public void getNotificationContextByUniqueConstraints_success() {
    when(session.getEntityManagerFactory()).thenReturn(entityManagerFactory);
    when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);

    when(criteriaBuilder.createQuery(NotificationContext.class)).thenReturn(criteriaQuery);
    when(criteriaQuery.from(NotificationContext.class)).thenReturn(root);

    when(session.createQuery(criteriaQuery)).thenReturn(query);
    when(query.uniqueResult()).thenReturn(notificationContext);

    NotificationContext result =
        dao.getNotificationContextByUniqueConstraints(
            notificationContext.getFhirServerBaseUrl(),
            notificationContext.getPatientId(),
            notificationContext.getNotificationResourceId(),
            notificationContext.getNotificationResourceType());

    assertNotNull(result);
    assertEquals("patient-123", result.getPatientId());
    assertEquals("resource-123", result.getNotificationResourceId());

    verify(session).getEntityManagerFactory();
    verify(entityManagerFactory).createEntityManager();
    verify(entityManager).getCriteriaBuilder();
  }

  @Test
  public void testGetAllNotificationContext_withLimit() {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("limit", "5");
    searchParams.put("fhirServerBaseURL", "http://example.com");
    searchParams.put("notificationResourceId", "resourceId1");
    searchParams.put("patientId", "patient123");
    searchParams.put("notificationProcessingStatus", "processed");
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);
    when(session.createQuery(jpaCriteriaQuery)).thenReturn((query));
    when(hibernateCriteriaBuilder.createQuery(eq(NotificationContext.class)))
        .thenReturn(jpaCriteriaQuery);
    doReturn(jpaRoot).when(jpaCriteriaQuery).from(eq(NotificationContext.class));
    when(jpaCriteriaQuery.select(jpaRoot)).thenReturn(jpaCriteriaQuery);
    when(jpaRoot.get(anyString())).thenReturn(jpaPath);
    when(hibernateCriteriaBuilder.and(any(), any(), any(), any())).thenReturn(jpaPredicate);
    when(hibernateCriteriaBuilder.asc(jpaPath)).thenReturn(jpaOrder);
    when(jpaCriteriaQuery.where(any(JpaExpression.class))).thenReturn(jpaCriteriaQuery);
    when(jpaCriteriaQuery.orderBy(any(JpaOrder.class))).thenReturn(jpaCriteriaQuery);

    List<NotificationContext> result = dao.getAllNotificationContext(null, searchParams);
    assertNotNull(result);

    verify(session).getCriteriaBuilder();
    verify(session).createQuery(jpaCriteriaQuery);
  }

  @Test
  public void testGetNotificationContextData_withAllParams() {

    UUID id = UUID.randomUUID();
    String fhirServerBaseUrl = "http://example.com";
    String notificationResourceId = "resource-123";
    String patientId = "patient-123";

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setId(id);
    notificationContext.setFhirServerBaseUrl(fhirServerBaseUrl);
    notificationContext.setNotificationResourceId(notificationResourceId);
    notificationContext.setPatientId(patientId);

    List<NotificationContext> notificationContextList = new ArrayList<>();
    notificationContextList.add(notificationContext);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(entityManagerFactory);
    when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);

    when(criteriaBuilder.createQuery(NotificationContext.class)).thenReturn(criteriaQuery);
    when(criteriaQuery.from(NotificationContext.class)).thenReturn(root);

    when(session.createQuery(criteriaQuery)).thenReturn(notificationContextQuery);
    when(notificationContextQuery.getResultList()).thenReturn(notificationContextList);

    List<NotificationContext> result =
        dao.getNotificationContextData(id, fhirServerBaseUrl, notificationResourceId, patientId);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(id, result.get(0).getId());
    assertEquals(fhirServerBaseUrl, result.get(0).getFhirServerBaseUrl());
  }

  @Test
  public void testGetNotificationContextData_withNullId() {

    String fhirServerBaseUrl = "http://example.com";
    String notificationResourceId = "resource-123";
    String patientId = "patient-123";
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl(fhirServerBaseUrl);
    notificationContext.setNotificationResourceId(notificationResourceId);
    notificationContext.setPatientId(patientId);
    List<NotificationContext> notificationContextList = new ArrayList<>();
    notificationContextList.add(notificationContext);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(entityManagerFactory);
    when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);

    when(criteriaBuilder.createQuery(NotificationContext.class)).thenReturn(criteriaQuery);
    when(criteriaQuery.from(NotificationContext.class)).thenReturn(root);

    when(session.createQuery(criteriaQuery)).thenReturn(notificationContextQuery);
    when(notificationContextQuery.getResultList()).thenReturn(notificationContextList);

    List<NotificationContext> result =
        dao.getNotificationContextData(null, fhirServerBaseUrl, notificationResourceId, patientId);
    assertNotNull(result);

    verify(session).getEntityManagerFactory();
    verify(entityManagerFactory).createEntityManager();
    verify(entityManager).getCriteriaBuilder();
  }

  @Test
  public void testGetAllNotificationContext_withValidDateParams() throws Exception {

    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("startDateTime", "2022-01-01 12:00");
    searchParams.put("endDateTime", "2022-02-01 12:00");
    searchParams.put("lastUpdatedStartTime", "2022-01-01 10:00");
    searchParams.put("lastUpdatedEndTime", "2022-02-01 10:00");

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);
    when(session.createQuery(jpaCriteriaQuery)).thenReturn((query));
    when(hibernateCriteriaBuilder.createQuery(eq(NotificationContext.class)))
        .thenReturn(jpaCriteriaQuery);
    doReturn(jpaRoot).when(jpaCriteriaQuery).from(eq(NotificationContext.class));

    when(jpaCriteriaQuery.select(jpaRoot)).thenReturn(jpaCriteriaQuery);
    when(jpaRoot.get(anyString())).thenReturn(jpaPath);
    when(hibernateCriteriaBuilder.and(any(), any(), any(), any())).thenReturn(jpaPredicate);
    when(hibernateCriteriaBuilder.asc(jpaPath)).thenReturn(jpaOrder);
    when(jpaCriteriaQuery.where(any(JpaExpression.class))).thenReturn(jpaCriteriaQuery);
    when(jpaCriteriaQuery.orderBy(any(JpaOrder.class))).thenReturn(jpaCriteriaQuery);
    List<NotificationContext> result = dao.getAllNotificationContext(null, searchParams);

    verify(session).getCriteriaBuilder();
    verify(jpaCriteriaQuery).select(jpaRoot);
  }

  @Test
  public void testGetAllNotificationContext_withid() {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("limit", "5");
    UUID id = UUID.fromString("123e4567-e89b-12d3-a456-426614174000");
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);
    when(session.createQuery(jpaCriteriaQuery)).thenReturn((query));
    when(hibernateCriteriaBuilder.createQuery(eq(NotificationContext.class)))
        .thenReturn(jpaCriteriaQuery);
    doReturn(jpaRoot).when(jpaCriteriaQuery).from(eq(NotificationContext.class));

    when(jpaCriteriaQuery.select(jpaRoot)).thenReturn(jpaCriteriaQuery);
    when(jpaRoot.get(anyString())).thenReturn(jpaPath);
    when(hibernateCriteriaBuilder.and(any())).thenReturn(jpaPredicate);
    when(hibernateCriteriaBuilder.asc(jpaPath)).thenReturn(jpaOrder);
    when(jpaCriteriaQuery.where(any(JpaExpression.class))).thenReturn(jpaCriteriaQuery);
    when(jpaCriteriaQuery.orderBy(any(JpaOrder.class))).thenReturn(jpaCriteriaQuery);

    List<NotificationContext> result = dao.getAllNotificationContext(id, searchParams);
    verify(session).getCriteriaBuilder();
    verify(jpaCriteriaQuery).select(jpaRoot);
  }

  @Test
  public void testgetNotificationContextForReprocessing() {
    UUID id = new UUID(12345678, 12345678);
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("limit", "5");
    searchParams.put("lastUpdatedStartTime", "2022-01-01 10:00:23.000");
    searchParams.put("lastUpdatedEndTime", "2022-02-01 10:00:23.000");

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getCriteriaBuilder()).thenReturn(hibernateCriteriaBuilder);
    when(session.createQuery(jpaCriteriaQuery)).thenReturn((query));
    when(hibernateCriteriaBuilder.createQuery(eq(NotificationContext.class)))
        .thenReturn(jpaCriteriaQuery);
    when(jpaCriteriaQuery.subquery(Long.class)).thenReturn(subQuery);
    when(subQuery.from(NotificationContext.class)).thenReturn(subQueryRoot);
    when(subQuery.select(any())).thenReturn(subQuery);
    when(subQuery.where(any(), any(), any())).thenReturn(subQuery);
    when(hibernateCriteriaBuilder.equal(any(), any())).thenReturn(jpaPredicate);
    when(jpaRoot.get(anyString())).thenReturn(jpaPath);
    when(subQueryRoot.get(anyString())).thenReturn(jpaPath);
    when(jpaCriteriaQuery.select(jpaRoot)).thenReturn(jpaCriteriaQuery);
    when(jpaCriteriaQuery.where(any(), any(), any(), any())).thenReturn(jpaCriteriaQuery);
    when(hibernateCriteriaBuilder.asc(jpaPath)).thenReturn(jpaOrder);
    when(jpaCriteriaQuery.orderBy(jpaOrder)).thenReturn(jpaCriteriaQuery);

    doReturn(jpaRoot).when(jpaCriteriaQuery).from(eq(NotificationContext.class));

    List<NotificationContext> result = dao.getNotificationContextForReprocessing(id, searchParams);
    assertEquals(0, result.size());
    assertNotNull(result);
  }

  @Test
  public void testDeleteNotificationContext() {
    NotificationContext notificationContext = new NotificationContext();
    when(sessionFactory.getCurrentSession()).thenReturn(session);

    dao.delete(notificationContext);

    verify(sessionFactory).getCurrentSession();
    verify(session).delete(notificationContext);
  }
}
