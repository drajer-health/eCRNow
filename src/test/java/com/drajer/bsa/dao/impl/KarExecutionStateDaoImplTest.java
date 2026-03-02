package com.drajer.bsa.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.KarExecutionState;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class KarExecutionStateDaoImplTest {

  @Mock private SessionFactory sessionFactory;
  @Mock private Session session;
  @Mock private EntityManagerFactory entityManagerFactory;
  @Mock private EntityManager entityManager;
  @Mock private CriteriaBuilder criteriaBuilder;
  @Mock private CriteriaQuery<KarExecutionState> criteriaQuery;
  @Mock private Root<KarExecutionState> root;
  @Mock private Query<KarExecutionState> query;

  @InjectMocks private KarExecutionStateDaoImpl karExecutionStateDaoImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(entityManagerFactory);
    when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);

    when(criteriaBuilder.createQuery(KarExecutionState.class)).thenReturn(criteriaQuery);
    when(criteriaQuery.from(KarExecutionState.class)).thenReturn(root);
    when(session.createQuery(criteriaQuery)).thenReturn(query);
  }

  private KarExecutionState createKarExecutionState() throws Exception {
    Constructor<KarExecutionState> ctor = KarExecutionState.class.getDeclaredConstructor();
    ctor.setAccessible(true);
    return ctor.newInstance();
  }

  @Test
  public void testSaveOrUpdate() throws Exception {
    KarExecutionState kar = createKarExecutionState();
    kar.setNcId(UUID.randomUUID());
    kar.setHsFhirServerUrl("http://example.com/fhir");
    kar.setKarUniqueId("kar-12345");

    karExecutionStateDaoImpl.saveOrUpdate(kar);

    verify(session, times(1)).saveOrUpdate(kar);
  }

  @Test
  public void testGetKarExecutionStateById() throws Exception {
    UUID id = UUID.randomUUID();
    KarExecutionState kar = createKarExecutionState();

    when(session.get(KarExecutionState.class, id)).thenReturn(kar);

    KarExecutionState result = karExecutionStateDaoImpl.getKarExecutionStateById(id);

    assertNotNull(result);
    assertSame(kar, result);
  }

  @Test
  public void testGetKarExecutionStateById_NotFound() {
    UUID id = UUID.randomUUID();
    when(session.get(KarExecutionState.class, id)).thenReturn(null);

    KarExecutionState result = karExecutionStateDaoImpl.getKarExecutionStateById(id);

    assertNull(result);
  }

  @Test
  public void testGetAllKarExecutionStates() {
    List<KarExecutionState> mockList = mock(List.class);
    when(query.getResultList()).thenReturn(mockList);

    List<KarExecutionState> result = karExecutionStateDaoImpl.getAllKarExecutionStates();

    assertNotNull(result);
    assertSame(mockList, result);
  }

  @Test
  public void testDelete() throws Exception {
    KarExecutionState kar = createKarExecutionState();

    karExecutionStateDaoImpl.delete(kar);

    verify(session, times(1)).delete(kar);
  }

  @Test
  public void testGetExecutionIdsByNotificationContextDetails() throws Exception {

    String patientId = "patient-123";
    String fhirServerBaseUrl = "http://fhir.test";
    String notificationResourceId = "notif-456";

    Constructor<KarExecutionState> ctor = KarExecutionState.class.getDeclaredConstructor();
    ctor.setAccessible(true);
    KarExecutionState kar1 = ctor.newInstance();
    kar1.setId(UUID.randomUUID());
    KarExecutionState kar2 = ctor.newInstance();
    kar2.setId(UUID.randomUUID());
    List<KarExecutionState> mockResultList = Arrays.asList(kar1, kar2);

    when(session.createQuery(anyString(), eq(KarExecutionState.class))).thenReturn(query);
    when(query.setParameter("patientId", patientId)).thenReturn(query);
    when(query.setParameter("notificationResourceId", notificationResourceId)).thenReturn(query);
    when(query.setParameter("fhirServerBaseUrl", fhirServerBaseUrl)).thenReturn(query);
    when(query.getResultList()).thenReturn(mockResultList);

    List<String> ids =
        karExecutionStateDaoImpl.getExecutionIdsByNotificationContextDetails(
            patientId, fhirServerBaseUrl, notificationResourceId);

    assertNotNull(ids);
    assertEquals(2, ids.size());
    assertTrue(ids.contains(kar1.getId().toString()));
    assertTrue(ids.contains(kar2.getId().toString()));
    verify(session, times(1)).createQuery(anyString(), eq(KarExecutionState.class));
    verify(query).setParameter("patientId", patientId);
    verify(query).setParameter("notificationResourceId", notificationResourceId);
    verify(query).setParameter("fhirServerBaseUrl", fhirServerBaseUrl);
    verify(query, times(1)).getResultList();
  }
}
