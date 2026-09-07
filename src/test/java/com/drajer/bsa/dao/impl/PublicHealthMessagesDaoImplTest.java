package com.drajer.bsa.dao.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.PublicHealthMessage;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.*;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PublicHealthMessagesDaoImplTest {

  @Mock private SessionFactory sessionFactory;

  @InjectMocks private PublicHealthMessagesDaoImpl publicHealthMessagesDao;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testsaveOrUpdate() {
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setId(UUID.randomUUID());
    publicHealthMessage.setFhirServerBaseUrl("http://fhirServerBaseUrl");

    Session mocksession = mock(Session.class);
    lenient().when(sessionFactory.getCurrentSession()).thenReturn(mocksession);

    PublicHealthMessage result = publicHealthMessagesDao.saveOrUpdate(publicHealthMessage);
    assertNotNull(result);
    assertNotNull(result);
    assertEquals(publicHealthMessage.getId(), result.getId());
    assertEquals(publicHealthMessage.getFhirServerBaseUrl(), result.getFhirServerBaseUrl());

    verify(mocksession).saveOrUpdate(publicHealthMessage);
  }

  @Test
  public void testgetById() {
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setId(UUID.randomUUID());
    Session mocksession = mock(Session.class);
    lenient().when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
    when(mocksession.get(PublicHealthMessage.class, publicHealthMessage.getId()))
        .thenReturn(publicHealthMessage);

    PublicHealthMessage result = publicHealthMessagesDao.getById(publicHealthMessage.getId());
    assertNotNull(result);
    assertEquals(publicHealthMessage.getId(), result.getId());

    verify(mocksession).get(PublicHealthMessage.class, publicHealthMessage.getId());
  }

  @Test
  public void testgetMaxVersionId() {
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setFhirServerBaseUrl("fhirServerBaseUrl");
    publicHealthMessage.setNotifiedResourceId("notifiedResourceId");
    publicHealthMessage.setNotifiedResourceType("notifiedResourceType");
    publicHealthMessage.setPatientId("patientId");
    publicHealthMessage.setKarUniqueId("karUniqueId");
    publicHealthMessage.setSubmittedVersionNumber(1);

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.uniqueResultOptional()).thenReturn(Optional.of(publicHealthMessage));

    Integer result = publicHealthMessagesDao.getMaxVersionId(publicHealthMessage);

    assertNotNull(result);
    assertEquals(Integer.valueOf(1), result);

    verify(sessionFactory, atLeastOnce()).getCurrentSession();
    verify(session).getEntityManagerFactory();
    verify(cq).from(PublicHealthMessage.class);
    verify(session).createQuery(cq);
    verify(query).uniqueResultOptional();
  }

  //    @Test
  //    public void testgetMaxVersionId() {
  //      PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //      publicHealthMessage.setFhirServerBaseUrl("fhirServerBaseUrl");
  //      publicHealthMessage.setNotifiedResourceId("notifiedResourceId");
  //      publicHealthMessage.setNotifiedResourceType("notifiedResourceType");
  //      publicHealthMessage.setPatientId("patientId");
  //      publicHealthMessage.setKarUniqueId("karUniqueId");
  //
  //      publicHealthMessage.setSubmittedVersionNumber(1);
  //
  //      Session mocksession = mock(Session.class);
  //      Criteria mockCriteria = mock(Criteria.class);
  //
  //      when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //      when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //
  // when(mockCriteria.addOrder(Order.desc("submittedVersionNumber"))).thenReturn(mockCriteria);
  //      when(mockCriteria.setMaxResults(1)).thenReturn(mockCriteria);
  //      when(mockCriteria.uniqueResult()).thenReturn(publicHealthMessage);
  //
  //      Integer result = publicHealthMessagesDao.getMaxVersionId(publicHealthMessage);
  //
  //      assertNotNull(result);
  //      assertEquals(publicHealthMessage.getSubmittedVersionNumber(), result);
  //
  //      verify(mockCriteria).setMaxResults(1);
  //      verify(mockCriteria).uniqueResult();
  //    }

  @Test
  public void testgetByCorrelationId() {

    String correlationId = "xCorrelationId";

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setId(UUID.randomUUID());
    publicHealthMessage.setxCorrelationId(correlationId);

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.uniqueResult()).thenReturn(publicHealthMessage);

    PublicHealthMessage result = publicHealthMessagesDao.getByCorrelationId(correlationId);

    assertNotNull(result);
    assertEquals(publicHealthMessage.getId(), result.getId());
    assertEquals(publicHealthMessage.getxCorrelationId(), result.getxCorrelationId());

    verify(sessionFactory, times(2)).getCurrentSession();
    verify(session).getEntityManagerFactory();
    verify(emf).createEntityManager();
    verify(em).getCriteriaBuilder();
    verify(cb).createQuery(PublicHealthMessage.class);
  }

  //  @Test
  //  public void testgetByCorrelationId() {
  //    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //    publicHealthMessage.setId(UUID.randomUUID());
  //    publicHealthMessage.setxCorrelationId("xCorrelationId");
  //
  //    Session mocksession = mock(Session.class);
  //    Criteria mockCriteria = mock(Criteria.class);
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //
  //    when(mockCriteria.add(Restrictions.eq(X_CORRELATION_ID,
  // "coorelId"))).thenReturn(mockCriteria);
  //    when(mockCriteria.uniqueResult()).thenReturn(publicHealthMessage);
  //
  //    PublicHealthMessage result =
  //        publicHealthMessagesDao.getByCorrelationId(publicHealthMessage.getxCorrelationId());
  //    assertNotNull(result);
  //    assertEquals(publicHealthMessage.getId(), result.getId());
  //    assertEquals(publicHealthMessage.getxCorrelationId(), result.getxCorrelationId());
  //
  //    verify(mockCriteria).uniqueResult();
  //  }
  //

  @Test
  public void testgetPublicHealthMessage() {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("key", "value");
    List<PublicHealthMessage> expectedList = new ArrayList<>();
    expectedList.add(new PublicHealthMessage());

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.getResultList()).thenReturn(expectedList);

    List<PublicHealthMessage> result = publicHealthMessagesDao.getPublicHealthMessage(searchParams);

    assertNotNull(result);
    assertEquals(expectedList, result);
    verify(sessionFactory, times(2)).getCurrentSession();
    verify(session).getEntityManagerFactory();
    verify(emf).createEntityManager();
    verify(em).getCriteriaBuilder();
    verify(cb).createQuery(PublicHealthMessage.class);
    verify(cq).from(PublicHealthMessage.class);
  }

  @Test
  public void testgetPublicHealthMessage_withSubmissionMessageStatus_line107() {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("submissionMessageStatus", "SUBMITTED");
    List<PublicHealthMessage> expectedList = new ArrayList<>();
    expectedList.add(new PublicHealthMessage());

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.getResultList()).thenReturn(expectedList);

    List<PublicHealthMessage> result = publicHealthMessagesDao.getPublicHealthMessage(searchParams);

    assertNotNull("Result should not be null", result);
    assertEquals(expectedList, result);
    verify(sessionFactory, times(2)).getCurrentSession();
    verify(query).getResultList();
  }

  //  @Test
  //  public void testgetPublicHealthMessage() {
  //    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //    HashMap<String, String> searchParams = new HashMap<>();
  //    searchParams.put("key", "value");
  //    Session mocksession = mock(Session.class);
  //    Criteria mockCriteria = mock(Criteria.class);
  //
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //    when(mockCriteria.addOrder(Order.desc(any()))).thenReturn(mockCriteria);
  //    List<PublicHealthMessage> result =
  // publicHealthMessagesDao.getPublicHealthMessage(searchParams);
  //
  //    assertNotNull(result);
  //    verify(sessionFactory).getCurrentSession();
  //    verify(mocksession).createCriteria(PublicHealthMessage.class);
  //  }

  @Test
  public void testgetByXRequestId() {

    String xRequestId = "xRequestId";

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setId(UUID.randomUUID());
    publicHealthMessage.setNotificationId("notificationId");

    List<PublicHealthMessage> expectedList = new ArrayList<>();
    expectedList.add(publicHealthMessage);

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.getResultList()).thenReturn(expectedList);

    List<PublicHealthMessage> result = publicHealthMessagesDao.getByXRequestId(xRequestId);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(publicHealthMessage.getId(), result.get(0).getId());

    verify(sessionFactory, times(2)).getCurrentSession();
    verify(session).getEntityManagerFactory();
    verify(cb).createQuery(PublicHealthMessage.class);
    verify(cq).from(PublicHealthMessage.class);
    verify(session).createQuery(cq);
    verify(query).getResultList();
  }

  //  @Test
  //  public void testgetByXRequestId() {
  //    String Id = "xRequestId";
  //    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //    publicHealthMessage.setId(UUID.randomUUID());
  //    publicHealthMessage.setNotificationId("notificationId");
  //    List<PublicHealthMessage> publicHealthMessageList = new ArrayList<>();
  //    publicHealthMessageList.add(publicHealthMessage);
  //    Session mocksession = mock(Session.class);
  //    Criteria mockCriteria = mock(Criteria.class);
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //
  //    when(mockCriteria.add(Restrictions.eq(X_REQUEST_ID,
  // "xRequestId"))).thenReturn(mockCriteria);
  //    when(mockCriteria.addOrder(Order.desc(any()))).thenReturn(mockCriteria);
  //
  //    List<PublicHealthMessage> result = publicHealthMessagesDao.getByXRequestId(Id);
  //    assertNotNull(result);
  //    verify(mockCriteria).list();
  //  }
  //

  @Test
  public void testgetBySubmittedMessageId() {
    String submittedMessageId = "submittedMessageId";

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setSubmittedMessageId(submittedMessageId);

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.uniqueResult()).thenReturn(publicHealthMessage);

    PublicHealthMessage result =
        publicHealthMessagesDao.getBySubmittedMessageId(submittedMessageId);

    assertNotNull(result);
    assertEquals(submittedMessageId, result.getSubmittedMessageId());

    verify(sessionFactory, times(2)).getCurrentSession();
    verify(session).getEntityManagerFactory();
    verify(emf).createEntityManager();
    verify(em).getCriteriaBuilder();
    verify(cb).createQuery(PublicHealthMessage.class);
    verify(query).uniqueResult();
  }

  //  @Test
  //  public void testgetBySubmittedMessageId() {
  //    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //    publicHealthMessage.setSubmittedMessageId("submittedMessageId");
  //    Session mocksession = mock(Session.class);
  //    Criteria mockCriteria = mock(Criteria.class);
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //    when(mockCriteria.add(Restrictions.eq(SUBMITTED_MESSAGE_ID, "submittedMessageId")))
  //        .thenReturn(mockCriteria);
  //    when(mockCriteria.uniqueResult()).thenReturn(new PublicHealthMessage());
  //    PublicHealthMessage result =
  //        publicHealthMessagesDao.getBySubmittedMessageId(
  //            publicHealthMessage.getSubmittedMessageId());
  //     assertEquals(publicHealthMessage.getSubmittedMessageId(),result.getSubmittedMessageId());
  //    assertNotNull(result);
  //
  //    verify(mockCriteria).uniqueResult();
  //    verify(sessionFactory).getCurrentSession();
  //    verify(mocksession).createCriteria(PublicHealthMessage.class);
  //  }

  //  @Test
  //  public void testgetByResponseMessageId() {
  //    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //    publicHealthMessage.setResponseMessageId("responseMessageId");
  //    Session mocksession = mock(Session.class);
  //    Criteria mockCriteria = mock(Criteria.class);
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //    when(mockCriteria.add(Restrictions.eq(RESPONSE_MESSAGE_ID, "responseMessageId")))
  //        .thenReturn(mockCriteria);
  //    when(mockCriteria.uniqueResult()).thenReturn(new PublicHealthMessage());
  //    PublicHealthMessage result =
  //
  // publicHealthMessagesDao.getByResponseMessageId(publicHealthMessage.getResponseMessageId());
  //    assertNotNull(result);
  //
  //    verify(mockCriteria).uniqueResult();
  //    verify(sessionFactory).getCurrentSession();
  //    verify(mocksession).createCriteria(PublicHealthMessage.class);
  //  }

  @Test
  public void testgetByResponseMessageId() {
    String responseMessageId = "responseMessageId";

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setResponseMessageId(responseMessageId);

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.uniqueResult()).thenReturn(publicHealthMessage);

    PublicHealthMessage result = publicHealthMessagesDao.getByResponseMessageId(responseMessageId);

    assertNotNull(result);
    assertEquals(responseMessageId, result.getResponseMessageId());

    verify(sessionFactory, times(2)).getCurrentSession();
    verify(session).getEntityManagerFactory();
    verify(cb).createQuery(PublicHealthMessage.class);
    verify(cq).from(PublicHealthMessage.class);
    verify(session).createQuery(cq);
    verify(query).uniqueResult();
  }

  //  @Test
  //  public void testdelete() {
  //    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //    Session mocksession = mock(Session.class);
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    doNothing().when(mocksession).delete(any());
  //    publicHealthMessagesDao.delete(publicHealthMessage);
  //    verify(mocksession).delete(publicHealthMessage);
  //  }

  @Test
  public void testdelete() {

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    Session session = mock(Session.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    doNothing().when(session).delete(publicHealthMessage);

    publicHealthMessagesDao.delete(publicHealthMessage);
    verify(sessionFactory).getCurrentSession();
    verify(session).delete(publicHealthMessage);
  }

  //
  //  @Test
  //  public void testgetBySubmittedDataId() {
  //    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
  //    publicHealthMessage.setSubmittedDataId("submittedDataId");
  //    Session mocksession = mock(Session.class);
  //    Criteria mockCriteria = mock(Criteria.class);
  //
  //    when(sessionFactory.getCurrentSession()).thenReturn(mocksession);
  //    when(mocksession.createCriteria(PublicHealthMessage.class)).thenReturn(mockCriteria);
  //    when(mockCriteria.add(Restrictions.eq(SUBMITTED_DATA_ID,
  // "subId"))).thenReturn(mockCriteria);
  //    when(mockCriteria.uniqueResult()).thenReturn(new PublicHealthMessage());
  //
  //    PublicHealthMessage result =
  //        publicHealthMessagesDao.getBySubmittedDataId(publicHealthMessage.getSubmittedDataId());
  //
  //    assertNotNull(result);
  //    verify(mockCriteria).uniqueResult();
  //    verify(sessionFactory).getCurrentSession();
  //  }

  @Test
  public void testgetBySubmittedDataId() {
    String submittedDataId = "submittedDataId";

    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setSubmittedDataId(submittedDataId);

    Session session = mock(Session.class);
    EntityManagerFactory emf = mock(EntityManagerFactory.class);
    EntityManager em = mock(EntityManager.class);
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    CriteriaQuery<PublicHealthMessage> cq = mock(CriteriaQuery.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Query<PublicHealthMessage> query = mock(Query.class);

    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.getEntityManagerFactory()).thenReturn(emf);
    when(emf.createEntityManager()).thenReturn(em);

    when(em.getCriteriaBuilder()).thenReturn(cb);
    when(cb.createQuery(PublicHealthMessage.class)).thenReturn(cq);
    when(cq.from(PublicHealthMessage.class)).thenReturn(root);

    when(session.createQuery(cq)).thenReturn(query);
    when(query.uniqueResult()).thenReturn(publicHealthMessage);

    PublicHealthMessage result = publicHealthMessagesDao.getBySubmittedDataId(submittedDataId);

    assertNotNull(result);
    assertEquals(submittedDataId, result.getSubmittedDataId());

    verify(sessionFactory, times(2)).getCurrentSession();
    verify(cb).createQuery(PublicHealthMessage.class);
    verify(cq).from(PublicHealthMessage.class);
    verify(session).createQuery(cq);
    verify(query).uniqueResult();
  }

  @Test
  public void testPreparePredicate_allSearchParams() {
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Map<String, String> searchParams = new HashMap<>();

    searchParams.put("submittedDataId", "subId");
    searchParams.put("submittedVersionNumber", "1");
    searchParams.put("responseDataId", "respId");
    searchParams.put("fhirServerBaseUrl", "http://fhir");
    searchParams.put("patientId", "patient123");
    searchParams.put("encounterId", "enc123");
    searchParams.put("notifiedResourceId", "resource123");
    searchParams.put("notifiedResourceType", "resourceType");
    searchParams.put("karUniqueId", "kar123");
    searchParams.put("submittedMessageId", "msg123");
    searchParams.put("xRequestId", "req123");
    searchParams.put("xCorrelationId", "corr123");
    searchParams.put("responseMessageId", "respMsg123");
    searchParams.put("responseProcessingInstruction", "instruction");
    searchParams.put("responseProcessingStatus", "status");

    List<Predicate> predicates =
        PublicHealthMessagesDaoImpl.preparePredicate(cb, root, searchParams);

    assertNotNull(predicates);
    assertEquals(15, predicates.size());
  }

  @Test
  public void testPreparePredicate_withSingleParam() {
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Map<String, String> searchParams = new HashMap<>();

    searchParams.put("submittedDataId", "subId");

    List<Predicate> predicates =
        PublicHealthMessagesDaoImpl.preparePredicate(cb, root, searchParams);

    assertNotNull(predicates);
    assertEquals(1, predicates.size());
  }

  @Test
  public void testPreparePredicate_emptyParams() {
    CriteriaBuilder cb = mock(CriteriaBuilder.class);
    Root<PublicHealthMessage> root = mock(Root.class);
    Map<String, String> searchParams = new HashMap<>();

    List<Predicate> predicates =
        PublicHealthMessagesDaoImpl.preparePredicate(cb, root, searchParams);

    assertNotNull(predicates);
    assertEquals(0, predicates.size());
  }
}
