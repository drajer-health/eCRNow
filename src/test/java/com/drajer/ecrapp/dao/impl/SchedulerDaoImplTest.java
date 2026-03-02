package com.drajer.ecrapp.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;

import com.drajer.ecrapp.model.ScheduledTasks;
import jakarta.persistence.criteria.*;
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
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class SchedulerDaoImplTest {

  @Mock private SessionFactory sessionFactory;
  @Mock private Session session;
  @Mock private jakarta.persistence.EntityManagerFactory emf;
  @Mock private jakarta.persistence.EntityManager em;

  @Mock private CriteriaBuilder criteriaBuilder;
  @Mock private CriteriaQuery<ScheduledTasks> criteriaQuery;
  @Mock private Root<ScheduledTasks> root;
  @Mock private Predicate predicate;
  @Mock private Query<ScheduledTasks> hibernateQuery;
  @Mock private Expression<String> expression;

  @InjectMocks private SchedulerDaoImpl dao;

  @Before
  public void setUp() {
    Mockito.lenient().when(sessionFactory.getCurrentSession()).thenReturn(session);

    Mockito.lenient().when(session.getEntityManagerFactory()).thenReturn(emf);
    Mockito.lenient().when(emf.createEntityManager()).thenReturn(em);
    Mockito.lenient().when(em.getCriteriaBuilder()).thenReturn(criteriaBuilder);
    Mockito.lenient()
        .when(criteriaBuilder.createQuery(ScheduledTasks.class))
        .thenReturn(criteriaQuery);
    Mockito.lenient().when(criteriaQuery.from(ScheduledTasks.class)).thenReturn(root);
    Mockito.lenient().when(session.createQuery(criteriaQuery)).thenReturn(hibernateQuery);
    Mockito.lenient().when(criteriaQuery.select(any())).thenReturn(criteriaQuery);
    Mockito.lenient().when(criteriaQuery.select(any()).distinct(true)).thenReturn(criteriaQuery);
  }

  @Test
  public void testGetScheduledTasks_returnsList() {
    List<ScheduledTasks> list = Arrays.asList(new ScheduledTasks(), new ScheduledTasks());
    Mockito.lenient().when(criteriaBuilder.equal(any(), any())).thenReturn(predicate);
    Mockito.lenient().when(criteriaBuilder.like(any(), anyString())).thenReturn(predicate);
    Mockito.lenient()
        .when(criteriaBuilder.and(any(Predicate.class), any(Predicate.class)))
        .thenReturn(predicate);
    Mockito.lenient().when(hibernateQuery.getResultList()).thenReturn(list);

    List<ScheduledTasks> result = dao.getScheduledTasks("ACTION", "123");

    assertNotNull(result);
    assertEquals(2, result.size());
  }

  @Test
  public void testGetScheduledTasksBySearchQuery_withTerm_returnsList() {
    List<ScheduledTasks> list = Arrays.asList(new ScheduledTasks());

    Mockito.lenient().when(criteriaBuilder.lower(any(Expression.class))).thenReturn(expression);
    Mockito.lenient()
        .when(criteriaBuilder.like(any(Expression.class), anyString()))
        .thenReturn(predicate);
    Mockito.lenient().when(hibernateQuery.getResultList()).thenReturn(list);

    List<ScheduledTasks> result = dao.getScheduledTasksBySearchQuery("launch");

    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testGetScheduledTasksBySearchQuery_blankTerm_returnsEmpty() {
    Mockito.lenient().when(hibernateQuery.getResultList()).thenReturn(Arrays.asList());

    List<ScheduledTasks> result = dao.getScheduledTasksBySearchQuery("   ");

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetScheduledTasks_noResults_returnsEmpty() {
    Mockito.lenient().when(hibernateQuery.getResultList()).thenReturn(Arrays.asList());

    List<ScheduledTasks> result = dao.getScheduledTasks();

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetScheduledTasks_withResults_returnsList() {
    List<ScheduledTasks> list = Arrays.asList(new ScheduledTasks());
    Mockito.lenient().when(hibernateQuery.getResultList()).thenReturn(list);

    List<ScheduledTasks> result = dao.getScheduledTasks();

    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testSaveOrUpdate_invokesSession() {
    ScheduledTasks t = new ScheduledTasks();

    ScheduledTasks returned = dao.saveOrUpdate(t);

    verify(session).saveOrUpdate(t);
    assertSame(t, returned);
  }

  @Test
  public void testDelete_invokesSession() {
    ScheduledTasks t = new ScheduledTasks();

    ScheduledTasks returned = dao.delete(t);

    verify(session).delete(t);
    assertSame(t, returned);
  }
}
