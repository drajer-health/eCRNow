package com.drajer.ecrapp.dao.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.drajer.ecrapp.model.ScheduledTasks;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Criterion;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class SchedulerDaoImplTest {

  @InjectMocks private SchedulerDaoImpl schedulerDao;

  @Mock private SessionFactory sessionFactory;

  @Mock private Session session;

  @Mock private Criteria criteria;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.createCriteria(ScheduledTasks.class)).thenReturn(criteria);
  }

  @Test
  public void testGetScheduledTasks1() {
    List<ScheduledTasks> mockTasks = new ArrayList<>();
    mockTasks.add(new ScheduledTasks());
    when(criteria.add(any(Criterion.class))).thenReturn(criteria);
    when(criteria.list()).thenReturn(mockTasks);

    List<ScheduledTasks> result = schedulerDao.getScheduledTasks("actionType", "launchId");

    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testGetScheduledTasksBySearchQuery() {
    List<ScheduledTasks> mockTasks = new ArrayList<>();
    mockTasks.add(new ScheduledTasks());
    when(criteria.add(any(Criterion.class))).thenReturn(criteria);
    when(criteria.list()).thenReturn(mockTasks);

    List<ScheduledTasks> result = schedulerDao.getScheduledTasksBySearchQuery("taskInstance");

    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testGetScheduledTasks() {
    List<ScheduledTasks> mockTasks = new ArrayList<>();
    when(criteria.list()).thenReturn(mockTasks);

    List<ScheduledTasks> result = schedulerDao.getScheduledTasks();

    assertNotNull(result);
    assertEquals(Collections.EMPTY_LIST, result);
  }

  @Test
  public void testSaveOrUpdate() {
    ScheduledTasks task = new ScheduledTasks();
    doNothing().when(session).saveOrUpdate(task);

    ScheduledTasks result = schedulerDao.saveOrUpdate(task);

    assertNotNull(result);
    assertEquals(task, result);
  }
}
