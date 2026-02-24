package com.drajer.ersd.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;

import com.drajer.ersd.model.PlanDefinitionActionModel;
import com.drajer.ersd.model.ValueSetGrouperModel;
import com.drajer.ersd.model.ValueSetModel;
import java.util.List;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
public class ValueSetDaoImplTest {

  @Mock private Session session;

  @Mock private SessionFactory sessionFactory;

  @Mock private Transaction transaction;

  @InjectMocks private ValueSetDaoImpl valueSetDaoImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    Mockito.lenient().when(sessionFactory.openSession()).thenReturn(session);
    Mockito.lenient().when(session.beginTransaction()).thenReturn(transaction);
    Mockito.lenient().when(session.getTransaction()).thenReturn(transaction);
  }

  @Test
  public void getAllValuesets_Test_success() {
    List<ValueSetModel> valueSetModelList = valueSetDaoImpl.getAllValuesets();
    assertNotNull(valueSetModelList);
    assertTrue(valueSetModelList.isEmpty());
  }

  @Test
  public void createValueset_Test() {
    ValueSetModel valueSetModel = new ValueSetModel();
    valueSetModel.setValueSetId("123");
    valueSetDaoImpl.createValueset(valueSetModel);

    verify(sessionFactory).openSession();
    verify(session).beginTransaction();
    verify(session).getTransaction();
    verify(transaction).commit();
    verify(session).close();
  }

  @Test
  public void createValuesetGrouper_Test() {
    ValueSetGrouperModel valueSetGrouperModel = new ValueSetGrouperModel();
    valueSetGrouperModel.setId("123");
    valueSetDaoImpl.createValuesetGrouper(valueSetGrouperModel);

    verify(sessionFactory).openSession();
    verify(session).beginTransaction();
    verify(session).getTransaction();
    verify(transaction).commit();
    verify(session).close();
  }

  @Test
  public void createPlanDefinitionActions_Test() {
    PlanDefinitionActionModel planDefinitionActionModel = new PlanDefinitionActionModel();
    valueSetDaoImpl.createPlanDefinitionActions(planDefinitionActionModel);

    verify(sessionFactory).openSession();
    verify(session).beginTransaction();
    verify(session).getTransaction();
    verify(transaction).commit();
    verify(session).close();
  }

  @Test
  public void getValueSetById_Test() {
    ValueSetModel result = valueSetDaoImpl.getValueSetById(123);
    assertNull(result);
  }
}
