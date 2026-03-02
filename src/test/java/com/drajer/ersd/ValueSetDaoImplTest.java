package com.drajer.ersd;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.ersd.dao.impl.ValueSetDaoImpl;
import com.drajer.ersd.model.PlanDefinitionActionModel;
import com.drajer.ersd.model.ValueSetGrouperModel;
import com.drajer.ersd.model.ValueSetModel;
import java.util.List;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ValueSetDaoImplTest {

  @Mock private Session session;

  @Mock private SessionFactory sessionFactory;

  @Mock private Transaction transaction;

  @InjectMocks private ValueSetDaoImpl valueSetDaoImpl;

  @BeforeEach
  void setUp() {

    // Mock Hibernate session lifecycle
    when(sessionFactory.openSession()).thenReturn(session);
    when(session.beginTransaction()).thenReturn(transaction);
    when(session.getTransaction()).thenReturn(transaction);
  }

  @Test
  void getAllValuesets_Test_success() {

    // If DAO returns empty list by default
    List<ValueSetModel> valueSetModelList = valueSetDaoImpl.getAllValuesets();

    assertNotNull(valueSetModelList);
    assertTrue(valueSetModelList.isEmpty());
  }

  @Test
  void createValueset_Test() {

    ValueSetModel valueSetModel = new ValueSetModel();
    valueSetModel.setValueSetId("123");

    valueSetDaoImpl.createValueset(valueSetModel);

    verify(sessionFactory).openSession();
    verify(session).beginTransaction();
    verify(transaction).commit();
    verify(session).close();
  }

  @Test
  void createValuesetGrouper_Test() {

    ValueSetGrouperModel valueSetGrouperModel = new ValueSetGrouperModel();
    valueSetGrouperModel.setId("123");

    valueSetDaoImpl.createValuesetGrouper(valueSetGrouperModel);

    verify(sessionFactory).openSession();
    verify(session).beginTransaction();
    verify(transaction).commit();
    verify(session).close();
  }

  @Test
  void createPlanDefinitionActions_Test() {

    PlanDefinitionActionModel planDefinitionActionModel = new PlanDefinitionActionModel();

    valueSetDaoImpl.createPlanDefinitionActions(planDefinitionActionModel);

    verify(sessionFactory).openSession();
    verify(session).beginTransaction();
    verify(transaction).commit();
    verify(session).close();
  }

  @Test
  void getValueSetById_Test() {

    ValueSetModel result = valueSetDaoImpl.getValueSetById(123);

    assertNull(result);
  }
}
