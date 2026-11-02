package com.drajer.ersd;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.drajer.ersd.dao.impl.ValueSetDaoImpl;
import com.drajer.ersd.model.PlanDefinitionActionModel;
import com.drajer.ersd.model.ValueSetGrouperModel;
import com.drajer.ersd.model.ValueSetModel;
import java.util.List;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class ValueSetDaoImplTest {

  @Mock private Session session;

  @Mock private SessionFactory sessionFactory;

  @Mock private Transaction transaction;

  @InjectMocks private ValueSetDaoImpl valueSetDaoImpl;

  /**
   * Stubs the Hibernate session lifecycle. Only the create* methods open a session, so only those
   * tests call this; stubbing it for every test makes strict Mockito fail the read tests with
   * UnnecessaryStubbingException.
   */
  private void mockSessionLifecycle() {
    when(sessionFactory.openSession()).thenReturn(session);
    when(session.beginTransaction()).thenReturn(transaction);
    when(session.getTransaction()).thenReturn(transaction);
  }

  private void verifySessionCommittedAndClosed() {
    verify(sessionFactory).openSession();
    verify(session).beginTransaction();
    verify(transaction).commit();
    verify(session).close();
  }

  @Test
  public void getAllValuesets_returnsEmptyListWithoutOpeningASession() {

    List<ValueSetModel> valueSetModelList = valueSetDaoImpl.getAllValuesets();

    assertNotNull(valueSetModelList);
    assertTrue(valueSetModelList.isEmpty());
    verifyNoInteractions(sessionFactory);
  }

  @Test
  public void getValueSetById_returnsNullWithoutOpeningASession() {

    ValueSetModel result = valueSetDaoImpl.getValueSetById(123);

    assertNull(result);
    verifyNoInteractions(sessionFactory);
  }

  @Test
  public void createValueset_commitsAndClosesSession() {
    mockSessionLifecycle();

    ValueSetModel valueSetModel = new ValueSetModel();
    valueSetModel.setValueSetId("123");

    valueSetDaoImpl.createValueset(valueSetModel);

    verifySessionCommittedAndClosed();
  }

  @Test
  public void createValuesetGrouper_commitsAndClosesSession() {
    mockSessionLifecycle();

    ValueSetGrouperModel valueSetGrouperModel = new ValueSetGrouperModel();
    valueSetGrouperModel.setId("123");

    valueSetDaoImpl.createValuesetGrouper(valueSetGrouperModel);

    verifySessionCommittedAndClosed();
  }

  @Test
  public void createPlanDefinitionActions_commitsAndClosesSession() {
    mockSessionLifecycle();

    valueSetDaoImpl.createPlanDefinitionActions(new PlanDefinitionActionModel());

    verifySessionCommittedAndClosed();
  }
}
