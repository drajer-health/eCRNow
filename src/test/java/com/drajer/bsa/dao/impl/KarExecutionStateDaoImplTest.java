package com.drajer.bsa.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.times;

import com.drajer.bsa.model.KarExecutionState;
import java.util.List;
import java.util.UUID;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.criterion.Order;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class KarExecutionStateDaoImplTest {

  @Mock private SessionFactory sessionFactory;

  @Mock private Session session;

  @Mock private Criteria criteria;

  @InjectMocks private KarExecutionStateDaoImpl karExecutionStateDaoImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.createCriteria(KarExecutionState.class)).thenReturn(criteria);
  }

  @Test
  public void testSaveOrUpdate() {
    KarExecutionState kar = new KarExecutionState();
    kar.setNcId(UUID.randomUUID());
    kar.setHsFhirServerUrl("http://example.com/fhir");
    kar.setKarUniqueId("kar-12345");

    assertNotNull(kar);
    assertEquals("http://example.com/fhir", kar.getHsFhirServerUrl());
    assertEquals("kar-12345", kar.getKarUniqueId());

    karExecutionStateDaoImpl.saveOrUpdate(kar);

    verify(session, times(1)).saveOrUpdate(kar);
  }

  @Test
  public void testGetKarExecutionStateById() {
    UUID id = UUID.randomUUID();
    KarExecutionState kar = new KarExecutionState();
    kar.setNcId(UUID.randomUUID());
    kar.setHsFhirServerUrl("http://example.com/fhir");
    kar.setKarUniqueId("kar-12345");

    when(session.get(KarExecutionState.class, id)).thenReturn(kar);

    KarExecutionState result = karExecutionStateDaoImpl.getKarExecutionStateById(id);

    assertNotNull(result);
    assertEquals(kar, result);
    assertEquals("http://example.com/fhir", result.getHsFhirServerUrl());
    assertEquals("kar-12345", result.getKarUniqueId());
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
    when(sessionFactory.getCurrentSession()).thenReturn(session);
    when(session.createCriteria(KarExecutionState.class)).thenReturn(criteria);
    when(criteria.addOrder(any(Order.class))).thenReturn(criteria);

    List<KarExecutionState> karList = mock(List.class);
    when(criteria.list()).thenReturn(karList);

    List<KarExecutionState> result = karExecutionStateDaoImpl.getAllKarExecutionStates();

    assertNotNull(result);
    assertEquals(karList, result);
    assertSame(karList, result);
  }

  @Test
  public void testDelete() {
    KarExecutionState kar = new KarExecutionState();
    kar.setNcId(UUID.randomUUID());
    kar.setHsFhirServerUrl("http://example.com/fhir");
    kar.setKarUniqueId("kar-67890");

    assertNotNull(kar);
    assertEquals("http://example.com/fhir", kar.getHsFhirServerUrl());
    assertEquals("kar-67890", kar.getKarUniqueId());

    karExecutionStateDaoImpl.delete(kar);

    verify(session, times(1)).delete(kar);
  }
}
