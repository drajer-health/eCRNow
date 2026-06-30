package com.drajer.bsa.dao.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.NativeQuery;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class TimeZoneDaoImplTest {

  @Mock private SessionFactory sessionFactory;

  @Mock private Session session;

  @Mock private NativeQuery<String> nativeQuery;

  @InjectMocks private TimeZoneDaoImpl timeZoneDaoImpl;

  @Before
  public void setUp() {
    Mockito.lenient().when(sessionFactory.getCurrentSession()).thenReturn(session);
  }

  @Test
  public void testGetDatabaseTimezone() {

    String query = "SELECT current_setting('timezone')";
    String expectedTimeZone = "America/New_York";

    when(session.createNativeQuery(query)).thenReturn(nativeQuery);
    when(nativeQuery.getSingleResult()).thenReturn(expectedTimeZone);

    String actualTimeZone = timeZoneDaoImpl.getDatabaseTimezone(query);

    assertEquals(expectedTimeZone, actualTimeZone);

    verify(session).createNativeQuery(query);
    verify(nativeQuery).getSingleResult();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testSetDatabaseTimezone() {

    String timeZone = "Europe/London";

    NativeQuery<?> query = Mockito.mock(NativeQuery.class);

    when(session.createNativeQuery(anyString())).thenReturn((NativeQuery) query);

    when(query.setParameter("timeZone", timeZone)).thenReturn((NativeQuery) query);

    when(query.executeUpdate()).thenReturn(1);

    timeZoneDaoImpl.setDatabaseTimezone(timeZone);

    Mockito.verify(query).setParameter("timeZone", timeZone);
    Mockito.verify(query).executeUpdate();
  }
}
