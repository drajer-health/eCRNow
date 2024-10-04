package com.drajer.bsa.dao.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
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
    // Define the query and expected result
    String query = "SELECT current_setting('timezone')";
    String expectedTimeZone = "America/New_York";

    // Ensure nativeQuery mock is properly initialized
    when(session.createNativeQuery(query)).thenReturn(nativeQuery);
    when(nativeQuery.getSingleResult()).thenReturn(expectedTimeZone);

    // Call the DAO method and assert the result
    String actualTimeZone = timeZoneDaoImpl.getDatabaseTimezone(query);
    assertEquals(expectedTimeZone, actualTimeZone);
  }

  @Test
  public void testSetDatabaseTimezone() {
    // Define the query and time zone
    String query = "SET timezone = ";
    String timeZone = "Europe/London";

    String fullQuery = query + "'" + timeZone + "'";

    NativeQuery<?> nq = Mockito.mock(NativeQuery.class);
    Mockito.lenient().when(session.createNativeQuery(anyString())).thenReturn(nq);

    // Ensure nativeQuery mock is properly initialized
    when(nq.executeUpdate()).thenReturn(1);
    // No need to set up return value for executeUpdate() as it does not return anything

    // Call the DAO method (no return value to assert)
    timeZoneDaoImpl.setDatabaseTimezone(query, timeZone);
  }
}
