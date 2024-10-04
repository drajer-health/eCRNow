package com.drajer.bsa.service.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import com.drajer.bsa.dao.TimeZoneDao;
import com.drajer.ecrapp.config.QueryReaderConfig;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.server.ResponseStatusException;

@ExtendWith(MockitoExtension.class)
public class TimeZoneServiceImplTest {

  @Mock private TimeZoneDao timeZoneDao;

  @Mock private QueryReaderConfig queryReaderConfig;

  @InjectMocks private TimeZoneServiceImpl timeZoneService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testGetDatabaseTimezone_Success() {
    String expectedQuery = "SELECT timezone FROM settings";
    String expectedTimeZone = "America/New_York";

    when(queryReaderConfig.getQuery("query.getTimezone")).thenReturn(expectedQuery);
    when(timeZoneDao.getDatabaseTimezone(expectedQuery)).thenReturn(expectedTimeZone);

    String actualTimeZone = timeZoneService.getDatabaseTimezone();
    assertEquals(expectedTimeZone, actualTimeZone);
  }

  @Test(expected = ResponseStatusException.class)
  public void testGetDatabaseTimezone_QueryNotFound() {
    when(queryReaderConfig.getQuery("query.getTimezone")).thenReturn(null);

    timeZoneService.getDatabaseTimezone();
  }

  @Test(expected = ResponseStatusException.class)
  public void testGetDatabaseTimezone_DaoException() {
    String expectedQuery = "SELECT timezone FROM settings";

    when(queryReaderConfig.getQuery("query.getTimezone")).thenReturn(expectedQuery);
    when(timeZoneDao.getDatabaseTimezone(expectedQuery))
        .thenThrow(new RuntimeException("Database error"));

    timeZoneService.getDatabaseTimezone();
  }

  @Test
  public void testSetDatabaseTimezone_Success() {
    String expectedQuery = "UPDATE settings SET timezone = ?";
    String timeZone = "Europe/London";

    when(queryReaderConfig.getQuery("query.setTimezone")).thenReturn(expectedQuery);

    timeZoneService.setDatabaseTimezone(timeZone);
  }

  @Test(expected = ResponseStatusException.class)
  public void testSetDatabaseTimezone_QueryNotFound() {
    when(queryReaderConfig.getQuery("query.setTimezone")).thenReturn(null);

    timeZoneService.setDatabaseTimezone("America/Los_Angeles");
  }

  @Test(expected = ResponseStatusException.class)
  public void testSetDatabaseTimezone_InvalidTimeZone() {
    String expectedQuery = "UPDATE settings SET timezone = ";

    when(queryReaderConfig.getQuery("query.setTimezone")).thenReturn(expectedQuery);

    timeZoneService.setDatabaseTimezone(null);
  }
}
