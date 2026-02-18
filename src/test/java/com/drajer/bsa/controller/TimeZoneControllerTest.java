package com.drajer.bsa.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

import com.drajer.bsa.service.TimeZoneService;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
public class TimeZoneControllerTest {

  @Mock private TimeZoneService timeZoneService;

  @InjectMocks private TimeZoneController timeZoneController;

  @Before
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void TimeZoneController_test() {

    String timeZone = "UTC";
    doNothing().when(timeZoneService).setDatabaseTimezone(timeZone);

    ResponseEntity<String> response = timeZoneController.setTimeZone(timeZone);

    assertEquals(HttpStatus.OK, response.getStatusCode());

    verify(timeZoneService, times(1)).setDatabaseTimezone(timeZone);
  }

  @Test
  public void testGetDatabaseTimezone() {

    String expectedTimeZone = "UTC";
    when(timeZoneService.getDatabaseTimezone()).thenReturn(expectedTimeZone);

    String response = timeZoneController.getDatabaseTimezone();
    assertEquals(expectedTimeZone, response);
    verify(timeZoneService, times(1)).getDatabaseTimezone();
  }

  @Test
  public void testSetTimeZone_ExceptionHandling() {

    String timeZone = "UTC";

    doThrow(new RuntimeException("Database error"))
        .when(timeZoneService)
        .setDatabaseTimezone(timeZone);

    ResponseEntity<String> response = timeZoneController.setTimeZone(timeZone);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertTrue(response.getBody().contains("Error updating time zone: Database error"));

    verify(timeZoneService, times(1)).setDatabaseTimezone(timeZone);
  }
}
