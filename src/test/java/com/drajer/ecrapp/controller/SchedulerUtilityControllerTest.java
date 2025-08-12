package com.drajer.ecrapp.controller;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.ecrapp.util.ScheduledTaskUtil;
import java.io.IOException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class SchedulerUtilityControllerTest {

  @Mock private ScheduledTaskUtil scheduledTaskUtil;

  @InjectMocks private SchedulerUtilityController schedulerUtilityController;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this); // Initialize mocks
  }

  @Test
  public void testExportScheduledTasks_Success() throws IOException {
    String mockFilePath = "/path/to/exported/tasks.json";
    when(scheduledTaskUtil.exportScheduledTasks()).thenReturn(mockFilePath);
    ResponseEntity<String> response = schedulerUtilityController.exportScheduledTasks();
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(
        "Scheduled tasks exported successfully to file: " + mockFilePath, response.getBody());

    verify(scheduledTaskUtil, times(1)).exportScheduledTasks();
  }

  @Test
  public void testExportScheduledTasks_Failure() throws IOException {

    when(scheduledTaskUtil.exportScheduledTasks()).thenThrow(new IOException("Export failed"));

    ResponseEntity<String> response = schedulerUtilityController.exportScheduledTasks();
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("Error exporting scheduled tasks: Export failed", response.getBody());

    verify(scheduledTaskUtil, times(1)).exportScheduledTasks();
  }

  @Test
  public void testImportScheduledTasks_Success() throws IOException {
    String mockFilePath = "/path/to/imported/tasks.json";

    when(scheduledTaskUtil.importScheduledTasks()).thenReturn(mockFilePath);
    ResponseEntity<String> response = schedulerUtilityController.importScheduledTasks();
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(
        "Scheduled tasks imported successfully from file: " + mockFilePath, response.getBody());

    verify(scheduledTaskUtil, times(1)).importScheduledTasks();
  }

  @Test
  public void testImportScheduledTasks_Failure() throws IOException {

    when(scheduledTaskUtil.importScheduledTasks()).thenThrow(new IOException("Import failed"));

    ResponseEntity<String> response = schedulerUtilityController.importScheduledTasks();

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("Error importing scheduled tasks: Import failed", response.getBody());
    verify(scheduledTaskUtil, times(1)).importScheduledTasks();
  }
}
