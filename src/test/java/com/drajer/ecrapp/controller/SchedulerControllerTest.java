package com.drajer.ecrapp.controller;

import static org.junit.Assert.*;

import com.drajer.bsa.controller.SchedulerController;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.service.SchedulerService;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class SchedulerControllerTest {

  @InjectMocks private SchedulerController schedulerController;

  @Mock private SchedulerService schedulerService;

  private List<ScheduledTasks> scheduledTasksList;

  @Before
  public void setUp() {
    ScheduledTasks scheduledTasks1 = new ScheduledTasks();
    scheduledTasks1.setTask_instance("task1");
    scheduledTasks1.setTask_name("taskName");
    scheduledTasks1.setTask_data(new byte[] {1, 2, 3});

    ScheduledTasks scheduledTasks2 = new ScheduledTasks();
    scheduledTasks2.setTask_instance("task2");
    scheduledTasks2.setTask_name("taskName");
    scheduledTasks2.setTask_data(new byte[] {1, 2, 3});

    scheduledTasksList = Arrays.asList(scheduledTasks1, scheduledTasks2);
  }

  @Test
  public void testGetScheduledTasks() {
    Mockito.lenient()
        .when(schedulerService.getScheduledTasks("action1", "launch1"))
        .thenReturn(scheduledTasksList);
    List<ScheduledTasks> result = schedulerController.getScheduledTasks("action1", "launch1");
    assertNotNull(result);
    assertEquals(2, result.size());
  }

  @Test
  public void testGetScheduledTasksBySearchQuery() {
    Mockito.lenient()
        .when(schedulerService.getScheduledTasksBySearchQuery("taskInstance1"))
        .thenReturn(scheduledTasksList);
    List<ScheduledTasks> result =
        schedulerController.getScheduledTasksBySearchQuery("taskInstance1");
    assertNotNull(result);
    assertEquals(2, result.size());
  }

  @Test
  public void testGetScheduledTasks_noActionAndLaunch() {
    Mockito.lenient()
        .when(schedulerService.getScheduledTasks(null, null))
        .thenReturn(scheduledTasksList);
    List<ScheduledTasks> result = schedulerController.getScheduledTasks(null, null);
    assertNotNull(result);
    assertEquals(2, result.size());
  }

  @Test
  public void testGetScheduledTasksBySearchQuery_noTaskInstance() {
    Mockito.lenient()
        .when(schedulerService.getScheduledTasksBySearchQuery(null))
        .thenReturn(scheduledTasksList);
    List<ScheduledTasks> result = schedulerController.getScheduledTasksBySearchQuery(null);
    assertNotNull(result);
    assertEquals(2, result.size());
  }

  @Test
  public void testGetScheduledTasks_withEmptyResult() {
    Mockito.lenient()
        .when(schedulerService.getScheduledTasks("action1", "launch1"))
        .thenReturn(Arrays.asList());
    List<ScheduledTasks> result = schedulerController.getScheduledTasks("action1", "launch1");
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetScheduledTasksBySearchQuery_withEmptyResult() {
    Mockito.lenient()
        .when(schedulerService.getScheduledTasksBySearchQuery("taskInstance1"))
        .thenReturn(Arrays.asList());
    List<ScheduledTasks> result =
        schedulerController.getScheduledTasksBySearchQuery("taskInstance1");
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }
}
