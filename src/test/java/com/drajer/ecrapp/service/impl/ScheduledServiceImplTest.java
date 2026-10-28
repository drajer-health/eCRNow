package com.drajer.ecrapp.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.util.ScheduledTaskUtil;
import com.drajer.test.util.TestUtils;
import com.github.kagkarlsson.scheduler.Scheduler;
import com.github.kagkarlsson.scheduler.task.TaskInstanceId;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

public class ScheduledServiceImplTest {

  private SchedulerDao schedulerDao;
  private KarExecutionStateService karExecutionStateService;
  private Scheduler scheduler;
  private ScheduledTaskUtil scheduledTaskUtil;
  private ScheduledServiceImpl service;

  @Before
  public void setUp() {
    schedulerDao = mock(SchedulerDao.class);
    karExecutionStateService = mock(KarExecutionStateService.class);
    scheduler = mock(Scheduler.class);
    // ScheduledTaskUtil and ScheduledServiceImpl now use field injection (no-arg constructors).
    scheduledTaskUtil = new ScheduledTaskUtil();
    ReflectionTestUtils.setField(scheduledTaskUtil, "schedulerDao", schedulerDao);
    ReflectionTestUtils.setField(
        scheduledTaskUtil, "scheduledTaskFilePath", "scheduled-tasks.json");

    service = new ScheduledServiceImpl();
    service.karExecutionStateService = karExecutionStateService;
    service.schedulerDao = schedulerDao;
    service.scheduledTaskUtil = scheduledTaskUtil;
    service.scheduler = scheduler;
  }

  @Test
  public void getScheduledTasks_returnTheDaoRows() {
    List<ScheduledTasks> rows = List.of(new ScheduledTasks());
    when(schedulerDao.getScheduledTasks("EICRTask", "launch-1")).thenReturn(rows);
    when(schedulerDao.getScheduledTasksBySearchQuery("EICRTask")).thenReturn(rows);

    assertSame(rows, service.getScheduledTasks("EICRTask", "launch-1"));
    assertSame(rows, service.getScheduledTasksBySearchQuery("EICRTask"));
  }

  @Test
  public void delete_cancelsOnlyTheTasksOfTheEncountersKarExecutions() throws Exception {
    NotificationContext context =
        (NotificationContext)
            TestUtils.getResourceAsObject(
                "Bsa/NotificationContext/NotificationContext.json", NotificationContext.class);
    UUID encounterExecution = UUID.randomUUID();
    UUID otherExecution = UUID.randomUUID();
    ScheduledTasks[] tasks = new ScheduledTasks[4];
    UUID[] executions = {encounterExecution, otherExecution, encounterExecution, null};
    for (int i = 0; i < tasks.length; i++) {
      tasks[i] = new ScheduledTasks();
      tasks[i].setTask_name("EICRTask");
      tasks[i].setTask_instance("instance-" + i);
      if (executions[i] != null) {
        tasks[i].setTask_data(
            scheduledTaskUtil.serialize(
                new ScheduledJobData(
                    executions[i],
                    null,
                    BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW,
                    Instant.now(),
                    null,
                    context.getxRequestId(),
                    BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
                    new HashMap<>())));
      } else {
        tasks[i].setTask_data(new byte[] {1, 2, 3});
      }
    }
    when(karExecutionStateService.getExecutionIdsByNotificationContextDetails(
            context.getPatientId(),
            context.getFhirServerBaseUrl(),
            context.getNotificationResourceId()))
        .thenReturn(List.of(encounterExecution.toString()));
    when(schedulerDao.getScheduledTasks()).thenReturn(List.of(tasks));

    List<ScheduledTasks> deleted =
        service.delete(
            "ignored",
            context.getFhirServerBaseUrl(),
            context.getPatientId(),
            context.getNotificationResourceId());

    assertEquals(List.of(tasks[0], tasks[2]), deleted);
    ArgumentCaptor<TaskInstanceId> cancelled = ArgumentCaptor.forClass(TaskInstanceId.class);
    verify(scheduler, times(2)).cancel(cancelled.capture());
    assertEquals("instance-0", cancelled.getAllValues().get(0).getId());
    assertEquals("instance-2", cancelled.getAllValues().get(1).getId());
    assertEquals("EICRTask", cancelled.getAllValues().get(0).getTaskName());
  }

  @Test
  public void delete_withNoScheduledTasks_cancelsNothing() throws Exception {
    when(karExecutionStateService.getExecutionIdsByNotificationContextDetails(any(), any(), any()))
        .thenReturn(List.of());
    when(schedulerDao.getScheduledTasks()).thenReturn(List.of());

    assertTrue(service.delete("id", "https://fhir.example.com", "12345", "67890").isEmpty());
    verify(scheduler, never()).cancel(any(TaskInstanceId.class));
  }
}
