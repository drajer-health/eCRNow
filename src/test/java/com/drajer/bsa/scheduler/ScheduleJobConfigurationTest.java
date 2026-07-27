package com.drajer.bsa.scheduler;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.service.KarProcessor;
import com.github.kagkarlsson.scheduler.SchedulerClient;
import com.github.kagkarlsson.scheduler.SchedulerState;
import com.github.kagkarlsson.scheduler.task.Execution;
import com.github.kagkarlsson.scheduler.task.ExecutionContext;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.TaskInstance;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.hibernate.ObjectDeletedException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class ScheduleJobConfigurationTest {

  @Mock private KarProcessor karProcessor;

  @Mock private SchedulerClient schedulerClient;

  @Mock private SchedulerState schedulerState;

  private ScheduleJobConfiguration config;
  private Task<ScheduledJobData> task;

  @Before
  public void setUp() {
    config = new ScheduleJobConfiguration();

    ReflectionTestUtils.setField(config, "timerRetries", 3);

    task = config.sampleOneTimeJob(karProcessor);

    assertNotNull("Task should be initialized", task);
    assertEquals("Task name should match", "BsaScheduledJob", task.getName());
  }

  @After
  public void tearDown() {

    MDC.clear();
  }

  @Test
  public void testSuccessfulExecution() {

    UUID jobId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "create-eicr",
            BsaTypes.ActionType.CREATE_REPORT,
            Instant.now().plusSeconds(3600),
            "scheduled-job-001",
            "request-id-123",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            null);

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-001", data);
    Execution execution =
        new Execution(
            Instant.now(),
            taskInstance,
            true,
            "scheduler-instance-1",
            Instant.now().minusSeconds(10),
            null,
            0,
            Instant.now(),
            1L);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    // Act
    try {
      task.execute(taskInstance, ctx);

      assertTrue("Execution should complete successfully", true);
    } catch (Exception e) {
      fail("Execution should not throw exception: " + e.getMessage());
    }

    verify(karProcessor, times(1)).applyKarForScheduledJob(eq(data), eq(taskInstance), eq(ctx));
  }

  @Test
  public void testMdcContextPresent() {

    UUID jobId = UUID.randomUUID();
    Map<String, String> mdcContext = new HashMap<>();
    mdcContext.put("traceId", "trace-uuid-12345");
    mdcContext.put("userId", "user-789");
    mdcContext.put("requestId", "req-456");

    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "validate-eicr",
            BsaTypes.ActionType.VALIDATE_REPORT,
            Instant.now().plusSeconds(3600),
            "scheduled-job-002",
            "request-id-456",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            mdcContext);

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-002", data);
    Execution execution = new Execution(Instant.now(), taskInstance);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    assertNull("MDC should be cleared before test", MDC.get("traceId"));

    try {
      task.execute(taskInstance, ctx);
    } catch (Exception e) {
      fail("Execution should not throw exception: " + e.getMessage());
    }

    assertNull("MDC should be cleared in finally block", MDC.get("traceId"));

    verify(karProcessor, times(1)).applyKarForScheduledJob(any(), any(), any());
  }

  /**
   * Test 3: MDC context absent. Verifies that when MDC context is null in ScheduledJobData, the
   * task handles it gracefully.
   */
  @Test
  public void testMdcContextAbsent() {
    // Arrange
    UUID jobId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "submit-eicr",
            BsaTypes.ActionType.SUBMIT_REPORT, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "scheduled-job-003",
            "request-id-789",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            null // MDC context is null
            );

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-003", data);
    Execution execution = new Execution(Instant.now(), taskInstance);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    // Act
    try {
      task.execute(taskInstance, ctx);
      // Should complete without exception
      assertTrue("Execution should complete successfully with null MDC context", true);
    } catch (Exception e) {
      fail("Execution should not throw exception when MDC is null: " + e.getMessage());
    }

    // Verify
    verify(karProcessor, times(1)).applyKarForScheduledJob(any(), any(), any());
  }

  /**
   * Test 4: ObjectDeletedException handling. Verifies that ObjectDeletedException is caught and
   * handled gracefully, and the finally block is executed. This exception is caught and logged but
   * not rethrown.
   */
  @Test
  public void testObjectDeletedExceptionPath() {
    // Arrange
    UUID jobId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "delete-kar-ref",
            BsaTypes.ActionType.COMPLETE_REPORTING, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "scheduled-job-004",
            "request-id-deleted",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            null);

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-004", data);
    Execution execution = new Execution(Instant.now(), taskInstance);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    // Mock to throw ObjectDeletedException
    // ✅ FIXED: Use correct 3-argument constructor (message, entityId, entityName)
    UUID entityId = UUID.randomUUID();
    doThrow(
            new ObjectDeletedException(
                "Entity was deleted during processing",
                entityId,
                "KarExecutionState" // Entity name - required 3rd argument
                ))
        .when(karProcessor)
        .applyKarForScheduledJob(any(), any(), any());

    // Act - ObjectDeletedException should be caught and NOT rethrown
    try {
      task.execute(taskInstance, ctx);
      // Should complete without throwing
      assertTrue("ObjectDeletedException should be caught", true);
    } catch (Exception e) {
      fail("ObjectDeletedException should be caught, not rethrown: " + e.getMessage());
    }

    // Verify
    verify(karProcessor, times(1)).applyKarForScheduledJob(any(), any(), any());
  }

  /**
   * Test 5: Generic exception with retries remaining. Verifies that when a generic exception is
   * thrown and consecutive failures < timerRetries, the exception is rethrown for the scheduler to
   * retry.
   *
   * <p>Setup: timerRetries = 3, consecutiveFailures = 1 Condition: (1 + 1) < 3 = true → retries
   * remaining → rethrow
   */
  @Test
  public void testGenericExceptionWithRetriesRemaining() {
    // Arrange
    UUID jobId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "process-action",
            BsaTypes.ActionType.EVALUATE_CONDITION, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "scheduled-job-005",
            "request-id-retry",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            null);

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-005", data);

    // consecutiveFailures = 1, timerRetries = 3
    // Condition: (1 + 1) < 3 → retries remaining
    Execution execution =
        new Execution(
            Instant.now(),
            taskInstance,
            true,
            "scheduler-instance-1",
            null,
            Instant.now().minusSeconds(30),
            1, // consecutiveFailures = 1
            Instant.now(),
            1L);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    // Mock to throw a generic RuntimeException
    RuntimeException testException =
        new RuntimeException("Processing error during action execution");
    doThrow(testException).when(karProcessor).applyKarForScheduledJob(any(), any(), any());

    // Act & Assert - Exception should be rethrown for retry
    RuntimeException caughtException = null;
    try {
      task.execute(taskInstance, ctx);
      fail("Exception should be rethrown for retry");
    } catch (RuntimeException e) {
      caughtException = e;
    }

    // Verify
    assertNotNull("Exception should be rethrown", caughtException);
    assertEquals(
        "Exception message should match",
        "Processing error during action execution",
        caughtException.getMessage());

    verify(karProcessor, times(1)).applyKarForScheduledJob(any(), any(), any());
  }

  /**
   * Test 6: Generic exception when retries exhausted. Verifies that when a generic exception is
   * thrown and consecutive failures >= timerRetries, the exception is caught and NOT rethrown
   * (scheduler stops retrying).
   *
   * <p>Setup: timerRetries = 3, consecutiveFailures = 2 Condition: (2 + 1) >= 3 = true → retries
   * exhausted → do not rethrow
   */
  @Test
  public void testGenericExceptionRetriesExhausted() {
    // Arrange
    UUID jobId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "final-action",
            BsaTypes.ActionType.TERMINATE_REPORTING_WORKFLOW, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "scheduled-job-006",
            "request-id-exhausted",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            null);

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-006", data);

    // consecutiveFailures = 2, timerRetries = 3
    // Condition: (2 + 1) >= 3 → retries exhausted
    Execution execution =
        new Execution(
            Instant.now(),
            taskInstance,
            true,
            "scheduler-instance-1",
            null,
            Instant.now().minusSeconds(60),
            2, // consecutiveFailures = 2
            Instant.now(),
            1L);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    // Mock to throw a generic RuntimeException
    doThrow(new RuntimeException("Final processing error"))
        .when(karProcessor)
        .applyKarForScheduledJob(any(), any(), any());

    // Act - Exception should NOT be rethrown (max retries reached)
    try {
      task.execute(taskInstance, ctx);
      // Should complete without throwing
      assertTrue("Exception should be caught when retries exhausted", true);
    } catch (Exception e) {
      fail("Exception should be caught and NOT rethrown when retries exhausted: " + e.getMessage());
    }

    // Verify
    verify(karProcessor, times(1)).applyKarForScheduledJob(any(), any(), any());
  }

  /**
   * Test 7: MDC.clear() executes in finally block. Verifies that cleanup happens even when an
   * exception occurs. This is critical for preventing MDC context pollution across different task
   * executions.
   */
  @Test
  public void testMdcClearExecutesInFinallyBlock() {
    // Arrange
    UUID jobId = UUID.randomUUID();
    Map<String, String> mdcContext = new HashMap<>();
    mdcContext.put("correlationId", "corr-999");

    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "error-action",
            BsaTypes.ActionType.EVALUATE_MEASURE, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "scheduled-job-007",
            "request-id-final",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            mdcContext);

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-007", data);

    // Set consecutiveFailures to 2, so exception will be caught (not rethrown)
    Execution execution =
        new Execution(
            Instant.now(),
            taskInstance,
            true,
            "scheduler-instance-1",
            null,
            null,
            2, // retries exhausted
            Instant.now(),
            1L);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    // Mock to throw an exception that will be caught
    doThrow(new RuntimeException("Error in finally test"))
        .when(karProcessor)
        .applyKarForScheduledJob(any(), any(), any());

    // Act - execute task
    try {
      task.execute(taskInstance, ctx);
    } catch (Exception e) {
      fail("Exception should be caught due to exhausted retries: " + e.getMessage());
    }

    // Assert - MDC should be cleared after execution (finally block)
    assertNull(
        "MDC should be cleared in finally block even when exception occurs",
        MDC.get("correlationId"));

    // Verify
    verify(karProcessor, times(1)).applyKarForScheduledJob(any(), any(), any());
  }

  /**
   * Test 8: Edge case - retries exactly at threshold. Verifies boundary condition when
   * consecutiveFailures + 1 == timerRetries.
   *
   * <p>Setup: timerRetries = 3, consecutiveFailures = 2 Condition: (2 + 1) >= 3 = true → should not
   * rethrow
   */
  @Test
  public void testExceptionAtRetryThreshold() {
    // Arrange
    UUID jobId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData(
            jobId,
            "threshold-action",
            BsaTypes.ActionType.CHECK_TRIGGER_CODES, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "scheduled-job-008",
            "request-id-threshold",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            null);

    TaskInstance<ScheduledJobData> taskInstance = task.instance("instance-008", data);

    // consecutiveFailures = 2, timerRetries = 3
    // Boundary: (2 + 1) = 3, so (2 + 1) >= 3 is true
    Execution execution =
        new Execution(
            Instant.now(),
            taskInstance,
            true,
            "scheduler-instance-1",
            null,
            Instant.now().minusSeconds(120),
            2,
            Instant.now(),
            1L);
    ExecutionContext ctx = new ExecutionContext(schedulerState, execution, schedulerClient);

    // Mock to throw exception
    doThrow(new RuntimeException("Threshold boundary test"))
        .when(karProcessor)
        .applyKarForScheduledJob(any(), any(), any());

    // Act - Should not throw since we're at the threshold
    try {
      task.execute(taskInstance, ctx);
      assertTrue("Should not throw at retry threshold", true);
    } catch (Exception e) {
      fail("Should not throw exception at retry threshold: " + e.getMessage());
    }
  }

  /**
   * Test 9: Verify task metadata. Confirms that the task is properly configured with correct name
   * and data class.
   */
  @Test
  public void testTaskMetadata() {
    // Assert
    assertEquals("Task name should be BsaScheduledJob", "BsaScheduledJob", task.getName());
    assertEquals(
        "Task data class should be ScheduledJobData", ScheduledJobData.class, task.getDataClass());
    assertNotNull("Task should have a failure handler", task.getFailureHandler());
    assertNotNull("Task should have a dead execution handler", task.getDeadExecutionHandler());
  }

  /**
   * Test 10: Multiple sequential executions. Verifies that the task can be executed multiple times
   * independently without state pollution between executions.
   */
  @Test
  public void testMultipleSequentialExecutions() {
    // Arrange - Create two separate job executions
    UUID jobId1 = UUID.randomUUID();
    ScheduledJobData data1 =
        new ScheduledJobData(
            jobId1,
            "action-1",
            BsaTypes.ActionType.CHECK_PARTICIPANT_REGISTRATION, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "job-1",
            "request-1",
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING,
            null);

    UUID jobId2 = UUID.randomUUID();
    ScheduledJobData data2 =
        new ScheduledJobData(
            jobId2,
            "action-2",
            BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW, // ✅ FIXED: Use actual enum value
            Instant.now().plusSeconds(3600),
            "job-2",
            "request-2",
            BsaTypes.BsaJobType.DELAYED_REPORTING, // ✅ FIXED: Use actual enum value
            null);

    TaskInstance<ScheduledJobData> taskInstance1 = task.instance("instance-1", data1);
    TaskInstance<ScheduledJobData> taskInstance2 = task.instance("instance-2", data2);

    Execution execution1 = new Execution(Instant.now(), taskInstance1);
    Execution execution2 = new Execution(Instant.now().plusSeconds(1), taskInstance2);

    ExecutionContext ctx1 = new ExecutionContext(schedulerState, execution1, schedulerClient);
    ExecutionContext ctx2 = new ExecutionContext(schedulerState, execution2, schedulerClient);

    // Act - Execute both tasks
    try {
      task.execute(taskInstance1, ctx1);
      task.execute(taskInstance2, ctx2);
      // Both should complete without exception
      assertTrue("Both executions should complete successfully", true);
    } catch (Exception e) {
      fail("Sequential executions should not throw exception: " + e.getMessage());
    }

    // Verify both executions happened
    verify(karProcessor, times(2)).applyKarForScheduledJob(any(), any(), any());

    // Verify MDC is cleaned up (no pollution between tests)
    assertNull("MDC should be cleaned up after executions", MDC.get("correlationId"));
  }
}
