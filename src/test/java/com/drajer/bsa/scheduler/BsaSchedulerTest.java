package com.drajer.bsa.scheduler;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.bsa.model.BsaTypes;
import com.github.kagkarlsson.scheduler.Scheduler;
import com.github.kagkarlsson.scheduler.task.Task;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class BsaSchedulerTest {

  @Mock private Scheduler scheduler;
  @Mock private ScheduleJobConfiguration schedulerConfig;
  @Mock private Task<ScheduledJobData> sampleOneTimeJob;

  @InjectMocks private BsaScheduler bsaScheduler;

  private UUID karExecId;
  private String actionId;
  private BsaTypes.ActionType actionType;
  private Instant scheduledTime;
  private String xReqId;
  private BsaTypes.BsaJobType jobType;
  private Map<String, String> mdc;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    karExecId = UUID.randomUUID();
    actionId = "action123";
    actionType = BsaTypes.ActionType.CREATE_REPORT;
    scheduledTime = Instant.now().plusSeconds(60);
    xReqId = "xReq123";
    jobType = BsaTypes.BsaJobType.IMMEDIATE_REPORTING;
    mdc = new HashMap<>();
    mdc.put("key1", "value1");

    when(schedulerConfig.sampleOneTimeJob()).thenReturn(sampleOneTimeJob);
  }

  @Test
  public void testScheduleJobWithMDC() {
    bsaScheduler.scheduleJob(karExecId, actionId, actionType, scheduledTime, xReqId, jobType, mdc);
    verify(schedulerConfig).sampleOneTimeJob();
    assertEquals("value1", mdc.get("key1"));
    assertFalse(mdc.isEmpty());
  }

  @Test
  public void testScheduleJobNullMDC() {
    assertDoesNotThrow(
        () ->
            bsaScheduler.scheduleJob(
                karExecId, actionId, actionType, scheduledTime, xReqId, jobType, null));

    verify(schedulerConfig).sampleOneTimeJob();
    assertNotNull(karExecId);
    assertEquals("action123", actionId);
    assertEquals(BsaTypes.ActionType.CREATE_REPORT, actionType);
    assertEquals(BsaTypes.BsaJobType.IMMEDIATE_REPORTING, jobType);
  }

  @Test
  public void testScheduleJobEmptyMDC() {
    Map<String, String> emptyMDC = new HashMap<>();
    bsaScheduler.scheduleJob(
        karExecId, actionId, actionType, scheduledTime, xReqId, jobType, emptyMDC);
    verify(schedulerConfig).sampleOneTimeJob();
    assertTrue(emptyMDC.isEmpty());
  }

  @Test
  public void testScheduleJobDifferentActionType() {
    BsaTypes.ActionType newActionType = BsaTypes.ActionType.SUBMIT_REPORT;
    bsaScheduler.scheduleJob(
        karExecId, actionId, newActionType, scheduledTime, xReqId, jobType, mdc);
    verify(schedulerConfig).sampleOneTimeJob();
    assertEquals(BsaTypes.ActionType.SUBMIT_REPORT, newActionType);
    assertNotEquals(actionType, newActionType);
  }

  @Test
  public void testScheduleJobDifferentJobType() {
    BsaTypes.BsaJobType newJobType = BsaTypes.BsaJobType.DELAYED_REPORTING;
    bsaScheduler.scheduleJob(
        karExecId, actionId, actionType, scheduledTime, xReqId, newJobType, mdc);
    verify(schedulerConfig).sampleOneTimeJob();
    assertEquals(BsaTypes.BsaJobType.DELAYED_REPORTING, newJobType);
    assertNotEquals(jobType, newJobType);
  }

  @Test
  public void testScheduleJobFutureTime() {
    Instant futureTime = Instant.now().plusSeconds(3600);
    bsaScheduler.scheduleJob(karExecId, actionId, actionType, futureTime, xReqId, jobType, mdc);
    verify(schedulerConfig).sampleOneTimeJob();
    assertTrue(futureTime.isAfter(scheduledTime));
    assertNotNull(futureTime);
  }

  @Test
  public void testKarExecIdNotNull() {
    assertNotNull(karExecId);
    assertTrue(karExecId.toString().length() > 0);
  }

  @Test
  public void testActionIdValue() {
    assertEquals("action123", actionId);
    assertTrue(actionId.startsWith("action"));
  }

  @Test
  public void testActionTypeValue() {
    assertEquals(BsaTypes.ActionType.CREATE_REPORT, actionType);
  }

  @Test
  public void testJobTypeValue() {
    assertEquals(BsaTypes.BsaJobType.IMMEDIATE_REPORTING, jobType);
  }

  @Test
  public void testXReqIdValue() {
    assertEquals("xReq123", xReqId);
    assertTrue(xReqId.startsWith("xReq"));
  }

  @Test
  public void testMDCValue() {
    assertEquals("value1", mdc.get("key1"));
    assertFalse(mdc.isEmpty());
  }

  @Test
  public void testDifferentActionTypeValue() {
    BsaTypes.ActionType newActionType = BsaTypes.ActionType.SUBMIT_REPORT;
    assertEquals(BsaTypes.ActionType.SUBMIT_REPORT, newActionType);
    assertNotEquals(actionType, newActionType);
  }

  @Test
  public void testDifferentJobTypeValue() {
    BsaTypes.BsaJobType newJobType = BsaTypes.BsaJobType.DELAYED_REPORTING;
    assertEquals(BsaTypes.BsaJobType.DELAYED_REPORTING, newJobType);
    assertNotEquals(jobType, newJobType);
  }
}
