package com.drajer.bsa.scheduler;

import static org.junit.Assert.assertEquals;

import com.drajer.bsa.model.BsaTypes;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;

public class ScheduledJobDataTest {

  private ScheduledJobData scheduledJobData;
  private UUID karExecutionStateId;
  private String actionId;
  private BsaTypes.ActionType actionType;
  private Instant expirationTime;
  private String jobId;
  private Map<String, String> mdcContext;
  private String xRequestId;
  private BsaTypes.BsaJobType jobType;

  @Before
  public void setUp() {
    karExecutionStateId = UUID.randomUUID();
    actionId = "action123";
    actionType = BsaTypes.ActionType.CREATE_REPORT;
    expirationTime = Instant.now();
    jobId = "job123";
    mdcContext = new HashMap<>();
    mdcContext.put("key1", "value1");
    xRequestId = "xReq123";
    jobType = BsaTypes.BsaJobType.IMMEDIATE_REPORTING;

    scheduledJobData =
        new ScheduledJobData(
            karExecutionStateId,
            actionId,
            actionType,
            expirationTime,
            jobId,
            xRequestId,
            jobType,
            mdcContext);
  }

  @Test
  public void testGetKarExecutionStateId() {
    assertEquals(karExecutionStateId, scheduledJobData.getKarExecutionStateId());
  }

  @Test
  public void testSetKarExecutionStateId() {
    UUID newId = UUID.randomUUID();
    scheduledJobData.setKarExecutionStateId(newId);
    assertEquals(newId, scheduledJobData.getKarExecutionStateId());
  }

  @Test
  public void testGetActionId() {
    assertEquals(actionId, scheduledJobData.getActionId());
  }

  @Test
  public void testSetActionId() {
    String newActionId = "newAction123";
    scheduledJobData.setActionId(newActionId);
    assertEquals(newActionId, scheduledJobData.getActionId());
  }

  @Test
  public void testGetActionType() {
    assertEquals(actionType, scheduledJobData.getActionType());
  }

  @Test
  public void testSetActionType() {
    BsaTypes.ActionType newActionType = BsaTypes.ActionType.SUBMIT_REPORT;
    scheduledJobData.setActionType(newActionType);
    assertEquals(newActionType, scheduledJobData.getActionType());
  }

  @Test
  public void testGetExpirationTime() {
    assertEquals(expirationTime, scheduledJobData.getExpirationTime());
  }

  @Test
  public void testSetExpirationTime() {
    Instant newExpirationTime = Instant.now().plusSeconds(3600);
    scheduledJobData.setExpirationTime(newExpirationTime);
    assertEquals(newExpirationTime, scheduledJobData.getExpirationTime());
  }

  @Test
  public void testGetJobId() {
    assertEquals(jobId, scheduledJobData.getJobId());
  }

  @Test
  public void testSetJobId() {
    String newJobId = "newJob123";
    scheduledJobData.setJobId(newJobId);
    assertEquals(newJobId, scheduledJobData.getJobId());
  }

  @Test
  public void testGetMdcContext() {
    assertEquals(mdcContext, scheduledJobData.getMdcContext());
  }

  @Test
  public void testSetMdcContext() {
    Map<String, String> newMdcContext = new HashMap<>();
    newMdcContext.put("key2", "value2");
    scheduledJobData.setMdcContext(newMdcContext);
    assertEquals(newMdcContext, scheduledJobData.getMdcContext());
  }

  @Test
  public void testGetXRequestId() {
    assertEquals(xRequestId, scheduledJobData.getxRequestId());
  }

  @Test
  public void testSetXRequestId() {
    String newXRequestId = "newXReq123";
    scheduledJobData.setxRequestId(newXRequestId);
    assertEquals(newXRequestId, scheduledJobData.getxRequestId());
  }

  @Test
  public void testGetJobType() {
    assertEquals(jobType, scheduledJobData.getJobType());
  }

  @Test
  public void testSetJobType() {
    BsaTypes.BsaJobType newJobType = BsaTypes.BsaJobType.DELAYED_REPORTING;
    scheduledJobData.setJobType(newJobType);
    assertEquals(newJobType, scheduledJobData.getJobType());
  }
}
