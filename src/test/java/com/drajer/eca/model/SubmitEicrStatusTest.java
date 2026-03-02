// package com.drajer.eca.model;
//
// import static org.junit.Assert.*;
//
// import com.drajer.eca.model.EventTypes.JobStatus;
// import java.util.Date;
// import org.junit.Test;
//
// public class SubmitEicrStatusTest {
//
//  @Test
//  public void testDefaultConstructor() {
//
//    SubmitEicrStatus status = new SubmitEicrStatus();
//
//    assertEquals("", status.getActionId());
//    assertEquals(JobStatus.NOT_STARTED, status.getJobStatus());
//    assertFalse(status.getEicrSubmitted());
//    assertEquals("", status.geteICRId());
//  }
//
//  @Test
//  public void testSettersAndGetters() {
//
//    SubmitEicrStatus status = new SubmitEicrStatus();
//    Date now = new Date();
//    status.setActionId("ACTION_123");
//    status.setJobStatus(JobStatus.COMPLETED);
//    status.setEicrSubmitted(true);
//    status.seteICRId("EICR_001");
//    status.setSubmittedTime(now);
//    status.setTransportUsed("DIRECT");
//    assertEquals("ACTION_123", status.getActionId());
//    assertEquals(JobStatus.COMPLETED, status.getJobStatus());
//    assertEquals(Boolean.TRUE, status.getEicrSubmitted());
//    assertEquals("EICR_001", status.geteICRId());
//    assertEquals(now, status.getSubmittedTime());
//    assertEquals("DIRECT", status.getTransportUsed());
//  }
//
//  @Test
//  public void testSubmittedTimeNotNull() {
//    SubmitEicrStatus status = new SubmitEicrStatus();
//    status.setSubmittedTime(new Date());
//    assertNotNull(status.getSubmittedTime());
//  }
// }
