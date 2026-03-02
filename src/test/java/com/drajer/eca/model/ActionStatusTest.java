package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;

import java.math.BigDecimal;
import java.util.Date;
import org.junit.Test;

public class ActionStatusTest {

  @Test
  public void testGettersAndSetters() {
    ActionStatus status = new ActionStatus();

    status.setActionId("action-123");
    assertEquals("action-123", status.getActionId());

    status.setStatus(EventTypes.JobStatus.COMPLETED);
    assertEquals(EventTypes.JobStatus.COMPLETED, status.getStatus());

    Date now = new Date();
    status.setActionStartTime(now);
    assertEquals(now, status.getActionStartTime());

    BigDecimal nextTime = new BigDecimal("123.45");
    status.setNextJobExpirationTime(nextTime);
    assertEquals(nextTime, status.getNextJobExpirationTime());

    status.setUnits("seconds");
    assertEquals("seconds", status.getUnits());
  }
}
