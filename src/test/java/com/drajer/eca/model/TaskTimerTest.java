package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class TaskTimerTest {

  @Test
  public void testConstructorAndGetters() {
    Long id = 1L;
    Integer launchDetailsId = 100;
    EcrActionTypes actionType = EcrActionTypes.MATCH_TRIGGER;
    Instant instant = Instant.now();
    Map<String, String> mdc = new HashMap<>();
    mdc.put("requestId", "req-1");
    TaskTimer timer = new TaskTimer(id, launchDetailsId, actionType, instant, mdc);
    assertEquals(id, timer.getId());
    assertEquals(launchDetailsId, timer.getLaunchDetailsId());
    assertEquals(actionType, timer.getActionTypes());
    assertEquals(instant, timer.getT());
    assertEquals(mdc, timer.getMdcContext());
  }

  @Test
  public void testSetters() {

    TaskTimer timer = new TaskTimer(null, null, null, null, null);
    Instant instant = Instant.now();
    Map<String, String> mdc = new HashMap<>();
    mdc.put("correlationId", "corr-1");
    timer.setId(2L);
    timer.setLaunchDetailsId(200);
    timer.setActionTypes(EcrActionTypes.MATCH_TRIGGER);
    timer.setT(instant);
    timer.setMdcContext(mdc);
    assertEquals(Long.valueOf(2L), timer.getId());
    assertEquals(Integer.valueOf(200), timer.getLaunchDetailsId());
    assertEquals(EcrActionTypes.MATCH_TRIGGER, timer.getActionTypes());
    assertEquals(instant, timer.getT());
    assertEquals(mdc, timer.getMdcContext());
  }

  @Test
  public void testTransientInstantField() {
    TaskTimer timer = new TaskTimer(1L, 10, EcrActionTypes.MATCH_TRIGGER, null, null);
    Instant instant = Instant.now();
    timer.setT(instant);
    assertNotNull(timer.getT());
  }
}
