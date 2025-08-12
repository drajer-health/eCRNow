package com.drajer.bsa.utils;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Calendar;
import java.util.Date;
import org.junit.Before;
import org.junit.Test;

public class StartupUtilsTest {

  private static final int STARTUP_TIME_DELAY = 10;
  private static Date startTime;

  @Before
  public void setUp() throws Exception {
    startTime = new Date();
    StartupUtils.setStartTime(startTime);

    java.lang.reflect.Method method =
        StartupUtils.class.getDeclaredMethod("setStartupTimeDelay", Integer.class);
    method.setAccessible(true);
    method.invoke(new StartupUtils(), STARTUP_TIME_DELAY);
  }

  @Test
  public void testSetAndGetStartTime() {
    assertEquals("Start time should match the set value", startTime, StartupUtils.getStartTime());
  }

  @Test
  public void testGetStartupTimeDelay() {
    assertEquals(
        "Startup time delay should match the set value",
        Integer.valueOf(STARTUP_TIME_DELAY),
        StartupUtils.getStartupTimeDelay());
  }

  @Test
  public void testHasAppStarted() throws InterruptedException {
    assertFalse("App should not have started yet", StartupUtils.hasAppStarted());

    Thread.sleep((STARTUP_TIME_DELAY + 1) * 1000);

    assertTrue("App should have started", StartupUtils.hasAppStarted());
  }

  @Test
  public void testGetPatientLaunchInstanceTime() {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startTime);
    calendar.add(Calendar.SECOND, STARTUP_TIME_DELAY);
    Date expectedDelayedTime = calendar.getTime();

    long expectedTime = expectedDelayedTime.getTime();
    long actualTime = StartupUtils.getPatientLaunchInstanceTime().getTime();

    long toleranceInMillis = 1000;

    assertTrue(
        "Patient launch instance time should be within " + toleranceInMillis + " ms",
        Math.abs(expectedTime - actualTime) <= toleranceInMillis);
  }
}
