package com.drajer.sof.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;

public class ThreadPoolTaskSchedulerConfigTest {

  @Test
  public void testThreadPoolTaskSchedulerBeanCreation() {
    ThreadPoolTaskSchedulerConfig config = new ThreadPoolTaskSchedulerConfig();
    ThreadPoolTaskScheduler scheduler = config.threadPoolTaskScheduler();
    assertTrue(scheduler.getThreadNamePrefix().startsWith("ThreadPoolTaskScheduler"));
  }

  @Test
  public void testCronTriggerBeanCreation() {
    ThreadPoolTaskSchedulerConfig config = new ThreadPoolTaskSchedulerConfig();
    CronTrigger cronTrigger = config.cronTrigger();
    assertEquals("10 * * * * ?", cronTrigger.getExpression());
  }
}
