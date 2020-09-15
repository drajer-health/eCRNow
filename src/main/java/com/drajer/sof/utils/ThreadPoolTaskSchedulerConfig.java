package com.drajer.sof.utils;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;

@Configuration
@ComponentScan(
    basePackages = {"com.drajer.ecr"},
    basePackageClasses = {RefreshTokenScheduler.class})
@EnableScheduling
@EnableAsync
public class ThreadPoolTaskSchedulerConfig {

  @Bean
  public ThreadPoolTaskScheduler threadPoolTaskScheduler() {
    ThreadPoolTaskScheduler threadPoolTaskScheduler = new ThreadPoolTaskScheduler();
    threadPoolTaskScheduler.setPoolSize(5);
    threadPoolTaskScheduler.setThreadNamePrefix("ThreadPoolTaskScheduler");
    threadPoolTaskScheduler.initialize();
    return threadPoolTaskScheduler;
  }

  @Bean
  public CronTrigger cronTrigger() {
    return new CronTrigger("10 * * * * ?");
  }
}
