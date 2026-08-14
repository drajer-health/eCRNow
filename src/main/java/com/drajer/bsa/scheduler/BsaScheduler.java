package com.drajer.bsa.scheduler;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.BsaJobType;
import com.drajer.bsa.service.KarProcessor;
import com.github.kagkarlsson.scheduler.Scheduler;
import java.time.Instant;
import java.util.Map;
import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>BsaScheduler</h1>
 *
 * This class is used to schedule the various persistent scheduled jobs for the BSA.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class BsaScheduler {

  private final Logger logger = LoggerFactory.getLogger(BsaScheduler.class);

  private final ScheduleJobConfiguration schedulerConfig;
  private final Scheduler scheduler;
  private final KarProcessor karProcessor;

  /**
   * Instantiates a new BSA scheduler.
   *
   * @param schedulerConfig the scheduler configuration
   * @param scheduler the scheduler
   * @param karProcessor the KAR processor
   */
  @Autowired
  public BsaScheduler(
      ScheduleJobConfiguration schedulerConfig, Scheduler scheduler, KarProcessor karProcessor) {
    this.schedulerConfig = schedulerConfig;
    this.scheduler = scheduler;
    this.karProcessor = karProcessor;
  }

  public void scheduleJob(
      UUID karExecId,
      String actionId,
      BsaTypes.ActionType type,
      Instant t,
      String xReqId,
      BsaJobType jobtype,
      Map<String, String> mdc) {

    String jobId =
        actionId
            + "_"
            + type.toString()
            + "_"
            + karExecId.toString()
            + "_"
            + UUID.randomUUID().toString();

    logger.info(" Scheduling Job Id {} to be executed at : {}", jobId, t);

    scheduler.schedule(
        schedulerConfig
            .sampleOneTimeJob(karProcessor)
            .instance(
                jobId,
                new ScheduledJobData(karExecId, actionId, type, t, jobId, xReqId, jobtype, mdc)),
        t);
  }
}
