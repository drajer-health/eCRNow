package com.drajer.bsa.scheduler;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.BsaJobType;
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

  @Autowired ScheduleJobConfiguration schedulerConfig;

  @Autowired Scheduler scheduler;

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
            + java.util.UUID.randomUUID().toString();

    logger.info(" Scheduling Job Id {} to be executed at : {}", jobId, t);

    scheduler.schedule(
        schedulerConfig
            .sampleOneTimeJob()
            .instance(
                jobId, new ScheduledJobData(karExecId, actionId, type, t, jobId, xReqId, jobtype, mdc)),
        t);
  }
}
