package com.drajer.bsa.scheduler;

import com.drajer.bsa.service.KarProcessor;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 *
 *
 * <h1>ScheduleJobConfiguration</h1>
 *
 * This class will be used to configure the Schedule Job so that it can be scheduled as required.
 *
 * @author nbashyam
 */
@Configuration
public class ScheduleJobConfiguration {

  private final Logger logger = LoggerFactory.getLogger(ScheduleJobConfiguration.class);

  @Autowired KarProcessor karProcessor;

  /** Define a one-time job which has to be manually scheduled. */
  @Bean
  public Task<ScheduledJobData> sampleOneTimeJob() {
    logger.info("Initializing the One time task");

    return Tasks.oneTime("BsaScheduledJob", ScheduledJobData.class)
        .onFailureRetryLater()
        .execute(
            (inst, ctx) -> {
              try {

                logger.info(
                    "Executing Task for {}, Action Id : {}, KarExecutionStateId : {}",
                    inst.getTaskAndInstance(),
                    inst.getData().getActionId(),
                    inst.getData().getKarExecutionStateId());

                karProcessor.applyKarForScheduledJob(inst.getData());

              } catch (Exception e) {
                logger.error("Error in completing the Execution of the schedule job : ", e);
              } finally {
                logger.info(" Nothing to clean up ");
              }
            });
  }
}
