package com.drajer.bsa.scheduler;

import com.drajer.bsa.service.KarProcessor;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import org.hibernate.ObjectDeletedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.logging.LogLevel;
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

  @Value("${timer.retries:10}")
  private Integer timerRetries;

  /** Define a one-time job which has to be manually scheduled. */
  @Bean
  public Task<ScheduledJobData> sampleOneTimeJob() {
    logger.info("Initializing the One time task");

    return Tasks.oneTime("BsaScheduledJob", ScheduledJobData.class)
        .onFailureRetryLater()
        .execute(
            (inst, ctx) -> {
              try {

                if (inst.getData().getMdcContext() != null) {
                  MDC.setContextMap(inst.getData().getMdcContext());
                }

                logger.info(
                    "Executing Task for {}, Action Id : {}, KarExecutionStateId : {}, xRequestId : {}",
                    inst.getTaskAndInstance(),
                    inst.getData().getActionId(),
                    inst.getData().getKarExecutionStateId(),
                    inst.getData().getxRequestId());

                karProcessor.applyKarForScheduledJob(inst.getData());

                logger.info(
                    "Successfully Completed Executing Task for {}, Action Id: {}, KarExecutionStateId: {}, xRequestId : {}",
                    inst.getTaskAndInstance(),
                    inst.getData().getActionId(),
                    inst.getData().getKarExecutionStateId(),
                    inst.getData().getxRequestId());

              } catch (ObjectDeletedException deletedException) {

                logger.info(
                    "Error in Executing Task for {}, Action Id : {}, KarExecutionStateId : {}, xRequestId : {}",
                    inst.getTaskAndInstance(),
                    inst.getData().getActionId(),
                    inst.getData().getKarExecutionStateId(),
                    inst.getData().getxRequestId());

              } catch (Exception e) {
                if (ctx.getExecution().consecutiveFailures >= timerRetries) {
                  logger.error(
                      "Error in executing Task for {}, Action Id : {}, KarExecutionStateId : {}, xRequestId : {} removing the timer after {} retries",
                      inst.getTaskAndInstance(),
                      inst.getData().getActionId(),
                      inst.getData().getKarExecutionStateId(),
                      inst.getData().getxRequestId(),
                      ctx.getExecution().consecutiveFailures,
                      e);
                } else {
                  String msg =
                      "Error in executing Task for "
                          + inst.getTaskAndInstance()
                          + " Action Id : "
                          + inst.getData().getActionId()
                          + " KarExecutionStateId : "
                          + inst.getData().getKarExecutionStateId()
                          + " xRequestId : "
                          + inst.getData().getxRequestId()
                          + " , retry in 5 minutes";
                  ApplicationUtils.handleException(e, msg, LogLevel.ERROR);
                  throw e;
                }
              } finally {
                logger.info(" Clearing MDC");
                MDC.clear();
              }
            });
  }
}
