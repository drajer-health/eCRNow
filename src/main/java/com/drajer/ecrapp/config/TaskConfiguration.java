package com.drajer.ecrapp.config;

import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.eca.model.TaskTimer;
import com.drajer.ecrapp.model.WorkflowTask;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.OneTimeTask;
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
import org.springframework.web.client.RestTemplate;

@Configuration
public class TaskConfiguration {
  private static final Logger log = LoggerFactory.getLogger(TaskConfiguration.class);

  @Autowired RestTemplate restTemplate;

  @Value("${timer.retries:10}")
  private Integer timerRetries;

  @Value("${workflow.endpoint}")
  private String workflowEndpoint;

  /** Define a one-time task which have to be manually scheduled. */
  @Bean
  public Task<TaskTimer> sampleOneTimeTask() {
    log.info("Initializing the One time task");
    OneTimeTask<TaskTimer> myTask =
        Tasks.oneTime("EICRTask", TaskTimer.class)
            .onFailureRetryLater()
            .execute(
                (inst, ctx) -> {
                  try {
                    if (inst.getData().getMdcContext() != null) {
                      MDC.setContextMap(inst.getData().getMdcContext());
                    }
                    log.info(
                        "Executing Task for {}, Launch Id::: {}",
                        inst.getTaskAndInstance(),
                        inst.getData().getLaunchDetailsId());

                    WorkflowTask workflowTask = new WorkflowTask();
                    workflowTask.setLaunchId(inst.getData().getLaunchDetailsId());
                    workflowTask.setActionType(inst.getData().getActionTypes());
                    workflowTask.setWorkflowEvent(WorkflowEvent.SCHEDULED_JOB);
                    ActionRepo.getInstance()
                        .getWorkflowService()
                        .executeScheduledAction(
                            workflowTask.getLaunchId(),
                            workflowTask.getActionType(),
                            workflowTask.getWorkflowEvent(),
                            inst.getTaskAndInstance());

                    log.info(
                        "Successfully Completed Executing Task for {}, Launch Id::: {}",
                        inst.getTaskAndInstance(),
                        inst.getData().getLaunchDetailsId());

                  } catch (ObjectDeletedException deletedException) {

                    log.info(
                        "Error in completing the Execution of Task for {}, Launch Id::: {}, finishing task without retries",
                        inst.getTaskAndInstance(),
                        inst.getData().getLaunchDetailsId(),
                        deletedException);

                  } catch (Exception e) {

                    if (ctx.getExecution().consecutiveFailures >= timerRetries) {
                      log.error(
                          "Error in completing the Execution of Task for {}, Launch Id {}, removing the timer after {} retries",
                          inst.getTaskAndInstance(),
                          inst.getData().getLaunchDetailsId(),
                          ctx.getExecution().consecutiveFailures,
                          e);
                    } else {
                      ApplicationUtils.handleException(
                          e,
                          "Error in completing the Execution, retry in 5 minutes.",
                          LogLevel.ERROR);
                      throw e;
                    }

                  } finally {
                    MDC.clear();
                  }
                });
    return myTask;
  }
}
