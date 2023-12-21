package com.drajer.ecrapp.config;

import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.eca.model.TaskTimer;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.ecrapp.model.WorkflowTask;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.ecrapp.util.MDCUtils;
import com.drajer.sof.dao.LaunchDetailsDao;
import com.drajer.sof.model.LaunchDetails;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.OneTimeTask;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import java.time.Duration;
import java.time.Instant;
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

  @Autowired LaunchDetailsDao launchDetailsDao;

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
                      MDCUtils.removeEicrDocId();
                    }
                    long timeDiff =
                        Duration.between(ctx.getExecution().executionTime, Instant.now())
                            .toMinutes();
                    log.info(
                        "Executing Task for {}, Launch Id::: {}, ExecutionTime:: {}, Execution delay in minutes:: {}",
                        inst.getTaskAndInstance(),
                        inst.getData().getLaunchDetailsId(),
                        ctx.getExecution().executionTime,
                        timeDiff);

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
                  } catch (RetryableException | ResourceNotFoundException exception) {
                    if ((exception instanceof RetryableException
                            && exception.getCause() instanceof ResourceNotFoundException)
                        || exception instanceof ResourceNotFoundException) {
                      log.info(
                          "Error in completing the Execution of Task for {}, Launch Id::: {}, finishing task",
                          inst.getTaskAndInstance(),
                          inst.getData().getLaunchDetailsId(),
                          exception);
                      LaunchDetails details =
                          launchDetailsDao.getAuthDetailsById(inst.getData().getLaunchDetailsId());
                      if (details != null) {
                        details.setProcessingState(
                            LaunchDetails.getString(LaunchDetails.ProcessingStatus.Errors));
                        launchDetailsDao.saveOrUpdate(details);
                      }
                    }
                  } catch (Exception e) {

                    if (ctx.getExecution().consecutiveFailures >= timerRetries) {
                      log.error(
                          "Error in completing the Execution of Task for {}, Launch Id {}, removing the timer after {} retries",
                          inst.getTaskAndInstance(),
                          inst.getData().getLaunchDetailsId(),
                          ctx.getExecution().consecutiveFailures,
                          e);

                      LaunchDetails details =
                          launchDetailsDao.getAuthDetailsById(inst.getData().getLaunchDetailsId());
                      if (details != null) {
                        details.setProcessingState(
                            LaunchDetails.getString(LaunchDetails.ProcessingStatus.Errors));
                        launchDetailsDao.saveOrUpdate(details);
                      }

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
    log.info("One time task execution done");
    return myTask;
  }
}
