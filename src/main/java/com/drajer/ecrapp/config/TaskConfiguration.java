package com.drajer.ecrapp.config;

import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.eca.model.TaskTimer;
import com.drajer.ecrapp.model.WorkflowTask;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.OneTimeTask;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

@Configuration
public class TaskConfiguration {
  private static final Logger log = LoggerFactory.getLogger(TaskConfiguration.class);

  @Autowired RestTemplate restTemplate;

  @Value("${workflow.endpoint:}")
  private String workflowEndpoint;

  /** Define a one-time task which have to be manually scheduled. */
  @Bean
  public Task<TaskTimer> sampleOneTimeTask() {
    log.info("Initializing the One time task");
    final Map<String, String> loggingDiagnosticContext = MDC.getCopyOfContextMap();
    OneTimeTask<TaskTimer> myTask =
        Tasks.oneTime("EICRTask", TaskTimer.class)
            .onFailureRetryLater()
            .execute(
                (inst, ctx) -> {
                  try {
                    MDC.setContextMap(loggingDiagnosticContext);
                    log.info(
                        "Executing Task for {}, Launch Id::: {}",
                        inst.getTaskAndInstance(),
                        inst.getData().getLaunchDetailsId());

                    // log.info("Workflow Endpoint URL:::: {}", workflowEndpoint);
                    WorkflowTask workflowTask = new WorkflowTask();
                    workflowTask.setLaunchId(inst.getData().getLaunchDetailsId());
                    workflowTask.setActionType(inst.getData().getActionTypes());
                    workflowTask.setWorkflowEvent(WorkflowEvent.SCHEDULED_JOB);
                    ActionRepo.getInstance()
                        .getWorkflowService()
                        .executeScheduledAction(
                            workflowTask.getLaunchId(),
                            workflowTask.getActionType(),
                            workflowTask.getWorkflowEvent());
                    // restTemplate.postForObject(workflowEndpoint, workflowTask, String.class);
                  } catch (Exception e) {
                    log.error("Error in completing the Execution:::::", e);
                  } finally {
                    MDC.clear();
                  }
                });
    return myTask;
  }
}
