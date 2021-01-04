package com.drajer.ecrapp.config;

import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.eca.model.TaskTimer;
import com.drajer.ecrapp.service.WorkflowService;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.OneTimeTask;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TaskConfiguration {
  private static final Logger log = LoggerFactory.getLogger(TaskConfiguration.class);

  /** Define a one-time task which have to be manually scheduled. */
  @Bean
  public Task<TaskTimer> sampleOneTimeTask() {
    log.info("Initializing the One time task");
    OneTimeTask<TaskTimer> myTask =
        Tasks.oneTime("XYRAMSOFT Solution", TaskTimer.class)
            .onFailureRetryLater()
            .execute(
                (inst, ctx) -> {
                  log.info(
                      "Executing Task for "
                          + inst.getTaskAndInstance()
                          + " , Launch Id::: "
                          + inst.getData().getLaunchDetailsId());
                  new WorkflowService()
                      .executeScheduledAction(
                          inst.getData().getLaunchDetailsId(),
                          inst.getData().getActionTypes(),
                          WorkflowEvent.SCHEDULED_JOB);
                });
    return myTask;
  }
}
