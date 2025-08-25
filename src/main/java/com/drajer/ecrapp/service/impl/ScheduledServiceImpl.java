package com.drajer.ecrapp.service.impl;

import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.ecrapp.dao.SchedulerDao;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.service.SchedulerService;
import com.drajer.ecrapp.util.ScheduledTaskUtil;
import com.github.kagkarlsson.scheduler.Scheduler;
import com.github.kagkarlsson.scheduler.task.TaskInstanceId;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.transaction.Transactional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class ScheduledServiceImpl implements SchedulerService {

  private final Logger logger = LoggerFactory.getLogger(ScheduledServiceImpl.class);

  @Autowired KarExecutionStateService karExecutionStateService;
  @Autowired SchedulerDao schedulerDao;
  @Autowired ScheduledTaskUtil scheduledTaskUtil;

  @Autowired Scheduler scheduler;

  @Override
  public List<ScheduledTasks> getScheduledTasks(String actionType, String launchId) {
    List<ScheduledTasks> tasksList = schedulerDao.getScheduledTasks(actionType, launchId);
    logger.info("ScheduledTasks:{}", tasksList);
    return tasksList;
  }

  @Override
  public List<ScheduledTasks> getScheduledTasksBySearchQuery(String taskInstance) {
    List<ScheduledTasks> tasksList = schedulerDao.getScheduledTasksBySearchQuery(taskInstance);
    logger.info("ScheduledTasks:{}", tasksList);
    return tasksList;
  }

  @Override
  public List<ScheduledTasks> delete(
      String id, String fhirServerBaseUrl, String patientId, String encounterId)
      throws IOException {

    List<String> karExecutionStateIds =
        karExecutionStateService.getExecutionIdsByNotificationContextDetails(
            patientId, fhirServerBaseUrl, encounterId);

    List<ScheduledTasks> scheduledTasks = schedulerDao.getScheduledTasks();
    List<ScheduledTasks> deletedTasks = new ArrayList<>();

    for (ScheduledTasks task : scheduledTasks) {
      try {
        ScheduledJobData scheduledJobData = scheduledTaskUtil.deserialize(task.getTask_data());

        if (scheduledJobData != null
            && karExecutionStateIds.contains(
                scheduledJobData.getKarExecutionStateId().toString())) {

          // Cancel in scheduler
          scheduler.cancel(TaskInstanceId.of(task.getTask_name(), task.getTask_instance()));

          deletedTasks.add(task);

          logger.info(
              "Deleted Scheduled Task for patientId={} and encounterId={}, taskId={}",
              patientId,
              encounterId,
              task.getTask_instance());
        }
      } catch (Exception e) {
        logger.error(
            "Error deserializing task data for taskId={}: {}",
            task.getTask_instance(),
            e.getMessage());
      }
    }

    return deletedTasks;
  }
}
