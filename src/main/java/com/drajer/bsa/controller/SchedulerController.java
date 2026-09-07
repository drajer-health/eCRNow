package com.drajer.bsa.controller;

import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.service.SchedulerService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.*;

@RestController
public class SchedulerController {

  private final Logger logger = LoggerFactory.getLogger(SchedulerController.class);
  private final SchedulerService schedulerService;

  /**
   * Instantiates a new scheduler controller.
   *
   * @param schedulerService the scheduler service
   */
  public SchedulerController(SchedulerService schedulerService) {
    this.schedulerService = schedulerService;
  }

  @CrossOrigin
  @GetMapping("/api/scheduledTasks")
  public List<ScheduledTasks> getScheduledTasks(
      @RequestParam(name = "action", required = false) String actionType,
      @RequestParam(name = "launch", required = false) String launchId) {

    logger.info(
        "Received request to get Scheduled Tasks by action:{} and launch:{}", actionType, launchId);

    return schedulerService.getScheduledTasks(actionType, launchId);
  }

  @CrossOrigin
  @GetMapping("/api/scheduledTasks/search")
  public List<ScheduledTasks> getScheduledTasksBySearchQuery(
      @RequestParam(name = "taskInstance", required = false) String taskInstance) {
    logger.info("Received request to get Scheduled Tasks by taskInstance:{} ", taskInstance);

    return schedulerService.getScheduledTasksBySearchQuery(taskInstance);
  }
}
