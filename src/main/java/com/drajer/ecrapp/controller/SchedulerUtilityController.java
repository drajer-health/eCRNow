package com.drajer.ecrapp.controller;

import com.drajer.ecrapp.util.ScheduledTaskUtil;
import java.io.IOException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/scheduled-tasks")
public class SchedulerUtilityController {

  private final ScheduledTaskUtil scheduledTaskUtil;

  /**
   * Instantiates a new scheduler utility controller.
   *
   * @param scheduledTaskUtil the scheduled task utility
   */
  @Autowired
  public SchedulerUtilityController(ScheduledTaskUtil scheduledTaskUtil) {
    this.scheduledTaskUtil = scheduledTaskUtil;
  }

  @PostMapping("/export")
  public ResponseEntity<String> exportScheduledTasks() {
    try {
      String filePath = scheduledTaskUtil.exportScheduledTasks();
      return ResponseEntity.ok("Scheduled tasks exported successfully to file: " + filePath);
    } catch (IOException e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("Error exporting scheduled tasks: " + e.getMessage());
    }
  }

  @PostMapping("/import")
  public ResponseEntity<String> importScheduledTasks() {
    try {
      String filePath = scheduledTaskUtil.importScheduledTasks();
      return ResponseEntity.ok("Scheduled tasks imported successfully from file: " + filePath);
    } catch (IOException e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("Error importing scheduled tasks: " + e.getMessage());
    }
  }
}
