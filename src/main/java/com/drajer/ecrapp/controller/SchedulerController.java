package com.drajer.ecrapp.controller;

import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.service.SchedulerService;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
public class SchedulerController {

  private final Logger logger = LoggerFactory.getLogger(SchedulerController.class);

  @Autowired SchedulerService schedulerService;

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

  /**
   * Deletes scheduled tasks for the specified patient and encounter.
   *
   * <p>This endpoint deletes all scheduled tasks associated with the given patientId and
   * encounterId, optionally filtered by id and fhirServerBaseUrl. Returns an appropriate HTTP
   * response based on the result.
   *
   * @param id Optional identifier for the delete operation.
   * @param fhirServerBaseUrl Optional FHIR server base URL to filter tasks.
   * @param patientId The patient identifier (required).
   * @param encounterId The encounter identifier (required).
   * @return ResponseEntity with a message indicating the result and appropriate HTTP status code.
   * @throws IOException If an error occurs during task deletion.
   */
  @DeleteMapping("/api/scheduledTasks")
  public ResponseEntity<Object> deleteScheduledTasks(
      @RequestParam(required = false) String id,
      @RequestParam(required = false) String fhirServerBaseUrl,
      @RequestParam(required = false) String patientId,
      @RequestParam(required = false) String encounterId)
      throws IOException {

    Map<String, Object> response = new HashMap<>();
    if (StringUtils.isBlank(patientId) || StringUtils.isBlank(encounterId)) {
      response.put("message", "PatientId and EncounterId are required.");
      response.put("status", HttpStatus.BAD_REQUEST.value());
      return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
    }
    String validFhirServerBaseUrl =
        StringUtils.isNotBlank(fhirServerBaseUrl) ? fhirServerBaseUrl : null;
    String ValidId = StringUtils.isNotBlank(id) ? id : null;

    List<ScheduledTasks> deletedScheduledTasks =
        schedulerService.delete(ValidId, validFhirServerBaseUrl, patientId, encounterId);

    if (deletedScheduledTasks != null && !deletedScheduledTasks.isEmpty()) {
      response.put("message ", " Scheduled Tasks is deleted Successfully.");
      return new ResponseEntity<>(response, HttpStatus.OK);
    } else {

      logger.info(
          "No Scheduled Tasks found to delete for patientId={} and encounterId={}",
          patientId,
          encounterId);
      response.put(
          "message ",
          "No Scheduled Tasks found to delete for patientId="
              + patientId
              + "and encounterId="
              + encounterId);
      response.put("status", HttpStatus.NOT_FOUND.value());
      return new ResponseEntity<>(response, HttpStatus.NOT_FOUND);
    }
  }
}
