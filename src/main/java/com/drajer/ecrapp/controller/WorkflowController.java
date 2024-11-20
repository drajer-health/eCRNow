package com.drajer.ecrapp.controller;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.model.WorkflowTask;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class WorkflowController {

  private final Logger logger = LoggerFactory.getLogger(WorkflowController.class);

  @CrossOrigin
  @PostMapping(value = "/api/invokeWorkflow")
  public ResponseEntity<String> invokeWorkflow(
      @RequestBody WorkflowTask workflowTask,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {
      logger.info("Received LaunchId::::: {}", workflowTask.getLaunchId());
      logger.info("Received EcrActionTypes::::: {}", workflowTask.getActionType());
      logger.info("Received WorkflowEvent::::: {}", workflowTask.getWorkflowEvent());
      String taskInstanceId = "";
      ActionRepo.getInstance()
          .getWorkflowService()
          .executeScheduledAction(
              workflowTask.getLaunchId(),
              workflowTask.getActionType(),
              workflowTask.getWorkflowEvent(),
              taskInstanceId);

    } catch (Exception e) {
      logger.error("Error in Processing the request", e);
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }
}
