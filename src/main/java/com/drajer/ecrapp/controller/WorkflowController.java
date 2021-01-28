package com.drajer.ecrapp.controller;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.model.WorkflowTask;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class WorkflowController {

  private final Logger logger = LoggerFactory.getLogger(WorkflowController.class);

  @CrossOrigin
  @RequestMapping(value = "/api/invokeWorkflow", method = RequestMethod.POST)
  public ResponseEntity<String> invokeWorkflow(
      @RequestBody WorkflowTask workflowTask,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {
      logger.info("Received LaunchId::::: {}", workflowTask.getLaunchId());
      logger.info("Received EcrActionTypes::::: {}", workflowTask.getActionType());
      logger.info("Received WorkflowEvent::::: {}", workflowTask.getWorkflowEvent());
      ActionRepo.getInstance()
          .getWorkflowService()
          .executeScheduledAction(
              workflowTask.getLaunchId(),
              workflowTask.getActionType(),
              workflowTask.getWorkflowEvent());

    } catch (Exception e) {
      logger.error("Error in Processing the request", e);
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }
}
