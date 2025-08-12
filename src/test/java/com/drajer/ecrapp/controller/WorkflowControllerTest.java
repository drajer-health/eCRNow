package com.drajer.ecrapp.controller;

import static com.drajer.eca.model.EventTypes.EcrActionTypes.MATCH_TRIGGER;
import static com.drajer.eca.model.EventTypes.WorkflowEvent.SOF_LAUNCH;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.model.WorkflowTask;
import com.drajer.ecrapp.service.WorkflowService;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class WorkflowControllerTest {
  @Mock HttpServletResponse response;
  @Mock HttpServletRequest request;
  @Mock WorkflowTask workflowTask;
  @Mock WorkflowService workflowService;
  @InjectMocks WorkflowController workflowController;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testinvokeWorkflow_sucess() {
    when(workflowTask.getLaunchId()).thenReturn(123);
    when(workflowTask.getActionType()).thenReturn(MATCH_TRIGGER);
    when(workflowTask.getWorkflowEvent()).thenReturn(SOF_LAUNCH);

    ActionRepo mockActionRepo = mock(ActionRepo.class);

    workflowService.executeScheduledAction(123, MATCH_TRIGGER, SOF_LAUNCH, "");
    when(mockActionRepo.getWorkflowService()).thenReturn(workflowService);
    Mockito.doNothing()
        .when(workflowService)
        .executeScheduledAction(anyInt(), any(), any(), anyString());

    ResponseEntity<String> responseEntity =
        workflowController.invokeWorkflow(workflowTask, request, response);
    assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    assertEquals("Success", responseEntity.getBody());

    verify(workflowService).executeScheduledAction(123, MATCH_TRIGGER, SOF_LAUNCH, "");
  }
}
