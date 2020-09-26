package com.drajer.eca.model;

import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.util.HashSet;
import java.util.Set;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({EcaUtils.class, ApplicationUtils.class, WorkflowService.class})
public class ValidateEicrActionTest {

  private LaunchDetails mockDetails;
  private PatientExecutionState mockState;
  private RelatedAction mockRelActn;

  private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;

  @InjectMocks ValidateEicrAction validateEicrAction;

  @Before
  public void setUp() {

    mockDetails = PowerMockito.mock(LaunchDetails.class);
    mockState = PowerMockito.mock(PatientExecutionState.class);
    mockRelActn = PowerMockito.mock(RelatedAction.class);

    PowerMockito.mockStatic(EcaUtils.class);
    PowerMockito.mockStatic(ApplicationUtils.class);
    PowerMockito.mockStatic(WorkflowService.class);
  }

  @Test
  public void testExecute_RelatedActionNotCompleted() throws Exception {

    validateEicrAction.addRelatedAction(mockRelActn);
    setUpMockData();

    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
    when(mockRelActn.getRelatedAction()).thenReturn(validateEicrAction);
    when(mockState.hasActionCompleted(any())).thenReturn(false);

    // Test
    validateEicrAction.execute(mockDetails, launchType);

    // Validate
    verify(mockState, times(1)).hasActionCompleted("123");
    verify(mockRelActn, times(0)).getDuration();
  }

  @Test
  public void testExecute_RelatedActionCompleted() {

    validateEicrAction.addRelatedAction(mockRelActn);
    setUpMockData();

    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
    when(mockRelActn.getRelatedAction()).thenReturn(validateEicrAction);
    when(mockState.hasActionCompleted(any())).thenReturn(true);

    // Test
    validateEicrAction.execute(mockDetails, launchType);

    // validate
    assertNotNull(mockState.getEicrIdForCompletedActions("123"));
    assertNotNull(mockState.getEicrsReadyForValidation());
  }

  @Test
  public void testExecute_RelatedActionNotPresent() {

    setUpMockData();
    when(mockRelActn.getRelatedAction()).thenReturn(null);

    // Test
    validateEicrAction.execute(mockDetails, launchType);

    // validate
    assertNotNull(mockState.getEicrIdForCompletedActions("123"));
    assertNotNull(mockState.getEicrsReadyForValidation());
  }

  public void setUpMockData() {

    validateEicrAction.setActionId("123");
    Set<ValidateEicrStatus> validateEicrStatus = new HashSet<ValidateEicrStatus>();
    when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
    when(mockState.getValidateEicrStatus()).thenReturn(validateEicrStatus);
  }
}
