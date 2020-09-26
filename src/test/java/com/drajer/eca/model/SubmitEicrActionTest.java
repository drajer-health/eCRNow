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
@PrepareForTest({EcaUtils.class, ApplicationUtils.class, WorkflowService.class, ActionRepo.class})
public class SubmitEicrActionTest {

  private LaunchDetails mockDetails;
  private PatientExecutionState mockState;
  private RelatedAction mockRelActn;
  private ActionRepo mockActionRepo;

  @InjectMocks SubmitEicrAction submitEicrAction;

  Set<SubmitEicrStatus> mockSubmitEicrStatus;

  Set<Integer> mockIds;

  private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;

  @Before
  public void setUp() {

    mockDetails = PowerMockito.mock(LaunchDetails.class);
    mockState = PowerMockito.mock(PatientExecutionState.class);
    mockRelActn = PowerMockito.mock(RelatedAction.class);

    if (mockActionRepo == null) {

      mockActionRepo = PowerMockito.mock(ActionRepo.class);
    }

    PowerMockito.mockStatic(ActionRepo.class);
    PowerMockito.mockStatic(EcaUtils.class);
    PowerMockito.mockStatic(ApplicationUtils.class);
    PowerMockito.mockStatic(WorkflowService.class);
  }

  @Test
  public void testExecute_RelatedActionNotCompleted() throws Exception {

    submitEicrAction.addRelatedAction(mockRelActn);

    setUpMockData();

    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
    when(mockRelActn.getRelatedAction()).thenReturn(submitEicrAction);
    when(mockState.hasActionCompleted(any())).thenReturn(false);

    // Test
    submitEicrAction.execute(mockDetails, launchType);

    // Validate
    verify(mockState, times(1)).hasActionCompleted("123");
    verify(mockRelActn, times(0)).getDuration();
  }

  @Test
  public void testExecute_RelatedActionCompleted() {

    submitEicrAction.addRelatedAction(mockRelActn);

    setUpMockData();

    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
    when(mockRelActn.getRelatedAction()).thenReturn(submitEicrAction);
    when(mockState.hasActionCompleted(any())).thenReturn(true);

    // Test
    submitEicrAction.execute(mockDetails, launchType);

    // validate
    assertNotNull(mockState.getEicrIdForCompletedActions("123"));
    assertNotNull(mockState.getEicrsReadyForSubmission());
  }

  @Test
  public void testExecute_RelatedActionNotPresent() {

    setUpMockData();

    when(mockRelActn.getRelatedAction()).thenReturn(null);

    submitEicrAction.execute(mockDetails, launchType);

    // validate
    assertNotNull(mockState.getEicrIdForCompletedActions("123"));
    assertNotNull(mockState.getEicrsReadyForSubmission());
  }

  public void setUpMockData() {
    submitEicrAction.setActionId("123");
    mockSubmitEicrStatus = new HashSet<>();
    when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
    when(mockState.getSubmitEicrStatus()).thenReturn(mockSubmitEicrStatus);
  }
}
