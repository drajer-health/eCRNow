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
public class ReportabilityResponseActionTest {

  private LaunchDetails mockDetails;
  private PatientExecutionState mockState;
  private RelatedAction mockRelActn;

  private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;

  @InjectMocks ReportabilityResponseAction reportabilityResponseAction;

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

    reportabilityResponseAction.addRelatedAction(mockRelActn);

    reportabilityResponseAction.setActionId("123");

    when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
    when(mockRelActn.getRelatedAction()).thenReturn(reportabilityResponseAction);
    when(mockState.hasActionCompleted(any())).thenReturn(false);

    reportabilityResponseAction.execute(mockDetails, launchType);

    // Validate
    verify(mockState, times(1)).hasActionCompleted("123");
    verify(mockRelActn, times(0)).getDuration();
  }

  @Test
  public void testExecute_AllSubmittedEics() throws Exception {

    reportabilityResponseAction.addRelatedAction(mockRelActn);
    reportabilityResponseAction.setActionId("123");

    Set<Integer> id = new HashSet<Integer>();
    id.add(1);

    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
    when(mockRelActn.getRelatedAction()).thenReturn(reportabilityResponseAction);
    when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
    when(mockState.getEicrsForRRCheck()).thenReturn(id);
    when(mockState.hasActionCompleted(any())).thenReturn(true);

    reportabilityResponseAction.execute(mockDetails, launchType);

    // Validate
    verify(mockState, times(1)).hasActionCompleted("123");
    verify(mockRelActn, times(0)).getDuration();

    assertNotNull(mockState.getEicrsForRRCheck());
  }

  @Test
  public void testExecute_whenRelatedActionIsNotPresent() {

    reportabilityResponseAction.setActionId("123");

    Set<Integer> ids = new HashSet<Integer>();
    ids.add(1);

    when(mockRelActn.getRelatedAction()).thenReturn(null);
    when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
    when(mockState.getEicrsForRRCheck()).thenReturn(ids);

    reportabilityResponseAction.execute(mockDetails, launchType);

    verify(mockRelActn, times(0)).getDuration();

    assertNotNull(mockState.getEicrsForRRCheck());
  }
}
