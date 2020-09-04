package com.drajer.eca.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.meta.When;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import org.powermock.api.mockito.PowerMockito;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.core.classloader.annotations.PrepareForTest;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.eq;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.databind.ObjectMapper;

import static org.mockito.Mockito.when;

@RunWith(PowerMockRunner.class)
@PrepareForTest({EcaUtils.class, ApplicationUtils.class, WorkflowService.class})
public class ReportabilityResponseActionTest {
	
	private LaunchDetails mockDetails;
	private PatientExecutionState mockState;
	private RelatedAction mockRelActn;
	private MatchTriggerStatus mockTriggerStatus;
	private Eicr mockEicr;
	
	private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
	
	@Before
	public void setUp() {
		ObjectMapper mapper = new ObjectMapper();
		
		mockDetails = PowerMockito.mock(LaunchDetails.class);
		mockState = PowerMockito.mock(PatientExecutionState.class);
		mockRelActn = PowerMockito.mock(RelatedAction.class);
		mockTriggerStatus = PowerMockito.mock(MatchTriggerStatus.class);
		mockEicr = PowerMockito.mock(Eicr.class);
			
		PowerMockito.mockStatic(EcaUtils.class);
		PowerMockito.mockStatic(ApplicationUtils.class);
		PowerMockito.mockStatic(WorkflowService.class);
		
	}
	
	@Test
	public void testExecute_RelatedActionNotCompleted() throws Exception {
		ReportabilityResponseAction reportabilityResponseAction = new ReportabilityResponseAction();
		reportabilityResponseAction.addRelatedAction(mockRelActn);
		reportabilityResponseAction.setActionId("123");
		
		when(EcaUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
		when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
		when(mockRelActn.getRelatedAction()).thenReturn(reportabilityResponseAction);
		when(mockState.hasActionCompleted(any())).thenReturn(false);
		
		reportabilityResponseAction.execute(mockDetails, launchType);
		
		//Validate
		verify(mockState,times(1)).hasActionCompleted("123");
		verify(mockRelActn, times(0)).getDuration();
		
	}
	
	@Test
	public void testExecute_AllSubmittedEics() throws Exception {
		ReportabilityResponseAction reportabilityResponseAction = new ReportabilityResponseAction();
		reportabilityResponseAction.addRelatedAction(mockRelActn);
		reportabilityResponseAction.setActionId("123");
		
		Set<Integer> id = new HashSet<Integer>();
		id.add(1);
		
		when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
		when(mockRelActn.getRelatedAction()).thenReturn(reportabilityResponseAction);
		when(EcaUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
		when(mockState.getEicrsForRRCheck()).thenReturn(id);
		when(mockState.hasActionCompleted(any())).thenReturn(true);
		
		reportabilityResponseAction.execute(mockDetails, launchType);
		
		//Validate
		verify(mockState,times(1)).hasActionCompleted("123");
		verify(mockRelActn, times(0)).getDuration();
		
		assertNotNull(mockState.getEicrsForRRCheck());
		
		
	}
	

}
