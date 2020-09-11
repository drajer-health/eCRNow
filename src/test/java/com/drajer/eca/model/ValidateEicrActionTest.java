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
@PrepareForTest({ EcaUtils.class, ApplicationUtils.class, WorkflowService.class })
public class ValidateEicrActionTest {

	private LaunchDetails mockDetails;
	private PatientExecutionState mockState;
	private RelatedAction mockRelActn;

	private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;

	@InjectMocks
	ValidateEicrAction validateEicrAction;

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
		when(EcaUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
		when(mockState.getValidateEicrStatus()).thenReturn(validateEicrStatus);

	}
}
