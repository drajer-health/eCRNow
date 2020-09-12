package com.drajer.eca.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
import static org.junit.Assert.fail;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
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
public class CloseOutEicrActionTest {

	private LaunchDetails mockDetails;
	private PatientExecutionState mockState;
	private RelatedAction mockRelActn;
	private MatchTriggerStatus mockTriggerStatus;
	private Eicr mockEicr;

	private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;

	@InjectMocks
	CloseOutEicrAction closeOutEicrAction;

	@Before
	public void setUp() {

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

		try {

			CloseOutEicrStatus closeOutEicrStatus = new CloseOutEicrStatus();
			when(EcaUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
			when(mockState.getCloseOutEicrStatus()).thenReturn(closeOutEicrStatus);
			when(mockState.hasActionCompleted(any())).thenReturn(false);

			setupMockData();

			// Test
			closeOutEicrAction.execute(mockDetails, launchType);

			// Validate
			verify(mockState, times(1)).hasActionCompleted("123");
			verify(mockRelActn, times(0)).getDuration();

		} catch (Exception e) {

			e.printStackTrace();
			fail("This exception is not expected, fix the test method");
		}

	}

	@Test
	public void testExecute_NoDurationJobNotStarted() throws Exception {

		try {

			CloseOutEicrStatus closeOutEicrStatus = new CloseOutEicrStatus();
			closeOutEicrStatus.setJobStatus(JobStatus.NOT_STARTED);
			when(mockState.getCloseOutEicrStatus()).thenReturn(closeOutEicrStatus);
			when(mockState.hasActionCompleted(any())).thenReturn(true);

			TimingSchedule timeSchld = new TimingSchedule();
			closeOutEicrAction.addTimingData(timeSchld);

			setupMockData();

			when(mockRelActn.getDuration()).thenReturn(null);

			// Test
			closeOutEicrAction.execute(mockDetails, launchType);

			// Validate
			verify(mockState, times(1)).hasActionCompleted("123");
			verify(mockRelActn, times(1)).getDuration();
			PowerMockito.verifyStatic(WorkflowService.class, times(1));
			WorkflowService.scheduleJob(eq(1), any(TimingSchedule.class), eq(EcrActionTypes.CLOSE_OUT_EICR), any(Date.class));

			assertEquals(closeOutEicrStatus.getJobStatus(), JobStatus.SCHEDULED);

		} catch (Exception e) {

			e.printStackTrace();
			fail("This exception is not expected, fix the test method");
		}

	}

	@Test
	public void testExecute_NoDurationJobScheduledTriggerMatch() throws Exception {

		try {

			CloseOutEicrStatus closeOutEicrStatus = new CloseOutEicrStatus();
			closeOutEicrStatus.setJobStatus(JobStatus.SCHEDULED);
			when(mockState.getCloseOutEicrStatus()).thenReturn(closeOutEicrStatus);
			when(mockState.hasActionCompleted(any())).thenReturn(true);
			when(mockState.getMatchTriggerStatus()).thenReturn(mockTriggerStatus);

			TimingSchedule timeSchld = new TimingSchedule();

			closeOutEicrAction.addTimingData(timeSchld);
			List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
			matchedCodes.add(new MatchedTriggerCodes());

			setupMockData();

			when(EcaUtils.recheckTriggerCodes(mockDetails, launchType)).thenReturn(mockState);
			when(EcaUtils.createEicr(mockDetails)).thenReturn(mockEicr);

			when(mockTriggerStatus.getTriggerMatchStatus()).thenReturn(true);
			when(mockTriggerStatus.getMatchedCodes()).thenReturn(matchedCodes);

			when(mockEicr.getId()).thenReturn(10);
			when(mockEicr.getData()).thenReturn("**** Printing Eicr from CLOSE OUT EICR ACTION ****");

			// Test
			closeOutEicrAction.execute(mockDetails, launchType);

			// Validate
			PowerMockito.verifyStatic(ApplicationUtils.class, times(1));
			ApplicationUtils.saveDataToFile(eq("**** Printing Eicr from CLOSE OUT EICR ACTION ****"), anyString());

			assertEquals(closeOutEicrStatus.getJobStatus(), JobStatus.COMPLETED);

		} catch (Exception e) {

			e.printStackTrace();
			fail("This exception is not expected, fix the test method");
		}
	}

	@Test
	public void testExecute_NoDurationJobScheduledTriggerNoMatch() {

		try {

			CloseOutEicrStatus closeOutEicrStatus = new CloseOutEicrStatus();
			closeOutEicrStatus.setJobStatus(JobStatus.SCHEDULED);
			when(mockState.getCloseOutEicrStatus()).thenReturn(closeOutEicrStatus);
			when(mockState.hasActionCompleted(any())).thenReturn(true);
			when(mockState.getMatchTriggerStatus()).thenReturn(mockTriggerStatus);

			TimingSchedule timeSchld = new TimingSchedule();

			closeOutEicrAction.addTimingData(timeSchld);

			setupMockData();

			when(EcaUtils.recheckTriggerCodes(mockDetails, launchType)).thenReturn(mockState);
			when(mockTriggerStatus.getTriggerMatchStatus()).thenReturn(true);

			// Test
			closeOutEicrAction.execute(mockDetails, launchType);

			// Validate
			PowerMockito.verifyStatic(EcaUtils.class, times(0));
			EcaUtils.createEicr(mockDetails);

			assertEquals(closeOutEicrStatus.getJobStatus(), JobStatus.COMPLETED);

		} catch (Exception e) {

			e.printStackTrace();
			fail("This exception is not expected, fix the test method");
		}
	}

	private void setupMockData() {

		// Mock LaunchDetails
		when(mockDetails.getId()).thenReturn(1);
		when(mockDetails.getStatus()).thenReturn("MockStatus");
		when(mockDetails.getLaunchPatientId()).thenReturn("100");
		when(mockDetails.getStartDate()).thenReturn(new Date());

		// Mock EcaUtils
		when(EcaUtils.getDetailStatus(mockDetails)).thenReturn(mockState);

		// Mock RelatedActions
		when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
		when(mockRelActn.getRelatedAction()).thenReturn(closeOutEicrAction);

		// SetUp CreateEicrAction
		closeOutEicrAction.addRelatedAction(mockRelActn);
		closeOutEicrAction.setActionId("123");

	}

}
