package com.drajer.eca.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestUtils;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RunWith(PowerMockRunner.class)
@PrepareForTest({EcaUtils.class, ApplicationUtils.class, WorkflowService.class})
public class CreateEicrActionTest {

  private static final Logger logger = LoggerFactory.getLogger(CreateEicrActionTest.class);

  private LaunchDetails mockDetails;
  private PatientExecutionState mockState;
  private RelatedAction mockRelActn;
  private MatchTriggerStatus mockTriggerStatus;
  private Eicr mockEicr;

  private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;

  @InjectMocks CreateEicrAction createtEicrAction;

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

      // Setup
      CreateEicrStatus createEicrStatus = new CreateEicrStatus();
      when(mockState.getCreateEicrStatus()).thenReturn(createEicrStatus);
      when(mockState.hasActionCompleted(any())).thenReturn(false);
      setupMockData();

      // Test
      createtEicrAction.execute(mockDetails, launchType);

      // Validate
      verify(mockState, times(1)).hasActionCompleted("123");
      verify(mockRelActn, times(0)).getDuration();

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  @Test
  public void testExecute_RelatedActionWithDuration() throws Exception {

    try {

      // Setup
      CreateEicrStatus createEicrStatus = new CreateEicrStatus();
      createEicrStatus.setJobStatus(JobStatus.NOT_STARTED);
      when(mockState.getCreateEicrStatus()).thenReturn(createEicrStatus);
      when(mockState.hasActionCompleted(any())).thenReturn(true);

      setupMockData();

      Duration duration = new Duration();
      when(mockRelActn.getDuration()).thenReturn(duration);

      // Test
      createtEicrAction.execute(mockDetails, launchType);

      // Validate
      verify(mockState, times(1)).hasActionCompleted("123");
      verify(mockRelActn, times(2)).getDuration();
      PowerMockito.verifyStatic(WorkflowService.class, times(1));
      WorkflowService.scheduleJob(
          eq(1), any(Duration.class), eq(EcrActionTypes.CREATE_EICR), any(Date.class));

      assertEquals(JobStatus.SCHEDULED, createEicrStatus.getJobStatus());

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  @Test
  public void testExecute_NoDurationJobNotStarted() throws Exception {

    try {

      // Setup
      CreateEicrStatus createEicrStatus = new CreateEicrStatus();
      createEicrStatus.setJobStatus(JobStatus.NOT_STARTED);
      when(mockState.getCreateEicrStatus()).thenReturn(createEicrStatus);
      when(mockState.hasActionCompleted(any())).thenReturn(true);

      TimingSchedule timeSchld = new TimingSchedule();
      createtEicrAction.addTimingData(timeSchld);

      setupMockData();

      // Test
      createtEicrAction.execute(mockDetails, launchType);

      // Validate
      verify(mockState, times(1)).hasActionCompleted("123");
      verify(mockRelActn, times(1)).getDuration();
      PowerMockito.verifyStatic(WorkflowService.class, times(1));
      WorkflowService.scheduleJob(
          eq(1), any(TimingSchedule.class), eq(EcrActionTypes.CREATE_EICR), any(Date.class));

      assertEquals(JobStatus.SCHEDULED, createEicrStatus.getJobStatus());

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  @Test
  public void testExecute_NoDurationJobScheduledTriggerMatch() throws Exception {

    try {

      // Setup
      CreateEicrStatus createEicrStatus = new CreateEicrStatus();
      createEicrStatus.setJobStatus(JobStatus.SCHEDULED);
      when(mockState.getCreateEicrStatus()).thenReturn(createEicrStatus);
      when(mockState.hasActionCompleted(any())).thenReturn(true);
      when(mockState.getMatchTriggerStatus()).thenReturn(mockTriggerStatus);

      TimingSchedule timeSchld = new TimingSchedule();
      createtEicrAction.addTimingData(timeSchld);
      List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
      matchedCodes.add(new MatchedTriggerCodes());

      setupMockData();

      when(EcaUtils.recheckTriggerCodes(mockDetails, launchType)).thenReturn(mockState);
      when(EcaUtils.createEicr(mockDetails)).thenReturn(mockEicr);

      when(mockTriggerStatus.getTriggerMatchStatus()).thenReturn(true);
      when(mockTriggerStatus.getMatchedCodes()).thenReturn(matchedCodes);

      when(mockEicr.getId()).thenReturn(10);
      when(mockEicr.getEicrData()).thenReturn("This is Eicr Document");

      // Test
      createtEicrAction.execute(mockDetails, launchType);

      // Validate
      PowerMockito.verifyStatic(ApplicationUtils.class, times(1));
      ApplicationUtils.saveDataToFile(eq("This is Eicr Document"), anyString());

      assertEquals(JobStatus.COMPLETED, createEicrStatus.getJobStatus());

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  @Test
  public void testExecute_NoDurationJobScheduledTriggerNoMatch() throws Exception {

    try {

      // Setup
      CreateEicrStatus createEicrStatus = new CreateEicrStatus();
      createEicrStatus.setJobStatus(JobStatus.SCHEDULED);
      when(mockState.getCreateEicrStatus()).thenReturn(createEicrStatus);
      when(mockState.hasActionCompleted(any())).thenReturn(true);
      when(mockState.getMatchTriggerStatus()).thenReturn(mockTriggerStatus);

      TimingSchedule timeSchld = new TimingSchedule();
      createtEicrAction.addTimingData(timeSchld);

      setupMockData();

      when(EcaUtils.recheckTriggerCodes(mockDetails, launchType)).thenReturn(mockState);
      when(mockTriggerStatus.getTriggerMatchStatus()).thenReturn(true);

      // Test
      createtEicrAction.execute(mockDetails, launchType);

      // Validate
      PowerMockito.verifyStatic(EcaUtils.class, times(0));
      EcaUtils.createEicr(mockDetails);

      assertEquals(JobStatus.COMPLETED, createEicrStatus.getJobStatus());

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  @Test(expected = RuntimeException.class)
  public void testExecute_DetailObjIsInvalid() throws Exception {

    // Test
    createtEicrAction.execute(null, launchType);
  }

  @Test()
  public void testHasEicrCreated() throws Exception {
    PatientExecutionState patientExecutionState = new PatientExecutionState();
    patientExecutionState =
        (PatientExecutionState)
            TestUtils.getResourceAsObject(
                "R4/Misc/EcaUtils/NewState.json", PatientExecutionState.class);
    boolean hasEicrCreated = patientExecutionState.hasEicrBeenCreated();

    assertTrue(hasEicrCreated);
  }

  private void setupMockData() {

    // Mock LaunchDetails
    when(mockDetails.getId()).thenReturn(1);
    when(mockDetails.getStatus()).thenReturn("MockStatus");
    when(mockDetails.getLaunchPatientId()).thenReturn("100");
    when(mockDetails.getStartDate()).thenReturn(new Date());

    // Mock EcaUtils
    when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);

    // Mock RelatedActions
    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
    when(mockRelActn.getRelatedAction()).thenReturn(createtEicrAction);

    // SetUp CreateEicrAction
    createtEicrAction.addRelatedAction(mockRelActn);
    createtEicrAction.setActionId("123");
  }
}
