package com.drajer.eca.model;

import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
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
import com.fasterxml.jackson.databind.ObjectMapper;
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
public class PeriodicUpdateEicrActionTest {

  private static final Logger logger = LoggerFactory.getLogger(PeriodicUpdateEicrActionTest.class);
  private LaunchDetails mockDetails;
  private PatientExecutionState mockState;
  private RelatedAction mockRelActn;
  private MatchTriggerStatus mockTriggerStatus;
  private Eicr mockEicr;

  private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;

  @InjectMocks PeriodicUpdateEicrAction periodicUpdateEicrAction;

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

      EventTypes.JobStatus periodicUpdateJobStatus = JobStatus.NOT_STARTED;
      when(mockState.hasActionCompleted(any())).thenReturn(false);
      when(mockState.getPeriodicUpdateJobStatus()).thenReturn(periodicUpdateJobStatus);

      setupMockData();

      // Test
      periodicUpdateEicrAction.execute(mockDetails, launchType);

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

      EventTypes.JobStatus periodicUpdateJobStatus = JobStatus.NOT_STARTED;
      when(mockState.getPeriodicUpdateJobStatus()).thenReturn(periodicUpdateJobStatus);
      when(mockState.hasActionCompleted(any())).thenReturn(true);

      setupMockData();

      Duration duration = new Duration();
      when(mockRelActn.getDuration()).thenReturn(duration);

      // Test
      periodicUpdateEicrAction.execute(mockDetails, launchType);

      // Validate
      verify(mockState, times(1)).hasActionCompleted("123");
      verify(mockRelActn, times(2)).getDuration();
      PowerMockito.verifyStatic(WorkflowService.class, times(1));
      WorkflowService.scheduleJob(
          eq(1), any(Duration.class), eq(EcrActionTypes.PERIODIC_UPDATE_EICR), any(Date.class));

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  @Test
  public void testExecute_NoDurationJobNotStarted() throws Exception {

    try {
      EventTypes.JobStatus periodicUpdateJobStatus = JobStatus.NOT_STARTED;
      when(mockState.getPeriodicUpdateJobStatus()).thenReturn(periodicUpdateJobStatus);
      when(mockState.hasActionCompleted(any())).thenReturn(true);

      TimingSchedule timeSchld = new TimingSchedule();
      periodicUpdateEicrAction.addTimingData(timeSchld);

      setupMockData();

      periodicUpdateEicrAction.execute(mockDetails, launchType);

      // Validate
      verify(mockState, times(1)).hasActionCompleted("123");
      verify(mockRelActn, times(1)).getDuration();
      PowerMockito.verifyStatic(WorkflowService.class, times(1));
      WorkflowService.scheduleJob(
          eq(1),
          any(TimingSchedule.class),
          eq(EcrActionTypes.PERIODIC_UPDATE_EICR),
          any(Date.class));

      verify(mockState, times(1)).hasActionCompleted("123");

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

      PeriodicUpdateEicrAction periodicUpdateEicrAction = new PeriodicUpdateEicrAction();
      when(mockRelActn.getRelatedAction()).thenReturn(periodicUpdateEicrAction);

      CloseOutEicrStatus closeOutEicrStatus = new CloseOutEicrStatus();
      when(mockState.getCloseOutEicrStatus()).thenReturn(closeOutEicrStatus);

      TimingSchedule timeSchld = new TimingSchedule();
      periodicUpdateEicrAction.addTimingData(timeSchld);
      List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
      matchedCodes.add(new MatchedTriggerCodes());

      setupMockData();

      EventTypes.JobStatus periodicUpdateJobStatus = JobStatus.SCHEDULED;

      when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
      when(mockState.getPeriodicUpdateJobStatus()).thenReturn(periodicUpdateJobStatus);

      when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);

      when(mockState.hasActionCompleted(any())).thenReturn(true);
      when(mockRelActn.getDuration()).thenReturn(null);
      when(EcaUtils.recheckTriggerCodes(mockDetails, launchType)).thenReturn(mockState);
      when(mockState.getMatchTriggerStatus()).thenReturn(mockTriggerStatus);
      when(mockTriggerStatus.getTriggerMatchStatus()).thenReturn(true);
      when(mockTriggerStatus.getMatchedCodes()).thenReturn(matchedCodes);
      when(EcaUtils.createEicr(mockDetails)).thenReturn(mockEicr);
      when(mockEicr.getId()).thenReturn(10);
      when(mockEicr.getEicrData()).thenReturn("This is periodic EICR");

      // Test
      periodicUpdateEicrAction.execute(mockDetails, launchType);

      // Validate
      PowerMockito.verifyStatic(ApplicationUtils.class, times(1));
      ApplicationUtils.saveDataToFile(eq("This is periodic EICR"), anyString());

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  @Test
  public void testExecute_NoDurationJobScheduledTriggerNoMatch() throws Exception {

    try {

      // Setup
      PeriodicUpdateEicrAction periodicUpdateEicrAction = new PeriodicUpdateEicrAction();
      CreateEicrStatus createEicrStatus = new CreateEicrStatus();
      CloseOutEicrStatus closeOutEicrStatus = new CloseOutEicrStatus();
      TimingSchedule timeSchld = new TimingSchedule();
      setupMockData();

      periodicUpdateEicrAction.addRelatedAction(mockRelActn);
      periodicUpdateEicrAction.setActionId("123");
      periodicUpdateEicrAction.addTimingData(timeSchld);
      createEicrStatus.setJobStatus(JobStatus.SCHEDULED);

      EventTypes.JobStatus periodicUpdateJobStatus = JobStatus.SCHEDULED;

      when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
      when(mockState.getPeriodicUpdateJobStatus()).thenReturn(periodicUpdateJobStatus);
      when(mockState.getCreateEicrStatus()).thenReturn(createEicrStatus);
      when(mockState.getCloseOutEicrStatus()).thenReturn(closeOutEicrStatus);
      when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
      when(mockRelActn.getRelatedAction()).thenReturn(periodicUpdateEicrAction);
      when(mockState.hasActionCompleted(any())).thenReturn(true);
      when(mockRelActn.getDuration()).thenReturn(null);
      when(EcaUtils.recheckTriggerCodes(mockDetails, launchType)).thenReturn(mockState);
      when(mockState.getMatchTriggerStatus()).thenReturn(mockTriggerStatus);
      when(mockTriggerStatus.getTriggerMatchStatus()).thenReturn(true);

      // Test
      periodicUpdateEicrAction.execute(mockDetails, launchType);

      // Validate
      PowerMockito.verifyStatic(WorkflowService.class, times(1));
      WorkflowService.scheduleJob(
          eq(1),
          any(TimingSchedule.class),
          eq(EcrActionTypes.PERIODIC_UPDATE_EICR),
          any(Date.class));

      verify(mockState, times(1)).hasActionCompleted("123");

    } catch (Exception e) {

      logger.error("Exception occured during the test:::::{}", e);
      fail("This exception is not expected, fix the test method");
    }
  }

  /*
   * In case invalid Object passed to Execute method,
   */
  @Test(expected = RuntimeException.class)
  public void whenInvalidObjectPassedThrowRuntimeException() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
    PeriodicUpdateEicrAction periodicUpdateEicrAction = new PeriodicUpdateEicrAction();
    periodicUpdateEicrAction.execute(mapper, launchType);
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
    when(mockRelActn.getRelatedAction()).thenReturn(periodicUpdateEicrAction);

    // SetUp CreateEicrAction
    periodicUpdateEicrAction.addRelatedAction(mockRelActn);
    periodicUpdateEicrAction.setActionId("123");
  }
}
