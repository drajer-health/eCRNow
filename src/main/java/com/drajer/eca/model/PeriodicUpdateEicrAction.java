package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.time.LocalDateTime;
import java.util.List;
import org.hibernate.ObjectDeletedException;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PeriodicUpdateEicrAction extends AbstractAction {

  public static final String CREATE_A_JOB_BASED_ON_TIMING_DATA =
      "Timing Data is present , so create a job based on timing data.";
  private final Logger logger = LoggerFactory.getLogger(PeriodicUpdateEicrAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType, String taskInstanceId) {

    logger.info(" **** START Executing Periodic Update Eicr Action **** ");

    try {
      LaunchDetails details = validateInputAndGetLaunchDetails(obj);
      PatientExecutionState state = ApplicationUtils.getDetailStatus(details);
      PeriodicUpdateEicrStatus status = new PeriodicUpdateEicrStatus();
      status.setActionId(getActionId());

      logger.info(
          " Executing Periodic Update Eicr Action , Prior Execution State : = {}",
          details.getStatus());

      // Evaluate encounter status and conditions
      boolean conditionsMet = evaluatePreConditions(details);
      EncounterStatus encounterStatus = evaluateEncounterStatus(details);

      // Route to appropriate handler based on encounter status
      switch (encounterStatus) {
        case CLOSED:
          handleEncounterClosed(state, status, details);
          break;
        case LONG_RUNNING:
          handleLongRunningEncounter(state, status, details);
          break;
        case OPEN:
          if (conditionsMet) {
            handleOpenEncounter(state, status, details, launchType, taskInstanceId);
          } else {
            handleConditionsNotMet();
          }
          break;
      }
    } finally {
      logger.info(
          " **** END Executing Periodic Update Eicr Action after completing normal execution. **** ");
    }
  }

  private LaunchDetails validateInputAndGetLaunchDetails(Object obj) {
    if (obj instanceof LaunchDetails) {
      return (LaunchDetails) obj;
    }
    String msg =
        "Invalid Object passed to Execute method, Launch Details expected, found : "
            + (obj != null ? obj.getClass().getName() : null);
    logger.error(msg);
    throw new ObjectDeletedException(msg, "0", "launchDetails");
  }

  private boolean evaluatePreConditions(LaunchDetails details) {
    Boolean conditionsMet = matchCondition(details);
    return Boolean.TRUE.equals(conditionsMet);
  }

  private EncounterStatus evaluateEncounterStatus(LaunchDetails details) {
    boolean encounterClosed = EcaUtils.checkEncounterClose(details);
    logger.info(" Encounter is closed = {}", encounterClosed);

    if (encounterClosed) {
      return EncounterStatus.CLOSED;
    }

    boolean longRunningEncounter = EcaUtils.checkLongRunningEncounters(details);
    if (longRunningEncounter) {
      return EncounterStatus.LONG_RUNNING;
    }

    return EncounterStatus.OPEN;
  }

  private void handleEncounterClosed(
      PatientExecutionState state, PeriodicUpdateEicrStatus status, LaunchDetails details) {
    logger.info(" Encounter is closed, hence EICR will not be created. ");
    status.setEicrUpdated(false);
    status.seteICRId("0");
    status.setJobStatus(JobStatus.SKIPPED);
    state.getPeriodicUpdateStatus().add(status);
    state.setPeriodicUpdateJobStatus(JobStatus.COMPLETED);
    EcaUtils.updateDetailStatus(details, state);
  }

  private void handleLongRunningEncounter(
      PatientExecutionState state, PeriodicUpdateEicrStatus status, LaunchDetails details) {
    logger.info(" Encounter is Suspended, hence EICR will not be created. ");
    status.setEicrUpdated(false);
    status.seteICRId("0");
    status.setJobStatus(JobStatus.SKIPPED);
    state.getPeriodicUpdateStatus().add(status);
    state.setPeriodicUpdateJobStatus(JobStatus.SUSPENDED);
    details.setProcessingState(JobStatus.SUSPENDED.toString());
    EcaUtils.updateDetailStatus(details, state);
  }

  private void handleConditionsNotMet() {
    logger.info(" Conditions not met, hence EICR will not be created. ");
  }

  private void handleOpenEncounter(
      PatientExecutionState state,
      PeriodicUpdateEicrStatus status,
      LaunchDetails details,
      WorkflowEvent launchType,
      String taskInstanceId) {
    logger.info(" PreConditions have been Met, evaluating Related Actions. ");

    // Check if related actions are completed
    boolean relatedActsDone = evaluateRelatedActions(state, details, taskInstanceId);

    if (!relatedActsDone) {
      logger.info(" Related Actions are not completed, hence EICR will not be created. ");
      return;
    }

    logger.info(" All Related Actions are completed ");
    handleTimingDataAndJobScheduling(state, status, details, launchType, taskInstanceId);
  }

  private boolean evaluateRelatedActions(
      PatientExecutionState state, LaunchDetails details, String taskInstanceId) {
    if (getRelatedActions() == null || getRelatedActions().isEmpty()) {
      return true;
    }

    List<RelatedAction> racts = getRelatedActions();
    for (RelatedAction actn : racts) {
      if (actn.getRelationship() == ActionRelationshipType.AFTER) {
        String actionId = actn.getRelatedAction().getActionId();

        if (Boolean.FALSE.equals(state.hasActionCompleted(actionId))) {
          logger.info(" Action {} is not completed , hence this action has to wait ", actionId);
          return false;
        }

        logger.info(" Related Action has been completed : {}", actionId);

        // Check if there is any timing constraint based on duration
        if (handleRelatedActionWithDuration(actn, state, details, taskInstanceId)) {
          return false; // Early return after scheduling
        }
      } else {
        logger.info(
            " Action {} is related via {}",
            actn.getRelatedAction().getActionId(),
            actn.getRelationship());
      }
    }

    return true;
  }

  private boolean handleRelatedActionWithDuration(
      RelatedAction actn,
      PatientExecutionState state,
      LaunchDetails details,
      String taskInstanceId) {
    if (actn.getDuration() != null && state.getPeriodicUpdateJobStatus() == JobStatus.NOT_STARTED) {
      logger.info(" Schedule the job for Priodic Update EICR based on the duration.");
      WorkflowService.scheduleJob(
          details.getId(),
          actn.getDuration(),
          EcrActionTypes.PERIODIC_UPDATE_EICR,
          details.getStartDate(),
          taskInstanceId);
      state.setPeriodicUpdateJobStatus(JobStatus.SCHEDULED);
      EcaUtils.updateDetailStatus(details, state);
      return true; // Indicates early return should occur
    }

    logger.info(" No need to scheuled job as it has already been scheduled or completed. ");
    return false;
  }

  private void handleTimingDataAndJobScheduling(
      PatientExecutionState state,
      PeriodicUpdateEicrStatus status,
      LaunchDetails details,
      WorkflowEvent launchType,
      String taskInstanceId) {
    JobStatus jobStatus = state.getPeriodicUpdateJobStatus();

    if (jobStatus == JobStatus.NOT_STARTED) {
      handleJobNotStarted(state, details, taskInstanceId);
    } else if (jobStatus == JobStatus.SCHEDULED
        && state.getCloseOutEicrStatus().getJobStatus() != JobStatus.COMPLETED) {
      handleJobScheduled(state, status, details, launchType, taskInstanceId);
    } else {
      logger.info(" Periodic Update not needed , due to which EICR will not be created. ");
    }
  }

  private void handleJobNotStarted(
      PatientExecutionState state, LaunchDetails details, String taskInstanceId) {
    logger.info(" Related Actions Done and this action has not started ");

    if (getTimingData() != null && !getTimingData().isEmpty()) {
      logger.info(CREATE_A_JOB_BASED_ON_TIMING_DATA);
      scheduleJob(details, state, taskInstanceId);
    } else {
      logger.info(" No job to schedule since there is no timing data ");
    }
  }

  private void handleJobScheduled(
      PatientExecutionState state,
      PeriodicUpdateEicrStatus status,
      LaunchDetails details,
      WorkflowEvent launchType,
      String taskInstanceId) {
    logger.info(" Creating the Periodic Update EICR since the job has been scheduled ");

    // Check Trigger Codes again in case the data has changed
    PatientExecutionState newState = EcaUtils.recheckTriggerCodes(details, launchType);
    boolean dataChanged = EcaUtils.hasNewTriggerCodeMatches(state, newState);

    if (shouldCreateEicr(newState, state, dataChanged)) {
      handlePeriodicUpdateEicrCreation(state, status, details, taskInstanceId, newState);
    } else {
      handleNoTriggerCodeMatch(state, status, details, taskInstanceId, dataChanged);
    }
  }

  private boolean shouldCreateEicr(
      PatientExecutionState newState, PatientExecutionState state, boolean dataChanged) {
    return Boolean.TRUE.equals(newState.getMatchTriggerStatus().getTriggerMatchStatus())
        && newState.getMatchTriggerStatus().getMatchedCodes() != null
        && !newState.getMatchTriggerStatus().getMatchedCodes().isEmpty()
        && (dataChanged || Boolean.FALSE.equals(state.hasEicrBeenCreated()));
  }

  private void handlePeriodicUpdateEicrCreation(
      PatientExecutionState state,
      PeriodicUpdateEicrStatus status,
      LaunchDetails details,
      String taskInstanceId,
      PatientExecutionState newState) {
    logger.info(
        "Creating the EICR for {} action as new trigger code is matched",
        EcrActionTypes.PERIODIC_UPDATE_EICR);

    // Create EICR
    Eicr ecr = EcaUtils.createEicr(details);
    logger.info(
        " EICR created successfully for {} with eICRDocID: {} version: {}",
        EcrActionTypes.PERIODIC_UPDATE_EICR,
        ecr.getEicrDocId(),
        ecr.getDocVersion());

    // Update status
    status.setEicrUpdated(true);
    status.seteICRId(ecr.getId().toString());
    status.setJobStatus(JobStatus.COMPLETED);

    state.getPeriodicUpdateStatus().add(status);
    state.setMatchTriggerStatus(newState.getMatchTriggerStatus());
    EcaUtils.updateDetailStatus(details, state);

    // Save EICR to file
    saveEicrToFile(details, ecr);

    // Schedule next job if timing data exists
    if (getTimingData() != null && !getTimingData().isEmpty()) {
      logger.info(CREATE_A_JOB_BASED_ON_TIMING_DATA);
      scheduleJob(details, state, taskInstanceId);
    }
  }

  private void saveEicrToFile(LaunchDetails details, Eicr ecr) {
    logger.debug(" **** Printing Eicr from Periodic Update EICR ACTION **** ");
    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/"
            + details.getLaunchPatientId()
            + "_PeriodicUpdateEicrAction"
            + LocalDateTime.now().getHour()
            + LocalDateTime.now().getMinute()
            + LocalDateTime.now().getSecond()
            + ".xml";
    ApplicationUtils.saveDataToFile(ecr.getEicrData(), fileName);
    logger.debug(" **** End Printing Eicr from Periodic Update EICR ACTION **** ");
  }

  private void handleNoTriggerCodeMatch(
      PatientExecutionState state,
      PeriodicUpdateEicrStatus status,
      LaunchDetails details,
      String taskInstanceId,
      boolean dataChanged) {
    logger.info(" **** New Trigger Codes Detected: {}", dataChanged);
    logger.info(" Scheduling the timer job for a later time ");

    // Schedule job again if conditions are met
    if (state.getCloseOutEicrStatus().getJobStatus() != JobStatus.COMPLETED
        && getTimingData() != null
        && !getTimingData().isEmpty()) {
      logger.info(CREATE_A_JOB_BASED_ON_TIMING_DATA);
      scheduleJob(details, state, taskInstanceId);
      status.setEicrUpdated(false);
      status.seteICRId("0");
      status.setJobStatus(JobStatus.COMPLETED);
      state.getPeriodicUpdateStatus().add(status);
      EcaUtils.updateDetailStatus(details, state);
    }
  }

  private enum EncounterStatus {
    CLOSED,
    LONG_RUNNING,
    OPEN
  }

  private void scheduleJob(
      LaunchDetails details, PatientExecutionState state, String taskInstanceId) {

    List<TimingSchedule> tsjobs = getTimingData();

    for (TimingSchedule ts : tsjobs) {

      // TBD : Setup job using TS Timing after testing so that we can test faster.
      // For now setup a default job with 10 seconds.
      WorkflowService.scheduleJob(
          details.getId(),
          ts,
          EcrActionTypes.PERIODIC_UPDATE_EICR,
          details.getStartDate(),
          taskInstanceId);
      state.setPeriodicUpdateJobStatus(JobStatus.SCHEDULED);

      EcaUtils.updateDetailStatus(details, state);
      // No need to continue as the job will take over execution.
      logger.info(" **** End Executing Periodic Update Eicr Action **** ");
    }
  }

  @Override
  public void print() {

    logger.info(" **** Printing PeriodicUpdateEicrAction **** ");
    printBase();
    logger.info(" **** End Printing PeriodicUpdateEicrAction **** ");
  }
}
