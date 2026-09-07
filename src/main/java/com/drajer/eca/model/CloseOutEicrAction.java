package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.LaunchDetails.ProcessingStatus;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.LocalDateTime;
import java.util.List;
import org.apache.commons.text.StringEscapeUtils;
import org.hibernate.ObjectDeletedException;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CloseOutEicrAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(CloseOutEicrAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType, String taskInstanceId) {
    logger.info(" **** START Executing Close Out Eicr Action **** ");

    if (!(obj instanceof LaunchDetails)) {
      String msg =
          "Invalid Object passed to Execute method, Launch Details expected, found : "
              + (obj != null ? obj.getClass().getName() : null);
      logger.error(msg);
      throw new ObjectDeletedException(msg, "0", "launchDetails");
    }

    LaunchDetails details = (LaunchDetails) obj;
    ObjectMapper mapper = new ObjectMapper();
    PatientExecutionState state = ApplicationUtils.getDetailStatus(details);
    state.getCloseOutEicrStatus().setActionId(getActionId());

    logger.info(
        " Executing Close Out Eicr Action , Prior Execution State : = {}",
        StringEscapeUtils.escapeJava(details.getStatus()));

    Boolean conditionsMet = matchCondition(details);
    boolean encounterClosed = EcaUtils.checkEncounterClose(details);
    logger.info(" Encounter is closed = {}", encounterClosed);

    if (Boolean.TRUE.equals(conditionsMet) && encounterClosed) {
      processWhenConditionsMetAndEncounterClosed(
          details, state, launchType, mapper, taskInstanceId);
    } else if (encounterClosed) {
      processWhenEncounterClosedButConditionsNotMet(details, state, mapper, taskInstanceId);
    } else {
      logger.info(" Encounter is not closed, hence close out action will not be scheduled. ");
    }

    logger.info(
        " **** END Executing Close Out Eicr Action after completing normal execution. **** ");
  }

  private void processWhenConditionsMetAndEncounterClosed(
      LaunchDetails details,
      PatientExecutionState state,
      WorkflowEvent launchType,
      ObjectMapper mapper,
      String taskInstanceId) {
    logger.info(" PreConditions have been Met, evaluating Related Actions. ");

    RelatedActionResult result = processRelatedActions(details, state, mapper, taskInstanceId);

    if (result.jobScheduledFromDuration) {
      logger.info(" **** END Executing Close Out Eicr Action **** ");
      return;
    }

    if (result.relatedActsDone) {
      processTimingDataAndEicrCreation(details, state, launchType, mapper, taskInstanceId);
    } else {
      logger.info(" Related Actions are not completed, hence EICR will not be created.");
    }
  }

  private void processWhenEncounterClosedButConditionsNotMet(
      LaunchDetails details,
      PatientExecutionState state,
      ObjectMapper mapper,
      String taskInstanceId) {
    logger.info(" Conditions not met, hence Close Out Action will have to be rescheduled . ");
    List<RelatedAction> racts = getRelatedActions();

    if (racts != null) {
      for (RelatedAction ract : racts) {
        if (ract.getRelationship() == ActionRelationshipType.AFTER) {
          logger.info(" Scheduling the job using related actions ");
          scheduleJob(details, state, ract, mapper, taskInstanceId);
        }
      }
    }
  }

  private static class RelatedActionResult {
    boolean relatedActsDone;
    boolean jobScheduledFromDuration;

    RelatedActionResult(boolean relatedActsDone, boolean jobScheduledFromDuration) {
      this.relatedActsDone = relatedActsDone;
      this.jobScheduledFromDuration = jobScheduledFromDuration;
    }
  }

  private RelatedActionResult processRelatedActions(
      LaunchDetails details,
      PatientExecutionState state,
      ObjectMapper mapper,
      String taskInstanceId) {
    if (getRelatedActions() == null || getRelatedActions().isEmpty()) {
      return new RelatedActionResult(true, false);
    }

    for (RelatedAction ract : getRelatedActions()) {
      if (ract.getRelationship() != ActionRelationshipType.AFTER) {
        logger.info(
            " Action {} is related via {}",
            ract.getRelatedAction().getActionId(),
            ract.getRelationship());
        continue;
      }

      String actionId = ract.getRelatedAction().getActionId();
      if (Boolean.FALSE.equals(state.hasActionCompleted(actionId))) {
        logger.info(" Action {} is not completed , hence this action has to wait ", actionId);
        return new RelatedActionResult(false, false);
      }

      logger.info(" Related Action that has been completed : {}", actionId);

      if (shouldScheduleJobForDuration(ract, state)) {
        logger.info(" Schedule the job for Close Out EICR Action based on the duration.");
        scheduleJob(details, state, ract, mapper, taskInstanceId);
        return new RelatedActionResult(true, true);
      } else {
        logger.info(" No need to schedule job as it has already been scheduled or completed. ");
      }
    }

    return new RelatedActionResult(true, false);
  }

  private boolean shouldScheduleJobForDuration(RelatedAction ract, PatientExecutionState state) {
    return ract.getDuration() != null
        && state.getCloseOutEicrStatus().getJobStatus() == JobStatus.NOT_STARTED;
  }

  private void processTimingDataAndEicrCreation(
      LaunchDetails details,
      PatientExecutionState state,
      WorkflowEvent launchType,
      ObjectMapper mapper,
      String taskInstanceId) {
    logger.info(" All Related Actions are completed ");

    if (state.getCloseOutEicrStatus().getJobStatus() == JobStatus.NOT_STARTED) {
      processTimingDataScheduling(details, state, mapper, taskInstanceId);
    } else if (state.getCloseOutEicrStatus().getJobStatus() == JobStatus.SCHEDULED
        && launchType == WorkflowEvent.SCHEDULED_JOB) {
      createCloseOutEicrIfTriggered(details, state);
    } else {
      logger.info(
          " Close Out Eicr Action not creating Eicr because state = {}",
          state.getCloseOutEicrStatus().getJobStatus());
    }
  }

  private void processTimingDataScheduling(
      LaunchDetails details,
      PatientExecutionState state,
      ObjectMapper mapper,
      String taskInstanceId) {
    logger.info(" Related Actions Done and this action has not started ");

    if (getTimingData() == null || getTimingData().isEmpty()) {
      logger.info(" Job Not Scheduled since there is no timing data ");
      return;
    }

    logger.info(" Timing Data is present , so create a job based on timing data.");
    for (TimingSchedule ts : getTimingData()) {
      WorkflowService.scheduleJob(
          details.getId(),
          ts,
          EcrActionTypes.CLOSE_OUT_EICR,
          details.getStartDate(),
          taskInstanceId);
      state.getCloseOutEicrStatus().setJobStatus(JobStatus.SCHEDULED);
      EcaUtils.updateDetailStatus(details, state);
    }
  }

  private void createCloseOutEicrIfTriggered(LaunchDetails details, PatientExecutionState state) {
    logger.info(" Creating the Close Out EICR since the job has been scheduled ");

    PatientExecutionState newState =
        EcaUtils.recheckTriggerCodes(details, WorkflowEvent.SCHEDULED_JOB);

    if (!isTriggerCodeMatched(newState)) {
      logger.info(" **** Trigger Code did not match, hence not creating EICR **** ");
      newState.getCloseOutEicrStatus().setEicrClosed(false);
      newState.getCloseOutEicrStatus().seteICRId("0");
      newState.getCloseOutEicrStatus().setJobStatus(JobStatus.COMPLETED);
      details.setProcessingState(LaunchDetails.getString(ProcessingStatus.COMPLETED));
      EcaUtils.updateDetailStatus(details, newState);
      return;
    }

    logger.info(
        "Creating the EICR for {} action as new trigger code is matched",
        EcrActionTypes.CLOSE_OUT_EICR);

    Eicr ecr = EcaUtils.createEicr(details);
    logger.info(
        " EICR created successfully for {} with eICRDocID: {} version: {}",
        EcrActionTypes.CLOSE_OUT_EICR,
        ecr.getEicrDocId(),
        ecr.getDocVersion());

    newState.getCloseOutEicrStatus().setEicrClosed(true);
    newState.getCloseOutEicrStatus().seteICRId(ecr.getId().toString());
    newState.getCloseOutEicrStatus().setJobStatus(JobStatus.COMPLETED);
    details.setProcessingState(LaunchDetails.getString(ProcessingStatus.COMPLETED));
    EcaUtils.updateDetailStatus(details, newState);

    saveCloseOutEicrToFile(details, ecr);
  }

  private boolean isTriggerCodeMatched(PatientExecutionState state) {
    return Boolean.TRUE.equals(state.getMatchTriggerStatus().getTriggerMatchStatus())
        && state.getMatchTriggerStatus().getMatchedCodes() != null
        && !state.getMatchTriggerStatus().getMatchedCodes().isEmpty();
  }

  private void saveCloseOutEicrToFile(LaunchDetails details, Eicr ecr) {
    logger.debug(" **** Printing Eicr from CLOSE OUT EICR ACTION **** ");
    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/"
            + details.getLaunchPatientId()
            + "_CloseOutEicrAction"
            + LocalDateTime.now().getHour()
            + LocalDateTime.now().getMinute()
            + LocalDateTime.now().getSecond()
            + ".xml";
    ApplicationUtils.saveDataToFile(ecr.getEicrData(), fileName);
    logger.debug(" **** End Printing Eicr from CLOSE OUT EICR ACTION **** ");
  }

  public void scheduleJob(
      LaunchDetails details,
      PatientExecutionState state,
      RelatedAction ract,
      ObjectMapper mapper,
      String taskInstanceId) {

    try {
      logger.info(" **** Start Scheduling Close Out Eicr Action Job **** ");
      List<TimingSchedule> tsjobs = getTimingData();

      if (tsjobs != null) {
        for (TimingSchedule ts : tsjobs) {

          // TBD : Setup job using TS Timing after testing so that we can test faster.
          // For now setup a default job with 10 seconds.

          WorkflowService.scheduleJob(
              details.getId(),
              ts,
              EcrActionTypes.CLOSE_OUT_EICR,
              details.getStartDate(),
              taskInstanceId);
        }
      } else {
        WorkflowService.scheduleJob(
            details.getId(),
            ract.getDuration(),
            EcrActionTypes.CLOSE_OUT_EICR,
            details.getStartDate(),
            taskInstanceId);
      }

      state.getCloseOutEicrStatus().setJobStatus(JobStatus.SCHEDULED);
      details.setStatus(mapper.writeValueAsString(state));

      // No need to continue as the job will take over execution.

      logger.info(" **** Finished Scheduling Close Out Eicr Action Job **** ");
    } catch (JsonProcessingException e) {
      String msg = "Unable to read/write execution state";
      logger.error(msg, e);
      throw new RuntimeException(msg);
    }
  }

  @Override
  public void print() {

    logger.info(" **** Printing CloseOutEicrAction **** ");
    printBase();
    logger.info(" **** End Printing CloseOutEicrAction **** ");
  }
}
