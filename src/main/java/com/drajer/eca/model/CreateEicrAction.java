package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.time.LocalDateTime;
import org.hibernate.ObjectDeletedException;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.logging.LogLevel;
import org.springframework.stereotype.Service;

@Service
public class CreateEicrAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(CreateEicrAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType, String taskInstanceId) {
    logger.info(" **** START Executing Create Eicr Action **** ");

    if (!(obj instanceof LaunchDetails)) {
      String msg =
          "Invalid Object passed to Execute method, Launch Details expected, found : "
              + (obj != null ? obj.getClass().getName() : null);
      logger.error(msg);
      throw new ObjectDeletedException(msg, "0", "launchDetails");
    }

    PatientExecutionState state = null;
    try {
      LaunchDetails details = (LaunchDetails) obj;
      state = ApplicationUtils.getDetailStatus(details);
      state.getCreateEicrStatus().setActionId(getActionId());

      logger.info(
          " Executing Create Eicr Action , Prior Execution State : = {}", details.getStatus());

      Boolean conditionsMet = matchCondition(details);
      Boolean validationMode = details.getValidationMode();

      if (!Boolean.TRUE.equals(conditionsMet) && !Boolean.TRUE.equals(validationMode)) {
        logger.info("Conditions not met, hence EICR will not be created.");
        return;
      }

      logger.info(" PreConditions have been Met, evaluating Related Actions. ");
      RelatedActionResult result =
          processRelatedActions(details, state, launchType, taskInstanceId);

      if (result.jobScheduledFromDuration) {
        logger.info(" **** END Executing Create Eicr Action **** ");
        return;
      }

      if (result.relatedActsDone || Boolean.TRUE.equals(validationMode)) {
        processTimingDataAndEicrCreation(
            details, state, launchType, taskInstanceId, validationMode);
      } else {
        logger.info(" Related Actions are not completed, hence EICR will not be created.");
      }

    } catch (Exception e) {
      handleExecutionException(e, state);
    }

    logger.info("**** END Executing Create Eicr Action after completing normal execution. ****");
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
      WorkflowEvent launchType,
      String taskInstanceId) {
    if (getRelatedActions() == null || getRelatedActions().isEmpty()) {
      return new RelatedActionResult(true, false);
    }

    boolean relatedActsDone = true;
    Boolean validationMode = details.getValidationMode();

    for (RelatedAction act : getRelatedActions()) {
      if (act.getRelationship() != ActionRelationshipType.AFTER) {
        logger.info(
            " Action {} is related via {}",
            act.getRelatedAction().getActionId(),
            act.getRelationship());
        continue;
      }

      String actionId = act.getRelatedAction().getActionId();
      if (Boolean.FALSE.equals(state.hasActionCompleted(actionId))
          && Boolean.FALSE.equals(validationMode)) {
        logger.info(" Action {} is not completed , hence this action has to wait ", actionId);
        return new RelatedActionResult(false, false);
      }

      logger.info(" Related Action has been completed : {}", actionId);

      if (shouldScheduleJobForDuration(act, state, validationMode)) {
        logger.info(" Schedule the job for Create EICR based on the duration.");
        WorkflowService.scheduleJob(
            details.getId(),
            act.getDuration(),
            EcrActionTypes.CREATE_EICR,
            details.getStartDate(),
            taskInstanceId);
        state.getCreateEicrStatus().setJobStatus(JobStatus.SCHEDULED);
        EcaUtils.updateDetailStatus(details, state);
        return new RelatedActionResult(true, true);
      }
    }

    return new RelatedActionResult(relatedActsDone, false);
  }

  private boolean shouldScheduleJobForDuration(
      RelatedAction act, PatientExecutionState state, Boolean validationMode) {
    return act.getDuration() != null
        && state.getCreateEicrStatus().getJobStatus() == JobStatus.NOT_STARTED
        && Boolean.FALSE.equals(validationMode);
  }

  private void processTimingDataAndEicrCreation(
      LaunchDetails details,
      PatientExecutionState state,
      WorkflowEvent launchType,
      String taskInstanceId,
      Boolean validationMode) {
    logger.info(" All Related Actions are completed ");

    // Schedule job if not started
    if (state.getCreateEicrStatus().getJobStatus() == JobStatus.NOT_STARTED
        && Boolean.FALSE.equals(validationMode)) {
      processTimingData(details, state, taskInstanceId);
      return;
    }

    // Create EICR if job is scheduled
    if ((state.getCreateEicrStatus().getJobStatus() == JobStatus.SCHEDULED
            && launchType == WorkflowEvent.SCHEDULED_JOB)
        || Boolean.TRUE.equals(validationMode)) {
      createEicrIfTriggered(details, state);
    } else {
      logger.info(
          "EICR job is in a state of {} , due to which EICR will not be created.",
          state.getCreateEicrStatus().getJobStatus());
    }
  }

  private void processTimingData(
      LaunchDetails details, PatientExecutionState state, String taskInstanceId) {
    logger.info(" Related Actions Done and this action has not started ");

    if (getTimingData() == null || getTimingData().isEmpty()) {
      logger.info(" No job to schedule since there is no timing data ");
      return;
    }

    logger.info(" Timing Data is present , so create a job based on timing data.");
    for (TimingSchedule ts : getTimingData()) {
      WorkflowService.scheduleJob(
          details.getId(), ts, EcrActionTypes.CREATE_EICR, details.getStartDate(), taskInstanceId);
      state.getCreateEicrStatus().setJobStatus(JobStatus.SCHEDULED);
      EcaUtils.updateDetailStatus(details, state);
    }
  }

  private void createEicrIfTriggered(LaunchDetails details, PatientExecutionState state) {
    logger.info(" Creating the EICR since the job has been scheduled ");

    PatientExecutionState newState =
        EcaUtils.recheckTriggerCodes(details, WorkflowEvent.SCHEDULED_JOB);

    if (!isTriggerCodeMatched(newState)) {
      logger.info(" **** Trigger Code did not match, hence not creating EICR **** ");
      newState.getCreateEicrStatus().setEicrCreated(false);
      newState.getCreateEicrStatus().seteICRId("0");
      newState.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);
      EcaUtils.updateDetailStatus(details, newState);
      return;
    }

    logger.info(
        "Creating the EICR for {} action as new trigger code is matched",
        EcrActionTypes.CREATE_EICR);

    Eicr ecr = EcaUtils.createEicr(details);
    logger.info(
        " EICR created successfully for {} with eICRDocID: {} version: {}",
        EcrActionTypes.CREATE_EICR,
        ecr.getEicrDocId(),
        ecr.getDocVersion());

    newState.getCreateEicrStatus().setEicrCreated(true);
    newState.getCreateEicrStatus().seteICRId(ecr.getId().toString());
    newState.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);
    EcaUtils.updateDetailStatus(details, newState);

    saveEicrToFile(details, ecr);
  }

  private boolean isTriggerCodeMatched(PatientExecutionState state) {
    return Boolean.TRUE.equals(state.getMatchTriggerStatus().getTriggerMatchStatus())
        && state.getMatchTriggerStatus().getMatchedCodes() != null
        && !state.getMatchTriggerStatus().getMatchedCodes().isEmpty();
  }

  private void saveEicrToFile(LaunchDetails details, Eicr ecr) {
    logger.debug(" **** Printing Eicr from CREATE EICR ACTION **** ");
    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/"
            + details.getLaunchPatientId()
            + "_CreateEicrAction"
            + LocalDateTime.now().getHour()
            + LocalDateTime.now().getMinute()
            + LocalDateTime.now().getSecond()
            + ".xml";
    ApplicationUtils.saveDataToFile(ecr.getEicrData(), fileName);
    logger.debug(" **** End Printing Eicr from CREATE EICR ACTION **** ");
  }

  private void handleExecutionException(Exception e, PatientExecutionState state) {
    StringBuilder expMsg = new StringBuilder();
    if (state != null) {
      expMsg.append("Unable to create Eicr due to exceptions during processing");
      state.getCreateEicrStatus().setEicrCreated(false);
      state.getCreateEicrStatus().seteICRId("0");
      state.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);
    } else {
      expMsg.append(
          "Unable to create Eicr due to exceptions during processing. The state is not present hence not updating it");
    }
    ApplicationUtils.handleException(e, expMsg.toString(), LogLevel.ERROR);
  }

  @Override
  public void print() {

    logger.info(" **** Printing CreateEicrAction **** ");
    printBase();
    logger.info(" **** End Printing CreateEicrAction **** ");
  }
}
