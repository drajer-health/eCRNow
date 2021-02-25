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
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PeriodicUpdateEicrAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(PeriodicUpdateEicrAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType) {

    logger.info(" **** START Executing Periodic Update Eicr Action **** ");

    if (obj instanceof LaunchDetails) {

      LaunchDetails details = (LaunchDetails) obj;
      PatientExecutionState state = null;
      PeriodicUpdateEicrStatus status = new PeriodicUpdateEicrStatus();

      state = ApplicationUtils.getDetailStatus(details);
      status.setActionId(getActionId());

      logger.info(
          " Executing Periodic Update Eicr Action , Prior Execution State : = {}",
          details.getStatus());

      // Handle Conditions
      Boolean conditionsMet = true;
      conditionsMet = matchCondition(details);

      // PreConditions Met, then process related actions.
      Boolean relatedActsDone = true;
      if (conditionsMet) {

        logger.info(" PreConditions have been Met, evaluating Related Actions. ");

        if (getRelatedActions() != null && !getRelatedActions().isEmpty()) {

          List<RelatedAction> racts = getRelatedActions();

          for (RelatedAction actn : racts) {

            // Check for all actions AFTER which this action has to be executed for completion.
            if (actn.getRelationship() == ActionRelationshipType.AFTER) {

              // check if the action is completed.
              String actionId = actn.getRelatedAction().getActionId();

              if (!state.hasActionCompleted(actionId)) {

                logger.info(
                    " Action {} is not completed , hence this action has to wait ", actionId);
                relatedActsDone = false;
              } else {

                logger.info(" Related Action has been completed : {}", actionId);

                // Check if there is any timing constraint that needs to be handled.
                if (actn.getDuration() != null
                    && state.getPeriodicUpdateJobStatus() == JobStatus.NOT_STARTED) {

                  // Duration is not null, meaning that the create action has to be delayed by the
                  // duration.
                  logger.info(" Schedule the job for Priodic Update EICR based on the duration.");

                  WorkflowService.scheduleJob(
                      details.getId(),
                      actn.getDuration(),
                      EcrActionTypes.PERIODIC_UPDATE_EICR,
                      details.getStartDate());
                  state.setPeriodicUpdateJobStatus(JobStatus.SCHEDULED);
                  state.getPeriodicUpdateStatus().add(status);

                  EcaUtils.updateDetailStatus(details, state);
                  // No need to continue as the job will take over execution.
                  logger.info(" **** End Executing Periodic Update Eicr Action **** ");
                  return;
                } else {

                  logger.info(
                      " No need to scheuled job as it has already been scheduled or completed. ");
                }
              }
            } else {
              logger.info(
                  " Action {} is related via {}",
                  actn.getRelatedAction().getActionId(),
                  actn.getRelationship());
            }
          }
        }

        // Check Timing Data , No need to check if the state is already scheduled meaning the
        // job was scheduled already.
        if (relatedActsDone) {

          logger.info(" All Related Actions are completed ");

          // Timing constraints are applicable if this job has not started, once it is started
          // the State Machine has to manage the execution.
          if (state.getPeriodicUpdateJobStatus() == JobStatus.NOT_STARTED) {

            logger.info(" Related Actions Done and this action has not started ");

            if (getTimingData() != null && !getTimingData().isEmpty()) {

              logger.info(" Timing Data is present , so create a job based on timing data.");
              scheduleJob(details, state, status);
              return;
            }

            logger.info(" No job to schedule since there is no timing data ");
          } else if (state.getPeriodicUpdateJobStatus() == JobStatus.SCHEDULED
              // && !state.getCreateEicrStatus().getEicrCreated()
              && state.getCloseOutEicrStatus().getJobStatus() != JobStatus.COMPLETED) {

            // Do this only if the job is scheduled.
            logger.info(" Creating the Periodic Update EICR since the job has been scheduled ");

            // Check Trigger Codes again in case the data has changed.
            PatientExecutionState newState = EcaUtils.recheckTriggerCodes(details, launchType);
            boolean dataChanged = EcaUtils.hasNewTriggerCodeMatches(state, newState);

            if (newState.getMatchTriggerStatus().getTriggerMatchStatus()
                && newState.getMatchTriggerStatus().getMatchedCodes() != null
                && !newState.getMatchTriggerStatus().getMatchedCodes().isEmpty()
                && (dataChanged || !state.hasEicrBeenCreated())) {

              // Since the job has started, Execute the job.
              // Call the Loading Queries and create eICR.
              Eicr ecr = EcaUtils.createEicr(details);

              if (ecr != null) {

                status.setEicrUpdated(true);
                status.seteICRId(ecr.getId().toString());
                status.setJobStatus(JobStatus.COMPLETED);

                // newState.setPeriodicUpdateJobStatus(JobStatus.COMPLETED);
                newState.getPeriodicUpdateStatus().add(status);

                EcaUtils.updateDetailStatus(details, newState);

                logger.info(" **** Printing Eicr from Periodic Update EICR ACTION **** ");

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

                logger.info(" **** End Printing Eicr from Periodic Update EICR ACTION **** ");
              }

              // Schedule job again.
              if (getTimingData() != null && !getTimingData().isEmpty()) {

                logger.info(" Timing Data is present , so create a job based on timing data.");
                scheduleJob(details, state, status);
              }

            } // Check if Trigger Code Match found
            else {

              logger.info(" **** New Trigger Codes Detected: {}", dataChanged);
              logger.info(" Scheduling the timer job for a later time ");

              // Schedule job again.
              if (state.getCloseOutEicrStatus().getJobStatus() != JobStatus.COMPLETED
                  && getTimingData() != null
                  && !getTimingData().isEmpty()) {

                logger.info(" Timing Data is present , so create a job based on timing data.");
                scheduleJob(details, state, status);
                return;
              }
            }
          } else {
            logger.info(" Periodic Update not needed , due to which EICR will not be created. ");
          }

        } else {
          logger.info(" Related Actions are not completed, hence EICR will not be created. ");
        }

      } else {

        logger.info(" Conditions not met, hence EICR will not be created. ");
      }
    } else {

      String msg =
          "Invalid Object passed to Execute method, Launch Details expected, found : "
              + obj.getClass().getName();
      logger.error(msg);

      throw new RuntimeException(msg);
    }

    logger.info(" **** END Executing Create Eicr Action after completing normal execution. **** ");
  }

  private void scheduleJob(
      LaunchDetails details, PatientExecutionState state, PeriodicUpdateEicrStatus status) {

    List<TimingSchedule> tsjobs = getTimingData();

    for (TimingSchedule ts : tsjobs) {

      // TBD : Setup job using TS Timing after testing so that we can test faster.
      // For now setup a default job with 10 seconds.
      WorkflowService.scheduleJob(
          details.getId(), ts, EcrActionTypes.PERIODIC_UPDATE_EICR, details.getStartDate());

      state.setPeriodicUpdateJobStatus(JobStatus.SCHEDULED);
      state.getPeriodicUpdateStatus().add(status);

      EcaUtils.updateDetailStatus(details, state);
      // No need to continue as the job will take over execution.
      logger.info(" **** End Executing Periodic Update Eicr Action **** ");
      // return;
    }
  }

  @Override
  public void print() {

    logger.info(" **** Printing PeriodicUpdateEicrAction **** ");
    printBase();
    logger.info(" **** End Printing PeriodicUpdateEicrAction **** ");
  }
}
