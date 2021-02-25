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
import org.springframework.stereotype.Service;

@Service
public class CreateEicrAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(CreateEicrAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType) {

    logger.info(" **** START Executing Create Eicr Action **** ");

    if (obj instanceof LaunchDetails) {

      LaunchDetails details = (LaunchDetails) obj;
      PatientExecutionState state = null;

      state = ApplicationUtils.getDetailStatus(details);
      state.getCreateEicrStatus().setActionId(getActionId());

      logger.info(
          " Executing Create Eicr Action , Prior Execution State : = {}", details.getStatus());

      // Handle Conditions
      Boolean conditionsMet = true;
      conditionsMet = matchCondition(details);

      // PreConditions Met, then process related actions.
      Boolean relatedActsDone = true;
      Boolean validationMode = details.getValidationMode();
      if (conditionsMet || validationMode) {

        logger.info(" PreConditions have been Met, evaluating Related Actions. ");

        if (getRelatedActions() != null && !getRelatedActions().isEmpty()) {

          List<RelatedAction> racts = getRelatedActions();

          for (RelatedAction act : racts) {

            // Check for all actions AFTER which this action has to be executed for completion.
            if (act.getRelationship() == ActionRelationshipType.AFTER) {

              // check if the action is completed.
              String actionId = act.getRelatedAction().getActionId();

              if (!state.hasActionCompleted(actionId) && !validationMode) {

                logger.info(
                    " Action {} is not completed , hence this action has to wait ", actionId);
                relatedActsDone = false;
              } else {

                logger.info(" Related Action has been completed : {}", actionId);

                // Check if there is any timing constraint that needs to be handled.
                if (act.getDuration() != null
                    && state.getCreateEicrStatus().getJobStatus() == JobStatus.NOT_STARTED
                    && !validationMode) {

                  // Duration is not null, meaning that the create action has to be delayed by the
                  // duration.
                  logger.info(" Schedule the job for Create EICR based on the duration.");

                  WorkflowService.scheduleJob(
                      details.getId(),
                      act.getDuration(),
                      EcrActionTypes.CREATE_EICR,
                      details.getStartDate());
                  state.getCreateEicrStatus().setJobStatus(JobStatus.SCHEDULED);
                  EcaUtils.updateDetailStatus(details, state);

                  // No need to continue as the job will take over execution.
                  logger.info(" **** END Executing Create Eicr Action **** ");
                  return;
                } else {

                  logger.info(
                      " No need to scheuled job as it has already been scheduled or completed. ");
                }
              }
            } else {
              logger.info(
                  " Action {} is related via {}",
                  act.getRelatedAction().getActionId(),
                  act.getRelationship());
            }
          }
        }

        // Check Timing Data , No need to check if the state is already scheduled meaning the
        // job was scheduled already.
        if (relatedActsDone || validationMode) {

          logger.info(" All Related Actions are completed ");

          // Timing constraints are applicable if this job has not started, once it is started
          // the State Machine has to manage the execution.
          if (state.getCreateEicrStatus().getJobStatus() == JobStatus.NOT_STARTED
              && !validationMode) {

            logger.info(" Related Actions Done and this action has not started ");

            if (getTimingData() != null && !getTimingData().isEmpty()) {

              logger.info(" Timing Data is present , so create a job based on timing data.");
              List<TimingSchedule> tsjobs = getTimingData();

              for (TimingSchedule ts : tsjobs) {

                // TBD : Setup job using TS Timing after testing so that we can test faster.
                // For now setup a default job with 10 seconds.
                WorkflowService.scheduleJob(
                    details.getId(), ts, EcrActionTypes.CREATE_EICR, details.getStartDate());
                state.getCreateEicrStatus().setJobStatus(JobStatus.SCHEDULED);
                EcaUtils.updateDetailStatus(details, state);
                // No need to continue as the job will take over execution.
                logger.info(" **** End Executing Create Eicr Action **** ");
                // return;
              }
            }

            logger.info(" No job to schedule since there is no timing data ");
          } else if ((state.getCreateEicrStatus().getJobStatus() == JobStatus.SCHEDULED
                  && launchType == WorkflowEvent.SCHEDULED_JOB)
              || validationMode) {

            // Do this only if the job is scheduled.
            logger.info(" Creating the EICR since the job has been scheduled ");

            // Check Trigger Codes again in case the data has changed.
            PatientExecutionState newState = EcaUtils.recheckTriggerCodes(details, launchType);

            if (newState.getMatchTriggerStatus().getTriggerMatchStatus()
                && newState.getMatchTriggerStatus().getMatchedCodes() != null
                && !newState.getMatchTriggerStatus().getMatchedCodes().isEmpty()) {

              // Since the job has started, Execute the job.
              // Call the Loading Queries and create eICR.
              Eicr ecr = EcaUtils.createEicr(details);

              if (ecr != null) {

                newState.getCreateEicrStatus().setEicrCreated(true);
                newState.getCreateEicrStatus().seteICRId(ecr.getId().toString());
                newState.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);

                EcaUtils.updateDetailStatus(details, newState);

                logger.info(" **** Printing Eicr from CREATE EICR ACTION **** ");

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

                logger.info(" **** End Printing Eicr from CREATE EICR ACTION **** ");
              }

            } // Check if Trigger Code Match found
            else {

              logger.info(" **** Trigger Code did not match, hence not creating EICR **** ");

              newState.getCreateEicrStatus().setEicrCreated(false);
              newState.getCreateEicrStatus().seteICRId("0");
              newState.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);

              EcaUtils.updateDetailStatus(details, newState);
            }
          } else {
            logger.info(
                " EICR job is in a state of {} , due to which EICR will not be created. ",
                state.getCreateEicrStatus().getJobStatus());
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

  @Override
  public void print() {

    logger.info(" **** Printing CreateEicrAction **** ");
    printBase();
    logger.info(" **** End Printing CreateEicrAction **** ");
  }
}
