package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.util.Date;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReportabilityResponseAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(ReportabilityResponseAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType) {

    logger.info(" **** START Executing RR Check Eicr Action **** ");

    if (obj instanceof LaunchDetails) {

      logger.info(" Obtained Launch Details ");
      LaunchDetails launchDetails = (LaunchDetails) obj;
      PatientExecutionState state = null;

      state = ApplicationUtils.getDetailStatus(launchDetails);

      logger.info(
          " Executing RR Check Eicr Action , Prior Execution State : = {}",
          launchDetails.getStatus());

      if (getRelatedActions() != null && !getRelatedActions().isEmpty()) {

        logger.info(" Related Actions exist, so check dependencies ");

        List<RelatedAction> rrRelActs = getRelatedActions();

        for (RelatedAction rrRelAct : rrRelActs) {

          if (rrRelAct.getRelationship() == ActionRelationshipType.AFTER) {

            // check if the action is completed.
            String actionId = rrRelAct.getRelatedAction().getActionId();

            if (!state.hasActionCompleted(actionId)) {

              logger.info(" Action {} is not completed , hence this action has to wait ", actionId);
            } else {

              logger.info(" No related actions, so check all Eicrs that have been submitted ");

              Set<Integer> ids = state.getEicrsForRRCheck();

              checkRRForEicrs(state, ids);
            }
          } else {
            logger.info(
                " This action is not dependent on the action relationship : {}, Action Id = {}",
                rrRelAct.getRelationship(),
                rrRelAct.getRelatedAction().getActionId());
          }
        }
      } else {

        logger.info(" No related actions, so check all Eicrs that have been submitted ");

        Set<Integer> ids = state.getEicrsForRRCheck();

        checkRRForEicrs(state, ids);
      }

      EcaUtils.updateDetailStatus(launchDetails, state);
    }
  }

  @Override
  public void print() {

    logger.info(" **** Printing SubmitEicrAction **** ");
    printBase();
    logger.info(" **** End Printing SubmitEicrAction **** ");
  }

  public void checkRRForEicrs(PatientExecutionState state, Set<Integer> ids) {

    for (Integer id : ids) {

      logger.info(" Found eICR with Id {} to check for RR ", id);

      // Add a submission object every time.
      RRStatus submitState = new RRStatus();
      submitState.setActionId(getActionId());
      submitState.seteICRId(id.toString());
      submitState.setRrObtained(true);
      submitState.setJobStatus(JobStatus.COMPLETED);
      submitState.setRrTime(new Date());
      state.getRrStatus().add(submitState);
    }
  }
}
