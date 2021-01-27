package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.util.Date;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class SubmitEicrAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(SubmitEicrAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType) {

    logger.info(" **** START Executing Submit Eicr Action **** ");

    if (obj instanceof LaunchDetails) {

      logger.info(" Obtained Launch Details ");
      LaunchDetails details = (LaunchDetails) obj;
      PatientExecutionState state = null;

      state = ApplicationUtils.getDetailStatus(details);

      logger.info(
          " Executing Submit Eicr Action , Prior Execution State : = {}", details.getStatus());

      if (getRelatedActions() != null && !getRelatedActions().isEmpty()) {

        logger.info(" Related Actions exist, so check dependencies ");

        List<RelatedAction> racts = getRelatedActions();

        for (RelatedAction actn : racts) {

          if (actn.getRelationship() == ActionRelationshipType.AFTER) {

            // check if the action is completed.
            String actionId = actn.getRelatedAction().getActionId();

            if (!state.hasActionCompleted(actionId)) {

              logger.info(" Action {} is not completed , hence this action has to wait ", actionId);
            } else {

              // Get the eICR for the Action Completed after which validation has to be run.
              Set<Integer> ids = state.getEicrsReadyForSubmission();

              submitEicrs(details, state, ids);
            }
          } else {
            logger.info(
                " This action is not dependent on the action relationship : {}, Action Id = {}",
                actn.getRelationship(),
                actn.getRelatedAction().getActionId());
          }
        }
      } else {

        logger.info(" No related actions, so submit all Eicrs that are ready for submission ");

        Set<Integer> ids = state.getEicrsReadyForSubmission();

        submitEicrs(details, state, ids);
      }

      EcaUtils.updateDetailStatus(details, state);
    }
  }

  @Override
  public void print() {

    logger.info(" **** Printing SubmitEicrAction **** ");
    printBase();
    logger.info(" **** End Printing SubmitEicrAction **** ");
  }

  public void submitEicrs(LaunchDetails details, PatientExecutionState state, Set<Integer> ids) {

    for (Integer id : ids) {

      logger.info(" Found eICR with Id {} to submit ", id);

      Eicr ecr = ActionRepo.getInstance().getEicrRRService().getEicrById(id);

      if (ecr != null) {

        String data = ecr.getEicrData();

        if (!StringUtils.isBlank(details.getRestAPIURL())) {
          ActionRepo.getInstance().getRestTransport().sendEicrXmlDocument(details, data, ecr);
        } else if (!StringUtils.isBlank(details.getDirectHost())) {
          ActionRepo.getInstance().getDirectTransport().sendData(details, data);
        } else {
          String msg = "No Transport method specified to submit EICR.";
          logger.error(msg);

          throw new RuntimeException(msg);
        }

        // Add a submission object every time.
        SubmitEicrStatus submitState = new SubmitEicrStatus();
        submitState.setActionId(getActionId());
        submitState.seteICRId(id.toString());
        submitState.setEicrSubmitted(true);
        submitState.setJobStatus(JobStatus.COMPLETED);
        submitState.setSubmittedTime(new Date());
        state.getSubmitEicrStatus().add(submitState);
      } else {
        String msg = "No Eicr found for submission, Id = " + Integer.toString(id);
        logger.error(msg);

        throw new RuntimeException(msg);
      }
    }
  }
}
