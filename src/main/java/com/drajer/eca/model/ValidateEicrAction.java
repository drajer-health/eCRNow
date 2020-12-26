package com.drajer.eca.model;

import com.drajer.cda.utils.CdaValidatorUtil;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.util.Date;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValidateEicrAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(ValidateEicrAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType) {

    logger.info(" **** START Executing Validate Eicr Action **** ");

    if (obj instanceof LaunchDetails) {

      LaunchDetails details = (LaunchDetails) obj;
      PatientExecutionState state = null;

      state = ApplicationUtils.getDetailStatus(details);

      logger.info(
          " Executing Validate Eicr Action , Prior Execution State : = {}", details.getStatus());

      if (getRelatedActions() != null && !getRelatedActions().isEmpty()) {

        logger.info(" Validation actions to be performed based on other related actions. ");

        List<RelatedAction> racts = getRelatedActions();

        for (RelatedAction ract : racts) {

          if (ract.getRelationship() == ActionRelationshipType.AFTER) {

            // check if the action is completed.
            String actionId = ract.getRelatedAction().getActionId();

            if (!state.hasActionCompleted(actionId)) {

              logger.info(" Action {} is not completed , hence this action has to wait ", actionId);
            } else {

              // Get the eICR for the Action Completed after which validation has to be run.
              Set<Integer> ids = state.getEicrIdForCompletedActions(actionId);

              validateEicrs(state, ids);
            }
          } else {
            logger.info(
                " This action is not dependent on the action relationship : {}, Action Id = {}",
                ract.getRelationship(),
                ract.getRelatedAction().getActionId());
          }
        }
      } else {

        logger.info(" No related actions, so validate all Eicrs that are ready for validation ");

        Set<Integer> ids = state.getEicrsReadyForValidation();

        validateEicrs(state, ids);
      }

      EcaUtils.updateDetailStatus(details, state);
    }
  }

  @Override
  public void print() {

    logger.info(" **** Printing ValidateEicrAction **** ");
    printBase();
    logger.info(" **** End Printing ValidateEicrAction **** ");
  }

  public void validateEicrs(PatientExecutionState state, Set<Integer> ids) {

    for (Integer id : ids) {

      if (id != 0) {
        logger.info(" Found eICR with Id {} to validate ", id);
        String eICR = ActionRepo.getInstance().getEicrRRService().getEicrById(id).getEicrData();

        // Validate incoming XML
        if (StringUtils.isNotEmpty(eICR)) {

          boolean validationResultSchema = CdaValidatorUtil.validateEicrXMLData(eICR);

          logger.info(" Validation Result from Schema Validation = {}", validationResultSchema);

        } else {
          logger.info(" **** Skipping Eicr XML Validation: eICR is null**** ");
        }

        boolean validationResultSchematron = CdaValidatorUtil.validateEicrToSchematron(eICR);

        logger.info(" Validation Result from Schema Validation = {}", validationResultSchematron);

        // Add a validate object every time.
        ValidateEicrStatus validate = new ValidateEicrStatus();
        validate.setActionId(getActionId());
        validate.seteICRId(id.toString());
        validate.setEicrValidated(true);
        validate.setJobStatus(JobStatus.COMPLETED);
        validate.setValidationTime(new Date());
        state.getValidateEicrStatus().add(validate);
      }
    }
  }
}
