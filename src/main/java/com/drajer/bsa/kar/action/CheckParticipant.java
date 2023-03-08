package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CheckParticipant extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(CheckParticipant.class);

  public CheckParticipant() {
    logger.info("CheckParticipant is executing.");
  }

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    CheckParticipantStatus actStatus = new CheckParticipantStatus();

    actStatus.setActionId(actionId);
    actStatus.setActionType(type);

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.SCHEDULED) {

      logger.info(
          " Action {} can proceed as it does not have timing information ", this.getActionId());

      Map<ResourceType, Set<Resource>> res = ehrService.getFilteredData(data, this.getInputData());
      logger.info("Resource:{}", res);

      data.addActionStatus(getActionId(), actStatus);

      if (true) { // Assume participants have matched

        // Execute sub Actions
        executeSubActions(data, ehrService);

        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }

      actStatus.setActionStatus(BsaActionStatusType.COMPLETED);

    } else {

      logger.info(
          " Action may be executed in the future or Conditions have not been met, so cannot proceed any further. ");
      logger.info(" Setting Action Status : {}", status);
      actStatus.setActionStatus(status);
    }

    data.addActionStatus(data.getExecutionSequenceId(), actStatus);

    return actStatus;
  }
}
