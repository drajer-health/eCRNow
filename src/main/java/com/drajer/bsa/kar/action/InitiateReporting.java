package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The class provides implementation for the InitiateReporting Workflow.
 *
 * @author nbashyam
 */
public class InitiateReporting extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(InitiateReporting.class);

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    BsaActionStatus actStatus = new InitiateReportingStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Get the Resources that need to be retrieved.
    HashMap<String, ResourceType> resourceTypes = getInputResourceTypes();

    // Get necessary data to process.
    Map<ResourceType, Set<Resource>> res = ehrService.getFilteredData(data, resourceTypes);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.Scheduled && Boolean.TRUE.equals(conditionsMet(data))) {

      logger.info(" All conditions in the Actions have been met for {}", this.getActionId());

      // Execute sub Actions
      executeSubActions(data, ehrService);

      // Execute Related Actions.
      executeRelatedActions(data, ehrService);

      actStatus.setActionStatus(BsaActionStatusType.Completed);

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
