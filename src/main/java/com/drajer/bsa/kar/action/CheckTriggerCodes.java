package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CheckTriggerCodes extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(CheckTriggerCodes.class);

  private FhirPathProcessor fhirPathProcessor;

  public CheckTriggerCodes() {

    fhirPathProcessor = new FhirPathProcessor();
  }

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    CheckTriggerCodeStatus actStatus = new CheckTriggerCodeStatus();
    actStatus.setActionId(this.getActionId());
    actStatus.setActionType(ActionType.CheckTriggerCodes);

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.Scheduled) {

      logger.info(
          " Action {} can proceed as it does not have timing information ", this.getActionId());

      // Get the Resources that need to be retrieved.
      HashMap<String, ResourceType> resourceTypes = getInputResourceTypes();

      // Get necessary data to process.
      HashMap<ResourceType, Set<Resource>> res = ehrService.getFilteredData(data, resourceTypes);

      // Apply filters for data and then send the collections to the Condition Evaluator.
      for (DataRequirement dr : inputData) {

        if (dr.hasCodeFilter()) {

          logger.info(" Checking Trigger Codes based on code filter ");
          Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> matchInfo =
              fhirPathProcessor.filterResources(dr, data);

          if (matchInfo != null) {

            logger.info(" Found Match for Code Filter {}", dr.getType());

            HashMap<String, Set<Resource>> idres = new HashMap<>();
            Set<Resource> allResources = new HashSet<Resource>();

            matchInfo
                .getValue1()
                .values()
                .forEach(setOfResources -> allResources.addAll(setOfResources));
            idres.put(dr.getId(), allResources);
            data.resetResourcesById(idres);

            actStatus.addOutputProducedId(dr.getId());
            actStatus.copyFrom(matchInfo.getValue0());

          } else {
            logger.info(" No Match found for Code Filter {}", dr.getType());
          }
        } else {
          logger.error(" Not processing Data Requirement which is not a code filter ");
        }
      }

      actStatus.log();
      data.addActionStatus(getActionId(), actStatus);

      if (conditionsMet(data)) {

        // Execute sub Actions
        executeSubActions(data, ehrService);

        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }

      actStatus.setActionStatus(BsaActionStatusType.Completed);

    } else {

      logger.info(
          " Action may be executed in the future or Conditions have not been met, so cannot proceed any further. ");
      logger.info(" Setting Action Status : {}", status);
      actStatus.setActionStatus(status);
    }

    return actStatus;
  }
}
