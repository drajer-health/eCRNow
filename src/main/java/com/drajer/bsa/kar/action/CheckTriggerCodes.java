package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Resource;
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
    actStatus.setActionType(ActionType.CHECK_TRIGGER_CODES);

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.SCHEDULED) {

      logger.info(
          " Action {} can proceed as it does not have timing information ", this.getActionId());

      // Get the default queries.
      Map<String, FhirQueryFilter> queries =
          BsaServiceUtils.getDefaultQueriesForAction(this, data.getKar());

      if (queries != null && !queries.isEmpty()) {

        logger.info(" Data Requirements Exist wiht Queries, so executing queries to load data ");
        // Try to execute the queries.
        queries.forEach((key, value) -> ehrService.executeQuery(data, key, value));

      } else {

        logger.info(" No Queries, so just get data by Resource Type ");

        // Try to Get the Resources that need to be retrieved using Resource Type since queries are
        // not specified.
        ehrService.getFilteredData(data, getInputData());
      }

      data.saveDataToFile(
          KarProcessingData.DebugDataType.TRIGGER,
          getInputData(),
          KarProcessingData.TRIGGER_QUERY_FILE_NAME);

      HashMap<String, Set<Resource>> idres = new HashMap<>();
      Parameters params = new Parameters();
      CheckTriggerCodeStatusList ctcsl = new CheckTriggerCodeStatusList();

      // Apply filters for data and then send the collections to the Condition Evaluator.
      for (DataRequirement dr : inputData) {

        Set<Resource> allResources = new HashSet<>();
        if (dr.hasCodeFilter()) {

          logger.info(" Checking Trigger Codes based on code filter ");
          Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> matchInfo =
              fhirPathProcessor.applyCodeFilter(dr, data, this);

          if (matchInfo != null && matchInfo.getValue0().getTriggerMatchStatus()) {

            logger.info(" Found Match for Code Filter {}", dr.getType());

            matchInfo
                .getValue1()
                .values()
                .forEach(setOfResources -> allResources.addAll(setOfResources));

            idres.putAll(matchInfo.getValue1());

            actStatus.addOutputProducedId(dr.getId());
            actStatus.copyFrom(matchInfo.getValue0());

            ctcsl.addCheckTriggerCodeStatus(matchInfo.getValue0());

          } else {
            logger.info(" No Match found for Code Filter {}", dr.getType());
          }

        } else {
          logger.error(" Not processing Data Requirement which is not a code filter ");
        }

        // Add params
        BsaServiceUtils.convertDataToParameters(
            dr.getId(),
            dr.getType(),
            (dr.hasLimit() ? Integer.toString(dr.getLimit()) : "*"),
            allResources,
            params);
      }

      data.addParameters(actionId, params);
      actStatus.setMatchedResources(idres);
      data.addActionStatus(getActionId(), actStatus);
      data.setCurrentTriggerMatchStatus(ctcsl);

      if (Boolean.TRUE.equals(conditionsMet(data, ehrService) && data.hasNewTriggerCodeMatches())) {
        // Execute sub Actions
        executeSubActions(data, ehrService);
        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }
      actStatus.setActionStatus(BsaActionStatusType.COMPLETED);

    } else {
      logger.info(
          " Action may execute in future or Conditions not met, or there are no new Trigger Codes. Setting Action Status : {}",
          status);
      actStatus.setActionStatus(status);
    }

    data.addActionStatus(data.getExecutionSequenceId(), actStatus);
    return actStatus;
  }
}
