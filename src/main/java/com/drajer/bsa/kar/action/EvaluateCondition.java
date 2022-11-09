package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EvaluateCondition extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(EvaluateCondition.class);

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    BsaActionStatus actStatus = new EvaluateConditionStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Get the default queries.
    Map<String, FhirQueryFilter> queries =
        BsaServiceUtils.getDefaultQueriesForAction(this, data.getKar());

    if (queries != null && !queries.isEmpty()) {

      logger.info(" Found Default/Custom Queries for execution. ");
      // Try to execute the queries.
      queries.forEach((key, value) -> ehrService.executeQuery(data, key, value));

    } else {

      logger.info(" No Queries, so just get data by Resource Type. ");

      // Try to Get the Resources that need to be retrieved using Resource Type since queries are
      // not specified. Calling getFilteredData method
      ehrService.getFilteredData(data, getInputData());
    }

    // Create the necessary parameters for evaluation of the data using FHIR Path processor.
    populateParamsForConditionEvaluation(data);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.SCHEDULED
        && Boolean.TRUE.equals(conditionsMet(data, ehrService))) {

      logger.info(" All conditions in the Actions have been met for {}", this.getActionId());

      // Execute sub Actions
      executeSubActions(data, ehrService);

      // Execute Related Actions.
      executeRelatedActions(data, ehrService);

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
