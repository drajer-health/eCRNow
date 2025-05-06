package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.BsaTypes.NotificationProcessingStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CompleteReporting extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(CompleteReporting.class);

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
    BsaActionStatus actStatus = new CompleteReportingStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Get the default queries.
    Map<String, FhirQueryFilter> queries =
        BsaServiceUtils.getDefaultQueriesForAction(this, data.getKar());

    if (queries != null && !queries.isEmpty()) {

      logger.info(" Found Default/Custom Queries for execution ");
      // Try to execute the queries.
      queries.forEach((key, value) -> ehrService.executeQuery(data, key, value));

    } else {

      // Try to Get the Resources that need to be retrieved using Resource Type since queries are
      // not specified.
      HashMap<String, ResourceType> resourceTypes = getInputResourceTypes();

      // Get necessary data to process.
      ehrService.getFilteredData(data, resourceTypes);
    }

    // Create the necessary parameters for evaluation of the data using FHIR Path processor.
    Parameters params = null;
    populateParamsForConditionEvaluation(data, params);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.SCHEDULED
        && Boolean.TRUE.equals(conditionsMet(data, ehrService))) {

      logger.info(" All conditions in the Actions have been met for {}", this.getActionId());

      // Execute sub Actions
      executeSubActions(data, ehrService);

      // Execute Related Actions.
      executeRelatedActions(data, ehrService);

      actStatus.setActionStatus(BsaActionStatusType.COMPLETED);

      // Set the Status of the notification context to complete.
      logger.info(
          " Updating Notification Context for Patient Id {}, NC Id {} to Completed.",
          data.getNotificationContext().getPatientId(),
          data.getNotificationContext().getId());
      data.getNotificationContext()
          .setNotificationProcessingStatus(NotificationProcessingStatusType.COMPLETED.toString());

      // Set the end time of NC as current time when we are completing the Encounter.
      data.getNotificationContext().setEncounterEndTime(new Date());

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
