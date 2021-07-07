package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;

import java.time.LocalDate;
import java.time.Year;
import java.util.HashMap;
import java.util.Set;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.opencds.cqf.cql.evaluator.measure.r4.R4MeasureEvaluation;

public class EvaluateMeasure extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(EvaluateMeasure.class);

  private R4MeasureEvaluation measureProcessor;

  private String periodStart;
  private String periodEnd;

  private IBaseBundle bundle;

  public EvaluateMeasure() {
	 // meaasureProcessor = new R4MeasureEvaluation();
	  int year = Year.now().getValue();
	  periodStart = LocalDate.of(year, 01, 01).toString();
	  periodEnd = LocalDate.of(year, 12, 31).toString();
  }

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    EvaluateMeasureStatus actStatus = new EvaluateMeasureStatus();
    actStatus.setActionId(this.getActionId());

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

      // Evaluate Measure by passing the required parameters
   // Set up and evaluate the measure.
      /*
       * MeasureReport result =
                    measureProcessor.evaluateMeasure(
                        getMeasureUri(),
                        periodStart,
                        periodEnd,
                        data.getNotificationContext().getPatientId();
                        "subject",
                        null, // practitioner
                        null, // received on
                        data.getKar().getOriginalKarBundle(),
                        data.getKar().getOriginalKarBundle(),
                        this.dataEndpoint,
                        data.getKar().getOriginalKarBundle());
       * 
       * 
       * if result != null .. then proceed.
       * 
       */

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
