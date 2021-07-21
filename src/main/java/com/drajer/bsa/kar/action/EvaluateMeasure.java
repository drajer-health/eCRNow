package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import java.time.LocalDate;
import java.time.Year;
import java.util.HashMap;
import java.util.Set;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.MeasureReport;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.opencds.cqf.cql.evaluator.builder.Constants;
import org.opencds.cqf.cql.evaluator.measure.r4.MeasureProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EvaluateMeasure extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(EvaluateMeasure.class);

  private MeasureProcessor measureProcessor;

  private String periodStart;
  private String periodEnd;

  private String measureReportId;

  public String getPeriodStart() {
    return periodStart;
  }

  public void setPeriodStart(String periodStart) {
    this.periodStart = periodStart;
  }

  public String getPeriodEnd() {
    return periodEnd;
  }

  public void setPeriodEnd(String periodEnd) {
    this.periodEnd = periodEnd;
  }

  public String getMeasureReportId() {
    return measureReportId;
  }

  public void setMeasureReportId(String measureReportId) {
    this.measureReportId = measureReportId;
  }

  public MeasureProcessor getMeasureProcessor() {
    return measureProcessor;
  }

  public void setMeasureProcessor(MeasureProcessor measureProcessor) {
    this.measureProcessor = measureProcessor;
  }

  public EvaluateMeasure() {
    int year = Year.now().getValue();
    periodStart = LocalDate.of(year, 01, 01).toString();
    periodEnd = LocalDate.of(year, 12, 31).toString();
    measureReportId = "";
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

      Endpoint endpoint =
          new Endpoint()
              .setAddress(data.getKar().getKarPath())
              .setConnectionType(new Coding().setCode(Constants.HL7_FHIR_FILES));

      // Evaluate Measure by passing the required parameters
      // Set up and evaluate the measure.
      MeasureReport result =
          measureProcessor.evaluateMeasure(
              getMeasureUri(),
              periodStart,
              periodEnd,
              "subject",
              data.getNotificationContext().getPatientId(),
              null, // practitioner
              null, // received on
              endpoint, // Library Bundle
              endpoint, // Terminology Bundle
              null, // Endpoint for data
              data.getInputResourcesAsBundle()); // Data Bundle

      if (result != null) {

        data.addActionOutput(this.getActionId(), result);

        if (measureReportId != null && measureReportId.length() > 0)
          data.addActionOutputById(measureReportId, result);
      }

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
