package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import java.time.LocalDate;
import java.time.Year;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Endpoint;
import org.hl7.fhir.r4.model.MeasureReport;
import org.opencds.cqf.cql.evaluator.builder.Constants;
import org.opencds.cqf.cql.evaluator.measure.r4.R4MeasureProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EvaluateMeasure extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(EvaluateMeasure.class);

  private R4MeasureProcessor measureProcessor;

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

  public R4MeasureProcessor getMeasureProcessor() {
    return measureProcessor;
  }

  public void setMeasureProcessor(R4MeasureProcessor measureProcessor) {
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
    if (status != BsaActionStatusType.SCHEDULED) {

      logger.info(
          " Action {} can proceed as it does not have timing information ", this.getActionId());

      // Get the Resources that need to be retrieved.
      ehrService.getFilteredData(data, getInputData());

      Endpoint endpoint =
          new Endpoint()
              .setAddress(data.getKar().getKarPath())
              .setConnectionType(new Coding().setCode(Constants.HL7_FHIR_FILES));

      String measureUri = getMeasureUri();
      String patientId = data.getNotificationContext().getPatientId();

      Bundle additionalData = data.getInputResourcesAsBundle();

      logger.info(
          "evaluating Measure {} for Patient {} for period {} - {} with {} resource(s). Content / terminology bundle is {}.",
          measureUri,
          patientId,
          periodStart,
          periodEnd,
          additionalData.hasEntry() ? additionalData.getEntry().size() : 0,
          data.getKar().getKarPath());

      // Evaluate Measure by passing the required parameters
      // Set up and evaluate the measure.
      MeasureReport result =
          measureProcessor.evaluateMeasure(
              measureUri,
              periodStart,
              periodEnd,
              "subject",
              patientId,
              null, // practitioner
              null, // received on
              endpoint, // Library Bundle
              endpoint, // Terminology Bundle
              null, // Endpoint for data
              additionalData); // Data Bundle

      if (result != null) {

        actStatus.setReport(result);
        data.addActionOutput(this.getActionId(), result);

        if (measureReportId != null && measureReportId.length() > 0)
          data.addActionOutputById(measureReportId, result);
      }

      if (Boolean.TRUE.equals(conditionsMet(data))) {
        // Execute sub Actions
        executeSubActions(data, ehrService);
        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }
      actStatus.setActionStatus(BsaActionStatusType.COMPLETED);

    } else {
      logger.info(
          " Action may execute in future or Conditions not met, can't process further. Setting Action Status : {}",
          status);
      actStatus.setActionStatus(status);
    }

    data.addActionStatus(data.getExecutionSequenceId(), actStatus);
    return actStatus;
  }
}
