package com.drajer.bsa.kar.action;

import org.hl7.fhir.r4.model.MeasureReport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EvaluateMeasureStatus extends BsaActionStatus {
  private final Logger logger = LoggerFactory.getLogger(EvaluateMeasureStatus.class);
  private MeasureReport report;

  public MeasureReport getReport() {
    return report;
  }

  public void setReport(MeasureReport report) {
    this.report = report;
  }

  public EvaluateMeasureStatus() {
    logger.info("EvaluateMeasureStatus object initiated.");
  }
}
