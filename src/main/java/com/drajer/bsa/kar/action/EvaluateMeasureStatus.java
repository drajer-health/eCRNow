package com.drajer.bsa.kar.action;

import org.hl7.fhir.r4.model.MeasureReport;

public class EvaluateMeasureStatus extends BsaActionStatus {
  public MeasureReport report;

  public MeasureReport getReport() {
    return report;
  }

  public void setReport(MeasureReport report) {
    this.report = report;
  }

  public EvaluateMeasureStatus() {}
}
