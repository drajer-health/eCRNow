package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import org.hl7.fhir.r4.model.MeasureReport;
import org.junit.Before;
import org.junit.Test;

public class EvaluateMeasureStatusTest {

  private EvaluateMeasureStatus evaluateMeasureStatus;

  @Before
  public void setUp() {
    evaluateMeasureStatus = new EvaluateMeasureStatus();
  }

  @Test
  public void testConstructor() {
    assertEquals(true, evaluateMeasureStatus != null);
  }

  @Test
  public void testSetAndGetReport() {
    MeasureReport report = new MeasureReport();
    report.setId("measure-123");
    evaluateMeasureStatus.setReport(report);
    MeasureReport retrievedReport = evaluateMeasureStatus.getReport();
    assertNotNull(retrievedReport);
    assertEquals("measure-123", retrievedReport.getId());
    assertEquals(report, retrievedReport);
  }

  @Test
  public void testSetReportToNull() {
    evaluateMeasureStatus.setReport(null);
    assertEquals(null, evaluateMeasureStatus.getReport());
  }
}
