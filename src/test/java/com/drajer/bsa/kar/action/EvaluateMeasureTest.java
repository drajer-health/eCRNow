package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.MeasureReport;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.opencds.cqf.fhir.cr.measure.r4.R4MeasureService;

public class EvaluateMeasureTest {

  private EhrQueryService ehrService;
  private R4MeasureService measureService;
  private EvaluateMeasure evaluateMeasure;

  @Before
  public void setup() {

    ehrService = mock(EhrQueryService.class);
    measureService = mock(R4MeasureService.class);

    evaluateMeasure = new EvaluateMeasure();
    evaluateMeasure.setActionId("eval-measure-1");
    evaluateMeasure.setMeasureReportId("mr-1");
    evaluateMeasure.setMeasureService(measureService);
  }

  @Test
  public void testConstructorDefaults() {
    EvaluateMeasure em = new EvaluateMeasure();
    assertEquals("", em.getMeasureReportId());
    assertNotNull(em.getPeriodStart());
    assertNotNull(em.getPeriodEnd());
  }

  @Test
  public void testSetPeriodStart() {
    ZonedDateTime start = ZonedDateTime.now().minusDays(1);
    evaluateMeasure.setPeriodStart(start);
    assertEquals(start, evaluateMeasure.getPeriodStart());
  }

  @Test
  public void testSetPeriodEnd_DoesNotChangeValue() {
    ZonedDateTime originalEnd = evaluateMeasure.getPeriodEnd();
    evaluateMeasure.setPeriodEnd(ZonedDateTime.now().plusDays(5));
    assertEquals(originalEnd, evaluateMeasure.getPeriodEnd());
  }

  @Test
  public void testGetMeasureService() {
    assertEquals(measureService, evaluateMeasure.getMeasureService());
  }

  @Test
  public void testProcess_Scheduled() {
    KarProcessingData data = createMinimalProcessingData();
    EvaluateMeasure testEval =
        new EvaluateMeasure() {
          @Override
          public BsaActionStatusType processTimingData(KarProcessingData data) {
            return BsaActionStatusType.SCHEDULED;
          }
        };
    testEval.setActionId("eval-measure-1");
    testEval.setMeasureReportId("mr-1");
    testEval.setMeasureService(measureService);
    BsaActionStatus status = testEval.process(data, ehrService);
    assertNotNull(status);
    assertEquals(BsaActionStatusType.SCHEDULED, status.getActionStatus());
  }

  @Test
  public void testProcess_ResultIsNull() {
    KarProcessingData data = createMinimalProcessingData();

    Map<ResourceType, Set<Resource>> ehrData = new HashMap<>();
    ehrData.put(
        ResourceType.Patient,
        new HashSet<>(
            data.getInputResourcesAsBundle().getEntry().stream()
                .map(e -> e.getResource())
                .collect(Collectors.toSet())));
    Mockito.lenient()
        .when(ehrService.getFilteredData(any(KarProcessingData.class), any(Map.class)))
        .thenReturn(ehrData);

    EvaluateMeasure testEval = new EvaluateMeasure();
    testEval.setActionId("eval-measure-1");
    testEval.setMeasureReportId("mr-1");
    testEval.setMeasureService(measureService);

    Mockito.lenient()
        .when(
            measureService.evaluate(
                any(),
                any(),
                any(),
                anyString(),
                anyString(),
                any(),
                any(),
                any(),
                any(),
                any(Bundle.class),
                any(),
                any(),
                any()))
        .thenReturn(null);

    BsaActionStatus status = testEval.process(data, ehrService);
    assertNotNull(status);
    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertTrue(data.getActionOutputData().isEmpty());
  }

  @Test
  public void testProcess_ResultPresent_ConditionsMet() {
    KarProcessingData data = createMinimalProcessingData();
    MeasureReport report = new MeasureReport();
    report.setId("measure-report-1");
    report.setStatus(MeasureReport.MeasureReportStatus.COMPLETE);
    Map<ResourceType, Set<Resource>> ehrData = new HashMap<>();
    ehrData.put(
        ResourceType.Patient,
        new HashSet<>(
            data.getInputResourcesAsBundle().getEntry().stream()
                .map(e -> e.getResource())
                .collect(Collectors.toSet())));
    Mockito.lenient()
        .when(ehrService.getFilteredData(any(KarProcessingData.class), any(Map.class)))
        .thenReturn(ehrData);
    Mockito.lenient()
        .when(
            measureService.evaluate(
                any(),
                any(),
                any(),
                anyString(),
                anyString(),
                any(),
                any(),
                any(),
                any(),
                any(Bundle.class),
                any(),
                any(),
                any()))
        .thenReturn(report);
    EvaluateMeasure testEval = new EvaluateMeasure();
    testEval.setActionId("eval-measure-1");
    testEval.setMeasureReportId("mr-1");
    testEval.setMeasureService(measureService);
    BsaActionStatus status = testEval.process(data, ehrService);
    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());

    assertTrue(data.getActionOutputData().containsKey("eval-measure-1"));
    assertTrue(data.getActionOutputDataById().containsKey("mr-1"));
    MeasureReport stored =
        (MeasureReport) data.getActionOutputData().get("eval-measure-1").values().iterator().next();
    assertEquals("measure-report-1", stored.getId());
    assertEquals(MeasureReport.MeasureReportStatus.COMPLETE, stored.getStatus());
  }

  private KarProcessingData createMinimalProcessingData() {
    KarProcessingData data = new KarProcessingData();
    data.setExecutionSequenceId("exec-1");
    NotificationContext ctx = new NotificationContext();
    ctx.setPatientId("patient-1");
    data.setNotificationContext(ctx);
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarPath("file:///tmp/kar");
    data.setKar(kar);
    Patient patient = new Patient();
    patient.setId("patient-1");
    data.addResourceByType(ResourceType.Patient, patient);
    return data;
  }
}
