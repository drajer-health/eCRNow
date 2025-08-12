package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.powermock.api.mockito.PowerMockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.eca.model.TimingSchedule;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.opencds.cqf.cql.evaluator.measure.r4.R4MeasureProcessor;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
public class EvaluateMeasureTest {

  @InjectMocks EvaluateMeasure evaluateMeasure;

  @Mock R4MeasureProcessor measureProcessor;

  @Test
  public void testProcess_returnsCompletedStatusAndAddsReport() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-123");
    kar.setKarPath("http://example.org/fhir/library");
    NotificationContext context = new NotificationContext();
    context.setId(UUID.randomUUID());
    context.setPatientId("patient-123");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    Map<ResourceType, Set<Resource>> fhirInputDataByType = new HashMap<>();
    Set<Resource> patientSet = new HashSet<>();
    Patient patient = new Patient();
    patient.setId("patient-1");
    patient.addName().setFamily("Doe").addGiven("John");
    patientSet.add(patient);
    fhirInputDataByType.put(ResourceType.Patient, patientSet);
    ReflectionTestUtils.setField(data, "fhirInputDataByType", fhirInputDataByType);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    MeasureReport report = new MeasureReport();
    report.setId("report-123");
    when(measureProcessor.evaluateMeasure(
            any(),
            anyString(),
            anyString(),
            anyString(),
            anyString(),
            isNull(),
            isNull(),
            any(),
            any(),
            isNull(),
            any(Bundle.class)))
        .thenReturn(report);
    evaluateMeasure.setMeasureReportId("M-id");

    BsaActionStatus status = evaluateMeasure.process(data, ehrService);

    assertNotNull(status);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertTrue(status instanceof EvaluateMeasureStatus);
    EvaluateMeasureStatus emStatus = (EvaluateMeasureStatus) status;
    assertEquals(report, emStatus.getReport());
    assertTrue(data.getActionStatus().containsKey(data.getExecutionSequenceId()));
  }

  @Test
  public void testProcess_WhenStatusIsScheduled() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-123");
    kar.setKarPath("http://example.org/fhir/library");
    NotificationContext context = new NotificationContext();
    context.setId(UUID.randomUUID());
    context.setPatientId("patient-123");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(evaluateMeasure, "timingData", timingSchedules);
    ReflectionTestUtils.setField(evaluateMeasure, "ignoreTimers", false);

    BsaActionStatus status = evaluateMeasure.process(data, ehrService);

    assertNotNull(status);
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, status.getActionStatus());
  }

  @Test
  public void testSetAndGetPeriodEnd() {
    String periodEnd = "2024-12-31";
    evaluateMeasure.setPeriodEnd(periodEnd);

    assertEquals("Should return the correct period end", periodEnd, evaluateMeasure.getPeriodEnd());
  }

  @Test
  public void testSetAndGetPeriodStart() {
    String periodStart = "2024-01-01";
    evaluateMeasure.setPeriodStart(periodStart);

    assertEquals(
        "Should return the correct period start", periodStart, evaluateMeasure.getPeriodStart());
  }

  @Test
  public void testSetAndGetMeasureReportId() {
    String measureReportId = "report-456";
    evaluateMeasure.setMeasureReportId(measureReportId);

    assertEquals(
        "Should return the correct measure report ID",
        measureReportId,
        evaluateMeasure.getMeasureReportId());
  }
}
