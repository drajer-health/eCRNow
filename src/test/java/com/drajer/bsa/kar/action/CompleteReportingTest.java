package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.TimingSchedule;
import java.util.*;
import org.hl7.fhir.r4.model.DataRequirement;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.core.classloader.annotations.SuppressStaticInitializationFor;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest(BsaServiceUtils.class)
@SuppressStaticInitializationFor("com.drajer.bsa.utils.BsaServiceUtils")
public class CompleteReportingTest {

  @InjectMocks CompleteReporting completeReporting;

  @Before
  public void setUp() throws Exception {
    mockStatic(BsaServiceUtils.class);
  }

  @Test
  public void process_shouldCompleteActionWhenConditionsAreMet() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-ccr-001");
    kar.setKarName("Central Cancer Reporting Decision Support");
    kar.setKarVersion("r4");
    NotificationContext Context = new NotificationContext();
    Context.setId(UUID.randomUUID());
    Context.setPatientId("pa-1");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(Context);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setQueryString("Observation?patient=12345&code=http://loinc.org|1234-5");
    fhirQueryFilter.setDataReqId("obs-datareq-001");
    fhirQueryFilter.setRelatedDataId("related-to-condition-789");
    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("data", fhirQueryFilter);

    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(BsaAction.class), any(KnowledgeArtifact.class)))
        .thenReturn(queries);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("obs-datareq-001");
    dataRequirement.setType("Observation");
    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("cond-datareq-002");
    dataRequirement1.setType("Condition");
    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);
    ReflectionTestUtils.setField(completeReporting, "inputData", listdr);

    BsaActionStatus result = completeReporting.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    assertEquals(completeReporting.getActionId(), result.getActionId());
    assertEquals("COMPLETED", data.getNotificationContext().getNotificationProcessingStatus());
    assertNotNull(data.getNotificationContext().getEncounterEndTime());

    verify(ehrService).executeQuery(eq(data), eq("data"), eq(fhirQueryFilter));
  }

  @Test
  public void process_shouldSetStatusScheduledWhenTimingIsScheduled() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-ccr-001");
    kar.setKarName("Central Cancer Reporting Decision Support");
    kar.setKarVersion("r4");
    NotificationContext Context = new NotificationContext();
    Context.setId(UUID.randomUUID());
    Context.setPatientId("pa-1");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(Context);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setQueryString("Observation?patient=12345&code=http://loinc.org|1234-5");
    fhirQueryFilter.setDataReqId("obs-datareq-001");
    fhirQueryFilter.setRelatedDataId("related-to-condition-789");
    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("data", fhirQueryFilter);
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(completeReporting, "timingData", timingSchedules);
    ReflectionTestUtils.setField(completeReporting, "ignoreTimers", false);
    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(BsaAction.class), any(KnowledgeArtifact.class)))
        .thenReturn(queries);

    BsaActionStatus result = completeReporting.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
    assertEquals(completeReporting.getActionId(), result.getActionId());
  }

  @Test
  public void process_shouldSetStatusScheduledWhenTimingIsScheduledAndQueriesIsNull() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-ccr-001");
    kar.setKarName("Central Cancer Reporting Decision Support");
    kar.setKarVersion("r4");
    NotificationContext Context = new NotificationContext();
    Context.setId(UUID.randomUUID());
    Context.setPatientId("pa-1");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(Context);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(completeReporting, "timingData", timingSchedules);
    ReflectionTestUtils.setField(completeReporting, "ignoreTimers", false);
    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(BsaAction.class), any(KnowledgeArtifact.class)))
        .thenReturn(null);

    BsaActionStatus result = completeReporting.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
    assertEquals(completeReporting.getActionId(), result.getActionId());
  }
}
