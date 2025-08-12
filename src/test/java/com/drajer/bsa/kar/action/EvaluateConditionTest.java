package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.powermock.api.mockito.PowerMockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
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
@PrepareForTest({BsaServiceUtils.class})
@SuppressStaticInitializationFor("com.drajer.bsa.utils.BsaServiceUtils")
public class EvaluateConditionTest {

  @InjectMocks EvaluateCondition evaluateCondition;

  @Before
  public void setUp() throws Exception {
    mockStatic(BsaServiceUtils.class);
  }

  @Test
  public void testProcess_withQueriesAndConditionsMet() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-ccr-001");
    kar.setKarName("Central Cancer Reporting Decision Support");
    kar.setKarVersion("r4");
    NotificationContext Context = new NotificationContext();
    Context.setId(UUID.randomUUID());
    Context.setPatientId("pa-1");
    Context.setNotificationResourceId("R-id");
    Context.setNotificationResourceType("Encounter");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(Context);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setQueryString("query");
    fhirQueryFilter.setDataReqId("reqid");
    fhirQueryFilter.setRelatedDataId("RelatedDataId");
    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("data", fhirQueryFilter);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("id");
    dataRequirement.setType("type");
    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("id1");
    dataRequirement1.setType("type1");
    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);
    ReflectionTestUtils.setField(evaluateCondition, "inputData", listdr);

    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(EvaluateCondition.class), any(KnowledgeArtifact.class)))
        .thenReturn(queries);

    BsaActionStatus status = evaluateCondition.process(data, ehrService);

    assertNotNull(status);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
  }

  @Test
  public void testProcess_withQueriesIsNullAndConditionsMet() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-ccr-001");
    kar.setKarName("Central Cancer Reporting Decision Support");
    kar.setKarVersion("r4");
    NotificationContext Context = new NotificationContext();
    Context.setId(UUID.randomUUID());
    Context.setPatientId("pa-1");
    Context.setNotificationResourceId("R-id");
    Context.setNotificationResourceType("Encounter");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(Context);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("id");
    dataRequirement.setType("type");
    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("id1");
    dataRequirement1.setType("type1");
    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);
    ReflectionTestUtils.setField(evaluateCondition, "inputData", listdr);

    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(EvaluateCondition.class), any(KnowledgeArtifact.class)))
        .thenReturn(null);

    BsaActionStatus status = evaluateCondition.process(data, ehrService);

    assertNotNull(status);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
  }

  @Test
  public void testProcess_whenStatusIsScheduled() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("kar-ccr-001");
    kar.setKarName("Central Cancer Reporting Decision Support");
    kar.setKarVersion("r4");
    NotificationContext Context = new NotificationContext();
    Context.setId(UUID.randomUUID());
    Context.setPatientId("pa-1");
    Context.setNotificationResourceId("R-id");
    Context.setNotificationResourceType("Encounter");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(Context);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setQueryString("query");
    fhirQueryFilter.setDataReqId("reqid");
    fhirQueryFilter.setRelatedDataId("RelatedDataId");
    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("data", fhirQueryFilter);
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(evaluateCondition, "timingData", timingSchedules);
    ReflectionTestUtils.setField(evaluateCondition, "ignoreTimers", false);

    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(EvaluateCondition.class), any(KnowledgeArtifact.class)))
        .thenReturn(queries);

    BsaActionStatus status = evaluateCondition.process(data, ehrService);

    assertNotNull(status);
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, status.getActionStatus());
  }
}
