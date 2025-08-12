package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.TimingSchedule;
import java.util.*;
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
public class InitiateReportingTest {

  @InjectMocks InitiateReporting initiateReporting;

  @Before
  public void setUp() throws Exception {
    mockStatic(BsaServiceUtils.class);
  }

  @Test
  public void process_shouldCompleteActionAndInvokeSubActions() {
    String actionId = "action-1";
    initiateReporting.setActionId(actionId);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    NotificationContext mockContext = mock(NotificationContext.class);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(mockContext);
    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setQueryString("query");
    fhirQueryFilter.setDataReqId("reqid");
    fhirQueryFilter.setRelatedDataId("RelatedDataId");
    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("data", fhirQueryFilter);
    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(InitiateReporting.class), any(KnowledgeArtifact.class)))
        .thenReturn(queries);

    BsaActionStatus result = initiateReporting.process(data, ehrService);

    assertEquals("action-1", result.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
  }

  @Test
  public void process_shouldCompleteActionAndqueriesIsNull() {
    String actionId = "action-1";
    initiateReporting.setActionId(actionId);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    NotificationContext mockContext = mock(NotificationContext.class);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(mockContext);
    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(InitiateReporting.class), any(KnowledgeArtifact.class)))
        .thenReturn(null);

    BsaActionStatus result = initiateReporting.process(data, ehrService);

    assertEquals("action-1", result.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
  }

  @Test
  public void testProcess_whenStatusIsScheduled() {
    KarProcessingData data = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setQueryString("query");
    fhirQueryFilter.setDataReqId("reqid");
    fhirQueryFilter.setRelatedDataId("RelatedDataId");
    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("data", fhirQueryFilter);

    initiateReporting.setActionId("test-action-id");
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(initiateReporting, "timingData", timingSchedules);
    ReflectionTestUtils.setField(initiateReporting, "ignoreTimers", false);
    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(InitiateReporting.class), any(KnowledgeArtifact.class)))
        .thenReturn(queries);
    BsaActionStatus result = initiateReporting.process(data, ehrService);

    assertNotNull(result);
    assertEquals("test-action-id", result.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
  }
}
