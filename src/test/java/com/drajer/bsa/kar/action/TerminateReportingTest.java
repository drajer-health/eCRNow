// package com.drajer.bsa.kar.action;
//
// import static org.junit.Assert.assertEquals;
// import static org.junit.Assert.assertNotNull;
// import static org.mockito.Mockito.mock;
//
// import com.drajer.bsa.ehr.service.EhrQueryService;
// import com.drajer.bsa.kar.model.FhirQueryFilter;
// import com.drajer.bsa.kar.model.KnowledgeArtifact;
// import com.drajer.bsa.model.BsaTypes;
// import com.drajer.bsa.model.KarProcessingData;
// import com.drajer.bsa.model.NotificationContext;
// import com.drajer.eca.model.TimingSchedule;
// import java.util.*;
// import org.junit.Test;
// import org.junit.runner.RunWith;
// import org.mockito.InjectMocks;
// import org.powermock.modules.junit4.PowerMockRunner;
// import org.springframework.test.util.ReflectionTestUtils;
//
// @RunWith(PowerMockRunner.class)
// public class TerminateReportingTest {
//
//  @InjectMocks TerminateReporting terminateReporting;
//
//  @Test
//  public void testProcess_withConditionsMet_executesAndUpdatesStatus() {
//    terminateReporting.setActionId("terminate-action-001");
//    KnowledgeArtifact kar = new KnowledgeArtifact();
//    kar.setKarId("id");
//    kar.setKarName("kar name");
//    kar.setKarVersion("r4");
//    NotificationContext context = new NotificationContext();
//    context.setPatientId("patient-123");
//    context.setId(UUID.randomUUID());
//    context.setNotificationProcessingStatus("SUSPENDED");
//    context.setEncounterEndTime(new Date());
//    KarProcessingData data = new KarProcessingData();
//    data.setKar(kar);
//    data.setNotificationContext(context);
//    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
//    fhirQueryFilter.setQueryString("query");
//    fhirQueryFilter.setDataReqId("reqid");
//    fhirQueryFilter.setRelatedDataId("RelatedDataId");
//    Map<String, FhirQueryFilter> queries = new HashMap<>();
//    queries.put("Observation", fhirQueryFilter);
//    EhrQueryService ehrService = mock(EhrQueryService.class);
//
//    ReflectionTestUtils.setField(terminateReporting, "inputDataRequirementQueries", queries);
//
//    BsaActionStatus result = terminateReporting.process(data, ehrService);
//
//    assertNotNull(result);
//    assertEquals("terminate-action-001", result.getActionId());
//    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
//    assertEquals(
//        BsaTypes.NotificationProcessingStatusType.SUSPENDED.toString(),
//        data.getNotificationContext().getNotificationProcessingStatus());
//    assertNotNull(data.getNotificationContext().getEncounterEndTime());
//  }
//
//  @Test
//  public void testProcess_withUpdatesStatusIsSCHEDULED() {
//    terminateReporting.setActionId("terminate-action-001");
//    KnowledgeArtifact kar = new KnowledgeArtifact();
//    kar.setKarId("id");
//    kar.setKarName("kar name");
//    kar.setKarVersion("r4");
//    NotificationContext context = new NotificationContext();
//    context.setPatientId("patient-123");
//    context.setId(UUID.randomUUID());
//    context.setNotificationProcessingStatus("SUSPENDED");
//    context.setEncounterEndTime(new Date());
//    KarProcessingData data = new KarProcessingData();
//    data.setKar(kar);
//    data.setNotificationContext(context);
//    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
//    fhirQueryFilter.setQueryString("query");
//    fhirQueryFilter.setDataReqId("reqid");
//    fhirQueryFilter.setRelatedDataId("RelatedDataId");
//    Map<String, FhirQueryFilter> queries = new HashMap<>();
//    queries.put("Observation", fhirQueryFilter);
//    EhrQueryService ehrService = mock(EhrQueryService.class);
//    TimingSchedule timingSchedule = mock(TimingSchedule.class);
//    List<TimingSchedule> timingSchedules = new ArrayList<>();
//    timingSchedules.add(timingSchedule);
//    ReflectionTestUtils.setField(terminateReporting, "timingData", timingSchedules);
//    ReflectionTestUtils.setField(terminateReporting, "ignoreTimers", false);
//    ReflectionTestUtils.setField(terminateReporting, "inputDataRequirementQueries", queries);
//
//    BsaActionStatus result = terminateReporting.process(data, ehrService);
//
//    assertNotNull(result);
//    assertEquals("terminate-action-001", result.getActionId());
//    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
//  }
// }
