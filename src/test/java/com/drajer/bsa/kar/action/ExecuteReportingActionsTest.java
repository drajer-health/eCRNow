package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.eca.model.TimingSchedule;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class ExecuteReportingActionsTest {

  @Spy @InjectMocks ExecuteReportingActions executeReportingActions;

  @Test
  public void testProcess_whenConditionsMet_shouldExecuteSubAndRelatedActionsAndSetCompleted() {

    executeReportingActions.setActionId("action-123");
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

    doNothing().when(executeReportingActions).executeSubActions(eq(data), eq(ehrService));
    doNothing().when(executeReportingActions).executeRelatedActions(eq(data), eq(ehrService));

    BsaActionStatus result = executeReportingActions.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    assertEquals("action-123", result.getActionId());
    assertTrue(result instanceof ExecuteReportingActionsStatus);

    verify(executeReportingActions).executeSubActions(eq(data), eq(ehrService));
    verify(executeReportingActions).executeRelatedActions(eq(data), eq(ehrService));
  }

  @Test
  public void testProcess_whenStatusIsScheduled() {
    KarProcessingData data = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);

    executeReportingActions.setActionId("test-action-id");
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(executeReportingActions, "timingData", timingSchedules);
    ReflectionTestUtils.setField(executeReportingActions, "ignoreTimers", false);

    BsaActionStatus result = executeReportingActions.process(data, ehrService);

    assertNotNull(result);
    assertEquals("test-action-id", result.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
  }
}
