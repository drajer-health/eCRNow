package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import org.junit.Before;
import org.junit.Test;

public class InitiateReportingTest {

  private EhrQueryService ehrService;

  @Before
  public void setup() {
    ehrService = mock(EhrQueryService.class);
  }

  @Test
  public void process_shouldReturnCompleted_whenConditionsMet() {
    KarProcessingData data = new KarProcessingData();
    data.setExecutionSequenceId("exec-1");
    InitiateReporting action = new InitiateReporting();
    action.setActionId("init-report-1");
    BsaActionStatus status = action.process(data, ehrService);
    assertNotNull(status);
    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());
  }

  @Test
  public void process_shouldReturnCompleted_byDefaultFlow() {
    KarProcessingData data = new KarProcessingData();
    data.setExecutionSequenceId("exec-1");
    InitiateReporting action = new InitiateReporting();
    action.setActionId("init-report-1");
    BsaActionStatus status = action.process(data, ehrService);
    assertNotNull(status);
    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());
  }

  @Test
  public void process_shouldRemainInProgress_whenConditionsNotMet() {
    KarProcessingData data = new KarProcessingData();
    data.setExecutionSequenceId("exec-1");
    InitiateReporting action =
        new InitiateReporting() {
          @Override
          public Boolean conditionsMet(KarProcessingData data, EhrQueryService ehrService) {
            return false;
          }
        };
    action.setActionId("init-report-1");
    BsaActionStatus status = action.process(data, ehrService);
    assertNotNull(status);
    assertEquals(BsaActionStatusType.IN_PROGRESS, status.getActionStatus());
  }
}
