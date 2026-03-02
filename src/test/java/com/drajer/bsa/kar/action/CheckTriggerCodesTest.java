package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Resource;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({BsaServiceUtils.class})
public class CheckTriggerCodesTest {

  private CheckTriggerCodes checkTriggerCodes;

  @Mock private EhrQueryService ehrService;

  @Mock private FhirPathProcessor fhirPathProcessor;

  @Before
  public void setup() throws Exception {

    MockitoAnnotations.initMocks(this);

    PowerMockito.mockStatic(BsaServiceUtils.class);

    checkTriggerCodes = PowerMockito.spy(new CheckTriggerCodes());

    ReflectionTestUtils.setField(checkTriggerCodes, "fhirPathProcessor", fhirPathProcessor);
  }

  @Test
  public void testProcess_WhenScheduled() {

    doReturn(BsaTypes.BsaActionStatusType.SCHEDULED)
        .when(checkTriggerCodes)
        .processTimingData(any());

    KarProcessingData data = mock(KarProcessingData.class);

    BsaActionStatus result = checkTriggerCodes.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());

    verify(ehrService, never()).executeQuery(any(), any(), any());
  }

  @Test
  public void testProcess_WhenTriggerMatchFound() {

    doReturn(BsaTypes.BsaActionStatusType.IN_PROGRESS)
        .when(checkTriggerCodes)
        .processTimingData(any());

    doReturn(true).when(checkTriggerCodes).conditionsMet(any(), any());

    KarProcessingData data = mock(KarProcessingData.class);
    when(data.hasNewTriggerCodeMatches()).thenReturn(true);
    when(data.getExecutionSequenceId()).thenReturn("1");
    when(data.getKar()).thenReturn(null);

    when(BsaServiceUtils.getDefaultQueriesForAction(any(), any()))
        .thenReturn(Collections.emptyMap());

    DataRequirement dr = mock(DataRequirement.class);
    when(dr.hasCodeFilter()).thenReturn(true);
    when(dr.getId()).thenReturn("dr1");
    when(dr.getType()).thenReturn("Observation");

    ReflectionTestUtils.setField(checkTriggerCodes, "inputData", Collections.singletonList(dr));

    CheckTriggerCodeStatus triggerStatus = new CheckTriggerCodeStatus();
    triggerStatus.setTriggerMatchStatus(true);

    Resource resource = mock(Resource.class);
    Set<Resource> resourceSet = new HashSet<>();
    resourceSet.add(resource);

    Map<String, Set<Resource>> matchedMap = new HashMap<>();
    matchedMap.put("Observation", resourceSet);

    when(fhirPathProcessor.applyCodeFilter(any(), any(), any()))
        .thenReturn(Pair.with(triggerStatus, matchedMap));

    BsaActionStatus result = checkTriggerCodes.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());

    assertTrue(result instanceof CheckTriggerCodeStatus);

    CheckTriggerCodeStatus checkStatus = (CheckTriggerCodeStatus) result;

    assertEquals(1, checkStatus.getMatchedResources().get("Observation").size());

    verify(fhirPathProcessor, times(1)).applyCodeFilter(any(), any(), any());
  }

  @Test
  public void testProcess_WhenNoTriggerMatch() {

    doReturn(BsaTypes.BsaActionStatusType.IN_PROGRESS)
        .when(checkTriggerCodes)
        .processTimingData(any());

    doReturn(false).when(checkTriggerCodes).conditionsMet(any(), any());

    KarProcessingData data = mock(KarProcessingData.class);
    when(data.getExecutionSequenceId()).thenReturn("1");
    when(data.getKar()).thenReturn(null);

    when(BsaServiceUtils.getDefaultQueriesForAction(any(), any()))
        .thenReturn(Collections.emptyMap());

    DataRequirement dr = mock(DataRequirement.class);
    when(dr.hasCodeFilter()).thenReturn(true);
    when(dr.getId()).thenReturn("dr2");
    when(dr.getType()).thenReturn("Condition");

    ReflectionTestUtils.setField(checkTriggerCodes, "inputData", Collections.singletonList(dr));

    CheckTriggerCodeStatus triggerStatus = new CheckTriggerCodeStatus();
    triggerStatus.setTriggerMatchStatus(false);

    when(fhirPathProcessor.applyCodeFilter(any(), any(), any()))
        .thenReturn(Pair.with(triggerStatus, Collections.emptyMap()));

    BsaActionStatus result = checkTriggerCodes.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());

    CheckTriggerCodeStatus checkStatus = (CheckTriggerCodeStatus) result;

    assertTrue(checkStatus.getMatchedResources().isEmpty());
  }
}
