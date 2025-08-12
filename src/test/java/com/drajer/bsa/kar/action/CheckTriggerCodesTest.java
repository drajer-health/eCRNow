package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.TimingSchedule;
import java.util.*;
import org.hl7.fhir.Code;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Resource;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.core.classloader.annotations.SuppressStaticInitializationFor;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
@PrepareForTest({BsaServiceUtils.class})
@SuppressStaticInitializationFor("com.drajer.bsa.utils.BsaServiceUtils")
public class CheckTriggerCodesTest {

  @InjectMocks private CheckTriggerCodes checkTriggerCodes;

  @Mock private FhirPathProcessor fhirPathProcessor;

  @Before
  public void setUp() throws Exception {
    mockStatic(BsaServiceUtils.class);
    ReflectionTestUtils.setField(checkTriggerCodes, "fhirPathProcessor", fhirPathProcessor);
  }

  @Test
  public void testProcess_WhenTriggerMatchesAndTimingIsNotScheduled() {
    String actionId = "action-1";
    checkTriggerCodes.setActionId(actionId);
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

    DataRequirement.DataRequirementCodeFilterComponent codeFilter =
        new DataRequirement.DataRequirementCodeFilterComponent();
    codeFilter.setPath("code");

    List<Coding> codingList = new ArrayList<>();
    Code code = new Code();
    code.setId("code-id");
    code.setValue("code-value");
    Coding coding = new Coding();
    coding.setSystem("http://snomed.info/sct");
    coding.setCode("123456");
    coding.setDisplay("Example Display");
    codeFilter.setCode(codingList);

    List<DataRequirement.DataRequirementCodeFilterComponent> theCodeFilters = new ArrayList<>();
    theCodeFilters.add(codeFilter);

    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("id");
    dataRequirement.setType("type");
    dataRequirement.setCodeFilter(theCodeFilters);

    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("id1");
    dataRequirement1.setType("type1");
    dataRequirement1.setCodeFilter(theCodeFilters);

    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);

    ReflectionTestUtils.setField(checkTriggerCodes, "inputData", listdr);
    when(BsaServiceUtils.getDefaultQueriesForAction(
            any(CheckTriggerCodes.class), any(KnowledgeArtifact.class)))
        .thenReturn(queries);
    doNothing().when(ehrService).executeQuery(data, "dataReqId", fhirQueryFilter);
    data.saveDataToFile(KarProcessingData.DebugDataType.TRIGGER, listdr, "TriggerQueryBundle");

    CheckTriggerCodeStatus triggerStatus = new CheckTriggerCodeStatus();
    triggerStatus.setTriggerMatchStatus(true);

    Map<String, Set<Resource>> matchedResources = new HashMap<>();
    matchedResources.put("type", new HashSet<>());

    when(fhirPathProcessor.applyCodeFilter(any(), any(), any()))
        .thenReturn(Pair.with(triggerStatus, matchedResources));

    BsaActionStatus result = checkTriggerCodes.process(data, ehrService);

    assertNotNull(result);
    assertEquals(actionId, result.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
  }

  @Test
  public void testProcess_whenStatusIsScheduled() {
    KarProcessingData data = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);

    checkTriggerCodes.setActionId("test-action-id");
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(checkTriggerCodes, "timingData", timingSchedules);
    ReflectionTestUtils.setField(checkTriggerCodes, "ignoreTimers", false);

    BsaActionStatus result = checkTriggerCodes.process(data, ehrService);

    assertNotNull(result);
    assertEquals("test-action-id", result.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
  }
}
