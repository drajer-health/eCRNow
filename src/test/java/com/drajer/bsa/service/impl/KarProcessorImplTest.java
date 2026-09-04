package com.drajer.bsa.service.impl;

import static javax.management.remote.JMXConnectionNotification.FAILED;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.dao.impl.PublicHealthMessagesDaoImpl;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.interfaces.InfrastructureLoadManagerInterface;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.BsaJobType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarExecutionState;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.bsa.service.NotificationContextService;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.github.kagkarlsson.scheduler.task.Execution;
import com.github.kagkarlsson.scheduler.task.ExecutionContext;
import com.github.kagkarlsson.scheduler.task.TaskInstance;
import java.time.Instant;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * JUnit4 tests for {@link KarProcessorImpl}. The main class is not modified -- every private method
 * is exercised indirectly through its two public entry points ({@link
 * KarProcessorImpl#applyKarForNotification} and {@link KarProcessorImpl#applyKarForScheduledJob})
 * plus the other public method ({@link KarProcessorImpl#saveDataForDebug}).
 */
public class KarProcessorImplTest {

  private KnowledgeArtifactRepositorySystem karRepositorySystem;
  private EhrQueryService ehrInterface;
  private BsaServiceUtils serviceUtils;
  private KarExecutionStateService karExecutionStateService;
  private NotificationContextService ncService;
  private NotificationContextDao ncDao;
  private HealthcareSettingsService hsService;
  private PublicHealthMessagesDaoImpl phDao;
  private InfrastructureLoadManagerInterface loadManager;
  private IParser jsonParser;

  private KarProcessorImpl karProcessor;

  @Before
  public void setUp() {
    karRepositorySystem = mock(KnowledgeArtifactRepositorySystem.class);
    ehrInterface = mock(EhrQueryService.class);
    serviceUtils = mock(BsaServiceUtils.class);
    karExecutionStateService = mock(KarExecutionStateService.class);
    ncService = mock(NotificationContextService.class);
    ncDao = mock(NotificationContextDao.class);
    hsService = mock(HealthcareSettingsService.class);
    phDao = mock(PublicHealthMessagesDaoImpl.class);
    loadManager = mock(InfrastructureLoadManagerInterface.class);
    jsonParser = mock(IParser.class);

    karProcessor =
        new KarProcessorImpl(
            karRepositorySystem,
            ehrInterface,
            serviceUtils,
            karExecutionStateService,
            ncService,
            ncDao,
            hsService,
            phDao,
            loadManager,
            jsonParser);

    ReflectionTestUtils.setField(karProcessor, "throttlingEnabled", Boolean.FALSE);
    ReflectionTestUtils.setField(karProcessor, "throttleRecheckInterval", 5);
    ReflectionTestUtils.setField(karProcessor, "timerRetries", 3);
    ReflectionTestUtils.setField(karProcessor, "tokenRefreshThreshold", 25);

    // Default: no prior public health message found, unless a test overrides it.
    when(phDao.getPublicHealthMessage(anyMap())).thenReturn(new java.util.ArrayList<>());
  }

  // ==================== helpers ====================

  private NotificationContext buildNotificationContext(String triggerEvent) {
    NotificationContext nc = new NotificationContext();
    nc.setId(UUID.randomUUID());
    nc.setTriggerEvent(triggerEvent);
    nc.setFhirServerBaseUrl("http://fhir-server");
    nc.setPatientId("patient-1");
    nc.setNotificationResourceId("res-1");
    nc.setNotificationResourceType("Encounter");
    nc.setThrottleContext("throttle-ctx");
    return nc;
  }

  private KarProcessingData buildKarProcessingData(KnowledgeArtifact kar, NotificationContext nc) {
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(nc);
    return data;
  }

  private TaskInstance<ScheduledJobData> buildTaskInstance(ScheduledJobData data) {
    return new TaskInstance<>("scheduled-task", "instance-1", data);
  }

  private ExecutionContext buildExecutionContext(int consecutiveFailures) {
    TaskInstance<ScheduledJobData> dummy = new TaskInstance<>("t", "i");
    Execution execution = new Execution(Instant.now(), dummy);
    execution.consecutiveFailures = consecutiveFailures;
    ExecutionContext ctx = mock(ExecutionContext.class);
    when(ctx.getExecution()).thenReturn(execution);
    return ctx;
  }

  private Bundle buildNotificationBundle(Resource notifiedResource) {
    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(new Patient().setId("placeholder-entry-0"));
    bundle.addEntry().setResource(notifiedResource);
    return bundle;
  }

  // ==================== applyKarForNotification ====================

  @Test
  public void applyKarForNotification_happyPath_executesEachMatchedAction() {
    NotificationContext nc = buildNotificationContext("trigger-1");
    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    BsaAction action = mock(BsaAction.class);
    when(action.getActionId()).thenReturn("action-1");
    when(kar.getActionsForTriggerEvent("trigger-1")).thenReturn(new HashSet<>(Set.of(action)));
    when(kar.getVersionUniqueId()).thenReturn("kar-1|1.0");

    KarProcessingData data = buildKarProcessingData(kar, nc);

    karProcessor.applyKarForNotification(data);

    assertEquals(nc.getId().toString(), data.getExecutionSequenceId());
    assertSame(ehrInterface, data.getEhrQueryService());
    assertEquals(BsaJobType.IMMEDIATE_REPORTING, data.getJobType());
    verify(action, times(1)).process(data, ehrInterface);
    assertNull(data.getPhm());
  }

  @Test
  public void applyKarForNotification_existingPhmWithTriggerStatus_setsPreviousStatus() {
    NotificationContext nc = buildNotificationContext("trigger-1");
    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    BsaAction action = mock(BsaAction.class);
    when(kar.getActionsForTriggerEvent("trigger-1")).thenReturn(new HashSet<>(Set.of(action)));
    when(kar.getVersionUniqueId()).thenReturn("kar-1|1.0");

    PublicHealthMessage priorPhm = new PublicHealthMessage();
    priorPhm.setTriggerMatchStatus("{}");
    when(phDao.getPublicHealthMessage(anyMap())).thenReturn(List.of(priorPhm));

    KarProcessingData data = buildKarProcessingData(kar, nc);

    karProcessor.applyKarForNotification(data);

    assertSame(priorPhm, data.getPhm());
    assertNotNull(data.getPreviousTriggerMatchStatus());
  }

  @Test
  public void applyKarForNotification_existingPhmWithoutTriggerStatus_leavesPhmUnset() {
    NotificationContext nc = buildNotificationContext("trigger-1");
    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(kar.getActionsForTriggerEvent("trigger-1")).thenReturn(new HashSet<>());
    when(kar.getVersionUniqueId()).thenReturn("kar-1|1.0");

    PublicHealthMessage priorPhm = new PublicHealthMessage();
    // triggerMatchStatus intentionally left null.
    when(phDao.getPublicHealthMessage(anyMap())).thenReturn(List.of(priorPhm));

    KarProcessingData data = buildKarProcessingData(kar, nc);

    karProcessor.applyKarForNotification(data);

    assertNull(data.getPhm());
  }

  @Test(expected = RuntimeException.class)
  public void applyKarForNotification_actionThrows_exceptionPropagates() {
    NotificationContext nc = buildNotificationContext("trigger-1");
    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    BsaAction action = mock(BsaAction.class);
    when(kar.getActionsForTriggerEvent("trigger-1")).thenReturn(new HashSet<>(Set.of(action)));
    when(kar.getVersionUniqueId()).thenReturn("kar-1|1.0");
    doThrow(new RuntimeException("boom")).when(action).process(any(), any());

    KarProcessingData data = buildKarProcessingData(kar, nc);

    karProcessor.applyKarForNotification(data);
  }

  // ==================== saveDataForDebug ====================

  @Test
  public void saveDataForDebug_savesEveryResourceInOutputData() {
    KarProcessingData kd = new KarProcessingData();
    Resource res1 = new Patient().setId("p1");
    Resource res2 = new Encounter().setId("e1");
    HashMap<String, Resource> actionResources = new HashMap<>();
    actionResources.put("p1", res1);
    actionResources.put("e1", res2);
    HashMap<String, HashMap<String, Resource>> outputData = new HashMap<>();
    outputData.put("action-1", actionResources);
    kd.setActionOutputData(outputData);

    karProcessor.saveDataForDebug(kd);

    verify(serviceUtils, times(1)).saveResourceToFile(res1);
    verify(serviceUtils, times(1)).saveResourceToFile(res2);
  }

  // ==================== applyKarForScheduledJob: early-return validations ====================

  @Test
  public void applyKarForScheduledJob_executionStateNotFound_returnsWithoutError() {
    UUID stateId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder().karExecutionStateId(stateId).jobId("job-1").build();
    when(karExecutionStateService.getKarExecutionStateById(stateId)).thenReturn(null);

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(karExecutionStateService, times(1)).getKarExecutionStateById(stateId);
    verifyNoInteractions(ncService);
  }

  @Test
  public void applyKarForScheduledJob_notificationContextNotFound_returnsWithoutError() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder().karExecutionStateId(stateId).jobId("job-1").build();

    KarExecutionState state = mock(KarExecutionState.class);
    when(state.getNcId()).thenReturn(ncId);
    when(karExecutionStateService.getKarExecutionStateById(stateId)).thenReturn(state);
    when(ncService.getNotificationContext(ncId)).thenReturn(null);

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(ncService, times(1)).getNotificationContext(ncId);
    verifyNoInteractions(hsService);
  }

  @Test
  public void applyKarForScheduledJob_healthcareSettingMissing_returnsWithoutError() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder().karExecutionStateId(stateId).jobId("job-1").build();

    KarExecutionState state = mock(KarExecutionState.class);
    when(state.getNcId()).thenReturn(ncId);
    when(state.getHsFhirServerUrl()).thenReturn("http://hs-url");
    when(karExecutionStateService.getKarExecutionStateById(stateId)).thenReturn(state);

    NotificationContext nc = buildNotificationContext("trigger-1");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);
    when(hsService.getHealthcareSettingByUrl("http://hs-url")).thenReturn(null);
    // setupPreviousTriggerMatchStatus runs before the HS check and needs a non-null Kar to
    // avoid an unrelated NPE in getPublicHealthMessage.
    when(karRepositorySystem.getById(any())).thenReturn(mock(KnowledgeArtifact.class));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(hsService, times(1)).getHealthcareSettingByUrl("http://hs-url");
    verifyNoInteractions(jsonParser);
  }

  @Test
  public void applyKarForScheduledJob_healthcareSettingHasNoKars_returnsWithoutError() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder().karExecutionStateId(stateId).jobId("job-1").build();

    KarExecutionState state = mock(KarExecutionState.class);
    when(state.getNcId()).thenReturn(ncId);
    when(state.getHsFhirServerUrl()).thenReturn("http://hs-url");
    when(karExecutionStateService.getKarExecutionStateById(stateId)).thenReturn(state);

    NotificationContext nc = buildNotificationContext("trigger-1");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    HealthcareSetting hs = new HealthcareSetting();
    // hs.getKars() intentionally left null.
    when(hsService.getHealthcareSettingByUrl("http://hs-url")).thenReturn(hs);
    when(karRepositorySystem.getById(any())).thenReturn(mock(KnowledgeArtifact.class));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verifyNoInteractions(jsonParser);
  }

  @Test
  public void applyKarForScheduledJob_karStatusNotFoundForKarId_returnsWithoutError() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder().karExecutionStateId(stateId).jobId("job-1").build();

    KarExecutionState state = mock(KarExecutionState.class);
    when(state.getNcId()).thenReturn(ncId);
    when(state.getHsFhirServerUrl()).thenReturn("http://hs-url");
    when(state.getKarUniqueId()).thenReturn("kar-1|1.0");
    when(karExecutionStateService.getKarExecutionStateById(stateId)).thenReturn(state);

    NotificationContext nc = buildNotificationContext("trigger-1");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    // A non-matching status entry (rather than an empty set) so findAndSetKarStatus's loop
    // actually iterates at least once before concluding no match was found.
    KnowledgeArtifactStatus nonMatchingStatus = new KnowledgeArtifactStatus();
    nonMatchingStatus.setVersionUniqueKarId("some-other-kar|2.0");
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.addArtifactStatus(nonMatchingStatus);

    HealthcareSetting hs = new HealthcareSetting();
    hs.setKars(kars);
    when(hsService.getHealthcareSettingByUrl("http://hs-url")).thenReturn(hs);
    when(karRepositorySystem.getById(any())).thenReturn(mock(KnowledgeArtifact.class));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verifyNoInteractions(jsonParser);
  }

  @Test
  public void applyKarForScheduledJob_actionNotFoundOnKar_returnsWithoutError() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("missing-action")
            .build();

    KarExecutionState state = setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    when(kar.getAction("missing-action")).thenReturn(null);

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(kar, times(1)).getAction("missing-action");
    verify(karExecutionStateService, never()).delete(any());
  }

  private KarExecutionState setUpMatchingKarStatusState(
      UUID stateId, UUID ncId, String karUniqueId) {
    KarExecutionState state = mock(KarExecutionState.class);
    when(state.getNcId()).thenReturn(ncId);
    when(state.getHsFhirServerUrl()).thenReturn("http://hs-url");
    when(state.getKarUniqueId()).thenReturn(karUniqueId);
    when(karExecutionStateService.getKarExecutionStateById(stateId)).thenReturn(state);

    KnowledgeArtifactStatus matchingStatus = new KnowledgeArtifactStatus();
    matchingStatus.setVersionUniqueKarId(karUniqueId);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.addArtifactStatus(matchingStatus);

    HealthcareSetting hs = new HealthcareSetting();
    hs.setKars(kars);
    when(hsService.getHealthcareSettingByUrl("http://hs-url")).thenReturn(hs);

    return state;
  }

  // ==================== applyKarForScheduledJob: happy paths ====================

  @Test
  public void applyKarForScheduledJob_throttlingDisabled_executesActionImmediately() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .jobType(BsaJobType.IMMEDIATE_REPORTING)
            .build();

    KarExecutionState state = setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    // Notified resource is not an Encounter -> setupContextEncounter's early-return branch.
    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(kar.getAction("action-1")).thenReturn(action);

    // A prior Phm with a trigger match status -> setupPreviousTriggerMatchStatus's true branch.
    PublicHealthMessage priorPhm = new PublicHealthMessage();
    priorPhm.setTriggerMatchStatus("{}");
    when(phDao.getPublicHealthMessage(anyMap())).thenReturn(List.of(priorPhm));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(action, times(1)).process(any(KarProcessingData.class), eq(ehrInterface));
    // The prior Phm carried into kd by setupPreviousTriggerMatchStatus is still present when
    // executeAction checks kd.getPhm(), so it gets persisted.
    verify(phDao, times(1)).saveOrUpdate(priorPhm);
    verify(karExecutionStateService, times(1)).delete(state);
    verifyNoInteractions(loadManager);
  }

  @Test
  public void applyKarForScheduledJob_throttlingEnabledAndAllowed_executesActionAndSavesPhm() {
    ReflectionTestUtils.setField(karProcessor, "throttlingEnabled", Boolean.TRUE);

    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .jobType(BsaJobType.DELAYED_REPORTING)
            .build();

    KarExecutionState state = setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    // Notified resource IS an Encounter -> setupContextEncounter fetches it via ehrInterface.
    Bundle bundle = buildNotificationBundle(new Encounter().setId("enc-1"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    Encounter fetchedEncounter = new Encounter();
    fetchedEncounter.setId("enc-fetched");
    when(ehrInterface.getResourceById(
            any(KarProcessingData.class), eq("Encounter"), anyString(), eq(true)))
        .thenReturn(fetchedEncounter);

    when(loadManager.canExecuteJob(eq("throttle-ctx"), eq(BsaJobType.DELAYED_REPORTING)))
        .thenReturn(true);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(kar.getAction("action-1")).thenReturn(action);
    // Simulate the action populating a Phm during processing.
    PublicHealthMessage phm = new PublicHealthMessage();
    doAnswer(
            invocation -> {
              KarProcessingData kd = invocation.getArgument(0);
              kd.setPhm(phm);
              return null;
            })
        .when(action)
        .process(any(KarProcessingData.class), eq(ehrInterface));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    assertSame(fetchedEncounter, nc.getNotifiedResource());
    verify(loadManager, times(1)).canExecuteJob("throttle-ctx", BsaJobType.DELAYED_REPORTING);
    verify(action, times(1)).process(any(KarProcessingData.class), eq(ehrInterface));
    verify(phDao, times(1)).saveOrUpdate(phm);
    verify(karExecutionStateService, times(1)).delete(state);
  }

  @Test
  public void applyKarForScheduledJob_throttlingEnabledAndBlocked_reschedulesJob() {
    ReflectionTestUtils.setField(karProcessor, "throttlingEnabled", Boolean.TRUE);

    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .xRequestId("req-1")
            .jobType(BsaJobType.DELAYED_REPORTING)
            .mdcContext(new HashMap<>())
            .build();

    setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    when(loadManager.canExecuteJob(eq("throttle-ctx"), eq(BsaJobType.DELAYED_REPORTING)))
        .thenReturn(false);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(action.getActionId()).thenReturn("action-1");
    when(kar.getAction("action-1")).thenReturn(action);

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(action, never()).process(any(), any());
    verify(action, times(1))
        .scheduleJob(
            eq(stateId),
            eq("action-1"),
            any(),
            any(Instant.class),
            eq("req-1"),
            eq(BsaJobType.DELAYED_REPORTING),
            anyMap());
    verify(karExecutionStateService, never()).delete(any());
  }

  // ==================== applyKarForScheduledJob: exception / retry handling ====================

  @Test
  public void applyKarForScheduledJob_actionThrows_retriesNotMaxed_blankCdaData_isSwallowed() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .build();

    setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(kar.getAction("action-1")).thenReturn(action);
    doThrow(new RuntimeException("processing failed"))
        .when(action)
        .process(any(KarProcessingData.class), eq(ehrInterface));

    // timerRetries=3 (default from setUp), 0 consecutive failures -> (0+1)>=3 is false, so
    // handleRetryExecution rethrows, which is caught and logged by the outer wrapper -- no
    // exception should escape this call.
    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(phDao, never()).saveOrUpdate(any());
    verify(ncDao, never()).saveOrUpdate(any());
    verify(karExecutionStateService, never()).delete(any());
  }

  @Test
  public void applyKarForScheduledJob_actionThrows_retriesNotMaxed_nonBlankCdaData_noPriorPhm() {
    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .build();

    setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationResourceType("Encounter");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(kar.getAction("action-1")).thenReturn(action);
    when(phDao.getMaxVersionId(any())).thenReturn(5);
    doAnswer(
            invocation -> {
              KarProcessingData kd = invocation.getArgument(0);
              kd.setSubmittedCdaData("<xml>cda</xml>");
              throw new RuntimeException("processing failed");
            })
        .when(action)
        .process(any(KarProcessingData.class), eq(ehrInterface));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    // Retries not maxed out (0 consecutive failures, timerRetries=3) so no persistence happens
    // yet -- the created PublicHealthMessage lives only inside handleProcessingException's local
    // variable in this run.
    verify(phDao, never()).saveOrUpdate(any());
    verify(karExecutionStateService, never()).delete(any());
  }

  @Test
  public void applyKarForScheduledJob_actionThrows_retriesMaxed_savesFailureState() {
    ReflectionTestUtils.setField(karProcessor, "timerRetries", 1);

    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .build();

    setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationResourceType("Encounter");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(kar.getAction("action-1")).thenReturn(action);
    when(phDao.getMaxVersionId(any())).thenReturn(5);
    doAnswer(
            invocation -> {
              KarProcessingData kd = invocation.getArgument(0);
              kd.setSubmittedCdaData("<xml>cda</xml>");
              throw new RuntimeException("processing failed");
            })
        .when(action)
        .process(any(KarProcessingData.class), eq(ehrInterface));

    // 0 consecutive failures, timerRetries=1 -> (0+1)>=1 is true -> handleMaxRetriesExceeded.
    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(phDao, times(1))
        .saveOrUpdate(argThat(m -> FAILED.equals(m.getSubmissionMessageStatus())));
    verify(ncDao, times(1)).saveOrUpdate(nc);
    assertEquals(
        com.drajer.bsa.model.BsaTypes.NotificationProcessingStatusType.FAILED.toString(),
        nc.getNotificationProcessingStatus());
  }

  @Test
  public void
      applyKarForScheduledJob_actionThrows_retriesMaxed_nonEncounterType_usesUnknownEncounterId() {
    ReflectionTestUtils.setField(karProcessor, "timerRetries", 1);

    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .build();

    setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    // Notification resource type is NOT Encounter -> createPublicHealthMessage's else branch.
    nc.setNotificationResourceType("Patient");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(kar.getAction("action-1")).thenReturn(action);
    when(phDao.getMaxVersionId(any())).thenReturn(5);
    doAnswer(
            invocation -> {
              KarProcessingData kd = invocation.getArgument(0);
              kd.setSubmittedCdaData("<xml>cda</xml>");
              throw new RuntimeException("processing failed");
            })
        .when(action)
        .process(any(KarProcessingData.class), eq(ehrInterface));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(phDao, times(1)).saveOrUpdate(argThat(m -> "Unknown".equals(m.getEncounterId())));
  }

  @Test
  public void applyKarForScheduledJob_actionThrows_retriesMaxed_withPriorPhm_reusesVersionNumber() {
    ReflectionTestUtils.setField(karProcessor, "timerRetries", 1);

    UUID stateId = UUID.randomUUID();
    UUID ncId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder()
            .karExecutionStateId(stateId)
            .jobId("job-1")
            .actionId("action-1")
            .build();

    setUpMatchingKarStatusState(stateId, ncId, "kar-1|1.0");

    NotificationContext nc = buildNotificationContext("trigger-1");
    nc.setNotificationResourceType("Encounter");
    nc.setNotificationData("{}");
    when(ncService.getNotificationContext(ncId)).thenReturn(nc);

    Bundle bundle = buildNotificationBundle(new Patient().setId("notified"));
    when(jsonParser.parseResource(anyString())).thenReturn(bundle);

    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    when(karRepositorySystem.getById("kar-1|1.0")).thenReturn(kar);
    BsaAction action = mock(BsaAction.class);
    when(kar.getAction("action-1")).thenReturn(action);

    PublicHealthMessage existingPhm = new PublicHealthMessage();
    existingPhm.setSubmittedVersionNumber(7);
    // kd already carries a Phm (e.g. set earlier by the action) when it throws, so
    // createPublicHealthMessage should reuse its version number instead of calling
    // phDao.getMaxVersionId.
    doAnswer(
            invocation -> {
              KarProcessingData kd = invocation.getArgument(0);
              kd.setPhm(existingPhm);
              kd.setSubmittedCdaData("<xml>cda</xml>");
              throw new RuntimeException("processing failed");
            })
        .when(action)
        .process(any(KarProcessingData.class), eq(ehrInterface));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(phDao, times(1))
        .saveOrUpdate(argThat(m -> Integer.valueOf(7).equals(m.getSubmittedVersionNumber())));
    verify(phDao, never()).getMaxVersionId(any());
  }

  @Test
  public void applyKarForScheduledJob_exceptionBeforeContextEstablished_retriesMaxed_noNpe() {
    ReflectionTestUtils.setField(karProcessor, "timerRetries", 1);

    UUID stateId = UUID.randomUUID();
    ScheduledJobData data =
        new ScheduledJobData.Builder().karExecutionStateId(stateId).jobId("job-1").build();

    // Exception occurs while looking up the execution state -- nc and publicHealthMessage never
    // get assigned, exercising handleMaxRetriesExceeded's null-safety branches.
    when(karExecutionStateService.getKarExecutionStateById(stateId))
        .thenThrow(new RuntimeException("db down"));

    karProcessor.applyKarForScheduledJob(data, buildTaskInstance(data), buildExecutionContext(0));

    verify(phDao, never()).saveOrUpdate(any());
    verify(ncDao, never()).saveOrUpdate(any());
  }
}
