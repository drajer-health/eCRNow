package com.drajer.bsa.service.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.dao.NotificationContextDao;
import com.drajer.bsa.dao.impl.PublicHealthMessagesDaoImpl;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.interfaces.InfrastructureLoadManagerInterface;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatusList;
import com.drajer.bsa.kar.model.*;
import com.drajer.bsa.model.*;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.bsa.service.NotificationContextService;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.kagkarlsson.scheduler.task.ExecutionContext;
import com.github.kagkarlsson.scheduler.task.TaskInstance;
import java.util.*;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class KarProcessorImplTest {
  ObjectMapper mapper = new ObjectMapper();
  IParser jsonParser = FhirContext.forR4().newJsonParser();
  @Mock KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;
  @Mock EhrQueryService ehrQueryService;
  @Mock BsaServiceUtils bsaServiceUtils;
  @Mock KarExecutionStateService karExecutionStateService;
  @Mock NotificationContextService notificationContextService;
  @Mock NotificationContextDao notificationContextDao;
  @Mock HealthcareSettingsService healthcareSettingsService;
  @Mock PublicHealthMessagesDaoImpl publicHealthMessagesDaoImpl;
  @Mock InfrastructureLoadManagerInterface infrastructureLoadManagerInterface;

  @InjectMocks KarProcessorImpl karProcessorImpl;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testapplyKarForNotification() throws JsonProcessingException {
    KarProcessingData karProcessingData = new KarProcessingData();
    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    BsaAction mockBsaAction = mock(BsaAction.class);
    HashSet<BsaAction> bsaActions = new HashSet<>();
    bsaActions.add(mockBsaAction);
    HashMap<String, Set<BsaAction>> triggerEvenTAction = new HashMap<>();
    triggerEvenTAction.put("mock", bsaActions);
    knowledgeArtifact.setTriggerEventActionMap(triggerEvenTAction);
    knowledgeArtifact.setKarVersion("mock123");
    knowledgeArtifact.setKarId("1234");
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setNotificationId("1234");
    publicHealthMessage.setFhirServerBaseUrl("https:FHR.com");

    CheckTriggerCodeStatusList checkTriggerCodeStatusList = new CheckTriggerCodeStatusList();
    String checkTriggerCodeStatusLists = mapper.writeValueAsString(checkTriggerCodeStatusList);
    publicHealthMessage.setTriggerMatchStatus(checkTriggerCodeStatusLists);

    ArrayList<PublicHealthMessage> publicHealthMessages = new ArrayList<>();
    publicHealthMessages.add(publicHealthMessage);
    NotificationContext notificationContext = new NotificationContext();
    karProcessingData.setNotificationContext(notificationContext);
    notificationContext.setTriggerEvent("mock|Event");
    notificationContext.setId(UUID.randomUUID());
    notificationContext.setFhirServerBaseUrl("https:FHR.com");
    notificationContext.setPatientId("123");
    notificationContext.setNotificationResourceId("Mock|Resource");
    notificationContext.setNotificationResourceType("ResourceType");
    notificationContext.setNotificationData("MockNotificationData");
    karProcessingData.setKar(knowledgeArtifact);

    when(publicHealthMessagesDaoImpl.getPublicHealthMessage(any()))
        .thenReturn(publicHealthMessages);

    karProcessorImpl.applyKarForNotification(karProcessingData);

    assertEquals("mock123", knowledgeArtifact.getKarVersion());
    assertEquals("1234", knowledgeArtifact.getKarId());
    assertTrue(knowledgeArtifact.getTriggerEventActionMap().containsKey("mock"));
    assertEquals("ResourceType", notificationContext.getNotificationResourceType());
    assertEquals("Mock|Resource", notificationContext.getNotificationResourceId());

    verify(publicHealthMessagesDaoImpl, times(1)).getPublicHealthMessage(any());
  }

  @Test
  public void test_applyKarForScheduledJob() throws JsonProcessingException {
    ScheduledJobData mockScheduledJobData = mock(ScheduledJobData.class);

    String keyId = UUID.randomUUID().toString();
    when(mockScheduledJobData.getActionId()).thenReturn(keyId);

    TaskInstance<ScheduledJobData> objectTaskInstance = new TaskInstance<>("Task1", "123");
    HealthcareSettingOperationalKnowledgeArtifacts healthcareSettingOperationalKnowledgeArtifacts =
        new HealthcareSettingOperationalKnowledgeArtifacts();

    healthcareSettingOperationalKnowledgeArtifacts.setId(12);
    Set<KnowledgeArtifactStatus> knowledgeArtifactStatusSet = new HashSet<>();

    KnowledgeArtifactStatus knowledgeArtifactStatus = new KnowledgeArtifactStatus();
    knowledgeArtifactStatus.setVersionUniqueKarId(UUID.randomUUID().toString());

    healthcareSettingOperationalKnowledgeArtifacts.setArtifactStatus(knowledgeArtifactStatusSet);

    mockScheduledJobData.setJobId("1234");
    mockScheduledJobData.setxRequestId("Id345");
    mockScheduledJobData.setxRequestId("scheduledid12");
    ExecutionContext mockExectionContext = mock(ExecutionContext.class);
    KarExecutionState mockKarExecutionState = mock(KarExecutionState.class);

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setId(UUID.randomUUID());

    mockKarExecutionState.setKarUniqueId(notificationContext.getId().toString());

    knowledgeArtifactStatusSet.add(knowledgeArtifactStatus);
    knowledgeArtifactStatus.setVersionUniqueKarId(notificationContext.getId().toString());
    notificationContext.setxCorrelationId("1234");
    notificationContext.setxCorrelationId("id12");
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("https:FHR.com");
    healthcareSetting.setKars(healthcareSettingOperationalKnowledgeArtifacts);

    List<PublicHealthMessage> publicHealthMessageList = new ArrayList<>();
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setNotificationId("1234");
    publicHealthMessage.setFhirServerBaseUrl("https:FHR.com");

    CheckTriggerCodeStatusList checkTriggerCodeStatusList = new CheckTriggerCodeStatusList();
    String checkTriggerCodeStatusLists = mapper.writeValueAsString(checkTriggerCodeStatusList);
    publicHealthMessage.setTriggerMatchStatus(checkTriggerCodeStatusLists);

    karProcessorImpl.jsonParser = jsonParser;
    karProcessorImpl.throttlingEnabled = true;
    karProcessorImpl.throttleRecheckInterval = 1;
    Bundle bundle = new Bundle();
    bundle.setType(Bundle.BundleType.HISTORY);

    List<Bundle.BundleEntryComponent> bundleEntryComponentList = new ArrayList<>();

    Bundle.BundleEntryComponent bundleEntryComponent = new Bundle.BundleEntryComponent();
    Bundle.BundleEntryComponent bundleEntryComponent1 = new Bundle.BundleEntryComponent();
    Patient patient = new Patient();
    patient.setId(UUID.randomUUID().toString());
    Encounter encounter = new Encounter();
    encounter.setId(UUID.randomUUID().toString());
    bundleEntryComponent.setResource(patient);
    bundleEntryComponent1.setResource(encounter);
    bundleEntryComponentList.add(bundleEntryComponent);
    bundleEntryComponentList.add(bundleEntryComponent1);
    bundle.setEntry(bundleEntryComponentList);
    String bundleString = jsonParser.encodeResourceToString(bundle);

    notificationContext.setNotificationData(bundleString);
    when(karExecutionStateService.getKarExecutionStateById(any()))
        .thenReturn(mockKarExecutionState);

    when(notificationContextService.getNotificationContext(any())).thenReturn(notificationContext);

    when(healthcareSettingsService.getHealthcareSettingByUrl(any())).thenReturn(healthcareSetting);
    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();

    BsaAction mockBsaAction = mock(BsaAction.class);
    HashMap<String, BsaAction> actionHashMap = new HashMap<>();
    actionHashMap.put(keyId, mockBsaAction);
    knowledgeArtifact.setActionMap(actionHashMap);

    publicHealthMessageList.add(publicHealthMessage);
    when(publicHealthMessagesDaoImpl.getPublicHealthMessage(any()))
        .thenReturn(publicHealthMessageList);
    when(mockKarExecutionState.getKarUniqueId())
        .thenReturn(knowledgeArtifactStatus.getVersionUniqueKarId());

    when(knowledgeArtifactRepositorySystem.getById(any())).thenReturn(knowledgeArtifact);

    karProcessorImpl.applyKarForScheduledJob(
        mockScheduledJobData, objectTaskInstance, mockExectionContext);
    assertTrue(healthcareSetting.getKars().getArtifactStatus().contains(knowledgeArtifactStatus));
    assertEquals("1234", publicHealthMessage.getNotificationId());
    assertEquals("https:FHR.com", publicHealthMessage.getFhirServerBaseUrl());
    assertEquals("1234", publicHealthMessage.getNotificationId());

    verify(karExecutionStateService, times(1)).getKarExecutionStateById(any());
    verify(notificationContextService, times(1)).getNotificationContext(any());
    verify(healthcareSettingsService, times(1)).getHealthcareSettingByUrl(any());
    verify(publicHealthMessagesDaoImpl, times(1)).getPublicHealthMessage(any());
  }

  @Test
  public void testsaveDataForDebug() {
    KarProcessingData karProcessingData = new KarProcessingData();
    HashMap<String, HashMap<String, Resource>> actionOutputHashMap = new HashMap<>();
    HashMap<String, Resource> resourceMap = new HashMap<>();
    Resource mockResource = mock(Resource.class);
    resourceMap.put("Mock", mockResource);
    actionOutputHashMap.put("MockAction", resourceMap);
    karProcessingData.setActionOutputData(actionOutputHashMap);

    karProcessorImpl.saveDataForDebug(karProcessingData);

    assertNotNull(karProcessingData);
    assertTrue(actionOutputHashMap.containsKey("MockAction"));
    assertEquals(mockResource, actionOutputHashMap.get("MockAction").get("Mock"));

    verify(bsaServiceUtils, times(1)).saveResourceToFile(mockResource);
  }
}
