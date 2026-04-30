// package com.drajer.ecrapp.service;
//
// import static org.mockito.Mockito.*;
// import static org.springframework.test.util.ReflectionTestUtils.setField;
//
// import com.drajer.eca.model.AbstractAction;
// import com.drajer.eca.model.ActionRepo;
// import com.drajer.eca.model.EventTypes;
// import com.drajer.eca.model.EventTypes.WorkflowEvent;
// import com.drajer.eca.model.PatientExecutionState;
// import com.drajer.ecrapp.config.AppConfig;
// import com.drajer.ecrapp.config.TaskConfiguration;
// import com.drajer.routing.RestApiSender;
// import com.drajer.routing.impl.DirectEicrSender;
// import com.drajer.routing.impl.DirectResponseReceiver;
// import com.drajer.sof.model.LaunchDetails;
// import com.drajer.sof.service.ClientDetailsService;
// import com.drajer.sof.service.LaunchService;
// import com.drajer.sof.service.LoadingQueryService;
// import com.drajer.sof.service.TriggerQueryService;
// import com.drajer.sof.utils.FhirContextInitializer;
// import com.fasterxml.jackson.core.JsonProcessingException;
// import com.fasterxml.jackson.databind.ObjectMapper;
// import com.github.kagkarlsson.scheduler.Scheduler;
// import java.util.HashSet;
// import java.util.Set;
// import org.junit.Assert;
// import org.junit.Before;
// import org.junit.Test;
// import org.junit.runner.RunWith;
// import org.mockito.InjectMocks;
// import org.mockito.Mock;
// import org.mockito.junit.MockitoJUnitRunner;
// import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
//
// @RunWith(MockitoJUnitRunner.class)
// public class WorkflowServiceTest {
//  @InjectMocks private WorkflowService workflowService;
//
//  @Mock private TriggerQueryService triggerQueryService;
//  @Mock private LoadingQueryService loadingQueryService;
//  @Mock private LaunchService launchService;
//  @Mock private ClientDetailsService clientDetailsService;
//  @Mock private ThreadPoolTaskScheduler taskScheduler;
//  @Mock private EicrRRService eicrRRService;
//  @Mock private DirectEicrSender directTransport;
//  @Mock private DirectResponseReceiver directReceiver;
//  @Mock private RestApiSender restApiSender;
//  @Mock private ObjectMapper mapper;
//  @Mock private Scheduler scheduler;
//  @Mock private TaskConfiguration taskConfiguration;
//  @Mock private SchedulerService schedulerService;
//  @Mock private AppConfig appConfig;
//  @Mock private FhirContextInitializer fhirContextInitializer;
//  @Mock private ActionRepo actionRepoMock;
//  @Mock private LaunchDetails launchDetails;
//
//  @Mock private AbstractAction action1;
//  @Mock private AbstractAction action2;
//
//  @Before
//  public void setUp() throws Exception {
//    setField(workflowService, "triggerQueryService", triggerQueryService);
//    setField(workflowService, "loadingQueryService", loadingQueryService);
//    setField(workflowService, "launchService", launchService);
//    setField(workflowService, "clientDetailService", clientDetailsService);
//    setField(workflowService, "taskScheduler", taskScheduler);
//    setField(workflowService, "eicrRRService", eicrRRService);
//    setField(workflowService, "directTansport", directTransport);
//    setField(workflowService, "directReceiver", directReceiver);
//    setField(workflowService, "restApiTransport", restApiSender);
//    setField(workflowService, "mapper", mapper);
//    setField(workflowService, "scheduler", scheduler);
//    setField(workflowService, "taskConfiguration", taskConfiguration);
//    setField(workflowService, "schedulerService", schedulerService);
//    setField(workflowService, "appConfig", appConfig);
//    setField(workflowService, "fhirContextInitializer", fhirContextInitializer);
//    setField(workflowService, "schematronFileLocation", "schematron-location");
//    setField(workflowService, "logFileLocation", "log-location");
//    setField(workflowService, "xsdSchemasLocation", "xsd-location");
//
//    java.lang.reflect.Field instanceField = ActionRepo.class.getDeclaredField("instance");
//    instanceField.setAccessible(true);
//    instanceField.set(null, actionRepoMock);
//
//    PatientExecutionState defaultState = new PatientExecutionState(null, null);
//    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(defaultState);
//    when(launchDetails.getStatus()).thenReturn("{}");
//  }
//
//  /** ✅ initializeActionRepo() Tests * */
//  @Test
//  public void testInitializeActionRepo_Success() throws Exception {
//    workflowService.initializeActionRepo();
//
//    verify(actionRepoMock).setLoadingQueryService(loadingQueryService);
//    verify(actionRepoMock).setTriggerQueryService(triggerQueryService);
//    verify(actionRepoMock).setLaunchService(launchService);
//    verify(actionRepoMock).setClientDetailsService(clientDetailsService);
//    verify(actionRepoMock).setTaskScheduler(taskScheduler);
//    verify(actionRepoMock).setEicrRRService(eicrRRService);
//    verify(actionRepoMock).setSchematronFileLocation("schematron-location");
//    verify(actionRepoMock).setDirectTransport(directTransport);
//    verify(actionRepoMock).setDirectReceiver(directReceiver);
//    verify(actionRepoMock).setLogFileDirectory("log-location");
//    verify(actionRepoMock).setXsdSchemasLocation("xsd-location");
//    verify(actionRepoMock).setRestTransport(restApiSender);
//    verify(actionRepoMock).setAppConfig(appConfig);
//    verify(actionRepoMock).setFhirContextInitializer(fhirContextInitializer);
//    verify(actionRepoMock).setWorkflowService(workflowService);
//
//    assertStaticField("staticScheduler", scheduler);
//    assertStaticField("staticTaskConfiguration", taskConfiguration);
//    assertStaticField("staticSchedulerService", schedulerService);
//  }
//
//  /** ✅ handleWorkflowEvent() Tests * */
//  @Test
//  public void testHandleWorkflowEvent_SOF_LAUNCH_Success() throws Exception {
//    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
//    String patientId = "test-patient-123";
//    String encounterId = "test-encounter-456";
//
//    when(launchDetails.getLaunchPatientId()).thenReturn(patientId);
//    when(launchDetails.getEncounterId()).thenReturn(encounterId);
//    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
//
// .thenReturn("{\"patientId\":\"test-patient-123\",\"encounterId\":\"test-encounter-456\"}");
//
//    workflowService.handleWorkflowEvent(eventType, launchDetails);
//
//    verify(launchDetails, atLeast(2)).getLaunchPatientId();
//    verify(launchDetails, atLeast(2)).getEncounterId();
//    verify(mapper).writeValueAsString(any(PatientExecutionState.class));
//    verify(launchDetails).setStatus(anyString());
//  }
//
//  @Test
//  public void testHandleWorkflowEvent_SOF_LAUNCH_JsonProcessingException() throws Exception {
//    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
//    when(launchDetails.getLaunchPatientId()).thenReturn("test");
//    when(launchDetails.getEncounterId()).thenReturn("enc");
//    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
//        .thenThrow(mock(JsonProcessingException.class));
//
//    workflowService.handleWorkflowEvent(eventType, launchDetails);
//
//    verify(launchDetails, atLeast(2)).getLaunchPatientId();
//    verify(launchDetails, atLeast(2)).getEncounterId();
//  }
//
//  @Test
//  public void testHandleWorkflowEvent_SUBSCRIPTION_NOTIFICATION_DoNothing() {
//    workflowService.handleWorkflowEvent(WorkflowEvent.SUBSCRIPTION_NOTIFICATION, launchDetails);
//    verifyNoInteractions(launchDetails);
//    verifyNoInteractions(mapper);
//  }
//
//  @Test(expected = NullPointerException.class)
//  public void testHandleWorkflowEvent_WithNullDetails_ThrowsException() {
//    workflowService.handleWorkflowEvent(WorkflowEvent.SOF_LAUNCH, null);
//  }
//
//  @Test
//  public void testHandleWorkflowEvent_WithNullPatientAndEncounter() throws Exception {
//    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
//    when(launchDetails.getLaunchPatientId()).thenReturn(null);
//    when(launchDetails.getEncounterId()).thenReturn(null);
//    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
//        .thenReturn("{\"patientId\":null,\"encounterId\":null}");
//
//    workflowService.handleWorkflowEvent(eventType, launchDetails);
//
//    verify(launchDetails, atLeast(2)).getLaunchPatientId();
//    verify(launchDetails, atLeast(2)).getEncounterId();
//    verify(mapper).writeValueAsString(any(PatientExecutionState.class));
//    verify(launchDetails).setStatus(anyString());
//  }
//
//  @Test
//  public void testHandleWorkflowEvent_WithSpecialCharactersInIds() throws Exception {
//    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
//    when(launchDetails.getLaunchPatientId()).thenReturn("test'patient\"123");
//    when(launchDetails.getEncounterId()).thenReturn("test<encounter>456");
//    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
//        .thenReturn(
//            "{\"patientId\":\"test'patient\\\"123\",\"encounterId\":\"test<encounter>456\"}");
//
//    workflowService.handleWorkflowEvent(eventType, launchDetails);
//
//    verify(mapper).writeValueAsString(any(PatientExecutionState.class));
//    verify(launchDetails).setStatus(anyString());
//  }
//
//  @Test
//  public void testExecuteEicrWorkflow_WithValidState() throws Exception {
//    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
//    when(launchDetails.getLaunchPatientId()).thenReturn("test-patient-123");
//    when(launchDetails.getEncounterId()).thenReturn("test-encounter-456");
//    when(launchDetails.getStatus())
//
// .thenReturn("{\"patientId\":\"test-patient-123\",\"encounterId\":\"test-encounter-456\"}");
//
//    PatientExecutionState mockState =
//        new PatientExecutionState("test-patient-123", "test-encounter-456");
//    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(mockState);
//    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
//
// .thenReturn("{\"patientId\":\"test-patient-123\",\"encounterId\":\"test-encounter-456\"}");
//
//    workflowService.handleWorkflowEvent(eventType, launchDetails);
//
//    verify(mapper).readValue(anyString(), eq(PatientExecutionState.class));
//    verify(launchDetails).setStatus(anyString());
//  }
//
//  /** ✅ executeEicrWorkflow() Deep Tests * */
//  @Test
//  public void testExecuteEicrWorkflow_AllActionsTriggered() throws Exception {
//    LaunchDetails details = new LaunchDetails();
//    details.setStatus("{}");
//    details.setValidationMode(false);
//
//    PatientExecutionState state = mock(PatientExecutionState.class, RETURNS_DEEP_STUBS);
//    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(state);
//
// when(state.getMatchTriggerStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);
//    when(state.getCreateEicrStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);
//
// when(state.getCloseOutEicrStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);
//    when(state.getPeriodicUpdateJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);
//
//    WorkflowService spyService = spy(workflowService);
//    spyService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task1");
//
//    verify(spyService, atLeastOnce()).executeActionsForType(eq(details), any(), any(), any());
//  }
//
//  @Test(expected = RuntimeException.class)
//  public void testExecuteEicrWorkflow_JsonProcessingExceptionThrown() throws Exception {
//    LaunchDetails details = new LaunchDetails();
//    details.setStatus("invalid");
//    when(mapper.readValue(anyString(), eq(PatientExecutionState.class)))
//        .thenThrow(new JsonProcessingException("error") {});
//
//    workflowService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task2");
//  }
//
//  @Test
//  public void testExecuteEicrWorkflow_CloseOutCompleted() throws Exception {
//    LaunchDetails details = new LaunchDetails();
//    details.setStatus("{}");
//
//    PatientExecutionState state = mock(PatientExecutionState.class, RETURNS_DEEP_STUBS);
//    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(state);
//    when(state.getCloseOutEicrStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.COMPLETED);
//    when(state.getPeriodicUpdateJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);
//
//    WorkflowService spyService = spy(workflowService);
//    spyService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task3");
//
//    verify(spyService, atLeastOnce()).executeActionsForType(eq(details), any(), any(), any());
//  }
//
//  @Test
//  public void testExecuteEicrWorkflow_SkipSubmitInValidationMode() throws Exception {
//    LaunchDetails details = new LaunchDetails();
//    details.setStatus("{}");
//    details.setValidationMode(true);
//
//    PatientExecutionState state = mock(PatientExecutionState.class, RETURNS_DEEP_STUBS);
//    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(state);
//
//    WorkflowService spyService = spy(workflowService);
//    spyService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task4");
//
//    verify(spyService, never())
//        .executeActionsForType(
//            details, EventTypes.EcrActionTypes.SUBMIT_EICR, WorkflowEvent.SOF_LAUNCH, "task4");
//  }
//
//  @Test
//  public void testExecuteActions_WithMultipleActions_Success() {
//    Set<AbstractAction> actions = new HashSet<>();
//    actions.add(action1);
//    actions.add(action2);
//    WorkflowEvent event = WorkflowEvent.SOF_LAUNCH;
//    String taskId = "task-001";
//
//    workflowService.executeActions(launchDetails, actions, event, taskId);
//
//    verify(action1).execute(launchDetails, event, taskId);
//    verify(action2).execute(launchDetails, event, taskId);
//    verify(launchService).saveOrUpdate(launchDetails);
//  }
//
//  @Test
//  public void testExecuteActions_WithEmptyActions() {
//    Set<AbstractAction> actions = new HashSet<>();
//    WorkflowEvent event = WorkflowEvent.SOF_LAUNCH;
//
//    workflowService.executeActions(launchDetails, actions, event, "task-002");
//
//    verify(action1, never()).execute(any(), any(), any());
//    verify(action2, never()).execute(any(), any(), any());
//    verify(launchService).saveOrUpdate(launchDetails);
//  }
//
//  @Test(expected = NullPointerException.class)
//  public void testExecuteActions_WithNullActions_ShouldThrowException() {
//    workflowService.executeActions(launchDetails, null, WorkflowEvent.SOF_LAUNCH, "task-003");
//  }
//
//  @Test(expected = RuntimeException.class)
//  public void testExecuteActions_ActionThrowsException() {
//    WorkflowEvent event = WorkflowEvent.SOF_LAUNCH;
//    doThrow(new RuntimeException("Action failed"))
//        .when(action1)
//        .execute(launchDetails, event, "task-004");
//    Set<AbstractAction> actions = new HashSet<>();
//    actions.add(action1);
//    try {
//      workflowService.executeActions(launchDetails, actions, event, "task-004");
//    } finally {
//      verify(launchService, never()).saveOrUpdate(any());
//    }
//  }
//
//  /** Utility to check static fields * */
//  private void assertStaticField(String fieldName, Object expectedValue) throws Exception {
//    java.lang.reflect.Field field = WorkflowService.class.getDeclaredField(fieldName);
//    field.setAccessible(true);
//    Object actualValue = field.get(null);
//    Assert.assertEquals(expectedValue, actualValue);
//  }
// }
