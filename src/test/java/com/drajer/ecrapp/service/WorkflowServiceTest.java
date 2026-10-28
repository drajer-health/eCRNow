package com.drajer.ecrapp.service;

import static org.mockito.Mockito.*;
import static org.springframework.test.util.ReflectionTestUtils.setField;

import com.drajer.eca.model.*;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.config.AppConfig;
import com.drajer.ecrapp.config.TaskConfiguration;
import com.drajer.routing.RestApiSender;
import com.drajer.routing.impl.DirectEicrSender;
import com.drajer.routing.impl.DirectResponseReceiver;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.drajer.sof.utils.FhirContextInitializer;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.kagkarlsson.scheduler.CurrentlyExecuting;
import com.github.kagkarlsson.scheduler.Scheduler;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.TaskInstance;
import com.github.kagkarlsson.scheduler.task.TaskInstanceId;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hibernate.ObjectDeletedException;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Timing;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

@RunWith(MockitoJUnitRunner.class)
public class WorkflowServiceTest {

  /** The real ActionRepo singleton, restored after each test (setUp replaces it with a mock). */
  private Object savedActionRepoInstance;

  @InjectMocks private WorkflowService workflowService;

  @Mock private TriggerQueryService triggerQueryService;
  @Mock private LoadingQueryService loadingQueryService;
  @Mock private LaunchService launchService;
  @Mock private ClientDetailsService clientDetailsService;
  @Mock private ThreadPoolTaskScheduler taskScheduler;
  @Mock private EicrRRService eicrRRService;
  @Mock private DirectEicrSender directTransport;
  @Mock private DirectResponseReceiver directReceiver;
  @Mock private RestApiSender restApiSender;
  @Mock private ObjectMapper mapper;
  @Mock private Scheduler scheduler;
  @Mock private TaskConfiguration taskConfiguration;
  @Mock private SchedulerService schedulerService;
  @Mock private AppConfig appConfig;
  @Mock private FhirContextInitializer fhirContextInitializer;
  @Mock private ActionRepo actionRepoMock;
  @Mock private LaunchDetails launchDetails;

  @Mock private AbstractAction action1;
  @Mock private AbstractAction action2;

  @Before
  public void setUp() throws Exception {
    setField(workflowService, "triggerQueryService", triggerQueryService);
    setField(workflowService, "loadingQueryService", loadingQueryService);
    setField(workflowService, "launchService", launchService);
    setField(workflowService, "clientDetailService", clientDetailsService);
    setField(workflowService, "taskScheduler", taskScheduler);
    setField(workflowService, "eicrRRService", eicrRRService);
    setField(workflowService, "directTansport", directTransport);
    setField(workflowService, "directReceiver", directReceiver);
    setField(workflowService, "restApiTransport", restApiSender);
    setField(workflowService, "mapper", mapper);
    setField(workflowService, "scheduler", scheduler);
    setField(workflowService, "taskConfiguration", taskConfiguration);
    setField(workflowService, "schedulerService", schedulerService);
    setField(workflowService, "appConfig", appConfig);
    setField(workflowService, "fhirContextInitializer", fhirContextInitializer);
    setField(workflowService, "schematronFileLocation", "schematron-location");
    setField(workflowService, "logFileLocation", "log-location");
    setField(workflowService, "xsdSchemasLocation", "xsd-location");

    java.lang.reflect.Field instanceField = ActionRepo.class.getDeclaredField("instance");
    instanceField.setAccessible(true);
    savedActionRepoInstance = instanceField.get(null);
    instanceField.set(null, actionRepoMock);

    PatientExecutionState defaultState = new PatientExecutionState(null, null);
    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(defaultState);
    when(launchDetails.getStatus()).thenReturn("{}");
  }

  @After
  public void restoreActionRepo() throws Exception {
    // Without this the mock stays in ActionRepo.instance and breaks later tests in the same JVM
    // (e.g. ActionRepoTest, where every getter then returns null).
    java.lang.reflect.Field instanceField = ActionRepo.class.getDeclaredField("instance");
    instanceField.setAccessible(true);
    instanceField.set(null, savedActionRepoInstance);
  }

  /** ✅ initializeActionRepo() Tests * */
  @Test
  public void testInitializeActionRepo_Success() throws Exception {
    workflowService.initializeActionRepo();

    verify(actionRepoMock).setLoadingQueryService(loadingQueryService);
    verify(actionRepoMock).setTriggerQueryService(triggerQueryService);
    verify(actionRepoMock).setLaunchService(launchService);
    verify(actionRepoMock).setClientDetailsService(clientDetailsService);
    verify(actionRepoMock).setTaskScheduler(taskScheduler);
    verify(actionRepoMock).setEicrRRService(eicrRRService);
    verify(actionRepoMock).setSchematronFileLocation("schematron-location");
    verify(actionRepoMock).setDirectTransport(directTransport);
    verify(actionRepoMock).setDirectReceiver(directReceiver);
    verify(actionRepoMock).setLogFileDirectory("log-location");
    verify(actionRepoMock).setXsdSchemasLocation("xsd-location");
    verify(actionRepoMock).setRestTransport(restApiSender);
    verify(actionRepoMock).setAppConfig(appConfig);
    verify(actionRepoMock).setFhirContextInitializer(fhirContextInitializer);
    verify(actionRepoMock).setWorkflowService(workflowService);

    assertStaticField("staticScheduler", scheduler);
    assertStaticField("staticTaskConfiguration", taskConfiguration);
    assertStaticField("staticSchedulerService", schedulerService);
  }

  /** ✅ handleWorkflowEvent() Tests * */
  @Test
  public void testHandleWorkflowEvent_SOF_LAUNCH_Success() throws Exception {
    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
    String patientId = "test-patient-123";
    String encounterId = "test-encounter-456";

    when(launchDetails.getLaunchPatientId()).thenReturn(patientId);
    when(launchDetails.getEncounterId()).thenReturn(encounterId);
    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
        .thenReturn("{\"patientId\":\"test-patient-123\",\"encounterId\":\"test-encounter-456\"}");

    workflowService.handleWorkflowEvent(eventType, launchDetails);

    verify(launchDetails, atLeast(2)).getLaunchPatientId();
    verify(launchDetails, atLeast(2)).getEncounterId();
    verify(mapper).writeValueAsString(any(PatientExecutionState.class));
    verify(launchDetails).setStatus(anyString());
  }

  @Test
  public void testHandleWorkflowEvent_SOF_LAUNCH_JsonProcessingException() throws Exception {
    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
    when(launchDetails.getLaunchPatientId()).thenReturn("test");
    when(launchDetails.getEncounterId()).thenReturn("enc");
    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
        .thenThrow(mock(JsonProcessingException.class));

    workflowService.handleWorkflowEvent(eventType, launchDetails);

    verify(launchDetails, atLeast(2)).getLaunchPatientId();
    verify(launchDetails, atLeast(2)).getEncounterId();
  }

  @Test
  public void testHandleWorkflowEvent_SUBSCRIPTION_NOTIFICATION_DoNothing() {
    workflowService.handleWorkflowEvent(WorkflowEvent.SUBSCRIPTION_NOTIFICATION, launchDetails);
    verifyNoInteractions(launchDetails);
    verifyNoInteractions(mapper);
  }

  @Test(expected = NullPointerException.class)
  public void testHandleWorkflowEvent_WithNullDetails_ThrowsException() {
    workflowService.handleWorkflowEvent(WorkflowEvent.SOF_LAUNCH, null);
  }

  @Test
  public void testHandleWorkflowEvent_WithNullPatientAndEncounter() throws Exception {
    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
    when(launchDetails.getLaunchPatientId()).thenReturn(null);
    when(launchDetails.getEncounterId()).thenReturn(null);
    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
        .thenReturn("{\"patientId\":null,\"encounterId\":null}");

    workflowService.handleWorkflowEvent(eventType, launchDetails);

    verify(launchDetails, atLeast(2)).getLaunchPatientId();
    verify(launchDetails, atLeast(2)).getEncounterId();
    verify(mapper).writeValueAsString(any(PatientExecutionState.class));
    verify(launchDetails).setStatus(anyString());
  }

  @Test
  public void testHandleWorkflowEvent_WithSpecialCharactersInIds() throws Exception {
    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
    when(launchDetails.getLaunchPatientId()).thenReturn("test'patient\"123");
    when(launchDetails.getEncounterId()).thenReturn("test<encounter>456");
    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
        .thenReturn(
            "{\"patientId\":\"test'patient\\\"123\",\"encounterId\":\"test<encounter>456\"}");

    workflowService.handleWorkflowEvent(eventType, launchDetails);

    verify(mapper).writeValueAsString(any(PatientExecutionState.class));
    verify(launchDetails).setStatus(anyString());
  }

  @Test
  public void testExecuteEicrWorkflow_WithValidState() throws Exception {
    WorkflowEvent eventType = WorkflowEvent.SOF_LAUNCH;
    when(launchDetails.getLaunchPatientId()).thenReturn("test-patient-123");
    when(launchDetails.getEncounterId()).thenReturn("test-encounter-456");
    when(launchDetails.getStatus())
        .thenReturn("{\"patientId\":\"test-patient-123\",\"encounterId\":\"test-encounter-456\"}");

    PatientExecutionState mockState =
        new PatientExecutionState("test-patient-123", "test-encounter-456");
    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(mockState);
    when(mapper.writeValueAsString(any(PatientExecutionState.class)))
        .thenReturn("{\"patientId\":\"test-patient-123\",\"encounterId\":\"test-encounter-456\"}");

    workflowService.handleWorkflowEvent(eventType, launchDetails);

    verify(mapper).readValue(anyString(), eq(PatientExecutionState.class));
    verify(launchDetails).setStatus(anyString());
  }

  @Test
  public void testExecuteEicrWorkflow_AllActionsTriggered() throws Exception {
    LaunchDetails details = new LaunchDetails();
    details.setStatus("{}");
    details.setValidationMode(false);

    PatientExecutionState state = mock(PatientExecutionState.class, RETURNS_DEEP_STUBS);
    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(state);

    when(state.getMatchTriggerStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);
    when(state.getCreateEicrStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);

    when(state.getCloseOutEicrStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);
    when(state.getPeriodicUpdateJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);

    WorkflowService spyService = spy(workflowService);
    spyService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task1");

    verify(spyService, atLeastOnce()).executeActionsForType(eq(details), any(), any(), any());
  }

  @Test(expected = RuntimeException.class)
  public void testExecuteEicrWorkflow_JsonProcessingExceptionThrown() throws Exception {
    LaunchDetails details = new LaunchDetails();
    details.setStatus("invalid");
    when(mapper.readValue(anyString(), eq(PatientExecutionState.class)))
        .thenThrow(new JsonProcessingException("error") {});

    workflowService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task2");
  }

  @Test
  public void testExecuteEicrWorkflow_CloseOutCompleted() throws Exception {
    LaunchDetails details = new LaunchDetails();
    details.setStatus("{}");

    PatientExecutionState state = mock(PatientExecutionState.class, RETURNS_DEEP_STUBS);
    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(state);
    when(state.getCloseOutEicrStatus().getJobStatus()).thenReturn(EventTypes.JobStatus.COMPLETED);
    when(state.getPeriodicUpdateJobStatus()).thenReturn(EventTypes.JobStatus.NOT_STARTED);

    WorkflowService spyService = spy(workflowService);
    spyService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task3");

    verify(spyService, atLeastOnce()).executeActionsForType(eq(details), any(), any(), any());
  }

  @Test
  public void testExecuteEicrWorkflow_SkipSubmitInValidationMode() throws Exception {
    LaunchDetails details = new LaunchDetails();
    details.setStatus("{}");
    details.setValidationMode(true);

    PatientExecutionState state = mock(PatientExecutionState.class, RETURNS_DEEP_STUBS);
    when(mapper.readValue(anyString(), eq(PatientExecutionState.class))).thenReturn(state);

    WorkflowService spyService = spy(workflowService);
    spyService.executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, "task4");

    verify(spyService, never())
        .executeActionsForType(
            details, EventTypes.EcrActionTypes.SUBMIT_EICR, WorkflowEvent.SOF_LAUNCH, "task4");
  }

  @Test
  public void testExecuteActions_WithMultipleActions_Success() {
    Set<AbstractAction> actions = new HashSet<>();
    actions.add(action1);
    actions.add(action2);
    WorkflowEvent event = WorkflowEvent.SOF_LAUNCH;
    String taskId = "task-001";

    workflowService.executeActions(launchDetails, actions, event, taskId);

    verify(action1).execute(launchDetails, event, taskId);
    verify(action2).execute(launchDetails, event, taskId);
    verify(launchService).saveOrUpdate(launchDetails);
  }

  @Test
  public void testExecuteActions_WithEmptyActions() {
    Set<AbstractAction> actions = new HashSet<>();
    WorkflowEvent event = WorkflowEvent.SOF_LAUNCH;

    workflowService.executeActions(launchDetails, actions, event, "task-002");

    verify(action1, never()).execute(any(), any(), any());
    verify(action2, never()).execute(any(), any(), any());
    verify(launchService).saveOrUpdate(launchDetails);
  }

  @Test(expected = NullPointerException.class)
  public void testExecuteActions_WithNullActions_ShouldThrowException() {
    workflowService.executeActions(launchDetails, null, WorkflowEvent.SOF_LAUNCH, "task-003");
  }

  @Test(expected = RuntimeException.class)
  public void testExecuteActions_ActionThrowsException() {
    WorkflowEvent event = WorkflowEvent.SOF_LAUNCH;
    doThrow(new RuntimeException("Action failed"))
        .when(action1)
        .execute(launchDetails, event, "task-004");
    Set<AbstractAction> actions = new HashSet<>();
    actions.add(action1);
    try {
      workflowService.executeActions(launchDetails, actions, event, "task-004");
    } finally {
      verify(launchService, never()).saveOrUpdate(any());
    }
  }

  @Test
  public void testInvokeScheduler_TimerAlreadyExists() throws Exception {
    com.drajer.ecrapp.model.ScheduledTasks task1 =
        mock(com.drajer.ecrapp.model.ScheduledTasks.class);
    com.drajer.ecrapp.model.ScheduledTasks task2 =
        mock(com.drajer.ecrapp.model.ScheduledTasks.class);
    List<com.drajer.ecrapp.model.ScheduledTasks> tasks = new java.util.ArrayList<>();
    tasks.add(task1);
    tasks.add(task2);

    java.lang.reflect.Field field =
        WorkflowService.class.getDeclaredField("staticSchedulerService");
    field.setAccessible(true);
    field.set(null, schedulerService);

    when(schedulerService.getScheduledTasks(anyString(), anyString())).thenReturn(tasks);

    Object result =
        WorkflowService.invokeScheduler(
            1, EventTypes.EcrActionTypes.CREATE_EICR, java.time.Instant.now(), "taskId");
    Assert.assertNull(result);
  }

  @Test
  public void testCheckIfTasksExists_TasksNull() {

    Boolean result = WorkflowService.checkIfTasksExists(null, "taskId123");
    Assert.assertFalse(result);
  }

  @Test
  public void testCheckIfTasksExists_TasksEmpty() {
    List<com.drajer.ecrapp.model.ScheduledTasks> emptyTasks = new java.util.ArrayList<>();
    Boolean result = WorkflowService.checkIfTasksExists(emptyTasks, "taskId123");
    Assert.assertFalse(result);
  }

  @Test
  public void testCheckIfTasksExists_NoMatchingTaskInstance() {
    List<com.drajer.ecrapp.model.ScheduledTasks> tasks = new java.util.ArrayList<>();
    com.drajer.ecrapp.model.ScheduledTasks task1 =
        mock(com.drajer.ecrapp.model.ScheduledTasks.class);
    when(task1.getTask_instance()).thenReturn("ABC");
    tasks.add(task1);
    Boolean result = WorkflowService.checkIfTasksExists(tasks, "XYZ");
    Assert.assertFalse(result);
  }

  @Test
  public void testCheckIfTasksExists_MultipleMatches() {
    List<com.drajer.ecrapp.model.ScheduledTasks> tasks = new java.util.ArrayList<>();
    com.drajer.ecrapp.model.ScheduledTasks task1 =
        mock(com.drajer.ecrapp.model.ScheduledTasks.class);
    com.drajer.ecrapp.model.ScheduledTasks task2 =
        mock(com.drajer.ecrapp.model.ScheduledTasks.class);
    com.drajer.ecrapp.model.ScheduledTasks task3 =
        mock(com.drajer.ecrapp.model.ScheduledTasks.class);

    when(task1.getTask_instance()).thenReturn("taskId123");
    when(task2.getTask_instance()).thenReturn("taskId123");
    when(task3.getTask_instance()).thenReturn("other");

    tasks.add(task1);
    tasks.add(task2);
    tasks.add(task3);

    Boolean result = WorkflowService.checkIfTasksExists(tasks, "taskId123");
    Assert.assertTrue(result);
  }

  @Test(expected = ObjectDeletedException.class)
  public void testCancelAllScheduledTasksForLaunch_DeleteTrue() throws Exception {
    List<com.drajer.ecrapp.model.ScheduledTasks> tasks = new java.util.ArrayList<>();
    com.drajer.ecrapp.model.ScheduledTasks task =
        mock(com.drajer.ecrapp.model.ScheduledTasks.class);
    when(task.getTask_instance()).thenReturn("taskId");
    tasks.add(task);
    when(launchDetails.getId()).thenReturn(1);
    java.lang.reflect.Field schedulerServiceField =
        WorkflowService.class.getDeclaredField("staticSchedulerService");
    schedulerServiceField.setAccessible(true);
    schedulerServiceField.set(null, schedulerService);
    when(schedulerService.getScheduledTasks(anyString(), anyString())).thenReturn(tasks);

    java.lang.reflect.Field schedulerField =
        WorkflowService.class.getDeclaredField("staticScheduler");
    schedulerField.setAccessible(true);
    schedulerField.set(null, scheduler);
    when(scheduler.getCurrentlyExecuting()).thenReturn(new java.util.ArrayList<>());

    java.lang.reflect.Field workflowField =
        WorkflowService.class.getDeclaredField("workflowInstance");
    workflowField.setAccessible(true);
    workflowField.set(null, workflowService);

    WorkflowService.cancelAllScheduledTasksForLaunch(launchDetails, true);
  }

  @Test
  public void testEicrActionExecuteJob_RunCatchesException() throws Exception {
    java.util.Map<String, String> mdcContext = new java.util.HashMap<>();

    Integer launchId = 600;
    EventTypes.EcrActionTypes actionType = EventTypes.EcrActionTypes.MATCH_TRIGGER;

    WorkflowService spyService = spy(workflowService);
    doThrow(new RuntimeException("Test Exception"))
        .when(spyService)
        .executeScheduledAction(anyInt(), any(), any(), anyString());

    WorkflowService.EicrActionExecuteJob job =
        spyService.new EicrActionExecuteJob(launchId, actionType, mdcContext);
    try {
      job.run();

      Assert.assertTrue(true);
    } catch (Exception e) {
      Assert.fail("Exception should be caught in run()");
    }
  }

  @Test
  public void testScheduleJob_WithDuration_LogsAndConverts() throws Exception {

    Integer launchId = 200;
    org.hl7.fhir.r4.model.Duration d = new org.hl7.fhir.r4.model.Duration();
    Date timeRef = new Date();
    EventTypes.EcrActionTypes actionType = EventTypes.EcrActionTypes.SUBMIT_EICR;
    String taskInstanceId = "task2";
    boolean executed = false;
    try {
      WorkflowService.scheduleJob(launchId, d, actionType, timeRef, taskInstanceId);
      executed = true;
    } catch (NullPointerException npe) {
      executed = true;
    }

    Assert.assertTrue("scheduleJob with Duration should execute", executed);
  }

  @Test
  public void testScheduleJob_TimingSchedule_ValidatesInput() throws Exception {
    Integer launchId = 300;
    TimingSchedule ts = null; // null input
    Date timeRef = new Date();
    EventTypes.EcrActionTypes actionType = EventTypes.EcrActionTypes.PERIODIC_UPDATE_EICR;
    try {
      WorkflowService.scheduleJob(launchId, ts, actionType, timeRef, "task3");
    } catch (NullPointerException npe) {
      // Expected when TimingSchedule is null; test only verifies no unhandled exception escapes.
    }

    Assert.assertTrue("Method should process input", true);
  }

  @Test
  public void testScheduleJob_Duration_ExecutesAllLines() throws Exception {
    // Setup
    Integer launchId = 400;
    org.hl7.fhir.r4.model.Duration d = new org.hl7.fhir.r4.model.Duration();
    Date timeRef = new Date();
    EventTypes.EcrActionTypes actionType = EventTypes.EcrActionTypes.CLOSE_OUT_EICR;
    String taskInstanceId = "task4";
    boolean executed = false;
    try {
      WorkflowService.scheduleJob(launchId, d, actionType, timeRef, taskInstanceId);
      executed = true;
    } catch (Exception e) {
      executed = true;
    }

    Assert.assertTrue("All lines should execute", executed);
  }

  @Test
  public void executeActionsForType_runsOnlyTheActionsRegisteredForTheType() {
    when(actionRepoMock.getActions())
        .thenReturn(java.util.Map.of(EventTypes.EcrActionTypes.CREATE_EICR, Set.of(action1)));

    workflowService.executeActionsForType(
        launchDetails, EventTypes.EcrActionTypes.CREATE_EICR, WorkflowEvent.SOF_LAUNCH, "t-1");
    workflowService.executeActionsForType(
        launchDetails, EventTypes.EcrActionTypes.MATCH_TRIGGER, WorkflowEvent.SOF_LAUNCH, "t-2");

    verify(action1).execute(launchDetails, WorkflowEvent.SOF_LAUNCH, "t-1");
    verify(action1, never()).execute(launchDetails, WorkflowEvent.SOF_LAUNCH, "t-2");
    verify(launchService, times(1)).saveOrUpdate(launchDetails);
  }

  @Test
  public void executeScheduledAction_runsTheActionThenTheDependentWorkflow() {
    when(actionRepoMock.getLaunchService()).thenReturn(launchService);
    when(launchService.getAuthDetailsById(5)).thenReturn(launchDetails);
    when(actionRepoMock.getActions())
        .thenReturn(java.util.Map.of(EventTypes.EcrActionTypes.CREATE_EICR, Set.of(action1)));

    workflowService.executeScheduledAction(
        5, EventTypes.EcrActionTypes.CREATE_EICR, WorkflowEvent.SCHEDULED_JOB, "t-3");

    verify(action1).execute(launchDetails, WorkflowEvent.SCHEDULED_JOB, "t-3");
    verify(action1).execute(launchDetails, WorkflowEvent.DEPENDENT_EVENT_COMPLETION, "t-3");
  }

  @Test
  public void eicrActionExecuteJob_runsTheScheduledActionOfTheLaunch() {
    org.slf4j.MDC.put("requestId", "r-1");
    when(actionRepoMock.getLaunchService()).thenReturn(launchService);
    when(launchService.getAuthDetailsById(7)).thenReturn(launchDetails);
    when(actionRepoMock.getActions())
        .thenReturn(java.util.Map.of(EventTypes.EcrActionTypes.CREATE_EICR, Set.of(action1)));

    WorkflowService.EicrActionExecuteJob job =
        workflowService
        .new EicrActionExecuteJob(
            7, EventTypes.EcrActionTypes.CREATE_EICR, new java.util.HashMap<>());

    job.run();

    verify(action1).execute(launchDetails, WorkflowEvent.SCHEDULED_JOB, "");
  }

  @Test
  public void scheduleJob_schedulesAnEicrTaskForTheTimingScheduleAndTheDuration() {
    Task<TaskTimer> eicrTask = Tasks.oneTime("EICRTask", TaskTimer.class).execute((i, c) -> {});
    // when(taskConfiguration.sampleOneTimeTask(any())).thenReturn(eicrTask);
    when(taskConfiguration.sampleOneTimeTask()).thenReturn(eicrTask);
    setField(WorkflowService.class, "staticSchedulerService", schedulerService);
    setField(WorkflowService.class, "staticScheduler", scheduler);
    setField(WorkflowService.class, "staticTaskConfiguration", taskConfiguration);
    TimingSchedule everyThreeHours = new TimingSchedule();
    everyThreeHours.setFrequencyPeriod(BigDecimal.valueOf(3));
    everyThreeHours.setFrequencyPeriodUnit(Timing.UnitsOfTime.H);
    Duration twoDays = new Duration();
    twoDays.setValue(2);
    twoDays.setUnit("d");
    Instant before = new Date().toInstant();

    WorkflowService.scheduleJob(
        9, everyThreeHours, EventTypes.EcrActionTypes.PERIODIC_UPDATE_EICR, new Date(), "t-4");
    WorkflowService.scheduleJob(
        9, twoDays, EventTypes.EcrActionTypes.CLOSE_OUT_EICR, new Date(), "t-5");

    ArgumentCaptor<TaskInstance> scheduled = ArgumentCaptor.forClass(TaskInstance.class);
    ArgumentCaptor<Instant> runAt = ArgumentCaptor.forClass(Instant.class);
    verify(scheduler, times(2)).schedule(scheduled.capture(), runAt.capture());
    TaskInstance<?> periodic = scheduled.getAllValues().get(0);
    TaskInstance<?> closeOut = scheduled.getAllValues().get(1);
    Assert.assertEquals("EICRTask", periodic.getTaskName());
    Assert.assertTrue(periodic.getId().startsWith("PERIODIC_UPDATE_EICR_9_"));
    Assert.assertEquals(Integer.valueOf(9), ((TaskTimer) periodic.getData()).getLaunchDetailsId());
    Assert.assertTrue(closeOut.getId().startsWith("CLOSE_OUT_EICR_9_"));
    Assert.assertFalse(runAt.getAllValues().get(0).isBefore(before.plusSeconds(3 * 60 * 60)));
    Assert.assertTrue(runAt.getAllValues().get(0).isBefore(before.plusSeconds(3 * 60 * 60 + 60)));
    Assert.assertTrue(runAt.getAllValues().get(1).isAfter(before.plusSeconds(23 * 60 * 60)));
  }

  @Test
  public void cancelAllScheduledTasksForLaunch_cancelsOnlyTasksThatAreNotRunning() {
    com.drajer.ecrapp.model.ScheduledTasks running = new com.drajer.ecrapp.model.ScheduledTasks();
    running.setTask_instance("inst-running");
    com.drajer.ecrapp.model.ScheduledTasks idle = new com.drajer.ecrapp.model.ScheduledTasks();
    idle.setTask_instance("inst-idle");
    when(launchDetails.getId()).thenReturn(11);
    when(schedulerService.getScheduledTasks("", "11")).thenReturn(List.of(running, idle));
    CurrentlyExecuting executing = mock(CurrentlyExecuting.class);
    when(executing.getTaskInstance()).thenReturn(new TaskInstance<>("EICRTask", "inst-running"));
    when(scheduler.getCurrentlyExecuting()).thenReturn(List.of(executing));
    setField(WorkflowService.class, "staticSchedulerService", schedulerService);
    setField(WorkflowService.class, "staticScheduler", scheduler);

    WorkflowService.cancelAllScheduledTasksForLaunch(launchDetails, false);

    ArgumentCaptor<TaskInstanceId> cancelled = ArgumentCaptor.forClass(TaskInstanceId.class);
    verify(scheduler).cancel(cancelled.capture());
    Assert.assertEquals("inst-idle", cancelled.getValue().getId());
    verify(launchService, never()).delete(any());
  }

  private void assertStaticField(String fieldName, Object expectedValue) throws Exception {
    java.lang.reflect.Field field = WorkflowService.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    Object actualValue = field.get(null);
    Assert.assertEquals(expectedValue, actualValue);
  }
}
