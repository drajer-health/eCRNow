package com.drajer.ecrapp.service;

import com.drajer.eca.model.AbstractAction;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.eca.model.TaskTimer;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.ecrapp.config.AppConfig;
import com.drajer.ecrapp.config.TaskConfiguration;
import com.drajer.ecrapp.model.ScheduledTasks;
import com.drajer.ecrapp.util.ApplicationUtils;
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
import com.github.kagkarlsson.scheduler.task.TaskInstance;
import com.github.kagkarlsson.scheduler.task.TaskInstanceId;
import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.annotation.PostConstruct;
import javax.swing.*;
import org.apache.commons.text.StringEscapeUtils;
import org.hibernate.ObjectDeletedException;
import org.hl7.fhir.r4.model.Duration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Service;

@Service
public class WorkflowService {

  private static final Logger logger = LoggerFactory.getLogger(WorkflowService.class);
  private static WorkflowService workflowInstance = null;

  /*
   *  These are other services that the action classes use and we are storing it once for all of them instead
   *  of using it class variables which can be injected.
   *  We could do injection if we do not use NEW operator , but the ERSD processor will use new to create instead of Spring context, hence Autowired
   *   variables cannot be injected into this class or the action classes.
   */
  @Autowired TriggerQueryService triggerQueryService;

  @Autowired LoadingQueryService loadingQueryService;

  @Autowired LaunchService launchService;

  @Autowired ClientDetailsService clientDetailService;

  @Autowired ThreadPoolTaskScheduler taskScheduler;

  @Autowired EicrRRService eicrRRService;

  @Autowired DirectEicrSender directTansport;

  @Autowired RestApiSender restApiTransport;

  @Autowired DirectResponseReceiver directReceiver;

  @Autowired ObjectMapper mapper;

  @Autowired Scheduler scheduler;

  private static Scheduler staticScheduler;

  @Autowired TaskConfiguration taskConfiguration;

  private static TaskConfiguration staticTaskConfiguration;

  @Autowired SchedulerService schedulerService;

  @Autowired AppConfig appConfig;

  @Autowired FhirContextInitializer fhirContextInitializer;

  private static SchedulerService staticSchedulerService;

  @Value("${schematron.file.location}")
  String schematronFileLocation;

  @Value("${logging.file.name}")
  String logFileLocation;

  @Value("${xsd.schemas.location}")
  String xsdSchemasLocation;

  @PostConstruct
  public void initializeActionRepo() {
    ActionRepo.getInstance().setLoadingQueryService(loadingQueryService);
    ActionRepo.getInstance().setTriggerQueryService(triggerQueryService);
    ActionRepo.getInstance().setLaunchService(launchService);
    ActionRepo.getInstance().setClientDetailsService(clientDetailService);
    ActionRepo.getInstance().setTaskScheduler(taskScheduler);
    ActionRepo.getInstance().setEicrRRService(eicrRRService);
    ActionRepo.getInstance().setSchematronFileLocation(schematronFileLocation);
    ActionRepo.getInstance().setDirectTransport(directTansport);
    ActionRepo.getInstance().setDirectReceiver(directReceiver);
    ActionRepo.getInstance().setLogFileDirectory(logFileLocation);
    ActionRepo.getInstance().setXsdSchemasLocation(xsdSchemasLocation);
    ActionRepo.getInstance().setRestTransport(restApiTransport);
    ActionRepo.getInstance().setAppConfig(appConfig);
    ActionRepo.getInstance().setFhirContextInitializer(fhirContextInitializer);

    workflowInstance = this;
    ActionRepo.getInstance().setWorkflowService(workflowInstance);
    this.staticScheduler = scheduler;
    this.staticTaskConfiguration = taskConfiguration;
    this.staticSchedulerService = schedulerService;
  }

  public void handleWorkflowEvent(EventTypes.WorkflowEvent type, LaunchDetails details) {

    if (type == WorkflowEvent.SOF_LAUNCH) {

      // Identify the appropriate actions and execute it from the Action Repo.
      logger.info(
          " SOF Launch for Patient : {} and Encounter : {}",
          StringEscapeUtils.escapeJava(details.getLaunchPatientId()),
          StringEscapeUtils.escapeJava(details.getEncounterId()));

      // Setup Execution State.
      PatientExecutionState oldstate =
          new PatientExecutionState(details.getLaunchPatientId(), details.getEncounterId());

      try {
        details.setStatus(mapper.writeValueAsString(oldstate));
      } catch (JsonProcessingException e) {
        logger.error("Error while handling SOF Launch workflow", e);
      }

      logger.info("State = {}", details.getStatus());

      String taskInstanceId = "";
      // Use Action Repo to get the events that we need to fire.
      executeEicrWorkflow(details, WorkflowEvent.SOF_LAUNCH, taskInstanceId);

    } else if (type == WorkflowEvent.SUBSCRIPTION_NOTIFICATION) {

      // Do nothing for now.

    }
  }

  public void executeEicrWorkflow(
      LaunchDetails details, WorkflowEvent launchType, String taskInstanceId) {

    logger.info(" ***** START EXECUTING EICR WORKFLOW ***** ");

    PatientExecutionState state = null;

    try {
      logger.debug("Reading object State ");
      state = mapper.readValue(details.getStatus(), PatientExecutionState.class);
      logger.debug("Finished Reading object State ");
    } catch (JsonProcessingException e1) {

      String msg = "Unable to read/write execution state";
      logger.error(msg, e1);
      throw new RuntimeException(msg);
    }

    if (state.getMatchTriggerStatus().getJobStatus() != JobStatus.COMPLETED) {
      logger.info("Execute Match Trigger Action");
      executeActionsForType(details, EcrActionTypes.MATCH_TRIGGER, launchType, taskInstanceId);
    }

    if (state.getCreateEicrStatus().getJobStatus() != JobStatus.COMPLETED
        && state.getCloseOutEicrStatus().getJobStatus() != JobStatus.COMPLETED) {
      logger.info("Execute Create Eicr Action");
      executeActionsForType(details, EcrActionTypes.CREATE_EICR, launchType, taskInstanceId);
    } else if (state.getCloseOutEicrStatus().getJobStatus() == JobStatus.COMPLETED) {
      logger.info("Stopping Create Action");
      state.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);
    }

    if (state.getPeriodicUpdateJobStatus() == JobStatus.NOT_STARTED
        && state.getCloseOutEicrStatus().getJobStatus() != JobStatus.COMPLETED) {
      logger.info("Execute Periodic Update Action");
      executeActionsForType(
          details, EcrActionTypes.PERIODIC_UPDATE_EICR, launchType, taskInstanceId);
    } else if (state.getCloseOutEicrStatus().getJobStatus() == JobStatus.COMPLETED) {
      logger.info("Stopping Periodic Update Action");
      state.setPeriodicUpdateJobStatus(JobStatus.COMPLETED);
    }

    if (state.getCloseOutEicrStatus().getJobStatus() == JobStatus.NOT_STARTED) {
      logger.info("Execute Close Out Action");
      executeActionsForType(details, EcrActionTypes.CLOSE_OUT_EICR, launchType, taskInstanceId);
    }

    logger.info("Execute Validate Eicr Action");
    executeActionsForType(details, EcrActionTypes.VALIDATE_EICR, launchType, taskInstanceId);

    if (Boolean.FALSE.equals(details.getValidationMode())) {
      logger.info("Execute Submit Eicr Action");
      executeActionsForType(details, EcrActionTypes.SUBMIT_EICR, launchType, taskInstanceId);
    }

    logger.info("Execute RR Check Action");
    executeActionsForType(details, EcrActionTypes.RR_CHECK, launchType, taskInstanceId);

    logger.info("***** END EXECUTING EICR WORKFLOW *****");
  }

  public void executeActions(
      LaunchDetails details,
      Set<AbstractAction> actions,
      WorkflowEvent launchType,
      String taskInstanceId) {

    for (AbstractAction act : actions) {
      // Execute the event.
      act.execute(details, launchType, taskInstanceId);
    }

    // Update state for next action
    launchService.saveOrUpdate(details);
  }

  public void executeActionsForType(
      LaunchDetails details, EcrActionTypes type, WorkflowEvent launchType, String taskInstanceId) {

    ActionRepo repo = ActionRepo.getInstance();

    // Get Actions for Trigger Matching.
    if (repo.getActions() != null && repo.getActions().containsKey(type)) {

      executeActions(details, repo.getActions().get(type), launchType, taskInstanceId);
    }
  }

  public void executeScheduledAction(
      Integer launchDetailsId,
      EcrActionTypes actionType,
      WorkflowEvent launchType,
      String taskInstanceId) {

    logger.info(
        "Get Launch Details from Database for Id  : {} for Action Type {} and start execution ",
        launchDetailsId,
        actionType);

    LaunchDetails launchDetails =
        ActionRepo.getInstance().getLaunchService().getAuthDetailsById(launchDetailsId);

    executeActionsForType(launchDetails, actionType, launchType, taskInstanceId);

    // Execute the Eicr Workflow since a job executtion can unlock other dependencies and execute
    // other jobs.
    executeEicrWorkflow(launchDetails, WorkflowEvent.DEPENDENT_EVENT_COMPLETION, taskInstanceId);
  }

  public class EicrActionExecuteJob implements Runnable {

    private Integer launchDetailsId;

    private EcrActionTypes actionType;

    private Map<String, String> loggingDiagnosticContext;

    public EicrActionExecuteJob(
        Integer launchDetailsId,
        EcrActionTypes actionType,
        Map<String, String> loggingDiagnosticContext) {
      logger.info("loggingDiagnosticContext:{}", loggingDiagnosticContext);
      this.launchDetailsId = launchDetailsId;
      this.actionType = actionType;
      this.loggingDiagnosticContext = MDC.getCopyOfContextMap();
    }

    @Override
    public void run() {
      try {
        MDC.setContextMap(this.loggingDiagnosticContext);
        logger.info("Starting the Thread");
        String taskInstanceId = "";
        executeScheduledAction(
            launchDetailsId, actionType, WorkflowEvent.SCHEDULED_JOB, taskInstanceId);
      } catch (Exception e) {
        logger.error("Error in Getting Data=====>", e);
      } finally {
        MDC.clear();
      }
    }
  }

  public static void scheduleJob(
      Integer launchDetailsId,
      TimingSchedule ts,
      EcrActionTypes actionType,
      Date timeRef,
      String taskInstanceId) {

    Instant t = ApplicationUtils.convertTimingScheduleToInstant(ts, timeRef);

    invokeScheduler(launchDetailsId, actionType, t, taskInstanceId);

    String timing = t.toString();
    logger.info("Job Scheduled for Action to execute for : {} at time : {}", actionType, timing);
  }

  public static void scheduleJob(
      Integer launchDetailsId,
      Duration d,
      EcrActionTypes actionType,
      Date timeRef,
      String taskInstanceId) {
    logger.info("timeRef in scheduleJob:{}", timeRef);

    Instant t = ApplicationUtils.convertDurationToInstant(d);

    invokeScheduler(launchDetailsId, actionType, t, taskInstanceId);

    String timing = t.toString();
    logger.info("Job Scheduled for Action {} to execute at time {}", actionType, timing);
  }

  public static CommandLineRunner invokeScheduler(
      Integer launchDetailsId, EcrActionTypes actionType, Instant t, String taskInstanceId) {
    logger.info("Task Instance Id:{}", taskInstanceId);

    Boolean timerAlreadyExists = false;
    CommandLineRunner task = null;

    if (staticSchedulerService != null) {

      List<ScheduledTasks> tasks =
          staticSchedulerService.getScheduledTasks(
              actionType.toString(), String.valueOf(launchDetailsId));

      if (tasks != null && tasks.size() > 1) {
        logger.info(
            " {} Timer already exists for launch {}, so do not create new one ",
            actionType,
            launchDetailsId);
        timerAlreadyExists = true;
      } else {
        logger.info(
            " {} Timer does not exists for launch {}, hence will be creating new ",
            actionType,
            launchDetailsId);
      }
    }

    if (Boolean.FALSE.equals(timerAlreadyExists)) {

      logger.info("Scheduling {} task to execute at {}", actionType, t);

      task = ignored -> logger.info("Scheduling one time task to after!");
      staticScheduler.schedule(
          staticTaskConfiguration
              .sampleOneTimeTask()
              .instance(
                  actionType.toString()
                      + "_"
                      + launchDetailsId
                      + "_"
                      + java.util.UUID.randomUUID().toString(),
                  new TaskTimer(100L, launchDetailsId, actionType, t, MDC.getCopyOfContextMap())),
          t);

      logger.debug("task  ::: {}", task);
    }

    return task;
  }

  public static Boolean checkIfTasksExists(List<ScheduledTasks> tasks, String taskInstanceId) {

    int numOfTasksExisting = 0;
    if (tasks != null && !tasks.isEmpty()) {

      for (ScheduledTasks t : tasks) {

        if (t.getTask_instance().equals(taskInstanceId)) {
          logger.info(" Found a Task with the same task Instance Id {}", taskInstanceId);
          numOfTasksExisting++;
        } else {

          logger.info(
              " Task with Instance Id {} does not match passed in taskInstanceId {}",
              t.getTask_instance(),
              taskInstanceId);
        }
      }
    } else {
      logger.info(" No tasks exist, so return false ");
    }

    return (numOfTasksExisting > 1);
  }

  public static void cancelAllScheduledTasksForLaunch(
      LaunchDetails launchDetails, Boolean deleteLaunchDetails) {
    logger.info("Cancelling the scheduled tasks for launch_id {}", launchDetails.getId());
    List<ScheduledTasks> tasks =
        staticSchedulerService.getScheduledTasks("", String.valueOf(launchDetails.getId()));
    for (ScheduledTasks scheduledTask : tasks) {
      TaskInstance taskInstance = new TaskInstance("EICRTask", scheduledTask.getTask_instance());
      if (Boolean.FALSE.equals(isCurrentExecutionTask(taskInstance))) {
        logger.info("Cancelling scheduled task {}", taskInstance.getId());
        staticScheduler.cancel(taskInstance);
      }
    }

    if (Boolean.TRUE.equals(deleteLaunchDetails)) {
      workflowInstance.launchService.delete(launchDetails);
      String expMsg =
          "Deleted the launch_detail " + launchDetails.getId() + " as encounter was not found";
      logger.info(expMsg);
      throw new ObjectDeletedException(expMsg, launchDetails.getId(), "launch_details");
    }
  }

  public static boolean isCurrentExecutionTask(TaskInstanceId taskInstanceId) {

    Boolean currentTask = false;
    List<CurrentlyExecuting> currentExecutions = staticScheduler.getCurrentlyExecuting();
    for (CurrentlyExecuting currentExecution : currentExecutions) {
      if (currentExecution.getTaskInstance().getId().equals(taskInstanceId.getId())) {
        currentTask = true;
        break;
      }
    }
    return currentTask;
  }
}
