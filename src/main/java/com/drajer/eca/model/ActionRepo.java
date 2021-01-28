package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.ecrapp.service.EicrRRService;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.routing.RestApiSender;
import com.drajer.routing.impl.DirectEicrSender;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Service;

@Service
public class ActionRepo {

  private static ActionRepo instance;

  private Map<EcrActionTypes, Set<AbstractAction>> actions;

  private Map<TriggerType, Set<AbstractAction>> actionsByTriggers;

  // These are other services that the action classes use and we are storing it once for all of them
  // instead
  // of using it class variables which can be injected.
  // We could do injection if we do not use NEW operator , but the ERSD processor will use new to
  // create intead of
  // Spring context, hence Autowired variables cannot be injected into this class or the action
  // classes.

  TriggerQueryService triggerQueryService;

  LoadingQueryService loadingQueryService;

  LaunchService launchService;

  WorkflowService workflowService;

  ThreadPoolTaskScheduler taskScheduler;

  EicrRRService eicrRRService;

  DirectEicrSender directTransport;

  RestApiSender restTransport;

  String schematronFileLocation;

  String logFileDirectory;

  String xsdSchemasLocation;

  String rctcOid;

  String rctcVersion;

  private final Logger logger = LoggerFactory.getLogger(ActionRepo.class);

  public static ActionRepo getInstance() {
    if (instance == null) {
      instance = new ActionRepo();
    }
    return instance;
  }

  public String getRctcOid() {
    return rctcOid;
  }

  public void setRctcOid(String rctcOid) {
    this.rctcOid = rctcOid;
  }

  public String getRctcVersion() {
    return rctcVersion;
  }

  public void setRctcVersion(String rctcVersion) {
    this.rctcVersion = rctcVersion;
  }

  public String getLogFileDirectory() {
    return logFileDirectory;
  }

  public void setLogFileDirectory(String logFileLocation) {

    File f = new File(logFileLocation);

    if (f.getParentFile().isDirectory()) {
      logFileDirectory = f.getParentFile().getAbsolutePath();
    }
  }

  public DirectEicrSender getDirectTransport() {
    return directTransport;
  }

  public void setDirectTransport(DirectEicrSender directTransport) {
    this.directTransport = directTransport;
  }

  public RestApiSender getRestTransport() {
    return restTransport;
  }

  public void setRestTransport(RestApiSender restTransport) {
    this.restTransport = restTransport;
  }

  public String getSchematronFileLocation() {
    return schematronFileLocation;
  }

  public void setSchematronFileLocation(String schematronFileLocation) {
    this.schematronFileLocation = schematronFileLocation;
  }

  public String getXsdSchemasLocation() {
    return xsdSchemasLocation;
  }

  public void setXsdSchemasLocation(String xsdSchemasLocation) {
    this.xsdSchemasLocation = xsdSchemasLocation;
  }

  public EicrRRService getEicrRRService() {
    return eicrRRService;
  }

  public void setEicrRRService(EicrRRService eicrRRService) {
    this.eicrRRService = eicrRRService;
  }

  public ThreadPoolTaskScheduler getTaskScheduler() {
    return taskScheduler;
  }

  public void setTaskScheduler(ThreadPoolTaskScheduler taskScheduler) {
    this.taskScheduler = taskScheduler;
  }

  public LaunchService getLaunchService() {
    return launchService;
  }

  public void setLaunchService(LaunchService launchService) {
    this.launchService = launchService;
  }

  public WorkflowService getWorkflowService() {
    return workflowService;
  }

  public void setWorkflowService(WorkflowService workflowService) {
    this.workflowService = workflowService;
  }

  public Map<TriggerType, Set<AbstractAction>> getActionsByTriggers() {
    return actionsByTriggers;
  }

  public void setActionsByTriggers(Map<TriggerType, Set<AbstractAction>> actionsByTriggers) {
    this.actionsByTriggers = actionsByTriggers;
  }

  public TriggerQueryService getTriggerQueryService() {
    return triggerQueryService;
  }

  public void setTriggerQueryService(TriggerQueryService triggerQueryService) {
    this.triggerQueryService = triggerQueryService;
  }

  public LoadingQueryService getLoadingQueryService() {
    return loadingQueryService;
  }

  public void setLoadingQueryService(LoadingQueryService loadingQueryService) {
    this.loadingQueryService = loadingQueryService;
  }

  public Map<EcrActionTypes, Set<AbstractAction>> getActions() {
    return actions;
  }

  public void setActions(Map<EcrActionTypes, Set<AbstractAction>> actions) {
    this.actions = actions;
  }

  public void setupTriggerBasedActions() {

    if (actions != null) {

      for (Map.Entry<EcrActionTypes, Set<AbstractAction>> ent : actions.entrySet()) {

        Set<AbstractAction> aa = ent.getValue();

        if (aa != null) {

          for (AbstractAction a : aa) {

            // if Trigger is populated then we can add it.
            List<ActionData> td = a.getTriggerData();

            if (td != null && !td.isEmpty()) {

              if (actionsByTriggers == null) actionsByTriggers = new HashMap<>();

              for (ActionData ad : td) {

                if (actionsByTriggers.containsKey(ad.getTriggerType())) {

                  actionsByTriggers.get(ad.getTriggerType()).add(a);

                } else {
                  Set<AbstractAction> la = new HashSet<>();
                  la.add(a);

                  actionsByTriggers.put(ad.getTriggerType(), la);
                }
              }
            }

            // Add for Timing data
            // if Trigger is populated then we can add it.
            List<TimingSchedule> ts = a.getTimingData();

            if (ts != null && !ts.isEmpty()) {

              if (actionsByTriggers == null) actionsByTriggers = new HashMap<>();

              for (TimingSchedule tsd : ts) {

                if (actionsByTriggers.containsKey(tsd.getTriggerType())) {

                  actionsByTriggers.get(tsd.getTriggerType()).add(a);

                } else {
                  Set<AbstractAction> la = new HashSet<>();
                  la.add(a);

                  actionsByTriggers.put(tsd.getTriggerType(), la);
                }
              }
            }
          }
        }
      }
    }
  }

  private ActionRepo() {}

  public void print() {

    if (logger.isInfoEnabled()) {
      logger.info(" ***** Printing ACTION Repository ***** " + "\n");

      logger.info(" *************** Printing EicrTypes Repository **************** " + "\n");

      if (actions != null) {

        for (Map.Entry<EcrActionTypes, Set<AbstractAction>> ent : actions.entrySet()) {

          logger.info(" Printing Eicr Action Type : {}", ent.getKey().toString());

          Set<AbstractAction> aa = ent.getValue();

          if (aa != null) {

            for (AbstractAction a : aa) {

              logger.info(" Action that will be executed {}", a.toString());
            }
          }
        }
      }

      logger.info(" *************** End Printing EicrTypes Repository **************** " + "\n");

      logger.info(
          " *************** Start Printing Trigger Types Repository **************** " + "\n");

      if (actionsByTriggers != null) {

        for (Map.Entry<TriggerType, Set<AbstractAction>> ent : actionsByTriggers.entrySet()) {

          logger.info(" Printing Trigger for Action {}", ent.getKey().toString());

          Set<AbstractAction> aa = ent.getValue();

          if (aa != null) {

            for (AbstractAction a : aa) {

              logger.info(" Action that will be executed {}", a.toString());
            }
          }
        }
      }

      logger.info(
          " *************** End Printing Trigger Types Repository **************** " + "\n");

      logger.info(" ***** End Printing ACTION Repository ***** " + "\n");
    }
  }
}
