package com.drajer.eca.model;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.ecrapp.config.AppConfig;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.routing.RestApiSender;
import com.drajer.routing.impl.DirectEicrSender;
import com.drajer.routing.impl.DirectResponseReceiver;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.drajer.sof.utils.FhirContextInitializer;
import java.io.File;
import java.util.*;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.junit.Before;
import org.junit.Test;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

public class ActionRepoTest {

  private ActionRepo repo;

  @Before
  public void setup() {
    repo = ActionRepo.getInstance();
  }

  @Test
  public void testSingleton() {
    ActionRepo another = ActionRepo.getInstance();
    assertSame(repo, another);
  }

  @Test
  public void testSetupTriggerBasedActionsWithEmptyData() {
    repo.setActions(null);
    repo.setupTriggerBasedActions();
    repo.setActions(new HashMap<>());
    repo.setupTriggerBasedActions();
  }

  @Test
  public void testSetupTriggerBasedActionsWithData() {
    AbstractAction action = mock(AbstractAction.class);
    ActionData ad = mock(ActionData.class);
    when(ad.getTriggerType()).thenReturn(TriggerType.DATAACCESSED);
    List<ActionData> td = new ArrayList<>();
    td.add(ad);
    when(action.getTriggerData()).thenReturn(td);
    when(action.getTimingData()).thenReturn(Collections.emptyList());
    Set<AbstractAction> set = new HashSet<>();
    set.add(action);
    Map<EcrActionTypes, Set<AbstractAction>> actionsMap = new HashMap<>();
    actionsMap.put(EcrActionTypes.MATCH_TRIGGER, set);
    repo.setActions(actionsMap);
    repo.setupTriggerBasedActions();
    Map<TriggerType, Set<AbstractAction>> byTriggers = repo.getActionsByTriggers();
    assertNotNull(byTriggers);
    assertTrue(byTriggers.containsKey(TriggerType.DATAACCESSED));
    assertTrue(byTriggers.get(TriggerType.DATAACCESSED).contains(action));
  }

  @Test
  public void testPrintRunsWithoutError() {
    repo.print();
  }

  @Test
  public void testDirectAndRestTransport() {
    DirectEicrSender direct = new DirectEicrSender();
    RestApiSender rest = new RestApiSender();
    repo.setDirectTransport(direct);
    repo.setRestTransport(rest);
    assertSame(direct, repo.getDirectTransport());
    assertSame(rest, repo.getRestTransport());
  }

  @Test
  public void testRctcOidGetterSetter() {
    String oid = "oid-123";
    repo.setRctcOid(oid);
    assertEquals(oid, repo.getRctcOid());
  }

  @Test
  public void testRctcVersionGetterSetter() {
    String version = "v1.0";
    repo.setRctcVersion(version);
    assertEquals(version, repo.getRctcVersion());
  }

  @Test
  public void testSetLogFileDirectoryWithValidPath() {
    File tempFile = new File(System.getProperty("java.io.tmpdir"), "test.log");
    repo.setLogFileDirectory(tempFile.getAbsolutePath());

    assertEquals(tempFile.getParentFile().getAbsolutePath(), repo.getLogFileDirectory());
  }

  @Test
  public void testSchematronFileLocation() {
    repo.setSchematronFileLocation("/tmp/schematron.sch");
    assertEquals("/tmp/schematron.sch", repo.getSchematronFileLocation());
  }

  @Test
  public void testXsdSchemasLocation() {
    repo.setXsdSchemasLocation("/tmp/xsd/");
    assertEquals("/tmp/xsd/", repo.getXsdSchemasLocation());
  }

  @Test
  public void testWorkflowService() {
    WorkflowService workflowService = new WorkflowService();
    repo.setWorkflowService(workflowService);
    assertEquals(workflowService, repo.getWorkflowService());
  }

  @Test
  public void testTaskScheduler() {
    ThreadPoolTaskScheduler scheduler = new ThreadPoolTaskScheduler();
    repo.setTaskScheduler(scheduler);
    assertEquals(scheduler, repo.getTaskScheduler());
  }

  @Test
  public void testLaunchService() {
    LaunchService launchService = mock(LaunchService.class);
    repo.setLaunchService(launchService);
    assertEquals(launchService, repo.getLaunchService());
  }

  @Test
  public void testClientDetailsService() {
    ClientDetailsService clientService = mock(ClientDetailsService.class);
    repo.setClientDetailsService(clientService);
    assertEquals(clientService, repo.getClientDetailsService());
  }

  @Test
  public void testSetActionsByTriggers() {
    Map<TriggerType, Set<AbstractAction>> map = new HashMap<>();
    repo.setActionsByTriggers(map);
    assertEquals(map, repo.getActionsByTriggers());
  }

  @Test
  public void testTriggerQueryService() {
    TriggerQueryService triggerService = mock(TriggerQueryService.class);
    repo.setTriggerQueryService(triggerService);
    assertEquals(triggerService, repo.getTriggerQueryService());
  }

  @Test
  public void testLoadingQueryService() {
    LoadingQueryService loadingService = mock(LoadingQueryService.class);
    repo.setLoadingQueryService(loadingService);
    assertEquals(loadingService, repo.getLoadingQueryService());
  }

  @Test
  public void testSetActions() {
    Map<EcrActionTypes, Set<AbstractAction>> actionsMap = new HashMap<>();
    Set<AbstractAction> set = new HashSet<>();
    actionsMap.put(EcrActionTypes.CREATE_EICR, set);

    repo.setActions(actionsMap);
    assertEquals(actionsMap, repo.getActions());
  }

  @Test
  public void testAppConfig() {
    AppConfig appConfig = mock(AppConfig.class);
    repo.setAppConfig(appConfig);
    assertEquals(appConfig, repo.getAppConfig());
  }

  @Test
  public void testFhirContextInitializer() {
    FhirContextInitializer fhirContext = mock(FhirContextInitializer.class);
    repo.setFhirContextInitializer(fhirContext);
    assertEquals(fhirContext, repo.getFhirContextInitializer());
  }

  @Test
  public void testDirectResponseReceiver() {
    DirectResponseReceiver receiver = mock(DirectResponseReceiver.class);
    repo.setDirectReceiver(receiver);
    assertEquals(receiver, repo.getDirectReceiver());
  }

  @Test
  public void testSetupTriggerBasedActions_withTimingSchedule() {

    ActionRepo repo = ActionRepo.getInstance();
    repo.setActions(null);
    repo.setActionsByTriggers(null);

    AbstractAction action =
        new AbstractAction() {
          @Override
          public void execute(
              Object obj, EventTypes.WorkflowEvent launchType, String taskInstanceId) {}

          @Override
          public void print() {}
        };

    TimingSchedule ts =
        new TimingSchedule() {
          @Override
          public void print() {}

          @Override
          public TriggerType getTriggerType() {
            return TriggerType.DATACHANGED;
          }
        };

    action.addTimingData(ts);

    Map<EcrActionTypes, Set<AbstractAction>> actionsMap = new HashMap<>();
    Set<AbstractAction> actionSet = new HashSet<>();
    actionSet.add(action);
    actionsMap.put(EcrActionTypes.PERIODIC_UPDATE_EICR, actionSet);

    repo.setActions(actionsMap);
    repo.setupTriggerBasedActions();
    assertNotNull(repo.getActionsByTriggers());
    assertTrue(repo.getActionsByTriggers().containsKey(TriggerType.DATACHANGED));
    assertTrue(repo.getActionsByTriggers().get(TriggerType.DATACHANGED).contains(action));
  }

  @Test
  public void testPrintRunsWithoutErrosr() {
    ActionRepo repo = ActionRepo.getInstance();
    AbstractAction action =
        new AbstractAction() {
          @Override
          public void execute(
              Object obj, EventTypes.WorkflowEvent launchType, String taskInstanceId) {}

          @Override
          public void print() {}
        };
    Set<AbstractAction> actionSet = new HashSet<>();
    actionSet.add(action);
    Map<EcrActionTypes, Set<AbstractAction>> actionsMap = new HashMap<>();
    actionsMap.put(EcrActionTypes.CREATE_EICR, actionSet);
    repo.setActions(actionsMap);
    Set<AbstractAction> triggerSet = new HashSet<>();
    triggerSet.add(action);
    Map<TriggerType, Set<AbstractAction>> triggerMap = new HashMap<>();
    triggerMap.put(TriggerType.DATAACCESSED, triggerSet);
    repo.setActionsByTriggers(triggerMap);
    repo.print();

    assertNotNull(repo.getActions());
    assertTrue(repo.getActions().get(EcrActionTypes.CREATE_EICR).contains(action));

    assertNotNull(repo.getActionsByTriggers());
    assertTrue(repo.getActionsByTriggers().get(TriggerType.DATAACCESSED).contains(action));
  }
}
