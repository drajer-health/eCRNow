package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.BsaFhirPathCondition;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.BsaTypes.NotificationProcessingStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.test.util.TestUtils;
import java.util.Collections;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.Parameters;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.opencds.cqf.fhir.cql.EvaluationSettings;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.opencds.cqf.fhir.utility.repository.InMemoryFhirRepository;

public class TerminateReportingTest {

  private static final String ACTION_ID = "terminate-reporting";
  private static final String CONDITION = "%Encounter.status = 'in-progress'";

  private EhrQueryService ehrService;
  private R4CqlExecutionService expressionEngine;
  private NotificationContext notificationContext;
  private Encounter encounter;
  private KarProcessingData data;
  private TerminateReporting action;

  @Before
  public void setUp() {
    ehrService = mock(EhrQueryService.class);
    expressionEngine = mock(R4CqlExecutionService.class);

    notificationContext =
        (NotificationContext)
            TestUtils.getResourceAsObject(
                "Bsa/NotificationContext/NotificationContext.json", NotificationContext.class);
    notificationContext.setNotificationProcessingStatus(
        NotificationProcessingStatusType.IN_PROGRESS.toString());
    notificationContext.setEncounterEndTime(null);

    encounter =
        TestUtils.loadResourceDataFromFile(Encounter.class, "R4/Encounter/Encounter_97953900.json");

    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("ecr-kar");

    data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    data.setExecutionSequenceId("exec-1");
    data.addResourceById("Encounter", encounter);

    DataRequirement encounterRequirement = new DataRequirement();
    encounterRequirement.setId("Encounter");
    encounterRequirement.setType("Encounter");

    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    condition.setLogicExpression(
        new Expression().setLanguage("text/fhirpath").setExpression(CONDITION));
    ((FhirPathProcessor) condition.getConditionProcessor())
        .setExpressionEvaluatorFactory(() -> expressionEngine);

    action = new TerminateReporting();
    action.setActionId(ACTION_ID);
    action.setInputData(Collections.singletonList(encounterRequirement));
    action.getConditions().add(condition);
  }

  @Test
  public void testProcess_whenConditionHoldsCompletesActionAndSuspendsNotificationContext() {
    conditionEvaluatesTo(true);
    EvaluateCondition subAction = new EvaluateCondition();
    subAction.setActionId("sub-evaluate-condition");
    action.getSubActions().add(subAction);

    BsaActionStatus status = action.process(data, ehrService);

    assertEquals(ACTION_ID, status.getActionId());
    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertEquals(
        NotificationProcessingStatusType.SUSPENDED.toString(),
        notificationContext.getNotificationProcessingStatus());
    assertNotNull(
        "suspending the encounter must stamp its end time",
        notificationContext.getEncounterEndTime());
    assertEquals(
        "the sub action and TerminateReporting must both record a status",
        2,
        data.getActionStatus().get("exec-1").size());
    verify(ehrService).getFilteredData(data, action.getInputData());
  }

  /** The FHIRPath engine must be handed the real Encounter under the %Encounter variable. */
  @Test
  public void testProcess_passesTheRealEncounterToTheExpressionEngine() {
    conditionEvaluatesTo(true);

    action.process(data, ehrService);

    ArgumentCaptor<Parameters> parameters = ArgumentCaptor.forClass(Parameters.class);
    verify(expressionEngine)
        .evaluate(
            any(),
            eq(CONDITION),
            parameters.capture(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any(),
            any());
    assertSame(encounter, parameters.getValue().getParameter("%Encounter").getResource());
  }

  @Test
  public void testProcess_whenConditionFailsLeavesNotificationContextUntouched() {
    conditionEvaluatesTo(false);
    EvaluateCondition subAction = new EvaluateCondition();
    subAction.setActionId("sub-evaluate-condition");
    action.getSubActions().add(subAction);

    BsaActionStatus status = action.process(data, ehrService);

    assertEquals(BsaActionStatusType.IN_PROGRESS, status.getActionStatus());
    assertEquals(
        NotificationProcessingStatusType.IN_PROGRESS.toString(),
        notificationContext.getNotificationProcessingStatus());
    assertNull(notificationContext.getEncounterEndTime());
    assertEquals(
        "the sub action must not run when the condition is not met",
        1,
        data.getActionStatus().get("exec-1").size());
  }

  /** A timed action is scheduled for later, so its condition must not even be evaluated. */
  @Test
  public void testProcess_withTimingDataIsScheduledWithoutEvaluatingTheCondition() {
    action.setTimingData(Collections.singletonList(new TimingSchedule()));
    action.setIgnoreTimers(false);

    BsaActionStatus status = action.process(data, ehrService);

    assertEquals(BsaActionStatusType.SCHEDULED, status.getActionStatus());
    assertEquals(
        NotificationProcessingStatusType.IN_PROGRESS.toString(),
        notificationContext.getNotificationProcessingStatus());
    verify(expressionEngine, never())
        .evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any());
  }

  @Test
  public void testProcess_withDefaultQueriesExecutesThemInsteadOfFetchingByType() {
    conditionEvaluatesTo(true);
    FhirQueryFilter encounterQuery = new FhirQueryFilter();
    encounterQuery.setDataReqId("Encounter");
    encounterQuery.setQueryString("Encounter?_id=97953900");
    action.getInputDataRequirementQueries().put("Encounter", encounterQuery);

    BsaActionStatus status = action.process(data, ehrService);

    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());
    verify(ehrService).executeQuery(data, "Encounter", encounterQuery);
    verify(ehrService, never()).getFilteredData(eq(data), eq(action.getInputData()));
  }

  @Test
  public void testProcess_withRealFhirPathEngine_evaluatesConditionAgainstEncounterData() {
    R4CqlExecutionService realEngine =
        new R4CqlExecutionService(
            new InMemoryFhirRepository(FhirContext.forR4Cached()), EvaluationSettings.getDefault());
    BsaFhirPathCondition encounterExists = new BsaFhirPathCondition();
    encounterExists.setLogicExpression(
        new Expression().setLanguage("text/fhirpath").setExpression("%Encounter.exists()"));
    ((FhirPathProcessor) encounterExists.getConditionProcessor())
        .setExpressionEvaluatorFactory(() -> realEngine);
    action.getConditions().clear();
    action.getConditions().add(encounterExists);

    BsaActionStatus withEncounter = action.process(data, ehrService);

    assertEquals(BsaActionStatusType.COMPLETED, withEncounter.getActionStatus());
    assertEquals(
        NotificationProcessingStatusType.SUSPENDED.toString(),
        notificationContext.getNotificationProcessingStatus());

    NotificationContext untouchedContext =
        (NotificationContext)
            TestUtils.getResourceAsObject(
                "Bsa/NotificationContext/NotificationContext.json", NotificationContext.class);
    untouchedContext.setNotificationProcessingStatus(
        NotificationProcessingStatusType.IN_PROGRESS.toString());
    KarProcessingData withoutEncounter = new KarProcessingData();
    withoutEncounter.setKar(data.getKar());
    withoutEncounter.setNotificationContext(untouchedContext);
    withoutEncounter.setExecutionSequenceId("exec-2");

    BsaActionStatus missingEncounter = action.process(withoutEncounter, ehrService);

    assertEquals(BsaActionStatusType.IN_PROGRESS, missingEncounter.getActionStatus());
    assertEquals(
        NotificationProcessingStatusType.IN_PROGRESS.toString(),
        untouchedContext.getNotificationProcessingStatus());
  }

  private void conditionEvaluatesTo(boolean outcome) {
    Parameters result = new Parameters();
    result.addParameter().setName(FhirPathProcessor.PARAM).setValue(new BooleanType(outcome));
    when(expressionEngine.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(result);
  }
}
