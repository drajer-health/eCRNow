package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.BsaFhirPathCondition;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.test.util.TestUtils;
import java.util.Collections;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Expression;
import org.junit.Before;
import org.junit.Test;
import org.opencds.cqf.fhir.cql.EvaluationSettings;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.opencds.cqf.fhir.utility.repository.InMemoryFhirRepository;

public class ExecuteReportingActionsTest {

  private static final String ACTION_ID = "execute-reporting-actions";

  private EhrQueryService ehrService;
  private KnowledgeArtifact kar;
  private KarProcessingData data;
  private ExecuteReportingActions action;

  @Before
  public void setUp() {
    ehrService = mock(EhrQueryService.class);

    kar = new KnowledgeArtifact();
    kar.setKarId("ecr-kar");

    data = new KarProcessingData();
    data.setKar(kar);
    data.setExecutionSequenceId("exec-1");
    data.addResourceById(
        "Encounter",
        TestUtils.loadResourceDataFromFile(
            Encounter.class, "R4/Encounter/Encounter_97953900.json"));

    R4CqlExecutionService engine =
        new R4CqlExecutionService(
            new InMemoryFhirRepository(FhirContext.forR4Cached()), EvaluationSettings.getDefault());
    BsaFhirPathCondition encounterExists = new BsaFhirPathCondition();
    encounterExists.setLogicExpression(
        new Expression().setLanguage("text/fhirpath").setExpression("%Encounter.exists()"));
    ((FhirPathProcessor) encounterExists.getConditionProcessor())
        .setExpressionEvaluatorFactory(() -> engine);

    DataRequirement encounterRequirement = new DataRequirement();
    encounterRequirement.setId("Encounter");
    encounterRequirement.setType("Encounter");

    action = new ExecuteReportingActions();
    action.setActionId(ACTION_ID);
    action.setInputData(Collections.singletonList(encounterRequirement));
    action.getConditions().add(encounterExists);
  }

  @Test
  public void testProcess_whenEncounterIsPresentCompletesAndRunsSubActions() {
    ExecuteReportingActions subAction = new ExecuteReportingActions();
    subAction.setActionId("sub-execute-reporting-actions");
    action.getSubActions().add(subAction);

    BsaActionStatus status = action.process(data, ehrService);

    assertEquals(ACTION_ID, status.getActionId());
    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertEquals(
        "the sub action and ExecuteReportingActions must both record a status",
        2,
        data.getActionStatus().get("exec-1").size());
    verify(ehrService).getFilteredData(data, action.getInputData());
  }

  @Test
  public void testProcess_whenEncounterIsMissingStaysInProgressAndSkipsSubActions() {
    ExecuteReportingActions subAction = new ExecuteReportingActions();
    subAction.setActionId("sub-execute-reporting-actions");
    action.getSubActions().add(subAction);

    KarProcessingData withoutEncounter = new KarProcessingData();
    withoutEncounter.setKar(kar);
    withoutEncounter.setExecutionSequenceId("exec-2");

    BsaActionStatus status = action.process(withoutEncounter, ehrService);

    assertEquals(BsaActionStatusType.IN_PROGRESS, status.getActionStatus());
    assertEquals(
        "the sub action must not run when the condition is not met",
        1,
        withoutEncounter.getActionStatus().get("exec-2").size());
  }

  @Test
  public void testProcess_withTimingDataIsScheduled() {
    action.setTimingData(Collections.singletonList(new TimingSchedule()));
    action.setIgnoreTimers(false);

    BsaActionStatus status = action.process(data, ehrService);

    assertEquals(BsaActionStatusType.SCHEDULED, status.getActionStatus());
  }
}
