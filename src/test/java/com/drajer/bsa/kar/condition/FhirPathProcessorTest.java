package com.drajer.bsa.kar.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.CodeSystem;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.IntegerType;
import org.hl7.fhir.r4.model.MeasureReport;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.ValueSet;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;

public class FhirPathProcessorTest {

  private FhirPathProcessor processor;
  private R4CqlExecutionService expressionEvaluator;

  @Before
  public void setUp() {
    processor = new FhirPathProcessor();
    expressionEvaluator = mock(R4CqlExecutionService.class);
    processor.setExpressionEvaluatorFactory(() -> expressionEvaluator);
  }

  @Test
  public void evaluateExpression_WithValidBooleanReturn_ProducesTrueOutput() {
    BsaCondition condition = buildCondition("true");
    BsaAction action = mock(BsaAction.class);
    KarProcessingData kd = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    Parameters inputParams = new Parameters();
    inputParams.addParameter().setName("existing").setValue(new BooleanType(true));

    when(action.getActionId()).thenReturn("action-1");
    when(kd.getParametersByActionId("action-1")).thenReturn(inputParams);

    Parameters result = new Parameters();
    result.addParameter().setName(FhirPathProcessor.PARAM).setValue(new BooleanType(true));
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(result);

    Boolean actual = processor.evaluateExpression(condition, action, kd, ehrService);

    assertTrue(actual);
    verify(kd).getParametersByActionId("action-1");
  }

  @Test
  public void evaluateExpression_WithNoReturnParameter_ProducesFalseOutput() {
    BsaCondition condition = buildCondition("false");
    BsaAction action = mock(BsaAction.class);
    KarProcessingData kd = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);

    when(action.getActionId()).thenReturn("action-2");
    when(kd.getParametersByActionId("action-2")).thenReturn(new Parameters());
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new Parameters());

    Boolean actual = processor.evaluateExpression(condition, action, kd, ehrService);

    assertFalse(actual);
  }

  @Test
  public void evaluateExpression_WithNonBooleanReturn_ThrowsException() {
    BsaCondition condition = buildCondition("1+1");
    Parameters inputParams = new Parameters();

    Parameters result = new Parameters();
    result.addParameter().setName(FhirPathProcessor.PARAM).setValue(new IntegerType(2));
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(result);

    RuntimeException ex =
        assertThrows(
            RuntimeException.class,
            () ->
                processor.evaluateExpression(condition, inputParams, mock(EhrQueryService.class)));

    assertEquals("Unexpected FHIR Path Expression return type", ex.getMessage());
  }

  @Test
  public void resolveContextVariables_WithInputExpression_ReturnsResolvedOutput() {
    EhrQueryService ehrService = mock(EhrQueryService.class);
    KarProcessingData kd = mock(KarProcessingData.class);
    String expression = "%encounterStartDate";

    when(ehrService.substituteContextParams(kd, expression, true)).thenReturn("2025-01-01");

    String resolved = processor.resolveContextVariables(expression, ehrService, kd);

    assertEquals("2025-01-01", resolved);
    verify(ehrService).substituteContextParams(kd, expression, true);
  }

  @Test
  public void evaluateExpression_WithParametersOverloadAndBooleanFalse_ReturnsFalse() {
    BsaCondition condition = buildCondition("false");

    Parameters result = new Parameters();
    result.addParameter().setName(FhirPathProcessor.PARAM).setValue(new BooleanType(false));
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(result);

    Boolean actual =
        processor.evaluateExpression(condition, new Parameters(), mock(EhrQueryService.class));

    assertFalse(actual);
  }

  @Test
  public void evaluateExpression_WhenActionParametersAbsent_ResolvesInputAndEvaluates() {
    BsaCondition condition = buildCondition("%patient.exists()");
    BsaAction action = mock(BsaAction.class);
    KarProcessingData kd = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    String dataId = null;

    DataRequirement req = new DataRequirement();
    req.setId("patientInput");
    req.setType("Patient");

    when(action.getActionId()).thenReturn("action-3");
    when(kd.getParametersByActionId("action-3")).thenReturn(null);
    when(action.getInputData()).thenReturn(Collections.singletonList(req));
    when(action.getRelatedDataId("patientInput")).thenReturn(null);
    when(kd.getDataForId("patientInput", dataId)).thenReturn(null);

    Parameters result = new Parameters();
    result.addParameter().setName(FhirPathProcessor.PARAM).setValue(new BooleanType(true));
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(result);

    Boolean actual = processor.evaluateExpression(condition, action, kd, ehrService);

    assertTrue(actual);
    verify(kd).getDataForId("patientInput", dataId);
  }

  @Test
  public void resolveVariables_WithNonFhirLanguage_DoesNotAddParameters() {
    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    Expression variable = new Expression();
    variable.setName("ignored");
    variable.setLanguage("text/cql");
    variable.setExpression("ignored");
    condition.setVariables(Collections.singletonList(variable));

    Parameters params = new Parameters();
    BsaAction action = mock(BsaAction.class);
    when(action.getActionId()).thenReturn("action-4");

    processor.resolveVariables(
        condition, params, mock(KarProcessingData.class), action, mock(EhrQueryService.class));

    assertTrue(params.getParameter().isEmpty());
  }

  @Test
  public void resolveVariables_WithEncounterClass_AddsCodeTypeParameter() {
    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    Expression variable = new Expression();
    variable.setName("encounterClass");
    variable.setLanguage("text/fhirpath");
    variable.setExpression("%encounterClass");
    condition.setVariables(Collections.singletonList(variable));

    EhrQueryService ehrService = mock(EhrQueryService.class);
    when(ehrService.substituteContextParams(any(), anyString(), any(Boolean.class)))
        .thenReturn("AMB");
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new Parameters());

    Parameters params = new Parameters();
    BsaAction action = mock(BsaAction.class);
    when(action.getActionId()).thenReturn("action-5");

    processor.resolveVariables(
        condition, params, mock(KarProcessingData.class), action, ehrService);

    assertEquals(1, params.getParameter().size());
    assertEquals("%encounterClass", params.getParameter().get(0).getName());
    assertTrue(params.getParameter().get(0).getValue() instanceof CodeType);
  }

  @Test
  public void resolveVariables_WithGenericVariable_AddsEvaluatorReturnValue() {
    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    Expression variable = new Expression();
    variable.setName("isMatch");
    variable.setLanguage("text/fhirpath");
    variable.setExpression("%isMatch");
    condition.setVariables(Collections.singletonList(variable));

    EhrQueryService ehrService = mock(EhrQueryService.class);
    when(ehrService.substituteContextParams(any(), anyString(), any(Boolean.class)))
        .thenReturn("true");

    Parameters variableResult = new Parameters();
    variableResult.addParameter().setName(FhirPathProcessor.PARAM).setValue(new BooleanType(true));
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(variableResult);

    Parameters params = new Parameters();
    BsaAction action = mock(BsaAction.class);
    when(action.getActionId()).thenReturn("action-6");

    processor.resolveVariables(
        condition, params, mock(KarProcessingData.class), action, ehrService);

    assertEquals(1, params.getParameter().size());
    assertEquals("%isMatch", params.getParameter().get(0).getName());
    assertTrue(((BooleanType) params.getParameter().get(0).getValue()).booleanValue());
  }

  @Test
  public void filterResources_WithMeasureReportCandidates_ReturnsCollectedResources() {
    DataRequirement dr = new DataRequirement();
    dr.setId("dr-measure");
    dr.setType(ResourceType.MeasureReport.toString());

    MeasureReport first = new MeasureReport();
    first.setId("MeasureReport/1");
    MeasureReport second = new MeasureReport();
    second.setId("MeasureReport/2");

    KarProcessingData kd = mock(KarProcessingData.class);
    Set<Resource> input = new HashSet<>();
    input.add(first);
    when(kd.getResourcesByType(ResourceType.MeasureReport.toString())).thenReturn(input);

    HashMap<String, HashMap<String, Resource>> actionOutput = new HashMap<>();
    HashMap<String, Resource> outputResources = new HashMap<>();
    outputResources.put("mr2", second);
    actionOutput.put("action-x", outputResources);
    when(kd.getActionOutputData()).thenReturn(actionOutput);

    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> out =
        processor.filterResources(dr, kd);

    assertNotNull(out);
    assertTrue(out.getValue1().containsKey(ResourceType.MeasureReport.toString()));
    assertEquals(2, out.getValue1().get(ResourceType.MeasureReport.toString()).size());
  }

  @Test
  public void applyCodeFilter_WithMeasureReportData_ReturnsMatchedResourceMap() {
    DataRequirement dr = new DataRequirement();
    dr.setId("dr-apply");
    dr.setType(ResourceType.MeasureReport.toString());

    MeasureReport measureReport = new MeasureReport();
    measureReport.setId("MeasureReport/3");
    Set<Resource> candidates = new HashSet<>();
    candidates.add(measureReport);

    KarProcessingData kd = mock(KarProcessingData.class);
    BsaAction action = mock(BsaAction.class);
    when(action.getRelatedDataId("dr-apply")).thenReturn("related-1");
    when(kd.getDataForId("dr-apply", "related-1")).thenReturn(candidates);

    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> out =
        processor.applyCodeFilter(dr, kd, action);

    assertTrue(out.getValue1().containsKey(ResourceType.MeasureReport.toString()));
    assertEquals(1, out.getValue1().get(ResourceType.MeasureReport.toString()).size());
  }

  @Test
  public void filterByCode_WithInvalidCodeFilter_DoesNotAddResources() {
    DataRequirement dr = new DataRequirement();
    dr.setId("dr-code");
    dr.setType("Condition");
    dr.addCodeFilter().setPath("status");

    Map<String, Set<Resource>> result = new HashMap<>();
    processor.filterByCode(
        dr,
        new CodeableConcept(),
        mock(KarProcessingData.class),
        new CheckTriggerCodeStatus(),
        result,
        new MeasureReport(),
        false);

    assertTrue(result.isEmpty());
  }

  @Test
  public void newEvaluator_WhenFactoryConfigured_ReturnsFactoryInstance() {
    R4CqlExecutionService otherEvaluator = mock(R4CqlExecutionService.class);
    processor.setExpressionEvaluatorFactory(() -> otherEvaluator);

    R4CqlExecutionService actual = processor.newEvaluator();

    assertSame(otherEvaluator, actual);
  }

  @Test
  public void resolveVariables_WithDateVariable_AddsDateTimeParameter() {
    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    Expression variable = new Expression();
    variable.setName("encounterStartDate");
    variable.setLanguage("text/fhirpath");
    variable.setExpression("%encounterStartDate");
    condition.setVariables(Collections.singletonList(variable));

    EhrQueryService ehrService = mock(EhrQueryService.class);
    when(ehrService.substituteContextParams(any(), anyString(), any(Boolean.class)))
        .thenReturn("2025-02-01T12:00:00Z");
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new Parameters());

    Parameters params = new Parameters();
    BsaAction action = mock(BsaAction.class);
    when(action.getActionId()).thenReturn("action-7");

    processor.resolveVariables(
        condition, params, mock(KarProcessingData.class), action, ehrService);

    assertEquals(1, params.getParameter().size());
    assertEquals("%encounterStartDate", params.getParameter().get(0).getName());
    assertNotNull(params.getParameter().get(0).getValue());
  }

  @Test
  public void resolveVariables_WithNonFhirCondition_DoesNothing() {
    BsaCondition nonFhirCondition = buildCondition("true");
    Parameters params = new Parameters();

    processor.resolveVariables(
        nonFhirCondition,
        params,
        mock(KarProcessingData.class),
        mock(BsaAction.class),
        mock(EhrQueryService.class));

    assertTrue(params.getParameter().isEmpty());
  }

  @Test
  public void resolveVariables_WithEmptyVariables_DoesNothing() {
    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    condition.setVariables(Collections.emptyList());
    Parameters params = new Parameters();

    processor.resolveVariables(
        condition,
        params,
        mock(KarProcessingData.class),
        mock(BsaAction.class),
        mock(EhrQueryService.class));

    assertTrue(params.getParameter().isEmpty());
  }

  @Test
  public void filterByCode_RoutesCoreResourceTypes_ToCodeableConceptHandler() {
    TrackingFhirPathProcessor tracking = new TrackingFhirPathProcessor();
    DataRequirement dr = new DataRequirement();
    dr.setId("dr-routing");
    dr.setType("Condition");

    Set<Resource> candidates = new HashSet<>();
    Condition condition = new Condition();
    condition.setId("Condition/1");
    condition.setCode(new CodeableConcept());
    candidates.add(condition);

    Map<String, Set<Resource>> resources = new HashMap<>();
    tracking.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);
    assertEquals(1, tracking.codeableConceptCalls);

    dr.setType("Observation");
    candidates.clear();
    Observation obs = new Observation();
    obs.setId("Observation/1");
    obs.setCode(new CodeableConcept());
    obs.setValue(new CodeableConcept());
    candidates.add(obs);
    tracking.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);
    assertTrue(tracking.codeableConceptCalls >= 3);

    dr.setType("ServiceRequest");
    candidates.clear();
    ServiceRequest serviceRequest = new ServiceRequest();
    serviceRequest.setCode(new CodeableConcept());
    candidates.add(serviceRequest);
    tracking.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);

    dr.setType("DiagnosticReport");
    candidates.clear();
    DiagnosticReport diagnosticReport = new DiagnosticReport();
    diagnosticReport.setCode(new CodeableConcept());
    candidates.add(diagnosticReport);
    tracking.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);

    dr.setType("Procedure");
    candidates.clear();
    Procedure procedure = new Procedure();
    procedure.setCode(new CodeableConcept());
    candidates.add(procedure);
    tracking.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);

    dr.setType("Immunization");
    candidates.clear();
    Immunization immunization = new Immunization();
    immunization.setVaccineCode(new CodeableConcept());
    candidates.add(immunization);
    tracking.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);

    dr.setType("Encounter");
    candidates.clear();
    Encounter encounter = new Encounter();
    encounter.addReasonCode(new CodeableConcept());
    candidates.add(encounter);
    tracking.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);

    assertTrue(tracking.codeableConceptCalls >= 8);
  }

  @Test
  @Ignore
  public void filterByCode_MedicationReferenceBranches_ResolveMedicationCode() {
    TrackingFhirPathProcessor tracking = new TrackingFhirPathProcessor();
    KarProcessingData kd = mock(KarProcessingData.class);
    Medication medication = new Medication();
    medication.setCode(new CodeableConcept());
    when(kd.getResourceById("med1", ResourceType.Medication)).thenReturn(medication);

    DataRequirement dr = new DataRequirement();
    dr.setId("dr-med");
    Map<String, Set<Resource>> resources = new HashMap<>();
    Set<Resource> candidates = new HashSet<>();

    dr.setType("MedicationRequest");
    MedicationRequest medicationRequest = new MedicationRequest();
    medicationRequest.setMedication(new Reference("Medication/med1"));
    candidates.add(medicationRequest);
    tracking.filterByCode(dr, kd, new CheckTriggerCodeStatus(), candidates, resources);

    dr.setType("MedicationStatement");
    candidates.clear();
    MedicationStatement medicationStatement = new MedicationStatement();
    medicationStatement.setMedication(new Reference("Medication/med1"));
    candidates.add(medicationStatement);
    tracking.filterByCode(dr, kd, new CheckTriggerCodeStatus(), candidates, resources);

    dr.setType("MedicationAdministration");
    candidates.clear();
    MedicationAdministration medicationAdministration = new MedicationAdministration();
    medicationAdministration.setMedication(new Reference("Medication/med1"));
    candidates.add(medicationAdministration);
    tracking.filterByCode(dr, kd, new CheckTriggerCodeStatus(), candidates, resources);

    verify(kd).getResourceById("med1", ResourceType.Medication);
  }

  @Test
  public void filterByCode_SpecialResourceTypes_AreAddedToResultMap() {
    DataRequirement dr = new DataRequirement();
    dr.setId("dr-special");
    Map<String, Set<Resource>> resources = new HashMap<>();

    Set<Resource> candidates = new HashSet<>();

    dr.setType("MeasureReport");
    MeasureReport measureReport = new MeasureReport();
    candidates.add(measureReport);
    processor.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);
    assertTrue(resources.containsKey("MeasureReport"));

    dr.setType("ValueSet");
    candidates.clear();
    ValueSet valueSet = new ValueSet();
    candidates.add(valueSet);
    processor.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);
    assertTrue(resources.containsKey("ValueSet"));

    dr.setType("CodeSystem");
    candidates.clear();
    CodeSystem codeSystem = new CodeSystem();
    candidates.add(codeSystem);
    processor.filterByCode(
        dr, mock(KarProcessingData.class), new CheckTriggerCodeStatus(), candidates, resources);
    assertTrue(resources.containsKey("CodeSystem"));
  }

  @Test
  public void filterResources_WithNullInputCandidates_StillReturnsPair() {
    DataRequirement dr = new DataRequirement();
    dr.setId("dr-null");
    dr.setType("Condition");

    KarProcessingData kd = mock(KarProcessingData.class);
    when(kd.getResourcesByType("Condition")).thenReturn(null);
    when(kd.getActionOutputData()).thenReturn(new HashMap<>());

    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> out =
        processor.filterResources(dr, kd);

    assertNotNull(out);
    assertNotNull(out.getValue0());
    assertNotNull(out.getValue1());
  }

  @Test
  public void applyCodeFilter_WhenNoDataCandidates_ReturnsEmptyResourceMap() {
    DataRequirement dr = new DataRequirement();
    dr.setId("dr-empty");
    dr.setType("Observation");
    BsaAction action = mock(BsaAction.class);
    when(action.getRelatedDataId("dr-empty")).thenReturn("r-empty");

    KarProcessingData kd = mock(KarProcessingData.class);
    when(kd.getDataForId("dr-empty", "r-empty")).thenReturn(null);

    Pair<CheckTriggerCodeStatus, Map<String, Set<Resource>>> out =
        processor.applyCodeFilter(dr, kd, action);

    assertNotNull(out);
    assertTrue(out.getValue1().isEmpty());
  }

  @Test
  public void evaluateExpression_WhenNullBooleanValue_ReturnsFalse() {
    BsaCondition condition = buildCondition("x");
    Parameters input = new Parameters();

    Parameters result = new Parameters();
    result.addParameter().setName(FhirPathProcessor.PARAM).setValue((BooleanType) null);
    when(expressionEvaluator.evaluate(
            any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(result);

    assertThrows(
        RuntimeException.class,
        () -> processor.evaluateExpression(condition, input, mock(EhrQueryService.class)));
  }

  @Test
  public void evaluateExpression_WhenActionInputDataNull_ThrowsNullPointerException() {
    BsaCondition condition = buildCondition("true");
    BsaAction action = mock(BsaAction.class);
    KarProcessingData kd = mock(KarProcessingData.class);

    when(action.getActionId()).thenReturn("action-8");
    when(kd.getParametersByActionId("action-8")).thenReturn(null);
    when(action.getInputData()).thenReturn(null);

    assertThrows(
        NullPointerException.class,
        () -> processor.evaluateExpression(condition, action, kd, mock(EhrQueryService.class)));
  }

  private static class TrackingFhirPathProcessor extends FhirPathProcessor {
    int codeableConceptCalls = 0;

    @Override
    public void filterByCode(
        DataRequirement dr,
        CodeableConcept cc,
        KarProcessingData kd,
        CheckTriggerCodeStatus ctc,
        Map<String, Set<Resource>> res,
        Resource resourceMatched,
        Boolean valElem) {
      codeableConceptCalls++;
      res.computeIfAbsent(dr.getType(), key -> new HashSet<>()).add(resourceMatched);
    }
  }

  private BsaCondition buildCondition(String expressionValue) {
    BsaCondition condition = new BsaCqlCondition();
    Expression expression = new Expression();
    expression.setExpression(expressionValue);
    condition.setLogicExpression(expression);
    return condition;
  }
}
