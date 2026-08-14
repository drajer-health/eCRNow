package com.drajer.bsa.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.CreateReport;
import com.drajer.bsa.kar.condition.BsaFhirPathCondition;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import com.drajer.bsa.model.KarProcessingData;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.IntegerType;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Type;
import org.junit.Test;
import org.mockito.Mockito;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.springframework.beans.factory.ObjectProvider;

public class KarResolvedVariableCacheTest {

  private static final String FHIRPATH = "text/fhirpath";

  @Test
  public void putAndGet_ReturnsStoredValueForCorrectKar() {
    KarResolvedVariableCache cache = new KarResolvedVariableCache();

    Map<String, Type> vars = new HashMap<>();
    vars.put("customThreshold", new IntegerType(5));
    cache.put("karA|1.0", new ResolvedVariables(vars));

    Optional<ResolvedVariables> result = cache.get("karA|1.0");

    assertTrue(result.isPresent());
    assertTrue(result.get().hasVariable("customThreshold"));
    assertEquals(
        5, ((IntegerType) result.get().getVariable("customThreshold").get()).getValue().intValue());
  }

  @Test
  public void get_WithMissingKarId_ReturnsEmpty() {
    KarResolvedVariableCache cache = new KarResolvedVariableCache();

    Optional<ResolvedVariables> result = cache.get("does-not-exist|1.0");

    assertFalse(result.isPresent());
    assertEquals(1, cache.getMissCount());
  }

  @Test
  public void twoDifferentKars_HaveIndependentCachedValues() {
    KarResolvedVariableCache cache = new KarResolvedVariableCache();

    Map<String, Type> karAVars = new HashMap<>();
    karAVars.put("reportingPeriodDays", new IntegerType(30));
    cache.put("karA|1.0", new ResolvedVariables(karAVars));

    Map<String, Type> karBVars = new HashMap<>();
    karBVars.put("lookbackWindow", new IntegerType(90));
    cache.put("karB|2.0", new ResolvedVariables(karBVars));

    ResolvedVariables karA = cache.get("karA|1.0").orElseThrow();
    ResolvedVariables karB = cache.get("karB|2.0").orElseThrow();

    assertTrue(karA.hasVariable("reportingPeriodDays"));
    assertFalse(karA.hasVariable("lookbackWindow"));

    assertTrue(karB.hasVariable("lookbackWindow"));
    assertFalse(karB.hasVariable("reportingPeriodDays"));
  }

  @Test
  public void isInitialized_IsFalseUntilMarkInitializedIsCalled() {
    KarResolvedVariableCache cache = new KarResolvedVariableCache();

    assertFalse(cache.isInitialized());

    cache.markInitialized();

    assertTrue(cache.isInitialized());
  }

  @Test
  public void isContextVariable_MatchesKnownContextNamesAndContextMarker() {
    assertTrue(KarVariableClassifier.isContextDateVariable("encounterStartDate"));
    assertTrue(KarVariableClassifier.isContextDateVariable("encounterEndDate"));
    assertTrue(KarVariableClassifier.isContextDateVariable("lastReportSubmissionDate"));
    assertTrue(KarVariableClassifier.isContextCodeVariable("encounterClass"));

    Expression contextExpr = buildExpression("someVariable", "{{context.encounterId}}.exists()");
    assertTrue(KarVariableClassifier.isContextVariable(contextExpr));

    Expression staticExpr = buildExpression("customThreshold", "5");
    assertFalse(KarVariableClassifier.isContextVariable(staticExpr));
  }

  @Test
  public void concurrentPutAndGet_IsThreadSafe() throws InterruptedException {
    KarResolvedVariableCache cache = new KarResolvedVariableCache();
    int threadCount = 20;
    ExecutorService executor = Executors.newFixedThreadPool(threadCount);
    CountDownLatch latch = new CountDownLatch(threadCount);

    for (int i = 0; i < threadCount; i++) {
      final int idx = i;
      executor.submit(
          () -> {
            try {
              String key = "kar-" + idx + "|1.0";
              Map<String, Type> vars = new HashMap<>();
              vars.put("var" + idx, new StringType("value" + idx));
              cache.put(key, new ResolvedVariables(vars));
              cache.get(key);
              cache.get("kar-" + ((idx + 1) % threadCount) + "|1.0");
            } finally {
              latch.countDown();
            }
          });
    }

    assertTrue(latch.await(10, TimeUnit.SECONDS));
    executor.shutdown();

    assertEquals(threadCount, cache.size());

    for (int i = 0; i < threadCount; i++) {
      Optional<ResolvedVariables> result = cache.get("kar-" + i + "|1.0");
      assertTrue(result.isPresent());
      assertTrue(result.get().hasVariable("var" + i));
    }
  }

  @Test
  public void initializeCache_ResolvesStaticVariables_AndSkipsContextVariables() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("karX");
    kar.setKarVersion("1.0");

    BsaAction action = new CreateReport();
    action.setActionId("action1", "http://test/PlanDefinition/plan");

    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    Expression contextDateVar = buildExpression("encounterStartDate", "%encounterStartDate");
    Expression contextMarkerVar =
        buildExpression("customContextExpr", "{{context.encounterId}}.exists()");
    Expression staticVar = buildExpression("customThreshold", "5");
    condition.setVariables(List.of(contextDateVar, contextMarkerVar, staticVar));
    action.addCondition(condition);

    kar.addAction(action);

    KnowledgeArtifactRepositorySystem repoSystem = mock(KnowledgeArtifactRepositorySystem.class);
    HashMap<String, KnowledgeArtifact> artifacts = new HashMap<>();
    artifacts.put(kar.getVersionUniqueId(), kar);
    when(repoSystem.getArtifacts()).thenReturn(artifacts);

    R4CqlExecutionService evaluator = mock(R4CqlExecutionService.class);
    Parameters evaluatorResult = new Parameters();
    evaluatorResult.addParameter().setName("return").setValue(new IntegerType(5));
    Mockito.lenient()
        .when(
            evaluator.evaluate(
                any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(evaluatorResult);

    @SuppressWarnings("unchecked")
    ObjectProvider<R4CqlExecutionService> evaluatorProvider = mock(ObjectProvider.class);
    when(evaluatorProvider.getObject()).thenReturn(evaluator);

    KarResolvedVariableCacheInitializer initializer = new KarResolvedVariableCacheInitializer();
    initializer.knowledgeArtifactRepositorySystem = repoSystem;
    initializer.karResolvedVariableCache = new KarResolvedVariableCache();
    initializer.expressionEvaluators = evaluatorProvider;

    initializer.initializeCache();

    assertTrue(initializer.karResolvedVariableCache.isInitialized());

    ResolvedVariables resolved =
        initializer.karResolvedVariableCache.get(kar.getVersionUniqueId()).orElseThrow();

    assertFalse(resolved.hasVariable("encounterStartDate"));
    assertFalse(resolved.hasVariable("customContextExpr"));
    assertTrue(resolved.hasVariable("customThreshold"));
    assertEquals(
        5, ((IntegerType) resolved.getVariable("customThreshold").get()).getValue().intValue());
  }

  @Test
  public void initializeCache_OneKarFailure_DoesNotPreventOtherKarsFromBeingCached() {
    KnowledgeArtifact brokenKar = mock(KnowledgeArtifact.class);
    when(brokenKar.getVersionUniqueId()).thenReturn("karBroken|1.0");
    when(brokenKar.getActionMap()).thenThrow(new RuntimeException("Simulated KAR failure"));

    KnowledgeArtifact goodKar = new KnowledgeArtifact();
    goodKar.setKarId("karGood");
    goodKar.setKarVersion("2.0");

    BsaAction action = new CreateReport();
    action.setActionId("action1", "http://test/PlanDefinition/plan");

    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    Expression staticVar = buildExpression("customThreshold", "5");
    condition.setVariables(Collections.singletonList(staticVar));
    action.addCondition(condition);

    goodKar.addAction(action);

    KnowledgeArtifactRepositorySystem repoSystem = mock(KnowledgeArtifactRepositorySystem.class);
    HashMap<String, KnowledgeArtifact> artifacts = new HashMap<>();
    artifacts.put(brokenKar.getVersionUniqueId(), brokenKar);
    artifacts.put(goodKar.getVersionUniqueId(), goodKar);
    when(repoSystem.getArtifacts()).thenReturn(artifacts);

    R4CqlExecutionService evaluator = mock(R4CqlExecutionService.class);
    Parameters evaluatorResult = new Parameters();
    evaluatorResult.addParameter().setName("return").setValue(new IntegerType(5));
    Mockito.lenient()
        .when(
            evaluator.evaluate(
                any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(evaluatorResult);

    @SuppressWarnings("unchecked")
    ObjectProvider<R4CqlExecutionService> evaluatorProvider = mock(ObjectProvider.class);
    when(evaluatorProvider.getObject()).thenReturn(evaluator);

    KarResolvedVariableCacheInitializer initializer = new KarResolvedVariableCacheInitializer();
    initializer.knowledgeArtifactRepositorySystem = repoSystem;
    initializer.karResolvedVariableCache = new KarResolvedVariableCache();
    initializer.expressionEvaluators = evaluatorProvider;

    initializer.initializeCache();

    assertTrue(initializer.karResolvedVariableCache.isInitialized());
    assertFalse(initializer.karResolvedVariableCache.get("karBroken|1.0").isPresent());

    ResolvedVariables goodResolved =
        initializer.karResolvedVariableCache.get("karGood|2.0").orElseThrow();
    assertTrue(goodResolved.hasVariable("customThreshold"));
  }

  @Test
  public void resolveVariables_WithCacheMiss_FallsBackToInlineEvaluationAndRecordsFallback() {
    KarResolvedVariableCache cache = new KarResolvedVariableCache();

    FhirPathProcessor processor = new FhirPathProcessor();
    R4CqlExecutionService evaluator = mock(R4CqlExecutionService.class);
    processor.setExpressionEvaluatorFactory(() -> evaluator);
    processor.setKarVariableCache(cache);

    BsaFhirPathCondition condition = new BsaFhirPathCondition();
    Expression variable = buildExpression("customThreshold", "%customThreshold");
    condition.setVariables(Collections.singletonList(variable));

    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("karFallback");
    kar.setKarVersion("1.0");

    KarProcessingData kd = mock(KarProcessingData.class);
    Mockito.lenient().when(kd.getKar()).thenReturn(kar);

    EhrQueryService ehrService = mock(EhrQueryService.class);
    Mockito.lenient()
        .when(ehrService.substituteContextParams(any(), anyString(), any(Boolean.class)))
        .thenReturn("5");

    Parameters evaluatorResult = new Parameters();
    evaluatorResult.addParameter().setName(FhirPathProcessor.PARAM).setValue(new IntegerType(5));
    Mockito.lenient()
        .when(
            evaluator.evaluate(
                any(), anyString(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(evaluatorResult);

    Parameters params = new Parameters();
    BsaAction action = mock(BsaAction.class);
    Mockito.lenient().when(action.getActionId()).thenReturn("action-fallback");

    processor.resolveVariables(condition, params, kd, action, ehrService);

    assertEquals(1, params.getParameter().size());
    assertEquals("%customThreshold", params.getParameter().get(0).getName());
    assertTrue(params.getParameter().get(0).getValue() instanceof IntegerType);
    assertEquals(5, ((IntegerType) params.getParameter().get(0).getValue()).getValue().intValue());

    assertEquals(1, cache.getFallbackCount());
    assertEquals(1, cache.getMissCount());
  }

  private static Expression buildExpression(String name, String expression) {
    Expression exp = new Expression();
    exp.setName(name);
    exp.setLanguage(FHIRPATH);
    exp.setExpression(expression);
    return exp;
  }
}
