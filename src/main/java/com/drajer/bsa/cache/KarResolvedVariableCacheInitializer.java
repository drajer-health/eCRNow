package com.drajer.bsa.cache;

import com.drajer.bsa.kar.condition.BsaFhirPathCondition;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactRepositorySystem;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.Expression;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Type;
import org.opencds.cqf.fhir.cr.cpg.r4.R4CqlExecutionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

/**
 * Resolves and caches the STATIC PlanDefinition variables for every Knowledge Artifact that is
 * already loaded in memory by {@code KarParserImpl}, once application startup has completed.
 *
 * <p>This runs on a background thread after {@link ApplicationReadyEvent} so that it never delays
 * application startup, and it never reloads KARs from disk/DB - it only reads the KARs already held
 * by {@link KnowledgeArtifactRepositorySystem}.
 *
 * <p>Each KAR is processed completely independently: a failure resolving one KAR (or one variable)
 * is logged and skipped, and never prevents any other KAR (or variable) from being resolved and
 * cached.
 *
 * @author nbashyam
 */
@Component
public class KarResolvedVariableCacheInitializer {

  private static final Logger logger =
      LoggerFactory.getLogger(KarResolvedVariableCacheInitializer.class);

  private static final String RETURN_PARAM = "return";
  private static final String FHIR_PATH_LANGUAGE = "text/fhirpath";

  @Autowired KnowledgeArtifactRepositorySystem knowledgeArtifactRepositorySystem;

  @Autowired KarResolvedVariableCache karResolvedVariableCache;

  @Autowired
  @Qualifier("R4CqlExecutionEvaluator")
  ObjectProvider<R4CqlExecutionService> expressionEvaluators;

  @EventListener(ApplicationReadyEvent.class)
  @Async
  public void initializeCache() {

    logger.info(" Starting background resolution of STATIC KAR PlanDefinition variables ");

    Map<String, KnowledgeArtifact> artifacts = knowledgeArtifactRepositorySystem.getArtifacts();

    if (artifacts != null) {

      for (KnowledgeArtifact kar : artifacts.values()) {

        try {
          resolveAndCacheKar(kar);
        } catch (Exception e) {
          logger.error(
              " Unable to resolve STATIC plan variables for KAR {}, skipping this KAR ",
              kar != null ? kar.getVersionUniqueId() : "unknown",
              e);
        }
      }
    }

    karResolvedVariableCache.markInitialized();

    logger.info(
        " Completed background resolution of STATIC KAR PlanDefinition variables for {} KAR(s) ",
        artifacts != null ? artifacts.size() : 0);
  }

  private void resolveAndCacheKar(KnowledgeArtifact kar) {

    if (kar == null) {
      return;
    }

    // Deduplicate variables by name since the same variable can be present on multiple actions.
    Map<String, Expression> uniqueVariables = new LinkedHashMap<>();

    if (kar.getActionMap() != null) {

      for (BsaAction action : kar.getActionMap().values()) {

        if (action == null || action.getConditions() == null) {
          continue;
        }

        for (BsaCondition cond : action.getConditions()) {

          if (cond instanceof BsaFhirPathCondition fhirPathCondition) {

            List<Expression> variables = fhirPathCondition.getVariables();

            if (variables != null) {
              for (Expression exp : variables) {
                if (exp != null && exp.getName() != null) {
                  uniqueVariables.putIfAbsent(exp.getName(), exp);
                }
              }
            }
          }
        }
      }
    }

    Map<String, Type> resolvedValues = new HashMap<>();

    for (Expression exp : uniqueVariables.values()) {

      if (KarVariableClassifier.isContextVariable(exp)) {
        logger.debug(
            " Skipping context variable {} for KAR {}, resolved per-patient instead ",
            exp.getName(),
            kar.getVersionUniqueId());
        continue;
      }

      if (!exp.hasLanguage() || !FHIR_PATH_LANGUAGE.contentEquals(exp.getLanguage())) {
        continue;
      }

      try {
        Type value = resolveStaticVariable(exp);

        if (value != null) {
          resolvedValues.put(exp.getName(), value);
        }
      } catch (Exception e) {
        logger.error(
            " Error resolving STATIC plan variable {} for KAR {}, skipping this variable ",
            exp.getName(),
            kar.getVersionUniqueId(),
            e);
      }
    }

    // Store the completed set for this KAR only once all variables have been attempted, so no
    // other component ever observes a partially resolved entry for this KAR.
    karResolvedVariableCache.put(kar.getVersionUniqueId(), new ResolvedVariables(resolvedValues));

    logger.info(
        " Cached {} of {} unique STATIC plan variable(s) for KAR {} ",
        resolvedValues.size(),
        uniqueVariables.size(),
        kar.getVersionUniqueId());
  }

  private Type resolveStaticVariable(Expression exp) {

    R4CqlExecutionService evaluator = expressionEvaluators.getObject();

    Parameters result =
        (Parameters)
            evaluator.evaluate(
                null, exp.getExpression(), null, null, null, null, null, null, null, null, null);

    if (result == null || result.getParameter(RETURN_PARAM) == null) {
      logger.warn(
          " No value returned from evaluator for STATIC plan variable {} with expression {} ",
          exp.getName(),
          exp.getExpression());
      return null;
    }

    return result.getParameter(RETURN_PARAM).getValue();
  }
}
