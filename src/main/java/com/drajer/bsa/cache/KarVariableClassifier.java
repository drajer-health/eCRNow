package com.drajer.bsa.cache;

import java.util.Set;
import org.hl7.fhir.r4.model.Expression;

/**
 * Classifies KAR PlanDefinition variables as either CONTEXT (patient/encounter specific, must never
 * be cached globally) or STATIC (same value for every patient, safe to resolve once per KAR and
 * reuse). Used by both {@link KarResolvedVariableCacheInitializer}, which decides what to resolve
 * and cache in the background, and {@link com.drajer.bsa.kar.condition.FhirPathProcessor}, which
 * decides how to resolve a variable at runtime.
 *
 * @author nbashyam
 */
public final class KarVariableClassifier {

  public static final String ENCOUNTER_START_DATE = "encounterStartDate";
  public static final String ENCOUNTER_END_DATE = "encounterEndDate";
  public static final String LAST_REPORT_SUBMISSION_DATE = "lastReportSubmissionDate";
  public static final String ENCOUNTER_CLASS = "encounterClass";

  private static final Set<String> CONTEXT_DATE_VARIABLE_NAMES =
      Set.of(ENCOUNTER_START_DATE, ENCOUNTER_END_DATE, LAST_REPORT_SUBMISSION_DATE);

  private static final String CONTEXT_EXPRESSION_MARKER = "{{context.";

  private KarVariableClassifier() {}

  /** Returns true when the variable name matches one of the context date variables. */
  public static boolean isContextDateVariable(String name) {
    return name != null && CONTEXT_DATE_VARIABLE_NAMES.contains(name);
  }

  /** Returns true when the variable name is the context encounter class variable. */
  public static boolean isContextCodeVariable(String name) {
    return ENCOUNTER_CLASS.equals(name);
  }

  /**
   * Returns true when the variable is patient/encounter specific and must never be resolved once
   * and shared across patients. This includes the known context date/code variables as well as any
   * expression that references the {{context.*}} substitution marker.
   */
  public static boolean isContextVariable(Expression exp) {

    if (exp == null) {
      return false;
    }

    if (isContextDateVariable(exp.getName()) || isContextCodeVariable(exp.getName())) {
      return true;
    }

    String expression = exp.getExpression();

    return expression != null && expression.contains(CONTEXT_EXPRESSION_MARKER);
  }
}
