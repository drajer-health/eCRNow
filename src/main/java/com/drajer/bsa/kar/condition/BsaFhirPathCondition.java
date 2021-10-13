package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaCondition;
import org.opencds.cqf.cql.evaluator.expression.ExpressionEvaluator;

/**
 * This class is used to identify Conditions of type FHIRPath.
 *
 * @author nbashyam
 */
public class BsaFhirPathCondition extends BsaCondition {

  public BsaFhirPathCondition() {

    setConditionProcessor(new FhirPathProcessor());
  }

  public ExpressionEvaluator getExpressionEvaluator() {
    return ((FhirPathProcessor) this.getConditionProcessor()).getExpressionEvaluator();
  }

  public void setExpressionEvaluator(ExpressionEvaluator expressionEvaluator) {
    ((FhirPathProcessor) this.getConditionProcessor()).setExpressionEvaluator(expressionEvaluator);
  }
}
