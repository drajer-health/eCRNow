package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaCondition;
import org.hl7.fhir.r4.model.Expression;
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

  protected Expression variableExpression;

  public Expression getVariableExpression() {
    return variableExpression;
  }

  public void setVariableExpression(Expression variableExpression) {
    this.variableExpression = variableExpression;
  }

  public ExpressionEvaluator getExpressionEvaluator() {
    return ((FhirPathProcessor) this.getConditionProcessor()).getExpressionEvaluator();
  }

  public void setExpressionEvaluator(ExpressionEvaluator expressionEvaluator) {
    ((FhirPathProcessor) this.getConditionProcessor()).setExpressionEvaluator(expressionEvaluator);
  }
}
