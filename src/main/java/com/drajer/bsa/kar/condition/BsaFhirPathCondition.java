package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaCondition;
import java.util.List;
import org.hl7.fhir.r4.model.Expression;

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

  private List<Expression> variables;

  public List<Expression> getVariables() {
    return variables;
  }

  public void setVariables(List<Expression> variables) {
    this.variables = variables;
  }

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
