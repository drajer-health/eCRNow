package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaCondition;
import java.util.List;
import java.util.function.Supplier;

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

  public Supplier<ExpressionEvaluator> getExpressionEvaluatorSupplier() {
    return ((FhirPathProcessor) this.getConditionProcessor()).getExpressionEvaluatorSupplier();
  }

  public void setExpressionEvaluatorSupplier(Supplier<ExpressionEvaluator> expressionEvaluator) {
    ((FhirPathProcessor) this.getConditionProcessor()).setExpressionEvaluatorSupplier(expressionEvaluator);
  }
}
