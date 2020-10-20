package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CQLExpressionCondition extends AbstractCondition {

  private String expression;

  private final Logger logger = LoggerFactory.getLogger(CQLExpressionCondition.class);

  @Override
  public Boolean evaluate(Object obj) {

    // In the future a CQL Engine can be plugged in here to evaluate CQL Expressions.

    return true;
  }

  public String getExpression() {
    return expression;
  }

  public void setExpression(String expression) {
    this.expression = expression;
  }

  @Override
  public void print() {

    logger.info(" **** Printing CQL Condition **** ");

    printBase();

    logger.info(" Expression = {}", expression);

    logger.info(" **** End Printing CQL Condition **** ");
  }
}
