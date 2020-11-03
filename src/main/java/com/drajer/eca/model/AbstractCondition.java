package com.drajer.eca.model;

import org.hl7.fhir.r4.model.PlanDefinition.ActionConditionKind;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractCondition {

  private final Logger logger = LoggerFactory.getLogger(AbstractCondition.class);

  public abstract Boolean evaluate(Object obj);

  private ActionConditionKind conditionType;

  public ActionConditionKind getConditionType() {
    return conditionType;
  }

  public void setConditionType(ActionConditionKind conditionType) {
    this.conditionType = conditionType;
  }

  public abstract void print();

  public void printBase() {

    if (conditionType != null && logger.isInfoEnabled())
      logger.info(" Condition Type = {}", conditionType.toString());
  }
}
