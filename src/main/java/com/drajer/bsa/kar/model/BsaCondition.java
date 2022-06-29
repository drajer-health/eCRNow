package com.drajer.bsa.kar.model;

import com.drajer.bsa.kar.condition.BsaConditionProcessor;

import org.hl7.fhir.r4.model.Expression;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is used to represent the PlanDefinition Action Condition. The model has been
 * simplified for processing as compared to the FHIR Resource for the Action to avoid all the
 * nestings.
 *
 * @author nbashyam
 */
public abstract class BsaCondition {

  private final Logger logger = LoggerFactory.getLogger(BsaCondition.class);

  /**
   * The actual expression that needs to be evaluated. This is a FHIR Path Expression for MedMorph
   * currently.
   *
   * <p>The expression may have a CQL extension in MedMorph that could also be used in the future.
   */
  private Expression logicExpression;

  private BsaConditionProcessor conditionProcessor;

  public BsaCondition() {}

  public BsaConditionProcessor getConditionProcessor() {
    return conditionProcessor;
  }

  public void setConditionProcessor(BsaConditionProcessor conditionProcessor) {
    this.conditionProcessor = conditionProcessor;
  }

  public Expression getLogicExpression() {
    return logicExpression;
  }

  public void setLogicExpression(Expression logicExpression) {
    this.logicExpression = logicExpression;
  }

  public void log() {

    logger.info(" **** START Printing Expression **** ");

    if (logicExpression != null) {
      logger.info(" Expression Type : {}", logicExpression.getLanguage());
      logger.info(" Expression Logic : {}", logicExpression.getExpression());
    } else {
      logger.info(" Expression is not populated for this Condition. ");
    }

    logger.info(" **** END Printing Expression **** ");
  }
}
