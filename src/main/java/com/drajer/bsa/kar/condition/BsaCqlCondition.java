package com.drajer.bsa.kar.condition;

import com.drajer.bsa.kar.model.BsaCondition;

/**
 * This class is used to identify Conditions with Cql Expressions.
 *
 * @author nbashyam
 */
public class BsaCqlCondition extends BsaCondition {

  public BsaCqlCondition() {

    setConditionProcessor(new CqlProcessor());
  }
}
