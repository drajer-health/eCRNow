package com.drajer.bsa.kar.condition;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.model.KarProcessingData;
import org.hl7.fhir.r4.model.Parameters;

/**
 * The class provides an interface for plugging in different types of condition evaluators.
 *
 * @author nbashyam
 */
public interface BsaConditionProcessor {

  public Boolean evaluateExpression(
      BsaCondition cond, BsaAction act, KarProcessingData kd, EhrQueryService ehrService);

  public Boolean evaluateExpression(
      BsaCondition cond, Parameters params, EhrQueryService ehrService);
}
