package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.FhirPathProcessor;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;

public class EvaluateCondition extends BsaAction {

  private FhirPathProcessor fhirPathProcessor;

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrservice) {
    // TODO Auto-generated method stub
    return null;
  }
}
