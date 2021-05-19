package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValidateReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(ValidateReport.class);

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
    // TODO Auto-generated method stub

    logger.info(" Executing the Validation of the Report");
    return null;
  }
}
