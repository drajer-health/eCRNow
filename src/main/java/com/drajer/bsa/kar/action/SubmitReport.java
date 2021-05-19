package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.KarProcessingData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SubmitReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(SubmitReport.class);

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    logger.info(" Executing the submission of the Report");

    return null;
  }
}
