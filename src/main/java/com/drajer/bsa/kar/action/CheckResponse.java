package com.drajer.bsa.kar.action;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class CheckResponse extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(CheckResponse.class);

  PublicHealthMessagesDao phDao;

  DirectTransportImpl directReceiver;

  public PublicHealthMessagesDao getPhDao() {
    return phDao;
  }

  public void setPhDao(PublicHealthMessagesDao phDao) {
    this.phDao = phDao;
  }

  public DirectTransportImpl getDirectReceiver() {
    return directReceiver;
  }

  public void setDirectReceiver(DirectTransportImpl directReceiver) {
    this.directReceiver = directReceiver;
  }

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrservice) {

    logger.info(" Start Processing CheckResponse Action ");

    BsaActionStatus actStatus = new CheckResponseStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    if (status != BsaActionStatusType.SCHEDULED || Boolean.TRUE.equals(getIgnoreTimers())) {

      HealthcareSetting hs = data.getHealthcareSetting();

      if (hs != null && hs.getIsDirect()) {

        directReceiver.receiveRrDataUsingDirect(data);

      } else {

        logger.info(
            " Not a Direct Interface, so the response will be received via API, nothing to do here ");
      }
    }

    logger.info(" Finished Processing CheckResponse Action ");

    return null;
  }
}
