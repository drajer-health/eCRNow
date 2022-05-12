package com.drajer.bsa.routing.impl;

import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.routing.DataTransportInterface;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class RestfulTransportImpl implements DataTransportInterface {

  private static final Logger logger = LoggerFactory.getLogger(RestfulTransportImpl.class);

  @Override
  public void sendEicrDataUsingDirect(KarProcessingData data) {

    String error = " Direct Transport method invoked on Restful Transport which is not supported ";
    logger.error(error);
  }

  @Override
  public void receiveRrDataUsingDirect(KarProcessingData data) {

    String error = " Direct Transport method invoked on Restful Transport which is not supported ";
    logger.error(error);
  }

  @Override
  public void sendEicrDataUsingRestfulApi(KarProcessingData data) {}
}
