package com.drajer.sof.service.impl;

import com.drajer.sof.model.RRReceiver;
import com.drajer.sof.service.RRReceiverService;
import com.drajer.sof.utils.R4ResourcesData;
import org.hl7.fhir.r4.model.DocumentReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class RRReceiverServiceImpl implements RRReceiverService {

  private final Logger logger = LoggerFactory.getLogger(RRReceiverServiceImpl.class);

  @Autowired R4ResourcesData r4ResourcesData;

  @Override
  public DocumentReference constructDocumentReference(RRReceiver rrReceiver) {

    return r4ResourcesData.constructR4DocumentReference(
        rrReceiver.getRrXml(), rrReceiver.getPatientId(), rrReceiver.getEncounterId());
  }
}
