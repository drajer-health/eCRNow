package com.drajer.sof.service;

import com.drajer.sof.model.RRReceiver;
import org.hl7.fhir.r4.model.DocumentReference;

public interface RRReceiverService {

  DocumentReference constructDocumentReference(RRReceiver rrReceiver);
}
