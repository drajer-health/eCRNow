package com.drajer.sof.service;

import org.hl7.fhir.r4.model.DocumentReference;

public interface RRReceiverService {

  DocumentReference constructDocumentReference(
      String obj, String type, String patientId, String encounterId);
}
