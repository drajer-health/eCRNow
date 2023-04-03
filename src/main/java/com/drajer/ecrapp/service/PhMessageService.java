package com.drajer.ecrapp.service;

import com.drajer.bsa.model.PublicHealthMessage;
import java.util.List;
import java.util.Map;

/**
 * The Interface is used to save, update or extract the ph messsage from the eCRNow Database.
 *
 * @author thiru
 */
public interface PhMessageService {
  /**
   * The method is used to retrieve ph message data by search parameters.
   *
   * @param searchParams - The supported parameters currently are
   *     fhirServerBaseUrl,patientId,encounterId,xRequestId,submittedDataId,version,
   *     responsDataId,responseProcessingInstruction,notifiedResourceId,notifiedResourceType,karUniqueId,responsDataId.
   * @return
   */
  List<PublicHealthMessage> getPhMessageData(Map<String, String> searchParams);
}
