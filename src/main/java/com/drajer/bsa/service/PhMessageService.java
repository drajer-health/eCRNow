package com.drajer.bsa.service;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.sof.model.PublicHealthMessageData;
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
   * @param summaryFlag A flag indicating whether to include additional details in the result. If
   *     `true`, only selected fields are included in the result.
   * @return
   */
  List<PublicHealthMessage> getPhMessageData(Map<String, String> searchParams, boolean summaryFlag);

  /**
   * The method is used to retrieve phmessage data Summary by search parameters.
   *
   * @param searchParams - The supported parameters currently are
   *     fhirServerBaseUrl,patientId,encounterId,xRequestId,submittedDataId,version,
   *     responsDataId,responseProcessingInstruction,notifiedResourceId,notifiedResourceType,karUniqueId,responsDataId.
   * @return
   */
  List<PublicHealthMessage> getPhMessageDataSummary(Map<String, String> searchParams);

  /**
   * Retrieves a list of PublicHealthMessage objects based on the provided XRequest IDs.
   *
   * @param xRequestIds A list of XRequest IDs to filter the PublicHealthMessage data.
   * @param summaryFlag A flag indicating whether to include additional details in the result. If
   *     `true`, only selected fields are included in the result.
   * @return A list of PublicHealthMessage objects corresponding to the provided XRequest IDs.
   */
  List<PublicHealthMessage> getPhMessagesContainingXRequestIds(
      List<String> xRequestIds, boolean summaryFlag);

  /**
   * Retrieves a list of PublicHealthMessage objects based on the provided XRequest IDs.
   *
   * @param xRequestIds A list of XRequest IDs to filter the PublicHealthMessage data.
   * @param summaryFlag A flag indicating whether to include additional details in the result. If
   *     `true`, only selected fields are included in the result.
   * @return A list of PublicHealthMessage objects corresponding to the provided XRequest IDs.
   */
  List<PublicHealthMessage> getPhMessageDataByXRequestIds(
      List<String> xRequestIds, boolean summaryFlag);

  /**
   * Retrieves A list of PublicHealthMessage based on specified parameters.
   *
   * @param publicHealthMessageData object
   * @return A list of PublicHealthMessage objects corresponding to the provided Parameters
   */
  List<PublicHealthMessage> getPhMessageByParameters(
      PublicHealthMessageData publicHealthMessageData);

  /**
   * Deletes a PublicHealthMessage data object.
   *
   * @param publicHealthMessage The PublicHealthMessage object to be deleted.
   */
  void deletePhMessage(PublicHealthMessage publicHealthMessage);
}
