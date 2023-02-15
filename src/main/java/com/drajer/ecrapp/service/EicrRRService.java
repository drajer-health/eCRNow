package com.drajer.ecrapp.service;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import java.util.List;
import java.util.Map;
import org.json.JSONObject;

/**
 * The Interface is used to save, update or extract the eICRs and RR from the eCRNow Database.
 *
 * @author nbashyam
 */
public interface EicrRRService {

  /**
   * The method is used to save or update and EICR document.
   *
   * @param eicr - The Eicr that needs to be saved or updated.
   * @return
   */
  Eicr saveOrUpdate(Eicr eicr);

  /**
   * The method is used to get the eicr by the primary key of the table.
   *
   * @param id
   * @return
   */
  Eicr getEicrById(Integer id);

  /**
   * The method is used to get the eicr by the document id of the eICR sent to the PHA.
   *
   * @param id
   * @return
   */
  Eicr getEicrByDocId(String docId);

  /**
   * The method is used to get the eicr and RR by the XRequest Id that was used to generate the
   * eICR.
   *
   * @param id
   * @return
   */
  List<JSONObject> getEicrAndRRByXRequestId(String xRequestId);

  /**
   * The method is used to save or update a RR in the eCRNow database.
   *
   * @param id
   * @return
   */
  ReportabilityResponse saveOrUpdate(ReportabilityResponse rr);

  /**
   * The method is used to get the RR by the primary key of the table.
   *
   * @param id
   * @return
   */
  ReportabilityResponse getRRById(Integer id);

  /**
   * The method is used to get the current maximum version of the eICR.
   *
   * @param id
   * @return
   */
  Integer getMaxVersionId(Eicr eicr);

  /**
   * The method is used to handle a failure MDN that is received from the Direct channel.
   *
   * @param data - The RR data received which is nothing but the Failure MDN data.
   * @param xCorrelationId - The CorrelationId to correlate the eICR with the Failure MDN.
   * @param xRequestId - The RequestId to correlate with the eICR with the Failure MDN.
   * @return
   */
  void handleFailureMdn(ReportabilityResponse data, String xCorrelationId, String xRequestId);

  /**
   * The method is used to retrieve an eICR using various search parameters.
   *
   * @param searchParams - The supported parameters currently are EICR Doc Id, Patient Id, Encounter
   *     Id, FhirServerUrl, Version, RequestId and SetId.
   * @return
   */
  List<JSONObject> getEicrData(Map<String, String> searchParams);

  /**
   * The method is used to retrieve an RR using various search parameters.
   *
   * @param searchParams - The supported parameters currently are EICR Doc Id, Patient Id, Encounter
   *     Id, FhirServerUrl, Version, RequestId and SetId.
   * @return
   */
  List<JSONObject> getRRData(Map<String, String> searchParams);

  /**
   * The method is used to handle the RR received from the PHA.
   *
   * @param data
   * @param xRequestId
   * @param saveToEhr
   */
  void handleReportabilityResponse(ReportabilityResponse data, String xRequestId);

  /**
   * The method is used to purge eICR tables as needed.
   *
   * @param eicr - The eicr to be deleted.
   */
  void deleteEicr(Eicr eicr);
}
