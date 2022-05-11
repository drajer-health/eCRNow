package com.drajer.bsa.service;

import com.drajer.ecrapp.model.ReportabilityResponse;

/**
 * The Interface is used to process RRs received from PHAs.
 *
 * @author nbashyam
 */
public interface RrReceiver {

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
   * The method is used to handle the RR received from the PHA from the Direct channel.
   *
   * @param data
   * @param xRequestId
   * @param saveToEhr
   */
  void handleReportabilityResponse(ReportabilityResponse data, String xRequestId);
}
