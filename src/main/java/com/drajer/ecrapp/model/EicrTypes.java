package com.drajer.ecrapp.model;

public final class EicrTypes {

  public enum RrType {

    // These are the responses that need to be handled.
    FAILURE_MDN,
    REPORTABILITY_RESPONSE,
    REPORTABLE,
    MAYBE_REPORTABLE,
    NO_RULE_MET,
    NOT_REPORTABLE
  }

  public enum ReportabilityType {
    RRVS1, // Reportable
    RRVS2, // May be Reportable
    RRVS3, // Not Reportable
    RRVS4, // No rule met
    UNKNOWN
  }

  public enum RrProcessingStatus {
    FAILED_RR_CONVERSION_TO_DOCREF,
    FAILED_RR_SUBMISSION_TO_EHR,
    FAILED_RR_SUBMISSION_TO_REST_API,
    HEALTHCARE_SETTING_NOT_FOUND_FOR_RR,
    FAILURE,
    SUCCESS
  }
}
