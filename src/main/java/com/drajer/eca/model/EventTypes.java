package com.drajer.eca.model;

public final class EventTypes {

  public enum WorkflowEvent {

    // These are the two ways that the app can be initiated currently.
    SOF_LAUNCH,
    SUBSCRIPTION_NOTIFICATION,
    SCHEDULED_JOB,
    DEPENDENT_EVENT_COMPLETION
  }

  public enum JobStatus {
    NOT_STARTED,
    SCHEDULED,
    IN_PROGRESS,
    COMPLETED,
    ABORTED
  }

  public enum EcrActionTypes {

    // These are the actions that can happen for eCR reporting.
    MATCH_TRIGGER,
    CREATE_EICR,
    PERIODIC_UPDATE_EICR,
    CREATE_EICR_AFTER_RECHECK,
    CLOSE_OUT_EICR,
    VALIDATE_EICR,
    SUBMIT_EICR,
    RR_CHECK,
    UNKNOWN
  }

  public enum ConditionType {

    // condition types in FHIR Plan Definition
    APPLICABILITY,
    START,
    STOP
  }

  public enum RequestModeEnum {
    VALIDATION,
    PRODUCTION
  }

  public enum EicrProcStatusEnum {
    SUCCESSFULLY_PROCESSED,
    FAILED_VALIDATION,
    FAILED_SUBMISSION
  }

  public enum RrProcStatusEnum {
    SUCCESSFULLY_PROCESSED,
    FAILED_CORRELATION,
    FAILED_EHR_SUBMISSION
  }

  public EcrActionTypes getEcrActionTypes(String s) {

    if (s.equalsIgnoreCase("match-trigger")) return EcrActionTypes.MATCH_TRIGGER;
    else if (s.equalsIgnoreCase("create-eicr")) return EcrActionTypes.CREATE_EICR;
    else if (s.equalsIgnoreCase("periodic-update-eicr")) return EcrActionTypes.PERIODIC_UPDATE_EICR;
    else if (s.equalsIgnoreCase("close-out-eicr")) return EcrActionTypes.CLOSE_OUT_EICR;
    else if (s.equalsIgnoreCase("validate-eicr")) return EcrActionTypes.VALIDATE_EICR;
    else if (s.equalsIgnoreCase("route-and-send-eicr")) return EcrActionTypes.SUBMIT_EICR;
    else return EcrActionTypes.UNKNOWN;
  }
}
