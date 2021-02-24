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

  /*	public static enum ActionTriggerTypes {

  	// These are based on FHIR Trigger Types
  	// named-event | periodic | data-changed | data-added | data-modified | data-removed | data-accessed | data-access-ended
  	NAMED_EVENT,
  	PERIODIC,
  	DATA_CHANGED,
  	DATA_ADDED,
  	DATA_MODIFIED,
  	DATA_REMOVED,
  	DATA_ACCESSED,
  	DATA_ACCESS_ENDED
  } */

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

  /*	public static enum ActionRelationshipType {

  	// Maps to FHIR Related Actions
  	// before-start | before | before-end | concurrent-with-start | concurrent | concurrent-with-end | after-start | after | after-end

  	BEFORE_START,
  	BEFORE,
  	BEFORE_END,
  	CONCURRENT_WITH_START,
  	CONCURRENT,
  	CONCURRENT_WITH_END,
  	AFTER_START,
  	AFTER,
  	AFTER_END
  }*/

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
