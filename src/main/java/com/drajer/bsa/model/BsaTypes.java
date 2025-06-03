package com.drajer.bsa.model;

public final class BsaTypes {
  public static final String UNKNOWN = "Unknown";

  public enum BsaJobType {
    IMMEDIATE_REPORTING,
    DELAYED_REPORTING
  }

  public enum AuthenticationType {
    SYSTEM,
    MULTI_TENANT_SYSTEM_LAUNCH,
    SOF_PROVIDER,
    USER_NAME_PWD,
    SOF_BACKEND,
    UNKNOWN
  }

  public enum NotificationProcessingStatusType {
    IN_PROGRESS,
    SUSPENDED,
    COMPLETED,
    CANCELLED,
    RELAUNCHED,
    REPROCESSED,
    FAILED,
    Unknown
  }

  public enum ActionType {
    INITIATE_REPORTING_WORKFLOW,
    EXECUTE_REPORTING_WORKFLOW,
    CHECK_TRIGGER_CODES,
    CHECK_PARTICIPANT_REGISTRATION,
    EVALUATE_CONDITION,
    EVALUATE_MEASURE,
    CREATE_REPORT,
    VALIDATE_REPORT,
    SUBMIT_REPORT,
    DE_IDENTIFY_REPORT,
    ANONYMIZE_REPORT,
    PSUEDONYMIZE_REPORT,
    ENCRYPT_REPORT,
    COMPLETE_REPORTING,
    EXTRACT_RESEARCH_DATA,
    EXECUTE_RESEARCH_QUERY,
    CHECK_RESPONSE,
    TERMINATE_REPORTING_WORKFLOW,
    CANCEL_REPORT,
    UNKNOWN
  }

  public enum BsaActionStatusType {
    NOT_STARTED,
    SCHEDULED,
    IN_PROGRESS,
    COMPLETED,
    SUSPENDED,
    FAILED,
    CANCELLED
  }

  public enum SubmissionStatusType {
    DATA_SUBMISSION_SUCCCESSFUL,
    DATA_SUBMISSION_FAILED,
    RESPONSE_PROCESSING_SUCCESSFUL,
    RESPONSE_PROCESSING_FAILED,
    UNKOWN
  }

  public enum MessageType {
    CANCER_REPORT_MESSAGE,
    HEP_C_REPORT_MESSAGE,
    HEALTHCARE_SURVEY_REPORT_MESSAGE,
    RESPNET_CASE_REPORT_MESSAGE,
    CDA_EICR_MESSAGE,
    EICR_CASE_REPORT_MESSAGE,
    CDA_REPORTABILITY_RESPONSE_MESSAGE,
    FHIR_REPORTABILITY_RESPONSE_MESSAGE,
    MESSAGE_REPORT,
  }

  public enum SectionTypeEnum {
    REASON_FOR_VISIT,
    ALLERGIES,
    PROBLEM,
    MEDICATION_ADMINISTERED,
    ADMISSION_MEDICATIONS,
    MEDICATIONS,
    RESULTS,
    NOTES,
    PLAN_OF_TREATMENT,
    PLAN_OF_ASSESSMENT,
    IMMUNIZATIONS,
    PROCEDURES,
    VITAL_SIGNS,
    SOCIAL_HISTORY,
    MEDICAL_EQUIPMENT,
    CARE_TEAM,
    GOAL,
    CHIEF_COMPLAINT,
    HISTORY_OF_PRESENT_ILLNESS,
    REVIEW_OF_SYSTEMS,
    MEDICAL_HISTORY,
    SERVICE_REQUEST,
    PREGNANCY,
    REPORTABILITY_RESPONSE,
    EMERGENCY_OUTBREAK_SECTION,
    PRIMARY_CANCER_CONDITION,
    SECONDARY_CANCER_CONDITION,
    CANCER_STAGE_GROUP,
    RADIO_THERAPY_COURSE_SUMMARY,
    ODH,
    COVERAGE
  }

  public static String getNotificationStatusTypeString(NotificationProcessingStatusType nst) {

    if (nst == NotificationProcessingStatusType.IN_PROGRESS) return "IN_PROGRESS";
    else if (nst == NotificationProcessingStatusType.SUSPENDED) return "SUSPENDED";
    else if (nst == NotificationProcessingStatusType.COMPLETED) return "COMPLETED";
    else if (nst == NotificationProcessingStatusType.CANCELLED) return "CANCELLED";
    else if (nst == NotificationProcessingStatusType.RELAUNCHED) return "RELAUNCHED";
    else if (nst == NotificationProcessingStatusType.REPROCESSED) return "REPROCESSED";
    else if (nst == NotificationProcessingStatusType.FAILED) return "FAILED";
    else return UNKNOWN;
  }

  public static NotificationProcessingStatusType getNotificationProcessingStatusType(String s) {

    if (s.contentEquals("IN_PROGRESS")) return NotificationProcessingStatusType.IN_PROGRESS;
    else if (s.contentEquals("SUSPENDED")) return NotificationProcessingStatusType.SUSPENDED;
    else if (s.contentEquals("COMPLETED")) return NotificationProcessingStatusType.COMPLETED;
    else if (s.contentEquals("CANCELLED")) return NotificationProcessingStatusType.CANCELLED;
    else if (s.contentEquals("RELAUNCHED")) return NotificationProcessingStatusType.RELAUNCHED;
    else if (s.contentEquals("REPROCESSED")) return NotificationProcessingStatusType.REPROCESSED;
    else if (s.contentEquals("FAILED")) return NotificationProcessingStatusType.FAILED;
    else return NotificationProcessingStatusType.Unknown;
  }

  public static String getMessageTypeString(MessageType t) {

    if (t == MessageType.CANCER_REPORT_MESSAGE) return "cancer-report-message";
    else if (t == MessageType.HEP_C_REPORT_MESSAGE) return "hepc-report-message";
    else if (t == MessageType.HEALTHCARE_SURVEY_REPORT_MESSAGE)
      return "healthcare-survey-report-message";
    else if (t == MessageType.RESPNET_CASE_REPORT_MESSAGE) return "respnet-case-report-message";
    else if (t == MessageType.CDA_EICR_MESSAGE) return "CdaEicrMessage";
    else if (t == MessageType.EICR_CASE_REPORT_MESSAGE) return "eicr-case-report-message";
    else if (t == MessageType.CDA_REPORTABILITY_RESPONSE_MESSAGE)
      return "CdaReportabilityResponseMessage";
    else if (t == MessageType.FHIR_REPORTABILITY_RESPONSE_MESSAGE)
      return "FhirReportabilityResponseMessage";
    else return "message-report";
  }

  public static ActionType getActionType(String code) {

    if (code.equalsIgnoreCase("initiate-reporting-workflow"))
      return ActionType.INITIATE_REPORTING_WORKFLOW;
    else if (code.equalsIgnoreCase("execute-reporting-workflow"))
      return ActionType.EXECUTE_REPORTING_WORKFLOW;
    else if (code.equalsIgnoreCase("check-trigger-codes")) return ActionType.CHECK_TRIGGER_CODES;
    else if (code.equalsIgnoreCase("check-participant-registration"))
      return ActionType.CHECK_PARTICIPANT_REGISTRATION;
    else if (code.equalsIgnoreCase("evaluate-condition")) return ActionType.EVALUATE_CONDITION;
    else if (code.equalsIgnoreCase("evaluate-measure")) return ActionType.EVALUATE_MEASURE;
    else if (code.equalsIgnoreCase("create-report")) return ActionType.CREATE_REPORT;
    else if (code.equalsIgnoreCase("validate-report")) return ActionType.VALIDATE_REPORT;
    else if (code.equalsIgnoreCase("submit-report")) return ActionType.SUBMIT_REPORT;
    else if (code.equalsIgnoreCase("deidentify-report")) return ActionType.DE_IDENTIFY_REPORT;
    else if (code.equalsIgnoreCase("anonymize-report")) return ActionType.ANONYMIZE_REPORT;
    else if (code.equalsIgnoreCase("psuedonymize-report")) return ActionType.PSUEDONYMIZE_REPORT;
    else if (code.equalsIgnoreCase("encrypt-report")) return ActionType.ENCRYPT_REPORT;
    else if (code.equalsIgnoreCase("complete-reporting")) return ActionType.COMPLETE_REPORTING;
    else if (code.equalsIgnoreCase("extract-research-data"))
      return ActionType.EXTRACT_RESEARCH_DATA;
    else if (code.equalsIgnoreCase("execute-research-query"))
      return ActionType.EXECUTE_RESEARCH_QUERY;
    else if (code.equalsIgnoreCase("check-response")) return ActionType.CHECK_RESPONSE;
    else if (code.equalsIgnoreCase("terminate-reporting-workflow"))
      return ActionType.TERMINATE_REPORTING_WORKFLOW;
    else if (code.equalsIgnoreCase("cancel-report")) return ActionType.CANCEL_REPORT;
    else return ActionType.UNKNOWN;
  }

  public static String getActionString(ActionType type) {

    if (type == ActionType.INITIATE_REPORTING_WORKFLOW) return "initiate-reporting-workflow";
    else if (type == ActionType.EXECUTE_REPORTING_WORKFLOW) return "execute-reporting-workflow";
    else if (type == ActionType.CHECK_TRIGGER_CODES) return "check-trigger-codes";
    else if (type == ActionType.CHECK_PARTICIPANT_REGISTRATION)
      return "check-participant-registration";
    else if (type == ActionType.EVALUATE_CONDITION) return "evaluate-condition";
    else if (type == ActionType.EVALUATE_MEASURE) return "evaluate-measure";
    else if (type == ActionType.CREATE_REPORT) return "create-report";
    else if (type == ActionType.VALIDATE_REPORT) return "validate-report";
    else if (type == ActionType.SUBMIT_REPORT) return "submit-report";
    else if (type == ActionType.DE_IDENTIFY_REPORT) return "deidentify-report";
    else if (type == ActionType.PSUEDONYMIZE_REPORT) return "psuedonymize-report";
    else if (type == ActionType.ENCRYPT_REPORT) return "encrypt-report";
    else if (type == ActionType.COMPLETE_REPORTING) return "complete-reporting";
    else if (type == ActionType.EXTRACT_RESEARCH_DATA) return "extract-research-data";
    else if (type == ActionType.EXECUTE_RESEARCH_QUERY) return "execute-research-query";
    else if (type == ActionType.CHECK_RESPONSE) return "check-response";
    else if (type == ActionType.TERMINATE_REPORTING_WORKFLOW) return "terminate-reporting-workflow";
    else if (type == ActionType.CANCEL_REPORT) return "cancel-report";
    else return UNKNOWN;
  }

  public enum OutputContentType {
    FHIR,
    CDA_R11,
    CDA_R30,
    CDA_R31,
    BOTH,
    TEST_FHIR_NOT_FOR_PRODUCTION,
    UNKNOWN
  }

  public static String getOutputContentType(OutputContentType t) {

    if (t == OutputContentType.FHIR || t == OutputContentType.TEST_FHIR_NOT_FOR_PRODUCTION)
      return "FHIR";
    else if (t == OutputContentType.CDA_R11) return "CDA_R11";
    else if (t == OutputContentType.CDA_R30 || t == OutputContentType.CDA_R31) return "CDA_R30";
    else if (t == OutputContentType.BOTH || t == OutputContentType.BOTH) return "Both";
    else return UNKNOWN;
  }

  public static OutputContentType getOutputContentType(String code) {

    if (code.equalsIgnoreCase("FHIR") || code.equalsIgnoreCase("TEST_FHIR_NOT_FOR_PRODUCTION"))
      return OutputContentType.FHIR;
    else if (code.equalsIgnoreCase("CDA_R11")) return OutputContentType.CDA_R11;
    else if (code.equalsIgnoreCase("CDA_R30") || code.equalsIgnoreCase("CDAR31"))
      return OutputContentType.CDA_R30;
    else if (code.equalsIgnoreCase("Both")) return OutputContentType.BOTH;
    else return OutputContentType.UNKNOWN;
  }

  public static String getString(AuthenticationType t) {

    if (t == AuthenticationType.SYSTEM) return "System";
    else if (t == AuthenticationType.SOF_PROVIDER) return "SofProvider";
    else if (t == AuthenticationType.USER_NAME_PWD) return "UserNamePwd";
    else if (t == AuthenticationType.SOF_BACKEND) return "SofBackend";
    else if (t == AuthenticationType.MULTI_TENANT_SYSTEM_LAUNCH) return "MultiTenantSystemLaunch";
    else return UNKNOWN;
  }

  public static AuthenticationType getAuthenticationType(String s) {

    if (s.contentEquals("System")) return AuthenticationType.SYSTEM;
    else if (s.contentEquals("SofProvider")) return AuthenticationType.SOF_PROVIDER;
    else if (s.contentEquals("UserNamePwd")) return AuthenticationType.USER_NAME_PWD;
    else if (s.contentEquals("SofBackend")) return AuthenticationType.SOF_BACKEND;
    else if (s.contentEquals("SofSystem")) return AuthenticationType.SOF_BACKEND;
    else if (s.contentEquals("MultiTenantSystemLaunch"))
      return AuthenticationType.MULTI_TENANT_SYSTEM_LAUNCH;
    else return AuthenticationType.UNKNOWN;
  }

  public static SubmissionStatusType getSubmissionStatus(String s) {

    if (s.contentEquals("DATA_SUBMISSION_SUCCCESSFUL"))
      return SubmissionStatusType.DATA_SUBMISSION_SUCCCESSFUL;
    else if (s.contentEquals("DATA_SUBMISSION_FAILED"))
      return SubmissionStatusType.DATA_SUBMISSION_FAILED;
    else if (s.contentEquals("RESPONSE_PROCESSING_SUCCESSFUL"))
      return SubmissionStatusType.RESPONSE_PROCESSING_SUCCESSFUL;
    else if (s.contentEquals("RESPONSE_PROCESSING_FAILED"))
      return SubmissionStatusType.RESPONSE_PROCESSING_FAILED;
    else return SubmissionStatusType.UNKOWN;
  }

  public static String getString(SubmissionStatusType type) {

    if (type == SubmissionStatusType.DATA_SUBMISSION_SUCCCESSFUL)
      return "DATA_SUBMISSION_SUCCCESSFUL";
    else if (type == SubmissionStatusType.DATA_SUBMISSION_FAILED) return "DATA_SUBMISSION_FAILED";
    else if (type == SubmissionStatusType.RESPONSE_PROCESSING_SUCCESSFUL)
      return "RESPONSE_PROCESSING_SUCCESSFUL";
    else if (type == SubmissionStatusType.RESPONSE_PROCESSING_FAILED)
      return "RESPONSE_PROCESSING_FAILED";
    else return "UNKOWN";
  }
}
