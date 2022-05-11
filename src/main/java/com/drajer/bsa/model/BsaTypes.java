package com.drajer.bsa.model;

public final class BsaTypes {

  public enum AuthenticationType {
    System,
    MultiTenantSystemLaunch,
    SofProvider,
    UserNamePwd,
    SofBackend,
    Unknown
  }

  public enum ActionType {
    InitiateReportingWorkflow,
    ExecuteReportingWorkflow,
    CheckTriggerCodes,
    CheckParticipantRegistration,
    EvaluateCondition,
    EvaluateMeasure,
    CreateReport,
    ValidateReport,
    SubmitReport,
    DeIdentifyReport,
    AnonymizeReport,
    PsuedonymizeReport,
    EncryptReport,
    CompleteReporting,
    ExtractResearchData,
    ExecuteResearchQuery,
    CheckResponse,
    TerminateReportingWorkflow,
    CancelReport,
    Unknown
  }

  public enum BsaActionStatusType {
    NotStarted,
    Scheduled,
    InProgress,
    Completed,
    Failed,
    Aborted
  }

  public enum MessageType {
    CancerReportMessage,
    HepCReportMessage,
    HealthcareSurveyReportMessage,
    CdaEicrMessage,
    FhirEicrMessage,
    CdaReportabilityResponseMessage,
    FhirReportabilityResponseMessage,
    MessageReport,
  }

  public static String getMessageTypeString(MessageType t) {

    if (t == MessageType.CancerReportMessage) return "cancer-report-message";
    else if (t == MessageType.HepCReportMessage) return "hepc-report-message";
    else if (t == MessageType.HealthcareSurveyReportMessage)
      return "healthcare-survey-report-message";
    else if (t == MessageType.CdaEicrMessage) return "CdaEicrMessage";
    else if (t == MessageType.FhirEicrMessage) return "FhirEicrMessage";
    else if (t == MessageType.CdaReportabilityResponseMessage)
      return "CdaReportabilityResponseMessage";
    else if (t == MessageType.FhirReportabilityResponseMessage)
      return "FhirReportabilityResponseMessage";
    else return "message-report";
  }

  public static ActionType getActionType(String code) {

    if (code.equalsIgnoreCase("initiate-reporting-workflow"))
      return ActionType.InitiateReportingWorkflow;
    else if (code.equalsIgnoreCase("execute-reporting-workflow"))
      return ActionType.ExecuteReportingWorkflow;
    else if (code.equalsIgnoreCase("check-trigger-codes")) return ActionType.CheckTriggerCodes;
    else if (code.equalsIgnoreCase("check-participant-registration"))
      return ActionType.CheckParticipantRegistration;
    else if (code.equalsIgnoreCase("evaluate-condition")) return ActionType.EvaluateCondition;
    else if (code.equalsIgnoreCase("evaluate-measure")) return ActionType.EvaluateMeasure;
    else if (code.equalsIgnoreCase("create-report")) return ActionType.CreateReport;
    else if (code.equalsIgnoreCase("validate-report")) return ActionType.ValidateReport;
    else if (code.equalsIgnoreCase("submit-report")) return ActionType.SubmitReport;
    else if (code.equalsIgnoreCase("deidentify-report")) return ActionType.DeIdentifyReport;
    else if (code.equalsIgnoreCase("anonymize-report")) return ActionType.AnonymizeReport;
    else if (code.equalsIgnoreCase("psuedonymize-report")) return ActionType.PsuedonymizeReport;
    else if (code.equalsIgnoreCase("encrypt-report")) return ActionType.EncryptReport;
    else if (code.equalsIgnoreCase("complete-reporting")) return ActionType.CompleteReporting;
    else if (code.equalsIgnoreCase("extract-research-data")) return ActionType.ExtractResearchData;
    else if (code.equalsIgnoreCase("execute-research-query"))
      return ActionType.ExecuteResearchQuery;
    else if (code.equalsIgnoreCase("check-response")) return ActionType.CheckResponse;
    else if (code.equalsIgnoreCase("terminate-reporting-workflow"))
      return ActionType.TerminateReportingWorkflow;
    else if (code.equalsIgnoreCase("cancel-report")) return ActionType.CancelReport;
    else return ActionType.Unknown;
  }

  public static String getActionString(ActionType type) {

    if (type == ActionType.InitiateReportingWorkflow) return "initiate-reporting-workflow";
    else if (type == ActionType.ExecuteReportingWorkflow) return "execute-reporting-workflow";
    else if (type == ActionType.CheckTriggerCodes) return "check-trigger-codes";
    else if (type == ActionType.CheckParticipantRegistration)
      return "check-participant-registration";
    else if (type == ActionType.EvaluateCondition) return "evaluate-condition";
    else if (type == ActionType.EvaluateMeasure) return "evaluate-measure";
    else if (type == ActionType.CreateReport) return "create-report";
    else if (type == ActionType.ValidateReport) return "validate-report";
    else if (type == ActionType.SubmitReport) return "submit-report";
    else if (type == ActionType.DeIdentifyReport) return "deidentify-report";
    else if (type == ActionType.PsuedonymizeReport) return "psuedonymize-report";
    else if (type == ActionType.EncryptReport) return "encrypt-report";
    else if (type == ActionType.CompleteReporting) return "complete-reporting";
    else if (type == ActionType.ExtractResearchData) return "extract-research-data";
    else if (type == ActionType.ExecuteResearchQuery) return "execute-research-query";
    else if (type == ActionType.CheckResponse) return "check-response";
    else if (type == ActionType.TerminateReportingWorkflow) return "terminate-reporting-workflow";
    else if (type == ActionType.CancelReport) return "cancel-report";
    else return "unknown";
  }

  public enum OutputContentType {
    FHIR,
    CDA_R11,
    CDA_R30,
    Both,
    Unknown
  }

  public static String getOutputContentType(OutputContentType t) {

    if (t == OutputContentType.FHIR) return "FHIR";
    else if (t == OutputContentType.CDA_R11) return "CDA_R11";
    else if (t == OutputContentType.CDA_R30) return "CDA_R30";
    else if (t == OutputContentType.Both) return "Both";
    else return "Unknown";
  }

  public static OutputContentType getOutputContentType(String code) {

    if (code.equalsIgnoreCase("FHIR")) return OutputContentType.FHIR;
    else if (code.equalsIgnoreCase("CDA_R11")) return OutputContentType.CDA_R11;
    else if (code.equalsIgnoreCase("CDA_R30")) return OutputContentType.CDA_R30;
    else if (code.equalsIgnoreCase("Both")) return OutputContentType.Both;
    else return OutputContentType.Unknown;
  }

  public static String getString(AuthenticationType t) {

    if (t == AuthenticationType.System) return "System";
    else if (t == AuthenticationType.SofProvider) return "SofProvider";
    else if (t == AuthenticationType.UserNamePwd) return "UserNamePwd";
    else if (t == AuthenticationType.SofBackend) return "SofBackend";
    else if (t == AuthenticationType.MultiTenantSystemLaunch) return "MultiTenantSystemLaunch";
    else return "Unknown";
  }

  public static AuthenticationType getAuthenticationType(String s) {

    if (s.contentEquals("System")) return AuthenticationType.System;
    else if (s.contentEquals("SofProvider")) return AuthenticationType.SofProvider;
    else if (s.contentEquals("UserNamePwd")) return AuthenticationType.UserNamePwd;
    else if (s.contentEquals("SofBackend")) return AuthenticationType.SofBackend;
    else if (s.contentEquals("MultiTenantSystemLaunch"))
      return AuthenticationType.MultiTenantSystemLaunch;
    else return AuthenticationType.Unknown;
  }
}
