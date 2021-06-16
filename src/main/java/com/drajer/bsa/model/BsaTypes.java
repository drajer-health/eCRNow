package com.drajer.bsa.model;

public final class BsaTypes {

  public enum AuthenticationType {
    SofSystem,
    SofProvider,
    UserNamePwd,
    Unknown
  }

  public enum ActionType {
    InitiateReportingWorkflow,
    ExecuteReportingWorkflow,
    CheckTriggerCodes,
    CheckParticipantRegistration,
    EvaluateCondition,
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
    TerminateReportingWorkflow,
    CancelReport,
    Unknown
  }

  public static String getString(AuthenticationType t) {

    if (t == AuthenticationType.SofSystem) return "SofSystem";
    else if (t == AuthenticationType.SofProvider) return "SofProvider";
    else if (t == AuthenticationType.UserNamePwd) return "UserNamePwd";
    else return "Unknown";
  }

  public static AuthenticationType getAuthenticationType(String s) {

    if (s.contentEquals("SofSystem")) return AuthenticationType.SofSystem;
    else if (s.contentEquals("SofProvider")) return AuthenticationType.SofProvider;
    else if (s.contentEquals("UserNamePwd")) return AuthenticationType.UserNamePwd;
    else return AuthenticationType.Unknown;
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
    MessageReport
  }

  public static String getMessageTypeString(MessageType t) {

    if (t == MessageType.CancerReportMessage) return "cancer-report-message";
    else if (t == MessageType.HepCReportMessage) return "hepc-report-message";
    else if (t == MessageType.HealthcareSurveyReportMessage)
      return "healthcare-survey-report-message";
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
    else if (code.equalsIgnoreCase("terminate-reporting-workflow"))
      return ActionType.TerminateReportingWorkflow;
    else if (code.equalsIgnoreCase("cancel-report")) return ActionType.CancelReport;
    else return ActionType.Unknown;
  }
}
