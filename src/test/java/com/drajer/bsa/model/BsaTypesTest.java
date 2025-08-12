package com.drajer.bsa.model;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

public class BsaTypesTest {

  @Test
  void testGetNotificationStatusTypeString_AllValues() {
    for (BsaTypes.NotificationProcessingStatusType type :
        BsaTypes.NotificationProcessingStatusType.values()) {
      String result = BsaTypes.getNotificationStatusTypeString(type);
      switch (type) {
        case IN_PROGRESS:
          assertEquals("IN_PROGRESS", result);
          break;
        case SUSPENDED:
          assertEquals("SUSPENDED", result);
          break;
        case COMPLETED:
          assertEquals("COMPLETED", result);
          break;
        case CANCELLED:
          assertEquals("CANCELLED", result);
          break;
        case RELAUNCHED:
          assertEquals("RELAUNCHED", result);
          break;
        case REPROCESSED:
          assertEquals("REPROCESSED", result);
          break;
        case FAILED:
          assertEquals("FAILED", result);
          break;
        default:
          assertEquals(BsaTypes.UNKNOWN, result);
      }
    }
    assertEquals(BsaTypes.UNKNOWN, BsaTypes.getNotificationStatusTypeString(null));
  }

  @Test
  void testGetNotificationProcessingStatusType_AllCases() {
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.IN_PROGRESS,
        BsaTypes.getNotificationProcessingStatusType("IN_PROGRESS"));
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.SUSPENDED,
        BsaTypes.getNotificationProcessingStatusType("SUSPENDED"));
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.COMPLETED,
        BsaTypes.getNotificationProcessingStatusType("COMPLETED"));
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.CANCELLED,
        BsaTypes.getNotificationProcessingStatusType("CANCELLED"));
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.RELAUNCHED,
        BsaTypes.getNotificationProcessingStatusType("RELAUNCHED"));
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.REPROCESSED,
        BsaTypes.getNotificationProcessingStatusType("REPROCESSED"));
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.FAILED,
        BsaTypes.getNotificationProcessingStatusType("FAILED"));
    assertEquals(
        BsaTypes.NotificationProcessingStatusType.Unknown,
        BsaTypes.getNotificationProcessingStatusType("other"));
  }

  @Test
  void testGetMessageTypeString_AllValues() {
    assertEquals(
        "cancer-report-message",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.CANCER_REPORT_MESSAGE));
    assertEquals(
        "hepc-report-message",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.HEP_C_REPORT_MESSAGE));
    assertEquals(
        "healthcare-survey-report-message",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.HEALTHCARE_SURVEY_REPORT_MESSAGE));
    assertEquals(
        "respnet-case-report-message",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.RESPNET_CASE_REPORT_MESSAGE));
    assertEquals(
        "CdaEicrMessage", BsaTypes.getMessageTypeString(BsaTypes.MessageType.CDA_EICR_MESSAGE));
    assertEquals(
        "eicr-case-report-message",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.EICR_CASE_REPORT_MESSAGE));
    assertEquals(
        "CdaReportabilityResponseMessage",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.CDA_REPORTABILITY_RESPONSE_MESSAGE));
    assertEquals(
        "FhirReportabilityResponseMessage",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.FHIR_REPORTABILITY_RESPONSE_MESSAGE));
    assertEquals(
        "message-report", BsaTypes.getMessageTypeString(BsaTypes.MessageType.MESSAGE_REPORT));
    assertEquals("message-report", BsaTypes.getMessageTypeString(null));
  }

  @Test
  void testGetActionType_AllCases() {
    assertEquals(
        BsaTypes.ActionType.INITIATE_REPORTING_WORKFLOW,
        BsaTypes.getActionType("initiate-reporting-workflow"));
    assertEquals(
        BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW,
        BsaTypes.getActionType("execute-reporting-workflow"));
    assertEquals(
        BsaTypes.ActionType.CHECK_TRIGGER_CODES, BsaTypes.getActionType("check-trigger-codes"));
    assertEquals(
        BsaTypes.ActionType.CHECK_PARTICIPANT_REGISTRATION,
        BsaTypes.getActionType("check-participant-registration"));
    assertEquals(
        BsaTypes.ActionType.EVALUATE_CONDITION, BsaTypes.getActionType("evaluate-condition"));
    assertEquals(BsaTypes.ActionType.EVALUATE_MEASURE, BsaTypes.getActionType("evaluate-measure"));
    assertEquals(BsaTypes.ActionType.CREATE_REPORT, BsaTypes.getActionType("create-report"));
    assertEquals(BsaTypes.ActionType.VALIDATE_REPORT, BsaTypes.getActionType("validate-report"));
    assertEquals(BsaTypes.ActionType.SUBMIT_REPORT, BsaTypes.getActionType("submit-report"));
    assertEquals(
        BsaTypes.ActionType.DE_IDENTIFY_REPORT, BsaTypes.getActionType("deidentify-report"));
    assertEquals(BsaTypes.ActionType.ANONYMIZE_REPORT, BsaTypes.getActionType("anonymize-report"));
    assertEquals(
        BsaTypes.ActionType.PSUEDONYMIZE_REPORT, BsaTypes.getActionType("psuedonymize-report"));
    assertEquals(BsaTypes.ActionType.ENCRYPT_REPORT, BsaTypes.getActionType("encrypt-report"));
    assertEquals(
        BsaTypes.ActionType.COMPLETE_REPORTING, BsaTypes.getActionType("complete-reporting"));
    assertEquals(
        BsaTypes.ActionType.EXTRACT_RESEARCH_DATA, BsaTypes.getActionType("extract-research-data"));
    assertEquals(
        BsaTypes.ActionType.EXECUTE_RESEARCH_QUERY,
        BsaTypes.getActionType("execute-research-query"));
    assertEquals(BsaTypes.ActionType.CHECK_RESPONSE, BsaTypes.getActionType("check-response"));
    assertEquals(
        BsaTypes.ActionType.TERMINATE_REPORTING_WORKFLOW,
        BsaTypes.getActionType("terminate-reporting-workflow"));
    assertEquals(BsaTypes.ActionType.CANCEL_REPORT, BsaTypes.getActionType("cancel-report"));
    assertEquals(BsaTypes.ActionType.UNKNOWN, BsaTypes.getActionType("unknown-action"));
  }

  @Test
  void testGetActionString_AllValues() {
    for (BsaTypes.ActionType type : BsaTypes.ActionType.values()) {
      String result = BsaTypes.getActionString(type);
      if (type == BsaTypes.ActionType.UNKNOWN) {
        assertEquals(BsaTypes.UNKNOWN, result);
      } else {
        assertNotNull(result);
        assertNotEquals(BsaTypes.UNKNOWN, result);
      }
    }
    assertEquals(BsaTypes.UNKNOWN, BsaTypes.getActionString(null));
  }

  @Test
  void testGetOutputContentType_AllValues() {
    assertEquals("FHIR", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.FHIR));
    assertEquals(
        "FHIR",
        BsaTypes.getOutputContentType(BsaTypes.OutputContentType.TEST_FHIR_NOT_FOR_PRODUCTION));
    assertEquals("CDA_R11", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.CDA_R11));
    assertEquals("CDA_R30", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.CDA_R30));
    assertEquals("CDA_R30", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.CDA_R31));
    assertEquals("Both", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.BOTH));
    assertEquals(
        BsaTypes.UNKNOWN, BsaTypes.getOutputContentType(BsaTypes.OutputContentType.UNKNOWN));
  }

  @Test
  void testGetOutputContentTypeFromString_AllCases() {
    assertEquals(BsaTypes.OutputContentType.FHIR, BsaTypes.getOutputContentType("FHIR"));
    assertEquals(
        BsaTypes.OutputContentType.FHIR,
        BsaTypes.getOutputContentType("TEST_FHIR_NOT_FOR_PRODUCTION"));
    assertEquals(BsaTypes.OutputContentType.CDA_R11, BsaTypes.getOutputContentType("CDA_R11"));
    assertEquals(BsaTypes.OutputContentType.CDA_R30, BsaTypes.getOutputContentType("CDA_R30"));
    assertEquals(BsaTypes.OutputContentType.CDA_R30, BsaTypes.getOutputContentType("CDAR31"));
    assertEquals(BsaTypes.OutputContentType.BOTH, BsaTypes.getOutputContentType("Both"));
    assertEquals(BsaTypes.OutputContentType.UNKNOWN, BsaTypes.getOutputContentType("invalid"));
  }

  @Test
  void testGetStringAuthenticationType_AllValues() {
    assertEquals("System", BsaTypes.getString(BsaTypes.AuthenticationType.SYSTEM));
    assertEquals("SofProvider", BsaTypes.getString(BsaTypes.AuthenticationType.SOF_PROVIDER));
    assertEquals("UserNamePwd", BsaTypes.getString(BsaTypes.AuthenticationType.USER_NAME_PWD));
    assertEquals("SofBackend", BsaTypes.getString(BsaTypes.AuthenticationType.SOF_BACKEND));
    assertEquals(
        "MultiTenantSystemLaunch",
        BsaTypes.getString(BsaTypes.AuthenticationType.MULTI_TENANT_SYSTEM_LAUNCH));
    assertEquals(BsaTypes.UNKNOWN, BsaTypes.getString(BsaTypes.AuthenticationType.UNKNOWN));
  }

  @Test
  void testGetAuthenticationType_AllCases() {
    assertEquals(BsaTypes.AuthenticationType.SYSTEM, BsaTypes.getAuthenticationType("System"));
    assertEquals(
        BsaTypes.AuthenticationType.SOF_PROVIDER, BsaTypes.getAuthenticationType("SofProvider"));
    assertEquals(
        BsaTypes.AuthenticationType.USER_NAME_PWD, BsaTypes.getAuthenticationType("UserNamePwd"));
    assertEquals(
        BsaTypes.AuthenticationType.SOF_BACKEND, BsaTypes.getAuthenticationType("SofBackend"));
    assertEquals(
        BsaTypes.AuthenticationType.SOF_BACKEND, BsaTypes.getAuthenticationType("SofSystem"));
    assertEquals(
        BsaTypes.AuthenticationType.MULTI_TENANT_SYSTEM_LAUNCH,
        BsaTypes.getAuthenticationType("MultiTenantSystemLaunch"));
    assertEquals(BsaTypes.AuthenticationType.UNKNOWN, BsaTypes.getAuthenticationType("invalid"));
  }

  @Test
  void testGetSubmissionStatus_AllCases() {
    assertEquals(
        BsaTypes.SubmissionStatusType.DATA_SUBMISSION_SUCCCESSFUL,
        BsaTypes.getSubmissionStatus("DATA_SUBMISSION_SUCCCESSFUL"));
    assertEquals(
        BsaTypes.SubmissionStatusType.DATA_SUBMISSION_FAILED,
        BsaTypes.getSubmissionStatus("DATA_SUBMISSION_FAILED"));
    assertEquals(
        BsaTypes.SubmissionStatusType.RESPONSE_PROCESSING_SUCCESSFUL,
        BsaTypes.getSubmissionStatus("RESPONSE_PROCESSING_SUCCESSFUL"));
    assertEquals(
        BsaTypes.SubmissionStatusType.RESPONSE_PROCESSING_FAILED,
        BsaTypes.getSubmissionStatus("RESPONSE_PROCESSING_FAILED"));
    assertEquals(BsaTypes.SubmissionStatusType.UNKOWN, BsaTypes.getSubmissionStatus("invalid"));
  }

  @Test
  void testGetStringSubmissionStatusType_AllValues() {
    assertEquals(
        "DATA_SUBMISSION_SUCCCESSFUL",
        BsaTypes.getString(BsaTypes.SubmissionStatusType.DATA_SUBMISSION_SUCCCESSFUL));
    assertEquals(
        "DATA_SUBMISSION_FAILED",
        BsaTypes.getString(BsaTypes.SubmissionStatusType.DATA_SUBMISSION_FAILED));
    assertEquals(
        "RESPONSE_PROCESSING_SUCCESSFUL",
        BsaTypes.getString(BsaTypes.SubmissionStatusType.RESPONSE_PROCESSING_SUCCESSFUL));
    assertEquals(
        "RESPONSE_PROCESSING_FAILED",
        BsaTypes.getString(BsaTypes.SubmissionStatusType.RESPONSE_PROCESSING_FAILED));
    assertEquals("UNKOWN", BsaTypes.getString(BsaTypes.SubmissionStatusType.UNKOWN));
  }
}
