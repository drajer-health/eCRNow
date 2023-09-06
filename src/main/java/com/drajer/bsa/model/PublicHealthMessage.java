package com.drajer.bsa.model;

import java.util.Date;
import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>PublicHealthMessage</h1>
 *
 * The class tracks the necessary outgoing and incoming messages from/to the BSA.
 *
 * @author nbashyam
 */
@Entity
@Table(name = "ph_messages")
@DynamicUpdate
public class PublicHealthMessage {

  @Transient private final Logger logger = LoggerFactory.getLogger(PublicHealthMessage.class);

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id @GeneratedValue private UUID id;

  /** The attribute represents the base FHIR Server URL from which the notification was received. */
  @Column(name = "fhir_server_base_url", nullable = false, columnDefinition = "nvarchar(max)")
  private String fhirServerBaseUrl;

  /** The attribute represents the patient id for whom the trigger event was received. */
  @Column(name = "patient_id", nullable = false, columnDefinition = "nvarchar(max)")
  private String patientId;

  /** The attribute represents the encounter id if the notified resource type was an encounter. */
  @Column(name = "encounter_id", nullable = false, columnDefinition = "nvarchar(max)")
  private String encounterId;

  /** The attribute represents the id of the resource received in the notification. */
  @Column(name = "notified_resource_id", nullable = false, columnDefinition = "nvarchar(max)")
  private String notifiedResourceId;

  /** The attribute represents the type of the resource in the received notification */
  @Column(name = "notified_resource_type", nullable = false, columnDefinition = "nvarchar(max)")
  private String notifiedResourceType;

  /** The attribute represents the KAR that gave rise to the message */
  @Column(name = "kar_unique_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String karUniqueId;

  /**
   * The attribute represents the id of the notification table which resulted in the messages to be
   * submitted.
   */
  @Column(name = "notification_id", nullable = false, columnDefinition = "nvarchar(max)")
  private String notificationId;

  /**
   * This attribute is used to correlate across APIs and processes as the data moves along for
   * submission.
   */
  @Column(name = "correlation_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String xCorrelationId;

  /** This attribute tracks the API Request Ids when present for submitted data */
  @Column(name = "x_request_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String xRequestId;

  /** The attribute represents the submitted data to the TTP/PHA in FHIR format. */
  @Column(name = "submitted_fhir_data", nullable = true, columnDefinition = "nvarchar(max)")
  private String submittedFhirData;

  /** The attribute represents the submitted data to the TTP/PHA in CDA format. */
  @Column(name = "submitted_cda_data", nullable = true, columnDefinition = "nvarchar(max)")
  private String submittedCdaData;

  /** The attribute represents the format of data submitted to the TTP/PHA. */
  @Column(name = "submitted_message_type", nullable = true, columnDefinition = "nvarchar(max)")
  private String submittedMessageType;

  /**
   * The attribute represents the id of data submitted to the TTP/PHA. This Id will be used for
   * coorrelation between the Submitted data and the Response data. For FHIR payloads, this would be
   * the ID of the Content Bundle. For Cda Payload this would be the docId of the CDA document.
   */
  @Column(name = "submitted_data_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String submittedDataId;

  /**
   * The attribute represents the version of the submitted data, for the same patient and encounter
   * when multiple reports are submitted the version Id will be incremented.
   */
  @Column(name = "submitted_version_number", nullable = true, columnDefinition = "INTEGER")
  private Integer submittedVersionNumber;

  /**
   * The attribute represents the Message ID of message submitted to the TTP/PHA. This Id will be
   * used for coorrelation between the Submitted message and the Response message. For both CDA and
   * FHIR payloads this will be the Message ID in the Message Header Resource of the Reporting
   * Bundle.
   */
  @Column(name = "submitted_message_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String submittedMessageId;

  /**
   * The attribute represents the status of the actions after the message is created, for e.g if the
   * validation or submission fails, it would be recorded for future processing.
   */
  @Column(name = "submission_message_status", nullable = true, columnDefinition = "nvarchar(max)")
  private String submissionMessageStatus;

  /** This attribute represents the submission time when the data was submitted. */
  @Column(name = "submission_time", nullable = true)
  @CreationTimestamp
  private Date submissionTime;

  /**
   * The attribute represents the response data received from the TTP/PHA for the healthcare setting
   * in FHIR format.
   */
  @Column(name = "fhir_response_data", nullable = true, columnDefinition = "nvarchar(max)")
  private String fhirResponseData;

  /**
   * The attribute represents the response data received from the TTP/PHA for the healthcare setting
   * in CDA format.
   */
  @Column(name = "cda_response_data", nullable = true, columnDefinition = "nvarchar(max)")
  private String cdaResponseData;

  /**
   * The attribute represents the response data received from the TTP/PHA for the healthcare setting
   * when the messages fail due to any reason. This will hold information such as FailureMDNs etc.
   */
  @Column(name = "failure_response_data", nullable = true, columnDefinition = "nvarchar(max)")
  private String failureResponseData;

  /**
   * The attribute represents the format of the response message from the TTP/PHA to the healthcare
   * setting.
   */
  @Column(name = "response_message_type", nullable = true, columnDefinition = "nvarchar(max)")
  private String responseMessageType;

  /**
   * The attribute represents the id of data received from the TTP/PHA. For FHIR payloads, this
   * would be the ID of the Content Bundle received. For Cda Payload this would be the docId of the
   * received CDA document.
   */
  @Column(name = "response_data_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String responseDataId;

  /**
   * The attribute represents the Message ID of message received from the TTP/PHA. For both CDA and
   * FHIR payloads this will be the Message ID in the Message Header Resource of the Reporting
   * Bundle.
   */
  @Column(name = "response_message_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String responseMessageId;

  /**
   * The attribute represents a instruction that will determine what needs to be done to the
   * response. For e.g in the case of eICRs, the status will be set using the EicrTypes.rrType that
   * will guide whether something needs to be reported to the provider or to the admin.
   */
  @Column(name = "response_processing_instruction", nullable = true, columnDefinition = "nvarchar(max)")
  private String responseProcessingInstruction;

  /**
   * The attribute represents a status on success, failure or other states while processing the
   * response. For e.g In the case of eICRs submitting the data to an EHR may fail and it may have
   * to be retried. So the status of failure would be recorded. that will determine what needs to be
   * done to the response.
   */
  @Column(name = "response_processing_status", nullable = true, columnDefinition = "nvarchar(max)")
  private String responseProcessingStatus;

  /** This attribute represents the response received time from the PHA. */
  @Column(name = "response_received_time", nullable = true)
  private Date responseReceivedTime;

  /**
   * The attribute represents the response that is persisted in the EHR as a document reference
   * object.
   */
  @Column(name = "ehr_doc_ref_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String responseEhrDocRefId;

  /** Log the initiating action for debugging purposes. */
  @Column(name = "initiating_action", nullable = true, columnDefinition = "nvarchar(max)")
  private String initiatingAction;

  /** Stores the trigger matches if any that resulted in the message to be submitted. */
  @Column(name = "trigger_match_status", nullable = true, columnDefinition = "nvarchar(max)")
  private String triggerMatchStatus;

  /**
   * Stores the data linker Id which is used if the data is de-identified. When the data is
   * de-identified, this Id is passed to the PHA and the PHA may want to link back to the Patient.
   * In these cases the this Id can be used to re-identify the specific patient.
   */
  @Column(name = "patient_linker_id", nullable = true, columnDefinition = "nvarchar(max)")
  private String patientLinkerId;

  /** This attribute represents the last time when the object was updated. */
  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  public PublicHealthMessage() {
    logger.info("PublicHealthMessage initiated.");
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public String getFhirServerBaseUrl() {
    return fhirServerBaseUrl;
  }

  public void setFhirServerBaseUrl(String fhirServerBaseUrl) {
    this.fhirServerBaseUrl = fhirServerBaseUrl;
  }

  public String getPatientId() {
    return patientId;
  }

  public void setPatientId(String patientId) {
    this.patientId = patientId;
  }

  public String getNotificationId() {
    return notificationId;
  }

  public void setNotificationId(String notificationId) {
    this.notificationId = notificationId;
  }

  public String getxCorrelationId() {
    return xCorrelationId;
  }

  public void setxCorrelationId(String xCorrelationId) {
    this.xCorrelationId = xCorrelationId;
  }

  public String getxRequestId() {
    return xRequestId;
  }

  public void setxRequestId(String xRequestId) {
    this.xRequestId = xRequestId;
  }

  public String getSubmittedFhirData() {
    return submittedFhirData;
  }

  public void setSubmittedFhirData(String submittedData) {
    this.submittedFhirData = submittedData;
  }

  public String getSubmittedCdaData() {
    return submittedCdaData;
  }

  public void setSubmittedCdaData(String submittedCdaData) {
    this.submittedCdaData = submittedCdaData;
  }

  public String getFhirResponseData() {
    return fhirResponseData;
  }

  public void setFhirResponseData(String responseData) {
    this.fhirResponseData = responseData;
  }

  public String getCdaResponseData() {
    return cdaResponseData;
  }

  public void setCdaResponseData(String cdaResponseData) {
    this.cdaResponseData = cdaResponseData;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public String getNotifiedResourceType() {
    return notifiedResourceType;
  }

  public void setNotifiedResourceType(String notifiedResourceType) {
    this.notifiedResourceType = notifiedResourceType;
  }

  public String getSubmittedMessageType() {
    return submittedMessageType;
  }

  public void setSubmittedMessageType(String submittedMessageType) {
    this.submittedMessageType = submittedMessageType;
  }

  public String getSubmittedDataId() {
    return submittedDataId;
  }

  public void setSubmittedDataId(String submittedDataId) {
    this.submittedDataId = submittedDataId;
  }

  public String getSubmittedMessageId() {
    return submittedMessageId;
  }

  public void setSubmittedMessageId(String submittedMessageId) {
    this.submittedMessageId = submittedMessageId;
  }

  public String getSubmissionMessageStatus() {
    return submissionMessageStatus;
  }

  public void setSubmissionMessageStatus(String submissionMessageStatus) {
    this.submissionMessageStatus = submissionMessageStatus;
  }

  public String getResponseMessageType() {
    return responseMessageType;
  }

  public void setResponseMessageType(String responseMessageType) {
    this.responseMessageType = responseMessageType;
  }

  public String getResponseDataId() {
    return responseDataId;
  }

  public void setResponseDataId(String responseDataId) {
    this.responseDataId = responseDataId;
  }

  public String getResponseMessageId() {
    return responseMessageId;
  }

  public void setResponseMessageId(String responseMessageId) {
    this.responseMessageId = responseMessageId;
  }

  public String getResponseProcessingInstruction() {
    return responseProcessingInstruction;
  }

  public void setResponseProcessingInstruction(String responseProcessingInstruction) {
    this.responseProcessingInstruction = responseProcessingInstruction;
  }

  public String getResponseProcessingStatus() {
    return responseProcessingStatus;
  }

  public void setResponseProcessingStatus(String responseProcessingStatus) {
    this.responseProcessingStatus = responseProcessingStatus;
  }

  public String getNotifiedResourceId() {
    return notifiedResourceId;
  }

  public void setNotifiedResourceId(String notifiedResourceId) {
    this.notifiedResourceId = notifiedResourceId;
  }

  public String getInitiatingAction() {
    return initiatingAction;
  }

  public void setInitiatingAction(String initiatingAction) {
    this.initiatingAction = initiatingAction;
  }

  public String getFailureResponseData() {
    return failureResponseData;
  }

  public void setFailureResponseData(String failureResponseData) {
    this.failureResponseData = failureResponseData;
  }

  public String getKarUniqueId() {
    return karUniqueId;
  }

  public void setKarUniqueId(String karUniqueId) {
    this.karUniqueId = karUniqueId;
  }

  public String getEncounterId() {
    return encounterId;
  }

  public void setEncounterId(String encounterId) {
    this.encounterId = encounterId;
  }

  public String getResponseEhrDocRefId() {
    return responseEhrDocRefId;
  }

  public void setResponseEhrDocRefId(String responseEhrDocRefId) {
    this.responseEhrDocRefId = responseEhrDocRefId;
  }

  public Integer getSubmittedVersionNumber() {
    return submittedVersionNumber;
  }

  public void setSubmittedVersionNumber(Integer submittedVersionNumber) {
    this.submittedVersionNumber = submittedVersionNumber;
  }

  public String getTriggerMatchStatus() {
    return triggerMatchStatus;
  }

  public void setTriggerMatchStatus(String triggerMatchStatus) {
    this.triggerMatchStatus = triggerMatchStatus;
  }

  public String getPatientLinkerId() {
    return patientLinkerId;
  }

  public void setPatientLinkerId(String patientLinkerId) {
    this.patientLinkerId = patientLinkerId;
  }

  public Date getSubmissionTime() {
    return submissionTime;
  }

  public void setSubmissionTime(Date submissionTime) {
    this.submissionTime = submissionTime;
  }

  public Date getResponseReceivedTime() {
    return responseReceivedTime;
  }

  public void setResponseReceivedTime(Date responseReceivedTime) {
    this.responseReceivedTime = responseReceivedTime;
  }
}
