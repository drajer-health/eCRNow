package com.drajer.bsa.model;

import java.util.Date;
import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>NotificationContext</h1>
 *
 * The Entity represents the notification context received from an EHR. The notification context
 * stores sufficient information persistently so that notifications can be processed and
 * KnowledgeArtifacts can be applied for the notification.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Entity
@Table(
    name = "notification_context",
    uniqueConstraints =
        @UniqueConstraint(
            columnNames = {
              "fhir_server_base_url",
              "patient_id",
              "notification_resource_id",
              "notification_resource_type",
              "trigger_event"
            }))
@DynamicUpdate
public class NotificationContext {

  @Transient private final Logger logger = LoggerFactory.getLogger(NotificationContext.class);

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id @GeneratedValue private UUID id;

  /** The attribute represents the trigger event (Medmorph Named Event) for processing. */
  @Column(name = "trigger_event", nullable = false, columnDefinition = "TEXT")
  private String triggerEvent;

  /** The attribute represents the base FHIR Server URL from which the notification was received. */
  @Column(name = "fhir_server_base_url", nullable = false, columnDefinition = "TEXT")
  private String fhirServerBaseUrl;

  /** The attribute represents the patient id for whom the trigger event was received. */
  @Column(name = "patient_id", nullable = false, columnDefinition = "TEXT")
  private String patientId;

  /** The attribute represents the resource id which is received as part of the notification. */
  @Column(name = "notification_resource_id", nullable = false, columnDefinition = "TEXT")
  private String notificationResourceId;

  /** The attribute represents the resource type which is received as part of the notification. */
  @Column(name = "notification_resource_type", nullable = false, columnDefinition = "TEXT")
  private String notificationResourceType;

  /** The attribute represents the notification data received for a re-launch request */
  @Column(name = "relaunch_notification_data", nullable = true, columnDefinition = "TEXT")
  private String relaunchNotificationData;

  /**
   * The attribute represents the status of the notification processing. IN_PROGRESS - Will be
   * status as long as the timers are scheduled for the encounter. SUSPENDED - Will be the status
   * when the processing is terminated in the app for any reason. (e.g Long Encounter Time threshold
   * reached). COMPLETED - Completed based on the status of the timers. CANCELLED - Launch Request
   * was cancelled.
   */
  @Column(name = "notification_processing_status", nullable = true, columnDefinition = "TEXT")
  private String notificationProcessingStatus;

  /**
   * Transient attribute containing the actual resource that was part of the notification, this is
   * used only during processing
   */
  @Transient Resource notifiedResource;

  /**
   * The attribute represents the data received as part of the notification. This is a FHIR Bundle,
   * stored as part of the table.
   */
  @Column(name = "notification_data", nullable = false, columnDefinition = "TEXT")
  private String notificationData;

  /** This attribute is used to correlate across APIs and processes as the data moves along. */
  @Column(name = "correlation_id", nullable = true, columnDefinition = "TEXT")
  private String xCorrelationId;

  /** This attribute tracks the API Request Ids when present. */
  @Column(name = "x_request_id", nullable = true, columnDefinition = "TEXT")
  private String xRequestId;

  /** The opaque throttle context to be passed to the EHR */
  @Column(name = "throttle_context", nullable = true, columnDefinition = "TEXT")
  private String throttleContext;

  /** The access token after authorization to the EHR */
  @Column(name = "ehr_access_token", nullable = true, columnDefinition = "TEXT")
  private String ehrAccessToken;

  /** The expiry duration in seconds for the EHR Access Token */
  @Column(name = "ehr_access_token_expiry_duration", nullable = true)
  private int ehrAccessTokenExpiryDuration;

  /** The expiration time for EHR Access Token. */
  @Column(name = "token_expiry_date", nullable = true)
  @Temporal(TemporalType.TIMESTAMP)
  private Date ehrAccessTokenExpirationTime;

  /**
   * This attribute represents the encounter start time to be used for limiting the data being
   * extracted from the EHR
   */
  @Column(name = "encounter_start_time", nullable = true)
  private Date encounterStartTime;

  /**
   * This attribute represents the encounter start time to be used for limiting the data being
   * extracted from the EHR
   */
  @Column(name = "encounter_end_time", nullable = true)
  private Date encounterEndTime;

  /**
   * This attribute represents the encounter class (AMB/IMP) which is used for choosing different
   * execution paths in the ERSD.
   */
  @Column(name = "encounter_class", nullable = true)
  private String encounterClass;

  /**
   * This attribute represents the ehr_launch_context that is received as part of the Patient Launch
   * / re-launch request. This is a Map of Key/Value pairs which is understood by the EHR's FHIR or
   * other APIs.
   */
  @Column(name = "ehr_launch_context", nullable = true)
  private String ehrLaunchContext;

  /** This attribute represents the last time when the object was updated. */
  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

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

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public String getActualTriggerEvent() {
    if (triggerEvent != null && triggerEvent.contains("|")) {
      String[] s = triggerEvent.split("\\|");
      return s[0];
    } else return triggerEvent;
  }

  public String getTriggerEvent() {
    return triggerEvent;
  }

  public void setTriggerEvent(String triggerEvent) {
    this.triggerEvent = triggerEvent;
  }

  public String getFhirServerBaseUrl() {
    return fhirServerBaseUrl;
  }

  public void setFhirServerBaseUrl(String fhirServerUrl) {
    this.fhirServerBaseUrl = fhirServerUrl;
  }

  public String getPatientId() {
    return patientId;
  }

  public void setPatientId(String patientId) {
    this.patientId = patientId;
  }

  public String getNotificationResourceId() {
    return notificationResourceId;
  }

  public void setNotificationResourceId(String notificationResourceId) {
    this.notificationResourceId = notificationResourceId;
  }

  public String getNotificationResourceType() {
    return notificationResourceType;
  }

  public void setNotificationResourceType(String notificationResourceType) {
    this.notificationResourceType = notificationResourceType;
  }

  public String getNotificationData() {
    return notificationData;
  }

  public void setNotificationData(String notificationData) {
    this.notificationData = notificationData;
  }

  public String getEhrAccessToken() {
    return ehrAccessToken;
  }

  public void setEhrAccessToken(String ehrAccessToken) {
    this.ehrAccessToken = ehrAccessToken;
  }

  public int getEhrAccessTokenExpiryDuration() {
    return ehrAccessTokenExpiryDuration;
  }

  public void setEhrAccessTokenExpiryDuration(int ehrAccessTokenExpiryDuration) {
    this.ehrAccessTokenExpiryDuration = ehrAccessTokenExpiryDuration;
  }

  public Date getEhrAccessTokenExpirationTime() {
    return ehrAccessTokenExpirationTime;
  }

  public void setEhrAccessTokenExpirationTime(Date ehrAccessTokenExpirationTime) {
    this.ehrAccessTokenExpirationTime = ehrAccessTokenExpirationTime;
  }

  public Resource getNotifiedResource() {
    return notifiedResource;
  }

  public void setNotifiedResource(Resource notifiedResource) {
    this.notifiedResource = notifiedResource;
  }

  public Date getEncounterStartTime() {
    return encounterStartTime;
  }

  public void setEncounterStartTime(Date encounterStartTime) {
    this.encounterStartTime = encounterStartTime;
  }

  public String getNotificationProcessingStatus() {
    return notificationProcessingStatus;
  }

  public void setNotificationProcessingStatus(String notificationProcessingStatus) {
    this.notificationProcessingStatus = notificationProcessingStatus;
  }

  public Date getEncounterEndTime() {
    return encounterEndTime;
  }

  public void setEncounterEndTime(Date encounterEndTime) {
    this.encounterEndTime = encounterEndTime;
  }

  public String getThrottleContext() {
    return throttleContext;
  }

  public void setThrottleContext(String throttleContext) {
    this.throttleContext = throttleContext;
  }

  public String getEncounterClass() {
    return encounterClass;
  }

  public void setEncounterClass(String encounterclass) {
    this.encounterClass = encounterClass;
  }

  public String getRelaunchNotificationData() {
    return relaunchNotificationData;
  }

  public void setRelaunchNotificationData(String relaunchNotificationData) {
    this.relaunchNotificationData = relaunchNotificationData;
  }

  public String getEhrLaunchContext() {
    return ehrLaunchContext;
  }

  public void setEhrLaunchContext(String ehrLaunchContext) {
    this.ehrLaunchContext = ehrLaunchContext;
  }
}
