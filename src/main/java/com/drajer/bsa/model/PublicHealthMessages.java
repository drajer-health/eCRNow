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
 * <h1>PublicHealthMessages</h1>
 *
 * The class tracks the necessary outgoing and incoming messages from/to the BSA.
 *
 * @author nbashyam
 */
@Entity
@Table(name = "ph_messages")
@DynamicUpdate
public class PublicHealthMessages {

  @Transient private final Logger logger = LoggerFactory.getLogger(NotificationContext.class);

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id @GeneratedValue private UUID id;

  /** The attribute represents the base FHIR Server URL from which the notification was received. */
  @Column(name = "fhir_server_base_url", nullable = false, columnDefinition = "TEXT")
  private String fhirServerBaseUrl;

  /** The attribute represents the patient id for whom the trigger event was received. */
  @Column(name = "patient_id", nullable = false, columnDefinition = "TEXT")
  private String patientId;

  /** The attribute represents the id of the notification table which resulted in the messages. */
  @Column(name = "notification_id", nullable = false, columnDefinition = "TEXT")
  private String notificationId;

  /** This attribute is used to correlate across APIs and processes as the data moves along. */
  @Column(name = "correlation_id", nullable = true, columnDefinition = "TEXT")
  private String xCorrelationId;

  /** This attribute tracks the API Request Ids when present. */
  @Column(name = "x_request_id", nullable = true, columnDefinition = "TEXT")
  private String xRequestId;

  /** The attribute represents the submitted data to the TTP/PHA. */
  @Column(name = "notification_data", nullable = true, columnDefinition = "TEXT")
  private String submittedData;

  /** The attribute represents the submitted data to the TTP/PHA. */
  @Column(name = "response_data", nullable = true, columnDefinition = "TEXT")
  private String responseData;

  /** This attribute represents the last time when the object was updated. */
  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  public PublicHealthMessages() {}

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

  public String getSubmittedData() {
    return submittedData;
  }

  public void setSubmittedData(String submittedData) {
    this.submittedData = submittedData;
  }

  public String getResponseData() {
    return responseData;
  }

  public void setResponseData(String responseData) {
    this.responseData = responseData;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }
}
