package com.drajer.sof.model;

import java.util.UUID;

public class NotificationContextData {

  private UUID id;
  private String fhirServerBaseUrl;
  private String patientId;
  private String notificationResourceId;

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

  public String getNotificationResourceId() {
    return notificationResourceId;
  }

  public void setNotificationResourceId(String notificationResourceId) {
    this.notificationResourceId = notificationResourceId;
  }
}
