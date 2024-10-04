package com.drajer.sof.model;

import java.util.UUID;

public class PublicHealthMessageData {

  private UUID id;

  private String fhirServerBaseUrl;

  private String notifiedResourceId;
  private String patientId;

  private Integer submittedVersionNumber;

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

  public String getNotifiedResourceId() {
    return notifiedResourceId;
  }

  public void setNotifiedResourceId(String notifiedResourceId) {
    this.notifiedResourceId = notifiedResourceId;
  }

  public String getPatientId() {
    return patientId;
  }

  public void setPatientId(String patientId) {
    this.patientId = patientId;
  }

  public Integer getSubmittedVersionNumber() {
    return submittedVersionNumber;
  }

  public void setSubmittedVersionNumber(Integer submittedVersionNumber) {
    this.submittedVersionNumber = submittedVersionNumber;
  }
}
