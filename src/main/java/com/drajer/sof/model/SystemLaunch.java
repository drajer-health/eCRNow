package com.drajer.sof.model;

public class SystemLaunch {

  private String fhirServerURL;
  private String patientId;
  private String encounterId;
  private String requestMode;
  private Boolean validationMode;

  public String getFhirServerURL() {
    return fhirServerURL;
  }

  public void setFhirServerURL(String fhirServerURL) {
    this.fhirServerURL = fhirServerURL;
  }

  public String getPatientId() {
    return patientId;
  }

  public void setPatientId(String patientId) {
    this.patientId = patientId;
  }

  public String getEncounterId() {
    return encounterId;
  }

  public void setEncounterId(String encounterId) {
    this.encounterId = encounterId;
  }

  public String getRequestMode() {
    return requestMode;
  }

  public void setRequestMode(String requestMode) {
    this.requestMode = requestMode;
  }

  public Boolean getValidationMode() {
    return validationMode;
  }

  public void setValidationMode(Boolean validationMode) {
    this.validationMode = validationMode;
  }
}
