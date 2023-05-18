package com.drajer.sof.model;

public class SystemLaunch {

  private String fhirServerURL;
  private String patientId;
  private String encounterId;
  private Boolean validationMode;

  private String organizationId;
  private String encounterStartDateTime;
  private String encounterEndDateTime;

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

  public Boolean getValidationMode() {
    return validationMode;
  }

  public void setValidationMode(Boolean validationMode) {
    this.validationMode = validationMode;
  }

  public String getOrganizationId() {
    return organizationId;
  }

  public void setOrganizationId(final String organizationId) {
    this.organizationId = organizationId;
  }

  public String getEncounterStartDateTime() {
    return encounterStartDateTime;
  }

  public void setEncounterStartDateTime(final String encounterStartDateTime) {
    this.encounterStartDateTime = encounterStartDateTime;
  }

  public String getEncounterEndDateTime() {
    return encounterEndDateTime;
  }

  public void setEncounterEndDateTime(final String encounterEndDateTime) {
    this.encounterEndDateTime = encounterEndDateTime;
  }
}
