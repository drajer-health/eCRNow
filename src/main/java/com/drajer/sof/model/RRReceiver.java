package com.drajer.sof.model;

public class RRReceiver {

  private String fhirServerURL;
  private String patientId;
  private String encounterId;
  private String rrXml;
  private String type;

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

  public String getRrXml() {
    return rrXml;
  }

  public void setRrXml(String rrXml) {
    this.rrXml = rrXml;
  }

  public String getType() {
    return type;
  }

  public void setType(String type) {
    this.type = type;
  }
}
