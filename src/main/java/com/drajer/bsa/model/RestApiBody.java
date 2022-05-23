package com.drajer.bsa.model;

/**
 *
 *
 * <h1>RestApiBody</h1>
 *
 * The class holds the data that is exchanged with the REST API for submitting data such as Eicr.
 *
 * @author nbashyam
 */
public class RestApiBody {

  /** The FHIR Server for the EHR initiating the launch. */
  private String fhirServerURL;

  /** The patient who is the subject of the launch and whose data is evaluated. */
  private String patientId;

  /** The encounter which is the reason why the patient is being evaluated. */
  private String encounterId;

  /** This is the version of the report for the same patient and encounter */
  private Boolean submittedVersionId;

  /** The payload attribute that is going to be sent representing the data */
  private String payload;

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

  public Boolean getSubmittedVersionId() {
    return submittedVersionId;
  }

  public void setSubmittedVersionId(Boolean submittedVersionId) {
    this.submittedVersionId = submittedVersionId;
  }

  public String getPayload() {
    return payload;
  }

  public void setPayload(String payload) {
    this.payload = payload;
  }
}
