package com.drajer.bsa.model;

/**
 * This class is used to launch a Patient instance in the app whose data will be evaluated for
 * public health reporting.
 *
 * @author nbashyam
 */
public class PatientLaunchContext {

  /** The FHIR Server for the EHR initiating the launch. */
  private String fhirServerURL;

  /** The patient who is the subject of the launch and whose data is evaluated. */
  private String patientId;

  /** The encounter which is the reason why the patient is being evaluated. */
  private String encounterId;

  /** This is to by pass all timers and validate the processing of various timers inline. */
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

  public Boolean getValidationMode() {
    return validationMode;
  }

  public void setValidationMode(Boolean validationMode) {
    this.validationMode = validationMode;
  }
}
