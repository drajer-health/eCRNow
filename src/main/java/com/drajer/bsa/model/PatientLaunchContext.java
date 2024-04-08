package com.drajer.bsa.model;

import java.util.HashMap;
import java.util.Map;

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

  /**
   * This is used to store the launch context that is to be passed as part of the EHR queries
   * executed by the App. The context can be used by the EHR or its proxy to handle multi-tenancy
   * tenant id, security and other types of requirements such as throttling parameters.
   */
  private Map<String, String> ehrLaunchContext;

  /**
   * This is to be used to check with the EHR on whether all the timers can be run or if they need
   * to be throttled. This context parameter is something that is passed by the EHR vendor to be
   * stored and then it will be passed back to the EHR for checking if throttling is required. So
   * for e.g based on the size of the infrastruture an EHR may decide to allow 10 parallel requests.
   * This context parameter may indicate the number of requests or the size of the infrastructure
   * (like small/medium/large) etc using which the EHR may make a decision on whether to allow a
   * timer to execute or not.
   */
  private String throttleContext;

  public PatientLaunchContext() {
    ehrLaunchContext = new HashMap<>();
  }

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

  public String getThrottleContext() {
    return throttleContext;
  }

  public void setThrottleContext(String throttleContext) {
    this.throttleContext = throttleContext;
  }

  public Map<String, String> getEhrLaunchContext() {
    return ehrLaunchContext;
  }

  public void setEhrLaunchContext(Map<String, String> ehrLaunchContext) {
    this.ehrLaunchContext = ehrLaunchContext;
  }
}
