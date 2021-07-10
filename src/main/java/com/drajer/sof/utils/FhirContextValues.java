package com.drajer.sof.utils;

public class FhirContextValues {

  private String resourceName;
  private String ehrServerURL;
  private String contentType;
  private String category;
  private String system;
  private String code;
  private String patientID;

  public String getCategory() {
    return category;
  }

  public void setCategory(String category) {
    this.category = category;
  }

  public String getPatientID() {
    return patientID;
  }

  public void setPatientID(String patientID) {
    this.patientID = patientID;
  }

  public String getResourceName() {
    return resourceName;
  }

  public void setResourceName(String resourceName) {
    this.resourceName = resourceName;
  }

  public String getEhrServerURL() {
    return ehrServerURL;
  }

  public void setEhrServerURL(String ehrServerURL) {
    this.ehrServerURL = ehrServerURL;
  }

  public String getContentType() {
    return contentType;
  }

  public void setContentType(String contentType) {
    this.contentType = contentType;
  }

  public void setSystem(String system) {
    this.system = system;
  }

  public String getSystem() {
    return system;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getCode() {
    return code;
  }
}
