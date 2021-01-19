package com.drajer.ecrapp.model;

public class ReportabilityResponse {

  private String rrXml;
  private String responseType;

  public String getRrXml() {
    return rrXml;
  }

  public void setRrXml(String rrXml) {
    this.rrXml = rrXml;
  }

  public String getResponseType() {
    return responseType;
  }

  public void setResponseType(String type) {
    this.responseType = type;
  }
}
