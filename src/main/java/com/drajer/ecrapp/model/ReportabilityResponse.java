package com.drajer.ecrapp.model;

public class ReportabilityResponse {

  public static final String MDN_RESPONSE_TYPE = "MDN";
  public static final String RR_RESPONSE_TYPE = "RR";

  private String rrXml;
  private String responseType;
  private String fhirUrl;

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

  public String getFhirUrl() {
    return fhirUrl;
  }

  public void setFhirUrl(String fhirUrl) {
    this.fhirUrl = fhirUrl;
  }
}
