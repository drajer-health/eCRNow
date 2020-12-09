package com.drajer.ecrapp.model;

import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;

@Entity
@Table(name = "eicr")
@DynamicUpdate
public class Eicr {

  public static final String MDN_RESPONSE_TYPE = "MDN";
  public static final String RR_RESPONSE_TYPE = "RR";

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  @Column(name = "x_req_id", nullable = true, columnDefinition = "TEXT")
  private String xRequestId;

  @Column(name = "eicr_data", nullable = true, columnDefinition = "TEXT")
  private String eicrData;

  @Column(name = "response_type", nullable = true, columnDefinition = "TEXT")
  private String responseType;

  @Column(name = "response_id", nullable = true, columnDefinition = "TEXT")
  private String responseId;

  @Column(name = "rr_data", nullable = true, columnDefinition = "TEXT")
  private String responseData;

  @Column(name = "fhir_server_url", nullable = true, columnDefinition = "TEXT")
  private String fhirServerUrl;

  @Column(name = "launch_patient_id", nullable = true, columnDefinition = "TEXT")
  private String launchPatientId;

  @Column(name = "encounter_id", nullable = true, columnDefinition = "TEXT")
  private String encounterId;

  @Column(name = "set_id", nullable = true, columnDefinition = "INTEGER")
  private Integer setId;

  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public String getxRequestId() {
    return xRequestId;
  }

  public void setxRequestId(String xRequestId) {
    this.xRequestId = xRequestId;
  }

  public String getEicrData() {
    return eicrData;
  }

  public void setEicrData(String eicrData) {
    this.eicrData = eicrData;
  }

  public String getResponseType() {
    return responseType;
  }

  public void setResponseType(String responseType) {
    this.responseType = responseType;
  }

  public String getResponseId() {
    return responseId;
  }

  public void setResponseId(String responseId) {
    this.responseId = responseId;
  }

  public String getResponseData() {
    return responseData;
  }

  public void setResponseData(String responseData) {
    this.responseData = responseData;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public String getFhirServerUrl() {
    return fhirServerUrl;
  }

  public void setFhirServerUrl(String fhirServerUrl) {
    this.fhirServerUrl = fhirServerUrl;
  }

  public String getLaunchPatientId() {
    return launchPatientId;
  }

  public void setLaunchPatientId(String launchPatientId) {
    this.launchPatientId = launchPatientId;
  }

  public String getEncounterId() {
    return encounterId;
  }

  public void setEncounterId(String encounterId) {
    this.encounterId = encounterId;
  }

  public Integer getSetId() {
    return setId;
  }

  public void setSetId(Integer setId) {
    this.setId = setId;
  }
}
