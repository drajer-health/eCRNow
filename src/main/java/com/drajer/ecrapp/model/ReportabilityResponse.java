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
@Table(name = "reportability_response")
@DynamicUpdate
public class ReportabilityResponse {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  @Column(name = "rr_data", nullable = true, columnDefinition = "TEXT")
  private String rrData;

  @Column(name = "rr_type", nullable = true, columnDefinition = "TEXT")
  private String rrType;

  @Column(name = "x_req_id", nullable = true, columnDefinition = "TEXT")
  private String xRequestId;

  @Column(name = "fhir_server_url", nullable = true, columnDefinition = "TEXT")
  private String fhirServerUrl;

  @Column(name = "launch_patient_id", nullable = true, columnDefinition = "TEXT")
  private String launchPatientId;

  @Column(name = "encounter_id", nullable = true, columnDefinition = "TEXT")
  private String encounterId;

  @Column(name = "set_id", nullable = true, columnDefinition = "TEXT")
  private String setId;

  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public String getRrData() {
    return rrData;
  }

  public void setRrData(String rrData) {
    this.rrData = rrData;
  }

  public String getRrType() {
    return rrType;
  }

  public void setRrType(String rrType) {
    this.rrType = rrType;
  }

  public String getxRequestId() {
    return xRequestId;
  }

  public void setxRequestId(String xRequestId) {
    this.xRequestId = xRequestId;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }
}
