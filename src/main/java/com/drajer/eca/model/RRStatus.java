package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;
import java.util.Date;

public class RRStatus extends EicrStatus {

  private String actionId;
  private EventTypes.JobStatus jobStatus;
  private Boolean rrObtained;
  private String eICRId;
  private Date rrTime;
  private String transportUsed;

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public EventTypes.JobStatus getJobStatus() {
    return jobStatus;
  }

  public void setJobStatus(EventTypes.JobStatus jobStatus) {
    this.jobStatus = jobStatus;
  }

  public String geteICRId() {
    return eICRId;
  }

  public void seteICRId(String eICRId) {
    this.eICRId = eICRId;
  }

  public String getTransportUsed() {
    return transportUsed;
  }

  public void setTransportUsed(String transportUsed) {
    this.transportUsed = transportUsed;
  }

  public Boolean getRrObtained() {
    return rrObtained;
  }

  public void setRrObtained(Boolean rrObtained) {
    this.rrObtained = rrObtained;
  }

  public Date getRrTime() {
    return rrTime;
  }

  public void setRrTime(Date rrTime) {
    this.rrTime = rrTime;
  }

  public RRStatus() {
    actionId = "";
    jobStatus = JobStatus.NOT_STARTED;
    rrObtained = false;
    eICRId = "";
  }
}
