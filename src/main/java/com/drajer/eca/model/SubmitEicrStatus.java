package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;
import java.util.Date;

public class SubmitEicrStatus extends EicrStatus {

  private String actionId;
  private EventTypes.JobStatus jobStatus;
  private Boolean eicrSubmitted;
  private String eICRId;
  private Date submittedTime;
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

  public Boolean getEicrSubmitted() {
    return eicrSubmitted;
  }

  public void setEicrSubmitted(Boolean eicrSubmitted) {
    this.eicrSubmitted = eicrSubmitted;
  }

  public String geteICRId() {
    return eICRId;
  }

  public void seteICRId(String eICRId) {
    this.eICRId = eICRId;
  }

  public Date getSubmittedTime() {
    return submittedTime;
  }

  public void setSubmittedTime(Date submittedTime) {
    this.submittedTime = submittedTime;
  }

  public String getTransportUsed() {
    return transportUsed;
  }

  public void setTransportUsed(String transportUsed) {
    this.transportUsed = transportUsed;
  }

  public SubmitEicrStatus() {
    actionId = "";
    jobStatus = JobStatus.NOT_STARTED;
    eicrSubmitted = false;
    eICRId = "";
  }
}
