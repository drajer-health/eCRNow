package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;

public class PeriodicUpdateEicrStatus extends EicrStatus {

  private String actionId;
  private EventTypes.JobStatus jobStatus;
  private Boolean eicrUpdated;
  private String eICRId;

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

  public Boolean getEicrUpdated() {
    return eicrUpdated;
  }

  public void setEicrUpdated(Boolean eicrUpdated) {
    this.eicrUpdated = eicrUpdated;
  }

  public String geteICRId() {
    return eICRId;
  }

  public void seteICRId(String eICRId) {
    this.eICRId = eICRId;
  }

  public PeriodicUpdateEicrStatus() {
    actionId = "";
    jobStatus = JobStatus.NOT_STARTED;
    eicrUpdated = false;
    eICRId = "";
  }
}
