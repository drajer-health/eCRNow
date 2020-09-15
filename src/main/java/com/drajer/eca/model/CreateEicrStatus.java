package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;

public class CreateEicrStatus extends EicrStatus {

  private String actionId;
  private EventTypes.JobStatus jobStatus;
  private Boolean eicrCreated;
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

  public Boolean getEicrCreated() {
    return eicrCreated;
  }

  public void setEicrCreated(Boolean eicrCreated) {
    this.eicrCreated = eicrCreated;
  }

  public String geteICRId() {
    return eICRId;
  }

  public void seteICRId(String eICRId) {
    this.eICRId = eICRId;
  }

  public CreateEicrStatus() {

    actionId = "";
    jobStatus = JobStatus.NOT_STARTED;
    eicrCreated = false;
    eICRId = "";
  }
}
