package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;

public class CloseOutEicrStatus extends EicrStatus {

  private String actionId;
  private EventTypes.JobStatus jobStatus;
  private Boolean eicrClosed;
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

  public Boolean getEicrClosed() {
    return eicrClosed;
  }

  public void setEicrClosed(Boolean eicrClosed) {
    this.eicrClosed = eicrClosed;
  }

  public String geteICRId() {
    return eICRId;
  }

  public void seteICRId(String eICRId) {
    this.eICRId = eICRId;
  }

  public CloseOutEicrStatus() {
    actionId = "";
    jobStatus = JobStatus.NOT_STARTED;
    eicrClosed = false;
    eICRId = "";
  }
}
