package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;
import java.util.Date;

public class ValidateEicrStatus extends EicrStatus {

  private String actionId;
  private EventTypes.JobStatus jobStatus;
  private Boolean eicrValidated;
  private String eICRId;
  private Date validationTime;

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

  public Boolean getEicrValidated() {
    return eicrValidated;
  }

  public void setEicrValidated(Boolean eicrValidated) {
    this.eicrValidated = eicrValidated;
  }

  public String geteICRId() {
    return eICRId;
  }

  public void seteICRId(String eICRId) {
    this.eICRId = eICRId;
  }

  public Date getValidationTime() {
    return validationTime;
  }

  public void setValidationTime(Date validationTime) {
    this.validationTime = validationTime;
  }

  public ValidateEicrStatus() {
    actionId = "";
    jobStatus = JobStatus.NOT_STARTED;
    eicrValidated = false;
    eICRId = "";
  }
}
