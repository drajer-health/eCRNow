package com.drajer.bsa.scheduler;

import com.drajer.bsa.model.BsaTypes;
import java.io.Serializable;
import java.time.Instant;

public class ScheduledJobData implements Serializable {

  Integer 				karExecutionStateId;
  String 				actionId;
  BsaTypes.ActionType 	actionType;
  String  				jobId;
  transient Instant 	expirationTime;

  ScheduledJobData(Integer id, String action, BsaTypes.ActionType type, Instant t, String job) {
    karExecutionStateId = id;
    actionId = action;
    actionType = type;
    expirationTime = t;
    jobId = job;
  }

  public Integer getKarExecutionStateId() {
    return karExecutionStateId;
  }

  public void setKarExecutionStateId(Integer karExecutionStateId) {
    this.karExecutionStateId = karExecutionStateId;
  }

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public BsaTypes.ActionType getActionType() {
    return actionType;
  }

  public void setActionType(BsaTypes.ActionType actionType) {
    this.actionType = actionType;
  }

  public Instant getExpirationTime() {
    return expirationTime;
  }

  public void setExpirationTime(Instant expirationTime) {
    this.expirationTime = expirationTime;
  }

public String getJobId() {
	return jobId;
}

public void setJobId(String jobId) {
	this.jobId = jobId;
}
  
  
}
