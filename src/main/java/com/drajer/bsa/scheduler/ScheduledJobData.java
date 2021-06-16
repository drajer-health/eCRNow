package com.drajer.bsa.scheduler;

import com.drajer.bsa.model.BsaTypes;
import java.io.Serializable;
import java.time.Instant;

public class ScheduledJobData implements Serializable {

  Integer karExecutionStateId;
  String actionId;
  BsaTypes.ActionType actionType;
  transient Instant expirationTime;

  ScheduledJobData(Integer id, String action, BsaTypes.ActionType type, Instant t) {
    karExecutionStateId = id;
    actionId = action;
    actionType = type;
    expirationTime = t;
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
}
