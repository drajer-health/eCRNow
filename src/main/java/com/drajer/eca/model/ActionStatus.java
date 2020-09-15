package com.drajer.eca.model;

import java.math.BigDecimal;
import java.util.Date;

public class ActionStatus {

  private String actionId;
  private EventTypes.JobStatus status;
  private Date actionStartTime;
  private BigDecimal nextJobExpirationTime;
  private String units;

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public EventTypes.JobStatus getStatus() {
    return status;
  }

  public void setStatus(EventTypes.JobStatus status) {
    this.status = status;
  }

  public Date getActionStartTime() {
    return actionStartTime;
  }

  public void setActionStartTime(Date actionStartTime) {
    this.actionStartTime = actionStartTime;
  }

  public BigDecimal getNextJobExpirationTime() {
    return nextJobExpirationTime;
  }

  public void setNextJobExpirationTime(BigDecimal nextJobExpirationTime) {
    this.nextJobExpirationTime = nextJobExpirationTime;
  }

  public String getUnits() {
    return units;
  }

  public void setUnits(String units) {
    this.units = units;
  }
}
