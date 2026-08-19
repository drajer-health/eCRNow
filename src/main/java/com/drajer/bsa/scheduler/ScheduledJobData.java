package com.drajer.bsa.scheduler;

import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.BsaJobType;
import java.io.Serializable;
import java.time.Instant;
import java.util.Map;
import java.util.UUID;

public class ScheduledJobData implements Serializable {

  static final long serialVersionUID = 1403267933737660088L;

  private UUID karExecutionStateId;
  private String actionId;
  private BsaTypes.ActionType actionType;
  private String jobId;
  private transient Instant expirationTime;
  private Map<String, String> mdcContext;
  private String xRequestId;
  private BsaJobType jobType;

  private ScheduledJobData(Builder builder) {
    karExecutionStateId = builder.karExecutionStateId;
    actionId = builder.actionId;
    actionType = builder.actionType;
    expirationTime = builder.expirationTime;
    jobId = builder.jobId;
    xRequestId = builder.xRequestId;
    jobType = builder.jobType;
    mdcContext = builder.mdcContext;
  }

  public static class Builder {
    private UUID karExecutionStateId;
    private String actionId;
    private BsaTypes.ActionType actionType;
    private Instant expirationTime;
    private String jobId;
    private String xRequestId;
    private BsaJobType jobType;
    private Map<String, String> mdcContext;

    public Builder karExecutionStateId(UUID id) {
      this.karExecutionStateId = id;
      return this;
    }

    public Builder actionId(String action) {
      this.actionId = action;
      return this;
    }

    public Builder actionType(BsaTypes.ActionType type) {
      this.actionType = type;
      return this;
    }

    public Builder expirationTime(Instant t) {
      this.expirationTime = t;
      return this;
    }

    public Builder jobId(String job) {
      this.jobId = job;
      return this;
    }

    public Builder xRequestId(String xReqId) {
      this.xRequestId = xReqId;
      return this;
    }

    public Builder jobType(BsaJobType jobtype) {
      this.jobType = jobtype;
      return this;
    }

    public Builder mdcContext(Map<String, String> mdc) {
      this.mdcContext = mdc;
      return this;
    }

    public ScheduledJobData build() {
      return new ScheduledJobData(this);
    }
  }

  public UUID getKarExecutionStateId() {
    return karExecutionStateId;
  }

  public void setKarExecutionStateId(UUID karExecutionStateId) {
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

  public Map<String, String> getMdcContext() {
    return mdcContext;
  }

  public void setMdcContext(Map<String, String> mdcContext) {
    this.mdcContext = mdcContext;
  }

  public String getxRequestId() {
    return xRequestId;
  }

  public void setxRequestId(String xRequestId) {
    this.xRequestId = xRequestId;
  }

  public BsaJobType getJobType() {
    return jobType;
  }

  public void setJobType(BsaJobType jobType) {
    this.jobType = jobType;
  }
}
