package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import java.io.Serializable;
import java.time.Instant;

public class TaskTimer implements Serializable {

  private static final long serialVersionUID = 1L;
  Long id;
  Integer launchDetailsId;
  EcrActionTypes actionTypes;
  Instant t;

  public TaskTimer(Long id1, Integer launchDetailsId, EcrActionTypes actionTypes1, Instant t) {
    this.id = id1;
    this.launchDetailsId = launchDetailsId;
    this.actionTypes = actionTypes1;
    this.t = t;
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Integer getLaunchDetailsId() {
    return launchDetailsId;
  }

  public void setLaunchDetailsId(Integer launchDetailsId) {
    this.launchDetailsId = launchDetailsId;
  }

  public EcrActionTypes getActionTypes() {
    return actionTypes;
  }

  public void setActionTypes(EcrActionTypes actionTypes) {
    this.actionTypes = actionTypes;
  }

  public Instant getT() {
    return t;
  }

  public void setT(Instant t) {
    this.t = t;
  }
}
