package com.drajer.eca.model;

import java.util.List;

public class PatientActionStatus {

  private String patiendId;

  List<ActionStatus> actionStatus;

  public String getPatiendId() {
    return patiendId;
  }

  public void setPatiendId(String patiendId) {
    this.patiendId = patiendId;
  }

  public List<ActionStatus> getActionStatus() {
    return actionStatus;
  }

  public void setActionStatus(List<ActionStatus> actionStatus) {
    this.actionStatus = actionStatus;
  }
}
