package com.drajer.bsa.model;

import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import java.util.List;

public class TriggerMatchExecution {

  public TriggerMatchExecution() {}

  private List<CheckTriggerCodeStatus> statuses;

  public List<CheckTriggerCodeStatus> getStatuses() {
    return statuses;
  }

  public void setStatuses(List<CheckTriggerCodeStatus> statuses) {
    this.statuses = statuses;
  }
}
