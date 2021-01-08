package com.drajer.ecrapp.model;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.WorkflowEvent;

public class WorkflowTask {

  private int launchId;
  private EcrActionTypes actionType;
  private WorkflowEvent workflowEvent;

  public int getLaunchId() {
    return launchId;
  }

  public void setLaunchId(int launchId) {
    this.launchId = launchId;
  }

  public EcrActionTypes getActionType() {
    return actionType;
  }

  public void setActionType(EcrActionTypes actionType) {
    this.actionType = actionType;
  }

  public WorkflowEvent getWorkflowEvent() {
    return workflowEvent;
  }

  public void setWorkflowEvent(WorkflowEvent workflowEvent) {
    this.workflowEvent = workflowEvent;
  }
}
