package com.drajer.bsa.kar.action;

import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import java.util.HashSet;
import java.util.Set;

/**
 *
 *
 * <h1>BsaActionStatus</h1>
 *
 * The class provides the common attributes required for tracking all action status.
 *
 * @author nbashyam
 */
public abstract class BsaActionStatus {

  /** The action to which the instance belongs to. */
  String actionId;

  /** The status of the action. */
  BsaActionStatusType actionStatus;

  /** The set of outputs produced identified using the Ids of the output objects. */
  Set<String> outputProduced;

  public BsaActionStatus() {
    actionId = "";
    actionStatus = BsaActionStatusType.NotStarted;
    outputProduced = new HashSet<>();
  }

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public BsaActionStatusType getActionStatus() {
    return actionStatus;
  }

  public void setActionStatus(BsaActionStatusType actionStatus) {
    this.actionStatus = actionStatus;
  }

  public Set<String> getOutputProduced() {
    return outputProduced;
  }

  public void setOutputProduced(Set<String> outputProduced) {
    this.outputProduced = outputProduced;
  }
}
