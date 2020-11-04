package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.EcrActionTypes;

public class ActionFactory {

  private ActionFactory() {}

  public static AbstractAction getActionForType(EcrActionTypes type) {

    // We could use Supplier Interfaces in Java 8 ..but it may not be required because these actions
    // do not change that often.
    if (type == EcrActionTypes.MATCH_TRIGGER) {
      return new MatchTriggerAction();
    } else if (type == EcrActionTypes.CREATE_EICR) {
      return new CreateEicrAction();
    } else if (type == EcrActionTypes.CLOSE_OUT_EICR) {
      return new CloseOutEicrAction();
    } else if (type == EcrActionTypes.PERIODIC_UPDATE_EICR) {
      return new PeriodicUpdateEicrAction();
    } else if (type == EcrActionTypes.VALIDATE_EICR) {
      return new ValidateEicrAction();
    } else if (type == EcrActionTypes.SUBMIT_EICR) {
      return new SubmitEicrAction();
    }

    return null;
  }
}
