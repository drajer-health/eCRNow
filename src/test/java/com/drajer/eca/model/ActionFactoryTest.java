package com.drajer.eca.model;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import org.junit.Test;

public class ActionFactoryTest {

  @Test
  public void testGetActionForType() {
    assertTrue(
        ActionFactory.getActionForType(EcrActionTypes.MATCH_TRIGGER) instanceof MatchTriggerAction);
    assertTrue(
        ActionFactory.getActionForType(EcrActionTypes.CREATE_EICR) instanceof CreateEicrAction);
    assertTrue(
        ActionFactory.getActionForType(EcrActionTypes.CLOSE_OUT_EICR)
            instanceof CloseOutEicrAction);
    assertTrue(
        ActionFactory.getActionForType(EcrActionTypes.PERIODIC_UPDATE_EICR)
            instanceof PeriodicUpdateEicrAction);
    assertTrue(
        ActionFactory.getActionForType(EcrActionTypes.VALIDATE_EICR) instanceof ValidateEicrAction);
    assertTrue(
        ActionFactory.getActionForType(EcrActionTypes.SUBMIT_EICR) instanceof SubmitEicrAction);

    assertNull(ActionFactory.getActionForType(null));
  }
}
