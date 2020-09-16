package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.WorkflowEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CreateEicrAfterRecheckAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(CreateEicrAfterRecheckAction.class);

  @Override
  public void print() {

    logger.info(" **** Printing CreateEirAfterRecheckAction **** ");
    printBase();
    logger.info(" **** End Printing CreateEirAfterRecheckAction **** ");
  }

  @Override
  public void execute(Object obj, WorkflowEvent launchType) {

    logger.info(" Executing Periodic Update Action ");
  }
}
