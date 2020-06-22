package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.eca.model.EventTypes.WorkflowEvent;

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
			// TODO Auto-generated method stub

			logger.info(" Executing Periodic Update Action ");
	}

}
