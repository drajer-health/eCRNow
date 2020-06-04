package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CreateEicrAfterReheckAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(CreateEicrAfterReheckAction.class);
		
	@Override
	public void print() {
			
			logger.info(" **** Printing CreateEirAfterRecheckAction **** ");
			printBase();
			logger.info(" **** End Printing CreateEirAfterRecheckAction **** ");
	}
	
	@Override
	public void execute(Object obj) {
			// TODO Auto-generated method stub

			logger.info(" Executing Periodic Update Action ");
	}

}
