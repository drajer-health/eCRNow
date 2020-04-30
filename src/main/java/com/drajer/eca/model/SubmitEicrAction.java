package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SubmitEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(SubmitEicrAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing SubmitEicrAction **** ");
		printBase();
		logger.info(" **** End Printing SubmitEicrAction **** ");
	}
	@Override
	public void execute(Object obj) {
		// TODO Auto-generated method stub

		logger.info(" Executing Submit Eicr Action ");
		
	}

}
