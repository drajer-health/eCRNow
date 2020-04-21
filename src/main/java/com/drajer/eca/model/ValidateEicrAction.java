package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValidateEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(ValidateEicrAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing ValidateEicrAction **** ");
		printBase();
		logger.info(" **** End Printing ValidateEicrAction **** ");
	}
	@Override
	public void execute(Object obj) {
		// TODO Auto-generated method stub

	}

}
