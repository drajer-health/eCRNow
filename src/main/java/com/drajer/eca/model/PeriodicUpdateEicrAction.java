package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PeriodicUpdateEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(PeriodicUpdateEicrAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing PeriodicUpdateEicrAction **** ");
		printBase();
		logger.info(" **** End Printing PeriodicUpdateEicrAction **** ");
	}
	@Override
	public void execute(Object obj) {
		// TODO Auto-generated method stub

		logger.info(" Executing Periodic Update Action ");
	}

}
