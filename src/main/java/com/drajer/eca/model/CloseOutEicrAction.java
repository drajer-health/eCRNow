package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CloseOutEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(CloseOutEicrAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing CloseOutEicrAction **** ");
		printBase();
		logger.info(" **** End Printing CloseOutEicrAction **** ");
	}
	
	@Override
	public void execute(Object obj) {
		// TODO Auto-generated method stub

		logger.info(" Executing Close Out Eicr Action ");
	}
	
	
}
