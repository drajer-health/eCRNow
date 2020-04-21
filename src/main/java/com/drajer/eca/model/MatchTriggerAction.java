package com.drajer.eca.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MatchTriggerAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(MatchTriggerAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing MatchTriggerAction **** ");
		printBase();
		logger.info(" **** End Printing MatchTriggerAction **** ");
	}
	
	@Override
	public void execute(Object obj) {
		// TODO Auto-generated method stub

	}

}
