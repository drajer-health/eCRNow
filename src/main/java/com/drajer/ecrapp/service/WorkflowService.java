package com.drajer.ecrapp.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CreateEicrAction;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.sof.model.LaunchDetails;

public class WorkflowService {
	
	private final static Logger logger = LoggerFactory.getLogger(WorkflowService.class);

	public static void handleWorkflowEvent(EventTypes.WorkflowEvent type, LaunchDetails details) {
		
		if(type == WorkflowEvent.SOF_LAUNCH) {
			
			// Identify the appropriate actions and execute it from the Action Repo.
			logger.info(" SOF Launch for Patient : " + details.getLaunchPatientId() + " and Encounter : " + details.getEncounterId());
			
			// Use Event Repo after we fix the code.
			
			CreateEicrAction act = new CreateEicrAction();
			
			act.execute(details);
			
			
		}
		else if(type == WorkflowEvent.SUBSCRIPTION_NOTIFICATION) {
			
			// Do nothing for now.
			
		}
		
	}
}
