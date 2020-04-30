package com.drajer.ecrapp.service;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CreateEicrAction;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;

@Service 
public class WorkflowService {
	
	private final Logger logger = LoggerFactory.getLogger(WorkflowService.class);
	
	@Autowired
	CreateEicrAction     act;
	
	@Autowired
	TriggerQueryService triggerQueryService;
	
	@Autowired
	LoadingQueryService loadingQueryService;
	
	@PostConstruct
	public void initializeActionRepo() {
		ActionRepo.getInstance().setLoadingQueryService(loadingQueryService);
		ActionRepo.getInstance().setTriggerQueryService(triggerQueryService);
	}

	public void handleWorkflowEvent(EventTypes.WorkflowEvent type, LaunchDetails details) {
		
		if(type == WorkflowEvent.SOF_LAUNCH) {
			
			// Identify the appropriate actions and execute it from the Action Repo.
			logger.info(" SOF Launch for Patient : " + details.getLaunchPatientId() + " and Encounter : " + details.getEncounterId());
			
			// Use Action Repo to get the events that we need to fire.
			ActionRepo repo = ActionRepo.getInstance();
			
			// kickofff Data Changed triggers
			
			
			
			
			// kickoff Data Added triggers
			
			
			// Till the above is fixed..just invoke the act.
			act.execute(details);
			
			
			
		}
		else if(type == WorkflowEvent.SUBSCRIPTION_NOTIFICATION) {
			
			// Do nothing for now.
			
		}
		
	}
}
