package com.drajer.ecrapp.service;

import java.util.Set;

import javax.annotation.PostConstruct;

import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.drajer.eca.model.AbstractAction;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CreateEicrAction;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service 
public class WorkflowService {
	
	private final Logger logger = LoggerFactory.getLogger(WorkflowService.class);
	
	@Autowired
	CreateEicrAction     act;
	
	@Autowired
	TriggerQueryService triggerQueryService;
	
	@Autowired
	LoadingQueryService loadingQueryService;
	
	@Autowired
	LaunchService 	    launchService;
	
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
			
			PatientExecutionState oldstate = new PatientExecutionState(details.getLaunchPatientId(), details.getEncounterId());
			ObjectMapper mapper = new ObjectMapper();
			
			try {
				details.setStatus(mapper.writeValueAsString(oldstate));
			} catch (JsonProcessingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			logger.info("State = " + details.getStatus());
			
			ActionRepo repo = ActionRepo.getInstance();
			// kickofff Data Changed triggers
			if(repo.getActionsByTriggers() != null && 
			   repo.getActionsByTriggers().containsKey(TriggerType.DATACHANGED) && 
			   repo.getActionsByTriggers().get(TriggerType.DATACHANGED) != null) {
				
				Set<AbstractAction> acts = repo.getActionsByTriggers().get(TriggerType.DATACHANGED);
				logger.info(" Found Actions for Data Changed " + acts.size());
				
				for(AbstractAction aa : acts) {
					
					// Execute the action
					aa.execute(details);
					launchService.saveOrUpdate(details);
					
				}
			}
					
			// kickoff Data Added triggers
			if(repo.getActionsByTriggers() != null && 
					repo.getActionsByTriggers().containsKey(TriggerType.DATAADDED) && 
					repo.getActionsByTriggers().get(TriggerType.DATAADDED) != null) {

				Set<AbstractAction> acts = repo.getActionsByTriggers().get(TriggerType.DATAADDED);
				logger.info(" Found Actions for Data Added " + acts.size());

				for(AbstractAction aa : acts) {

					// Execute the action
					aa.execute(details);
					launchService.saveOrUpdate(details);

				}
			}
			
			
			// Till the above is fixed..just invoke the act.
			act.execute(details);
			
			
			
		}
		else if(type == WorkflowEvent.SUBSCRIPTION_NOTIFICATION) {
			
			// Do nothing for now.
			
		}
		
	}
}
