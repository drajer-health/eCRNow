package com.drajer.ecrapp.service;

import java.util.Set;

import javax.annotation.PostConstruct;

import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Service;

import com.drajer.eca.model.AbstractAction;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CreateEicrAction;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
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
	
	
	/*
	 *  These are other services that the action classes use and we are storing it once for all of them instead
	 *  of using it class variables which can be injected.
 	 *  We could do injection if we do not use NEW operator , but the ERSD processor will use new to create instead of Spring context, hence Autowired
 	 *   variables cannot be injected into this class or the action classes.
	 */
	@Autowired
	TriggerQueryService triggerQueryService;
	
	@Autowired
	LoadingQueryService loadingQueryService;
	
	@Autowired
	LaunchService 	    launchService;
	
	@Autowired
	ThreadPoolTaskScheduler taskScheduler;
	
	@Autowired
	ObjectMapper		mapper;
	
	@PostConstruct
	public void initializeActionRepo() {
		ActionRepo.getInstance().setLoadingQueryService(loadingQueryService);
		ActionRepo.getInstance().setTriggerQueryService(triggerQueryService);
		ActionRepo.getInstance().setLaunchService(launchService);
		ActionRepo.getInstance().setTaskScheduler(taskScheduler);
	}

	public void handleWorkflowEvent(EventTypes.WorkflowEvent type, LaunchDetails details) {
		
		if(type == WorkflowEvent.SOF_LAUNCH) {
			
			// Identify the appropriate actions and execute it from the Action Repo.
			logger.info(" SOF Launch for Patient : " + details.getLaunchPatientId() + " and Encounter : " + details.getEncounterId());
			
			// Setup Execution State.
			PatientExecutionState oldstate = new PatientExecutionState(details.getLaunchPatientId(), details.getEncounterId());
			
			try {
				details.setStatus(mapper.writeValueAsString(oldstate));
			} catch (JsonProcessingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			logger.info("State = " + details.getStatus());
			
			
			// Use Action Repo to get the events that we need to fire.			
			ActionRepo repo = ActionRepo.getInstance();
						
			// Get Actions for Trigger Matching.
			if(repo.getActions() != null && 
					repo.getActions().containsKey(EcrActionTypes.MATCH_TRIGGER)) {
				
				Set<AbstractAction> actions = repo.getActions().get(EcrActionTypes.MATCH_TRIGGER);

				if(actions.size() > 0) {
					oldstate.getMatchTriggerStatus().setJobStatus(JobStatus.IN_PROGRESS);

					try {
						details.setStatus(mapper.writeValueAsString(oldstate));
						launchService.saveOrUpdate(details);
					} catch (JsonProcessingException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				
				
				for(AbstractAction act : actions) {					
					// Execute the event.
					act.execute(details);
				}
				
				// Update state for next action
				launchService.saveOrUpdate(details);
			}
			
			// Get Actions for Creation.
			if(repo.getActions() != null && 
					repo.getActions().containsKey(EcrActionTypes.CREATE_EICR)) {
				
				Set<AbstractAction> actions = repo.getActions().get(EcrActionTypes.CREATE_EICR);

				for(AbstractAction act : actions) {					
					// Execute the event.
					act.execute(details);
				}
				
				// Update state for next action
				launchService.saveOrUpdate(details);
			}
			
			
			
			// Get Actions for Close out.
			
			// Get Actions for Validation.
			
			// Get Actions for Submission.
			
			
			// kickofff Data Changed triggers
			/* 
			 * We can use the logic below to make it a generic pipeline for event processing.
			 * For now implement the above order of trigger matching, creation and close out.
			 * 
			 * if(repo.getActionsByTriggers() != null && 
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
			} */
			
			
			// Till the above is fixed..just invoke the act.
			//act.execute(details);
			
			
			
		}
		else if(type == WorkflowEvent.SUBSCRIPTION_NOTIFICATION) {
			
			// Do nothing for now.
			
		}
		
	}
}
