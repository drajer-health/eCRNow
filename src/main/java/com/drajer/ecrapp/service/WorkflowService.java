package com.drajer.ecrapp.service;

import java.time.Instant;
import java.util.Date;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.TriggerDefinition.TriggerType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Service;

import com.drajer.eca.model.AbstractAction;
import com.drajer.eca.model.ActionFactory;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.CreateEicrAction;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.routing.impl.DirectEicrSender;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service 
public class WorkflowService {
	
	private final Logger logger = LoggerFactory.getLogger(WorkflowService.class);
	private final static Logger logger2 = LoggerFactory.getLogger(WorkflowService.class);
	private static WorkflowService workflowInstance = null;
	
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
	EicrRRService			eicrRRService;
	
	@Autowired
	DirectEicrSender		directTansport;
	
	@Autowired
	ObjectMapper		mapper;
	
	@Value("${schematron.file.location}")
	String schematronFileLocation;
	
	@PostConstruct
	public void initializeActionRepo() {
		ActionRepo.getInstance().setLoadingQueryService(loadingQueryService);
		ActionRepo.getInstance().setTriggerQueryService(triggerQueryService);
		ActionRepo.getInstance().setLaunchService(launchService);
		ActionRepo.getInstance().setTaskScheduler(taskScheduler);
		ActionRepo.getInstance().setEicrRRService(eicrRRService);
		ActionRepo.getInstance().setSchematronFileLocation(schematronFileLocation);
		ActionRepo.getInstance().setDirectTransport(directTansport);
		workflowInstance = this;
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
			executeEicrWorkflow(details);
			
		
			// kickofff Data Changed triggers  -- This can be used in the future for developing a more subscription based launch.
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
	
	public void executeEicrWorkflow(LaunchDetails details) {
		
		logger.info(" ***** START EXECUTING EICR WORKFLOW ***** ");
		
		ActionRepo repo = ActionRepo.getInstance();
		executeActionsForType(details,EcrActionTypes.MATCH_TRIGGER);
		executeActionsForType(details,EcrActionTypes.CREATE_EICR);			
		executeActionsForType(details,EcrActionTypes.CLOSE_OUT_EICR);	
		executeActionsForType(details,EcrActionTypes.VALIDATE_EICR);	
		executeActionsForType(details,EcrActionTypes.SUBMIT_EICR);
		
		logger.info(" ***** END EXECUTING EICR WORKFLOW ***** ");
		
	}
	
	public void executeActions(LaunchDetails details, Set<AbstractAction> actions) {
		
		for(AbstractAction act : actions) {					
			// Execute the event.
			act.execute(details);
		}
		
		// Update state for next action
		launchService.saveOrUpdate(details);
		
	}
	
	public void executeActionsForType(LaunchDetails details, EcrActionTypes type) {
		
		ActionRepo repo = ActionRepo.getInstance();
		
		// Get Actions for Trigger Matching.
		if(repo.getActions() != null && 
				repo.getActions().containsKey(type)) {
			
			executeActions(details, repo.getActions().get(type));
			
		}
	}
	
	public void executeScheduledAction(Integer launchDetailsId, EcrActionTypes actionType) {
		
		logger.info("Get Launch Details from Database for Id  : " + launchDetailsId + " for Action Type " + actionType + " and start execution ");
		
		LaunchDetails launchDetails = ActionRepo.getInstance().getLaunchService().getAuthDetailsById(launchDetailsId);
		
		executeActionsForType(launchDetails, actionType);
		
		// Execute the Eicr Workflow since a job executtion can unlock other dependencies and execute other jobs.
		executeEicrWorkflow(launchDetails);
		
		
	}
	
	class EicrActionExecuteJob implements Runnable {

		public Integer launchDetailsId;
		
		public EcrActionTypes actionType;

		public EicrActionExecuteJob(Integer launchDetailsId, EcrActionTypes actionType) {
			this.launchDetailsId = launchDetailsId;
			this.actionType = actionType;
		}

		@Override
		public void run() {
			try {
				
				executeScheduledAction(launchDetailsId, actionType);			
				logger.info("Starting the Thread");
				Thread.currentThread().interrupt();
			} catch (Exception e) {
				logger.info("Error in Getting Data=====>" + e.getMessage());
				e.printStackTrace();
			}
		}
	}

	public static void scheduleJob(Integer launchDetailsId, TimingSchedule ts, EcrActionTypes actionType) {
		
		Instant t = new Date().toInstant().plusSeconds(10);
		
		ActionRepo.getInstance().getTaskScheduler().schedule(workflowInstance.new EicrActionExecuteJob(launchDetailsId, actionType), new Date().toInstant().plusSeconds(10));
		
		logger2.info("Job Scheduled for Action to executate for : " + actionType + " at time : " + t.toString());
		
	}
	
	public static void scheduleJob(Integer launchDetailsId, Duration d, EcrActionTypes actionType) {
		
		
		Instant t = ApplicationUtils.convertDurationToInstant(d);
		
		final WorkflowService ws = new WorkflowService();
		ActionRepo.getInstance().getTaskScheduler().schedule(workflowInstance.new EicrActionExecuteJob(launchDetailsId, actionType), t);
		
		logger2.info("Job Scheduled for Action to executate for : " + actionType + " at time : " + t.toString());
	}
}
