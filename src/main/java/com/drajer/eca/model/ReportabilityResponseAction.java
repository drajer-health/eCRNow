package com.drajer.eca.model;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class ReportabilityResponseAction extends AbstractAction {
	
	private final Logger logger = LoggerFactory.getLogger(ReportabilityResponseAction.class);

	@Override
	public void print() {
		
		logger.info(" **** Printing SubmitEicrAction **** ");
		printBase();
		logger.info(" **** End Printing SubmitEicrAction **** ");

	}

	@Override
	public void execute(Object obj, WorkflowEvent launchType) {
		
		logger.info(" **** START Executing RR Check Eicr Action **** ");
		
		if (obj instanceof LaunchDetails) {
			  
			logger.info(" Obtained Launch Details ");
			LaunchDetails details = (LaunchDetails) obj;
			ObjectMapper mapper = new ObjectMapper();
			PatientExecutionState state = null;
	
			try {
				state = readObjectValue(mapper, details);		
			}  catch (JsonProcessingException e1) {
				
				String msg = "Unable to read/write execution state";
				handleException(e1, logger, msg);
			}
	
			logger.info(" Executing RR Check Eicr Action , Prior Execution State : = {}" , details.getStatus());
			
			String data = "This is a eICR report for patient with Encounter Id : " + details.getEncounterId();
			
			//ActionRepo.getInstance().getDirectTransport().sendData(details, data);
			
			if (getRelatedActions() != null && getRelatedActions().size() > 0) {
				
				logger.info(" Related Actions exist, so check dependencies ");
	
				List<RelatedAction> racts = getRelatedActions();
	
				for (RelatedAction ract : racts) {
	
					if (ract.getRelationship() == ActionRelationshipType.AFTER) {
	
						// check if the action is completed.
						String actionId = ract.getRelatedAction().getActionId();
	
						if (!state.hasActionCompleted(actionId)) {
	
							logger.info(
									" Action {} is not completed , hence this action has to wait ",actionId);
						}
						else {
							
							logger.info(" No related actions, so check all Eicrs that have been submitted ");
							
							Set<Integer> ids = state.getEicrsForRRCheck();
							
							checkRRForEicrs(details, state, ids);	
							
						}
					}
					else {
						logger.info(" This action is not dependent on the action relationship : {}, Action Id = {}" ,ract.getRelationship(),ract.getRelatedAction().getActionId());
					}
				}
			}
			else {
				
				logger.info(" No related actions, so check all Eicrs that have been submitted ");
				
				Set<Integer> ids = state.getEicrsForRRCheck();
				
				checkRRForEicrs(details, state, ids);			
				
			}
			
			updateExecutionState(details,mapper,state);
		}
		
	}

	public void checkRRForEicrs(LaunchDetails details, PatientExecutionState state, Set<Integer> ids) {
		
		for(Integer id : ids) {
			
			logger.info(" Found eICR with Id {} to check for RR ",id);
			
			// String data = ActionRepo.getInstance().getEicrRRService().get(id).getData();
							
			// Add a submission object every time.
			RRStatus submitState = new RRStatus();
			submitState.setActionId(getActionId());
			submitState.seteICRId(id.toString());
			submitState.setRrObtained(true);
			submitState.setJobStatus(JobStatus.COMPLETED);
			submitState.setRrTime(new Date());
			state.getRrStatus().add(submitState);
			  			  
		}
		
	}
}
