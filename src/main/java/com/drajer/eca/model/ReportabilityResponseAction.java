package com.drajer.eca.model;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
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
				state = mapper.readValue(details.getStatus(), PatientExecutionState.class);			
			} catch (JsonMappingException e1) {
				
				String msg = "Unable to read/write execution state";
				logger.error(msg);
				throw new RuntimeException(msg);
				
			} catch (JsonProcessingException e1) {
				
				String msg = "Unable to read/write execution state";
				logger.error(msg);
				throw new RuntimeException(msg);
			}
	
			logger.info(" Executing RR Check Eicr Action , Prior Execution State : = " + details.getStatus());
			
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
									" Action " + actionId + " is not completed , hence this action has to wait ");
						}
						else {
							
							logger.info(" No related actions, so check all Eicrs that have been submitted ");
							
							Set<Integer> ids = state.getEicrsForRRCheck();
							
							checkRRForEicrs(details, state, ids);	
							
						}
					}
					else {
						logger.info(" This action is not dependent on the action relationship : " + ract.getRelationship() + ", Action Id = " + ract.getRelatedAction().getActionId());
					}
				}
			}
			else {
				
				logger.info(" No related actions, so check all Eicrs that have been submitted ");
				
				Set<Integer> ids = state.getEicrsForRRCheck();
				
				checkRRForEicrs(details, state, ids);			
				
			}
			
			try {
				details.setStatus(mapper.writeValueAsString(state));
			} catch (JsonProcessingException e) {
					
				String msg = "Unable to update execution state";
				logger.error(msg);
					
				throw new RuntimeException(msg);
			}
		}
		
	}

	public void checkRRForEicrs(LaunchDetails details, PatientExecutionState state, Set<Integer> ids) {
		
		for(Integer id : ids) {
			
			logger.info(" Found eICR with Id " + id  +" to check for RR ");
			
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
