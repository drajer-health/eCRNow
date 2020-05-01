package com.drajer.eca.model;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import com.drajer.cda.CdaEicrGenerator;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Patient;

@Service
public class CreateEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(CreateEicrAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing CreateEicrAction **** ");
		printBase();
		logger.info(" **** End Printing CreateEicrAction **** ");
	}
	
	@Override
	public void execute(Object obj) {

		logger.info(" Executing Match Trigger Action ");
		
		if(obj instanceof LaunchDetails) {
			
			logger.info(" Obtained Launch Details ");
			LaunchDetails details = (LaunchDetails)obj;
			
			ObjectMapper mapper = new ObjectMapper();
			PatientExecutionState state = null;
			
			try {
				state = mapper.readValue(details.getStatus(), PatientExecutionState.class);
				state.getCreateEicrStatus().setActionId(getActionId());
			} catch (JsonMappingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (JsonProcessingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			
			DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
			Date start = null;
			Date end = null;
			
			try {
				start = formatter.parse("2019-02-13");
				end = formatter.parse("2019-02-14");
				
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}			
			
			// Handle Conditions
			Boolean conditionsMet = true;
			if(getPreConditions() != null && getPreConditions().size() > 0) {
				
				List<AbstractCondition> conds = getPreConditions();
						
				for(AbstractCondition cond : conds) {
					
					if(!cond.evaluate(details)) {
						logger.info(" Condition Not met " + cond.getConditionType().toString());
						conditionsMet = false;
					}
				}
			}
			
			// PreConditions Met, then process related actions.
			Boolean relatedActsDone = true;
			if(conditionsMet) {
				
				if(getRelatedActions() != null && getRelatedActions().size() > 0) {
					
					List<RelatedAction> racts = getRelatedActions();
					
					for(RelatedAction ract: racts) {
						
						if(ract.getRelationship() == ActionRelationshipType.AFTER) {
							
							// check if the action is completed.
							String actionId = ract.getRelatedAction().getActionId();
							
							if(!state.hasActionCompleted(actionId)) {
								
								logger.info(" Action " + actionId + " is not completed , hence this action has to wait ");
								relatedActsDone = false;
							}	
						}
					}
				}			
			}
			
			// Check Timing Data , Dont check if the state is already scheduled meaning the job was scheduled already.
			if(relatedActsDone) {
				
				if(getTimingData() != null && getTimingData().size() > 0) {
					
					List<TimingSchedule> tsjobs = getTimingData();
					
					for(TimingSchedule ts : tsjobs) {
						
						// TBD : Setup job after testing so that we can test faster.
						
						state.getCreateEicrStatus().setJobStatus(JobStatus.SCHEDULED);
						try {
							details.setStatus(mapper.writeValueAsString(state));
						} catch (JsonProcessingException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					
				}
				
			}
			
			// Do not expect the trigger data to be present for CreateEicr
			// Call the Loading Queries and create eICR.
			if(ActionRepo.getInstance().getLoadingQueryService() != null ) { 
				
				logger.info(" Getting necessary data from Loading Queries ");
				FhirData data = ActionRepo.getInstance().getLoadingQueryService().getData(details, start, end);

				String eICR = null;

				if(data != null && data instanceof Dstu2FhirData) {

					Dstu2FhirData dstu2Data = (Dstu2FhirData)data;
					eICR = CdaEicrGenerator.convertDstu2FhirBundletoCdaEicr(dstu2Data, details);
					
					state.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);
					try {
						details.setStatus(mapper.writeValueAsString(state));
					} catch (JsonProcessingException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}
				
				logger.info(" **** Printing Eicr **** ");
				
				logger.info(eICR);;
				
				logger.info(" **** End Printing Eicr **** ");
			}	
			
			
		}
	}

}
