package com.drajer.eca.model;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.SetUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cda.CdaEicrGenerator;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.ecrapp.config.ValueSetSingleton;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;

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
		
		logger.info(" Executing Match Trigger Action ");
		
		if(obj instanceof LaunchDetails) {
			
			logger.info(" Obtained Launch Details ");
			LaunchDetails details = (LaunchDetails)obj;
			
			ObjectMapper mapper = new ObjectMapper();
			PatientExecutionState state = null;
			
			try {
				state = mapper.readValue(details.getStatus(), PatientExecutionState.class);
				state.getMatchTriggerStatus().setActionId(getActionId());
			} catch (JsonMappingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (JsonProcessingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			
			// Call the Trigger Queries.
			if(ActionRepo.getInstance().getTriggerQueryService() != null ) { 
							
				
				logger.info(" Getting necessary data from Trigger Queries ");
				FhirData data = ActionRepo.getInstance().getTriggerQueryService().getData(details, details.getStartDate(), details.getEndDate());
						
				if(data != null && data instanceof Dstu2FhirData) {
							
					Dstu2FhirData dstu2Data = (Dstu2FhirData)data;
					
					// For Match Trigger Action, we expect the following
					// No preConditions;
					// No relatedActions;
					// No timingData
					
					// We only expect to match codes. So get the paths and match the codes.
					if(getTriggerData() != null && getTriggerData().size() > 0) {
						
						// we have triggers to match against COVID Value Sets for now.
						// In the future we will use the specific paths provided by the ersd spec to match.
						
						List<ActionData> codePaths = getTriggerData();
						
						for(ActionData ad: codePaths) {
							
							logger.info(" Found Action Data for Matching Trigger Codes for : " + ad.getPath());
							List<CodeableConceptDt> ptCodes = dstu2Data.getCodesForExpression(ad.getPath());
							
							if(ptCodes != null && ptCodes.size() > 0) {
								
								logger.info(" Found Patient Codes of " + ptCodes.size() + " to match.");
								
								Set<String> codesToMatch = ApplicationUtils.convertCodeableConceptsToString(ptCodes);
								
								logger.info(" Found Patient Codes of " + codesToMatch.size() + " to match.");
								
								Set<String> codesToMatchAgainst = ValueSetSingleton.getInstance().getCovidValueSetsAsString();
								
								logger.info(" Number of Trigger Codes to match against " + codesToMatchAgainst.size() + " to match.");
								
								Set<String> intersection = SetUtils.intersection(codesToMatch, codesToMatchAgainst);
								
								if(intersection != null && intersection.size() > 0) {
									
									logger.info(" Number of Matched Codes = " + intersection.size());
									
									// For Testing purposes until we get test data assume the data has matched and continue processing.
									state.getMatchTriggerStatus().setTriggerMatchStatus(true);
									state.getMatchTriggerStatus().getMatchedCodes().addAll(intersection);
								}
								else {
									
									logger.info(" No Matched codes found for path ");
									// no need to change state..
								}
								
							}
						}
						
					}
					
					
					state.getMatchTriggerStatus().setJobStatus(JobStatus.COMPLETED);
					
					try {
						details.setStatus(mapper.writeValueAsString(state));
					} catch (JsonProcessingException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}

		}
	}
}
