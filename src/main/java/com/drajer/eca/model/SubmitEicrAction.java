package com.drajer.eca.model;

import java.io.StringReader;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.xml.transform.stream.StreamSource;

import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.PatientExecutionState.ValidateEicrStatus;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.helger.schematron.ISchematronResource;
import com.helger.schematron.svrl.jaxb.SchematronOutputType;
import com.helger.schematron.xslt.SchematronResourceSCH;

@Service
public class SubmitEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(SubmitEicrAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing SubmitEicrAction **** ");
		printBase();
		logger.info(" **** End Printing SubmitEicrAction **** ");
	}
	
	@Override
	public void execute(Object obj) {
		

		logger.info(" Executing Submit Eicr Action ");
		
		if (obj instanceof LaunchDetails) {
			  
			logger.info(" Obtained Launch Details ");
			LaunchDetails details = (LaunchDetails) obj;
			ObjectMapper mapper = new ObjectMapper();
			PatientExecutionState state = null;

			try {
				state = mapper.readValue(details.getStatus(), PatientExecutionState.class);			
			} catch (JsonMappingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (JsonProcessingException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			
			logger.info(" Submit Eicr State : Launch Details State before execution :" + details.getStatus());
			
			String data = "This is a test for patient " + details.getEncounterId();
			
			ActionRepo.getInstance().getDirectTransport().sendData(details, data);
			
			if (getRelatedActions() != null && getRelatedActions().size() > 0) {

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
							
							// Get the eICR for the Action Completed after which validation has to be run.
							Set<Integer> ids = state.getEicrIdForCompletedActions(actionId);
							
							for(Integer id : ids) {
								
								logger.info(" Found eICR with Id " + id  +" to submit ");
								
							//	  String data = ActionRepo.getInstance().getEicrRRService().getEicrById(id).getData();
								
							//	  ActionRepo.getInstance().getDirectTransport().sendData(details, data);
								
								
								  // Add a validate object every time.
								  PatientExecutionState.SubmitEicrStatus submitState = state.new SubmitEicrStatus();
								  submitState.setActionId(getActionId());
								  submitState.seteICRId(id.toString());
								  submitState.setEicrSubmitted(true);
								  submitState.setJobStatus(JobStatus.COMPLETED);
								  submitState.setSubmittedTime(new Date());
								  state.getSubmitEicrStatus().add(submitState);
							}
							
						}
					}
				}
			}
		}
		
	}

}
