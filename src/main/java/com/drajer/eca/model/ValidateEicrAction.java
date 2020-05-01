package com.drajer.eca.model;

import java.io.StringReader;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.xml.transform.stream.StreamSource;

import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.cda.CdaEicrGenerator;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.PatientExecutionState.ValidateEicrStatus;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.helger.schematron.ISchematronResource;
import com.helger.schematron.svrl.jaxb.SchematronOutputType;
import com.helger.schematron.xslt.SchematronResourceSCH;

public class ValidateEicrAction extends AbstractAction {

	private final Logger logger = LoggerFactory.getLogger(ValidateEicrAction.class);
	
	@Override
	public void print() {
		
		logger.info(" **** Printing ValidateEicrAction **** ");
		printBase();
		logger.info(" **** End Printing ValidateEicrAction **** ");
	}
	@Override
	public void execute(Object obj) {
		
		logger.info(" Executing Validate Eicr Action ");
		
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
			
			

			logger.info(" Validate Eicr State : Launch Details State before execution :" + details.getStatus());
			
			
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
								
								logger.info(" Found eICR with Id " + id  +" to validate ");
								
								Eicr ecrToValidate = ActionRepo.getInstance().getEicrRRService().getEicrById(id);
								
								Boolean validationResult = false;
								final ISchematronResource aResSCH = SchematronResourceSCH.fromFile(ActionRepo.getInstance().getSchematronFileLocation());
								
								  if (!aResSCH.isValidSchematron ()) {
								    
									  logger.info(" *** Cannot Validate since Schematron is not valid ** ");
								    
								  }
								  else {
									  
									  SchematronOutputType output = null;
									  try {
										  output = aResSCH.applySchematronValidationToSVRL(new StreamSource(new StringReader(ecrToValidate.getData())));
									  } catch (Exception e) {
										// TODO Auto-generated catch block
										  e.printStackTrace();
									  }
									  								  
									  if(output != null && output.getNsPrefixInAttributeValuesCount() > 0) {
										  logger.info(" Total # of Failed assertions " + output.getNsPrefixInAttributeValuesCount());
										  validationResult = false;
									  }
										  
									  else 
										  validationResult = true;
									  
								  }
								  
								  // Add a validate object every time.
								  PatientExecutionState.ValidateEicrStatus validate = state.new ValidateEicrStatus();
								  validate.setActionId(getActionId());
								  validate.seteICRId(id.toString());
								  validate.setEicrValidated(validationResult);
								  validate.setJobStatus(JobStatus.COMPLETED);
								  validate.setValidationTime(new Date());
								  state.getValidateEicrStatus().add(validate);
							}
							
						}
					}
				}
			}
		}
	}

}
