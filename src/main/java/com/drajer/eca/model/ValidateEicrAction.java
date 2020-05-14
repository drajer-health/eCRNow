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
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.helger.schematron.ISchematronResource;
import com.helger.schematron.svrl.jaxb.FailedAssert;
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
		
		logger.info(" **** START Executing Validate Eicr Action **** ");
		
		if (obj instanceof LaunchDetails) {

			LaunchDetails details = (LaunchDetails) obj;
			ObjectMapper mapper = new ObjectMapper();
			PatientExecutionState state = null;

			try {
				state = mapper.readValue(details.getStatus(), PatientExecutionState.class);			
			} catch (JsonMappingException e1) {
				
				String msg = "Unable to read/write execution state";
				logger.error(msg);
				e1.printStackTrace();
				throw new RuntimeException(msg);
				
			} catch (JsonProcessingException e1) {
				
				String msg = "Unable to read/write execution state";
				logger.error(msg);
				e1.printStackTrace();
				throw new RuntimeException(msg);
			}

			logger.info(" Executing Validate Eicr Action , Prior Execution State : = " + details.getStatus());
			
			
			if (getRelatedActions() != null && getRelatedActions().size() > 0) {
				
				logger.info(" Validation actions to be performed based on other related actions. ");

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
							
							validateEicrs(details, state, ids);
							
						}
					}
					else {
						logger.info(" This action is not dependent on the action relationship : " + ract.getRelationship() + ", Action Id = " + ract.getRelatedAction().getActionId());
					}
				}
			}
			else {
				
				logger.info(" No related actions, so validate all Eicrs that are ready for validation ");
				
				Set<Integer> ids = state.getEicrsReadyForValidation();
				
				validateEicrs(details, state, ids);			
				
			}
			
			try {
				details.setStatus(mapper.writeValueAsString(state));
			} catch (JsonProcessingException e) {
					
				String msg = "Unable to update execution state";
				logger.error(msg);
				e.printStackTrace();
					
				throw new RuntimeException(msg);
			}
		}
	}
	
	public void validateEicrs(LaunchDetails details, PatientExecutionState state, Set<Integer> ids) {
	
		for(Integer id : ids) {
			
			logger.info(" Found eICR with Id " + id  +" to validate ");
			
			Eicr ecrToValidate = ActionRepo.getInstance().getEicrRRService().getEicrById(id);
			
			Boolean validationResult = false;
			final ISchematronResource aResSCH = SchematronResourceSCH.fromFile(ActionRepo.getInstance().getSchematronFileLocation());
			
			  if (!aResSCH.isValidSchematron ()) {
			    
				  logger.info(" *** Cannot Validate since Schematron is not valid *** ");		    
			  }
			  else {
				  
				  SchematronOutputType output = null;
				  try {
					  logger.info(" Found Valid Schematron which can be applied EICR with Id = " + id);
					  output = aResSCH.applySchematronValidationToSVRL(new StreamSource(new StringReader(ecrToValidate.getData())));
				  } catch (Exception e) {
					  	String msg = "Unable to read/write execution state";
						logger.error(msg);
						e.printStackTrace();
						throw new RuntimeException(msg);
				  }
				  								  
				  if(output != null) {
					  
					  
					  //logger.info(" Total # of Failed assertions " + output.getNsPrefixInAttributeValuesCount());
					  //validationResult = false;
					  
					  List<Object> objs = output.getActivePatternAndFiredRuleAndFailedAssert();
					  Boolean foundFailures = false;
					  
					  logger.info(" Number of Failed Assertions " + objs.size());
					  
					  for(Object obj : objs) {
						  
						  if(obj instanceof FailedAssert) {
							  
							  FailedAssert fa = (FailedAssert)obj;
							  
							  //if(fa.getRole() != null && 
								//	  (fa.getRole().contentEquals("fatal") || fa.getRole().contentEquals("error")) ) {
							  
							  if(fa.getFlag() != null && (fa.getFlag().contentEquals("error"))) {
								  
								  foundFailures = true;
								  logger.info(" Failed Asertion : Id = " + fa.getId() + " , Location = " + fa.getLocation()
								  	+ " , Text = " + fa.getText() + ", Flag = " + fa.getFlag());
								  
							  }
							  else {
								  
								 //  logger.info("Failed Asertion : Id = " + fa.getId() + ", Flag = " + fa.getFlag());
								 // It is a warning, so need to print to log for analysis
							  }							  
							  
						  }
						  
					  }
					  
					  if(foundFailures)
						  validationResult = false;
					  else 
						  validationResult = true;
				  }					  
				  else {
					  
					  logger.info("Schematron Validation Ouput is null, so validation was not performed ");
					  validationResult = false;
				  }
				  
			  }
			  
			  // Add a validate object every time.
			  ValidateEicrStatus validate = new ValidateEicrStatus();
			  validate.setActionId(getActionId());
			  validate.seteICRId(id.toString());
			 // validate.setEicrValidated(validationResult);
			  validate.setEicrValidated(true);
			  validate.setJobStatus(JobStatus.COMPLETED);
			  validate.setValidationTime(new Date());
			  state.getValidateEicrStatus().add(validate);
			  
		}
		
	}

}
