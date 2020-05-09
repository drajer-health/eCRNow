package com.drajer.eca.model;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Date;
import java.util.List;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.stereotype.Service;

import com.drajer.cda.CdaEicrGenerator;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.LaunchService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

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

		logger.info(" **** START Executing Create Eicr Action **** ");

		if (obj instanceof LaunchDetails) {

			LaunchDetails details = (LaunchDetails) obj;
			ObjectMapper mapper = new ObjectMapper();
			PatientExecutionState state = null;

			try {
				state = mapper.readValue(details.getStatus(), PatientExecutionState.class);
				state.getCreateEicrStatus().setActionId(getActionId());
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

			logger.info(" Executing Create Eicr Action , Prior Execution State : = " + details.getStatus());

			// Handle Conditions
			Boolean conditionsMet = true;
			if (getPreConditions() != null && getPreConditions().size() > 0) {

				logger.info(" Evaluating PreConditions ");
				List<AbstractCondition> conds = getPreConditions();

				for (AbstractCondition cond : conds) {

					if (!cond.evaluate(details)) {
						logger.info(" Condition Not met " + cond.getConditionType().toString());
						conditionsMet = false;
					}
				}
			}

			// PreConditions Met, then process related actions.
			Boolean relatedActsDone = true;
			if (conditionsMet) {
				
				logger.info(" PreConditions have been Met, evaluating Related Actions. ");

				if (getRelatedActions() != null && getRelatedActions().size() > 0) {

					List<RelatedAction> racts = getRelatedActions();

					for (RelatedAction ract : racts) {

						// Check for all actions AFTER which this action has to be executed for completion.
						if (ract.getRelationship() == ActionRelationshipType.AFTER) {

							// check if the action is completed.
							String actionId = ract.getRelatedAction().getActionId();

							if (!state.hasActionCompleted(actionId)) {

								logger.info(
										" Action " + actionId + " is not completed , hence this action has to wait ");
								relatedActsDone = false;
							}
							else {
								
								logger.info(" Related Action has been completed : " + actionId);
								
								// Check if there is any timing constraint that needs to be handled.
								if(ract.getDuration() != null && 
								   state.getCreateEicrStatus().getJobStatus() == JobStatus.NOT_STARTED) {
									
									// Duration is not null, meaning that the create action has to be delayed by the duration.
									logger.info(" Schedule the job for Create EICR based on the duration.");
								
									try {
										
										WorkflowService.scheduleJob(details.getId(), ract.getDuration(), EcrActionTypes.CREATE_EICR);
										state.getCreateEicrStatus().setJobStatus(JobStatus.SCHEDULED);
										details.setStatus(mapper.writeValueAsString(state));
										
										// No need to continue as the job will take over execution.
										
										logger.info(" **** END Executing Create Eicr Action **** ");
										return;
									} catch (JsonProcessingException e) { 
										String msg = "Unable to read/write execution state";
										logger.error(msg);
										e.printStackTrace();
										
										throw new RuntimeException(msg);
									}
								}
								else {
									
									logger.info( " No need to scheuled job as it has already been scheduled or completed. ");
								}
							}
						}
						else {
							logger.info(" Action " + ract.getRelatedAction().getActionId() + " is related via " + ract.getRelationship());
							
						}
					}
				}
				
				// Check Timing Data , No need to check if the state is already scheduled meaning the
				// job was scheduled already.
				if (relatedActsDone) {
					
					logger.info(" All Related Actions are completed ");

					// Timing constraints are applicable if this job has not started, once it is started
					// the State Machine has to manage the execution.
					if (state.getCreateEicrStatus().getJobStatus() == JobStatus.NOT_STARTED) {
						
						logger.info(" Related Actions Done and this action has not started ");
						
						if (getTimingData() != null && getTimingData().size() > 0) {
							
							logger.info(" Timing Data is present , so create a job based on timing data.");
							List<TimingSchedule> tsjobs = getTimingData();

							for (TimingSchedule ts : tsjobs) {

								// TBD : Setup job using TS Timing after testing so that we can test faster.
								// For now setup a default job with 10 seconds.
								try {
									
									WorkflowService.scheduleJob(details.getId(), ts, EcrActionTypes.CREATE_EICR);
									state.getCreateEicrStatus().setJobStatus(JobStatus.SCHEDULED);
									details.setStatus(mapper.writeValueAsString(state));
									
									// No need to continue as the job will take over execution.
									logger.info(" **** End Executing Create Eicr Action **** ");
									return;
								} catch (JsonProcessingException e) { 
									String msg = "Unable to read/write execution state";
									logger.error(msg);
									e.printStackTrace();
									
									throw new RuntimeException(msg);
								}

								
							}

						}
						
						logger.info(" No job to schedule since there is no timing data ");
					}
					else if (state.getCreateEicrStatus().getJobStatus() == JobStatus.SCHEDULED) {
					
						// Do this only if the job is scheduled.
						logger.info(" Creating the EICR since the job has been scheduled ");

						// Since the job has started, Execute the job.
						// Call the Loading Queries and create eICR.
						if (ActionRepo.getInstance().getLoadingQueryService() != null) {

							logger.info(" Getting necessary data from Loading Queries ");
							FhirData data = ActionRepo.getInstance().getLoadingQueryService().getData(details, details.getStartDate(), details.getEndDate());

							String eICR = null;

							if (data != null && data instanceof Dstu2FhirData) {

								Dstu2FhirData dstu2Data = (Dstu2FhirData) data;
								eICR = CdaEicrGenerator.convertDstu2FhirBundletoCdaEicr(dstu2Data, details);

								// Create the object for persistence.
								Eicr ecr = new Eicr();
								ecr.setData(eICR);
								ActionRepo.getInstance().getEicrRRService().saveOrUpdate(ecr);


								state.getCreateEicrStatus().setEicrCreated(true);
								state.getCreateEicrStatus().seteICRId(ecr.getId().toString());
								state.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);

								try {
									details.setStatus(mapper.writeValueAsString(state));
								} catch (JsonProcessingException e) {
									
									String msg = "Unable to update execution state";
									logger.error(msg);
									e.printStackTrace();
									
									throw new RuntimeException(msg);
								}
								
								logger.info(" **** Printing Eicr from CREATE EICR ACTION **** ");

								logger.info(eICR);

								logger.info(" **** End Printing Eicr from CREATE EICR ACTION **** ");
							}
							else {
								
								String msg = "No Fhir Data retrieved to CREATE EICR.";
								logger.error(msg);
								
								throw new RuntimeException(msg);
							}

							
						}
						else {
							
							String msg = "System Startup Issue, Spring Injection not functioning properly, loading service is null.";
							logger.error(msg);
							
							throw new RuntimeException(msg);
						}
					}
					else {
						logger.info(" EICR job is in a state of " + state.getCreateEicrStatus().getJobStatus() + " , due to which EICR will not be created. ");
					}

				}
				else {
					logger.info(" Related Actions are not completed, hence EICR will not be created. ");
				}
				
			}
			else {
				
				logger.info(" Conditions not met, hence EICR will not be created. ");
			}
		}
		else {
			
			String msg = "Invalid Object passed to Execute method, Launch Details expected, found : " + obj.getClass().getName();
			logger.error(msg);
			
			throw new RuntimeException(msg);
			
		}
		
		logger.info(" **** END Executing Create Eicr Action after completing normal execution. **** ");
	}

}
