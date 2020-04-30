package com.drajer.eca.model;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.drajer.eca.model.EventTypes.JobStatus;

public class PatientExecutionState {
	
	// We could refactor this into one class as we move forward.
	private class MatchTriggerStatus {
		
		private EventTypes.JobStatus    jobStatus;
		private Boolean					triggerMatchStatus;
		private Set<String>				matchedCodes;
	}
	
	private class CreateEicrStatus {
		
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrCreated;
		private String					eICRId;
	}
	
	private class PeriodicUpdateEicrStatus {
		
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrUpdated;
		private String					eICRId;
	}
	
	private class CloseOutEicrStatus {
		
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrClosed;
		private String					eICRId;
	}
	
	private class ValidateEicrStatus {
		
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrValidated;
		private String					eICRId;
		private Date					validationTime;
	}
	
	private class SubmitEicrStatus {
		
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrSubmitted;
		private String					eICRId;
		private Date					submittedTime;
		private String					transportUsed;
	}
	
	String 							patientId;
	String 							encounterId;
	MatchTriggerStatus 				matchTriggerStatus;
	CreateEicrStatus				createEicrStatus;
	Set<PeriodicUpdateEicrStatus>   periodicUpdateStatus;
	CloseOutEicrStatus				closeOutEicrStatus;
	Set<ValidateEicrStatus> 		validateEicrStatus;
	Set<SubmitEicrStatus>			submitEicrStatus;
	
	public PatientExecutionState(String patId, String enId) {
		
		patientId = patId;
		encounterId = enId;
		
		matchTriggerStatus = new MatchTriggerStatus();
		matchTriggerStatus.jobStatus = JobStatus.NOT_STARTED;
		matchTriggerStatus.triggerMatchStatus = false;
		matchTriggerStatus.matchedCodes = new HashSet<String>();
		
		createEicrStatus = new CreateEicrStatus();
		createEicrStatus.jobStatus = JobStatus.NOT_STARTED;
		createEicrStatus.eicrCreated = false;
		createEicrStatus.eICRId = "";
		
		// Ignore Periodic Updates for now.
		periodicUpdateStatus = new HashSet<PeriodicUpdateEicrStatus>();
		
		closeOutEicrStatus = new CloseOutEicrStatus();
		closeOutEicrStatus.jobStatus = JobStatus.NOT_STARTED;
		closeOutEicrStatus.eicrClosed = false;
		closeOutEicrStatus.eICRId = "";
		
		validateEicrStatus = new HashSet<ValidateEicrStatus>();
		submitEicrStatus = new HashSet<SubmitEicrStatus>();
		
		
	}
	
	
}
