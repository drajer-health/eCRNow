package com.drajer.eca.model;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.drajer.eca.model.EventTypes.JobStatus;

public class PatientExecutionState {
	
	// We could refactor this into one class as we move forward.
	public class MatchTriggerStatus {
		
		private String 					actionId;
		private EventTypes.JobStatus    jobStatus;
		private Boolean					triggerMatchStatus; // Did anything match or not
		private Set<String>				matchedCodes;
		
		
		public String getActionId() {
			return actionId;
		}
		public void setActionId(String actionId) {
			this.actionId = actionId;
		}
		public EventTypes.JobStatus getJobStatus() {
			return jobStatus;
		}
		public void setJobStatus(EventTypes.JobStatus jobStatus) {
			this.jobStatus = jobStatus;
		}
		public Boolean getTriggerMatchStatus() {
			return triggerMatchStatus;
		}
		public void setTriggerMatchStatus(Boolean triggerMatchStatus) {
			this.triggerMatchStatus = triggerMatchStatus;
		}
		public Set<String> getMatchedCodes() {
			return matchedCodes;
		}
		public void setMatchedCodes(Set<String> matchedCodes) {
			this.matchedCodes = matchedCodes;
		}
		
		public MatchTriggerStatus() {
			actionId = "";
			matchedCodes = new HashSet<String>();
			triggerMatchStatus = false;
			jobStatus = JobStatus.NOT_STARTED;
		}
	}
	
	public class CreateEicrStatus {
		
		private String 					actionId;
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrCreated;
		private String					eICRId;
		
		
		public String getActionId() {
			return actionId;
		}
		public void setActionId(String actionId) {
			this.actionId = actionId;
		}
		public EventTypes.JobStatus getJobStatus() {
			return jobStatus;
		}
		public void setJobStatus(EventTypes.JobStatus jobStatus) {
			this.jobStatus = jobStatus;
		}
		public Boolean getEicrCreated() {
			return eicrCreated;
		}
		public void setEicrCreated(Boolean eicrCreated) {
			this.eicrCreated = eicrCreated;
		}
		public String geteICRId() {
			return eICRId;
		}
		public void seteICRId(String eICRId) {
			this.eICRId = eICRId;
		}
		
		public CreateEicrStatus() {
			
			actionId = "";
			jobStatus = JobStatus.NOT_STARTED;
			eicrCreated = false;
			eICRId = "";
		}
		
	}
	
	public class PeriodicUpdateEicrStatus {
		
		private String 					actionId;
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrUpdated;
		private String					eICRId;
		
		
		public String getActionId() {
			return actionId;
		}
		public void setActionId(String actionId) {
			this.actionId = actionId;
		}
		public EventTypes.JobStatus getJobStatus() {
			return jobStatus;
		}
		public void setJobStatus(EventTypes.JobStatus jobStatus) {
			this.jobStatus = jobStatus;
		}
		public Boolean getEicrUpdated() {
			return eicrUpdated;
		}
		public void setEicrUpdated(Boolean eicrUpdated) {
			this.eicrUpdated = eicrUpdated;
		}
		public String geteICRId() {
			return eICRId;
		}
		public void seteICRId(String eICRId) {
			this.eICRId = eICRId;
		}
		
		public PeriodicUpdateEicrStatus() {
			actionId = "";
			jobStatus = JobStatus.NOT_STARTED;
			eicrUpdated = false;
			eICRId = "";
		}
		
	}
	
	public class CloseOutEicrStatus {
		
		private String 					actionId;
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrClosed;
		private String					eICRId;
		
		
		public String getActionId() {
			return actionId;
		}
		public void setActionId(String actionId) {
			this.actionId = actionId;
		}
		public EventTypes.JobStatus getJobStatus() {
			return jobStatus;
		}
		public void setJobStatus(EventTypes.JobStatus jobStatus) {
			this.jobStatus = jobStatus;
		}
		public Boolean getEicrClosed() {
			return eicrClosed;
		}
		public void setEicrClosed(Boolean eicrClosed) {
			this.eicrClosed = eicrClosed;
		}
		public String geteICRId() {
			return eICRId;
		}
		public void seteICRId(String eICRId) {
			this.eICRId = eICRId;
		}
		
		public CloseOutEicrStatus() {
			actionId = "";
			jobStatus = JobStatus.NOT_STARTED;
			eicrClosed = false;
			eICRId = "";
		}
		
		
	}
	
	public class ValidateEicrStatus {
		
		private String 					actionId;
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrValidated;
		private String					eICRId;
		private Date					validationTime;
		
		
		public String getActionId() {
			return actionId;
		}
		public void setActionId(String actionId) {
			this.actionId = actionId;
		}
		public EventTypes.JobStatus getJobStatus() {
			return jobStatus;
		}
		public void setJobStatus(EventTypes.JobStatus jobStatus) {
			this.jobStatus = jobStatus;
		}
		public Boolean getEicrValidated() {
			return eicrValidated;
		}
		public void setEicrValidated(Boolean eicrValidated) {
			this.eicrValidated = eicrValidated;
		}
		public String geteICRId() {
			return eICRId;
		}
		public void seteICRId(String eICRId) {
			this.eICRId = eICRId;
		}
		public Date getValidationTime() {
			return validationTime;
		}
		public void setValidationTime(Date validationTime) {
			this.validationTime = validationTime;
		}
		
		public ValidateEicrStatus() {
			actionId = "";
			jobStatus = JobStatus.NOT_STARTED;
			eicrValidated = false;
			eICRId = "";
			
		}
		
	}
	
	public class SubmitEicrStatus {
		
		private String 					actionId;
		private EventTypes.JobStatus    jobStatus;
		private Boolean					eicrSubmitted;
		private String					eICRId;
		private Date					submittedTime;
		private String					transportUsed;
		
		
		public String getActionId() {
			return actionId;
		}
		public void setActionId(String actionId) {
			this.actionId = actionId;
		}
		public EventTypes.JobStatus getJobStatus() {
			return jobStatus;
		}
		public void setJobStatus(EventTypes.JobStatus jobStatus) {
			this.jobStatus = jobStatus;
		}
		public Boolean getEicrSubmitted() {
			return eicrSubmitted;
		}
		public void setEicrSubmitted(Boolean eicrSubmitted) {
			this.eicrSubmitted = eicrSubmitted;
		}
		public String geteICRId() {
			return eICRId;
		}
		public void seteICRId(String eICRId) {
			this.eICRId = eICRId;
		}
		public Date getSubmittedTime() {
			return submittedTime;
		}
		public void setSubmittedTime(Date submittedTime) {
			this.submittedTime = submittedTime;
		}
		public String getTransportUsed() {
			return transportUsed;
		}
		public void setTransportUsed(String transportUsed) {
			this.transportUsed = transportUsed;
		}
		
		public SubmitEicrStatus() {
			actionId = "";
			jobStatus = JobStatus.NOT_STARTED;
			eicrSubmitted = false;
			eICRId = "";
		}
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
		
		createEicrStatus = new CreateEicrStatus();
		
		// Ignore Periodic Updates for now.
		periodicUpdateStatus = new HashSet<PeriodicUpdateEicrStatus>();
		
		closeOutEicrStatus = new CloseOutEicrStatus();
		
		validateEicrStatus = new HashSet<ValidateEicrStatus>();
		
		submitEicrStatus = new HashSet<SubmitEicrStatus>();
		
	}
	
	public PatientExecutionState() {
		
		patientId = "";
		encounterId = "";
		
		matchTriggerStatus = new MatchTriggerStatus();
		
		createEicrStatus = new CreateEicrStatus();
		
		// Ignore Periodic Updates for now.
		periodicUpdateStatus = new HashSet<PeriodicUpdateEicrStatus>();
		
		closeOutEicrStatus = new CloseOutEicrStatus();
		
		validateEicrStatus = new HashSet<ValidateEicrStatus>();
		
		submitEicrStatus = new HashSet<SubmitEicrStatus>();
		
	}

	public String getPatientId() {
		return patientId;
	}

	public void setPatientId(String patientId) {
		this.patientId = patientId;
	}

	public String getEncounterId() {
		return encounterId;
	}

	public void setEncounterId(String encounterId) {
		this.encounterId = encounterId;
	}

	public MatchTriggerStatus getMatchTriggerStatus() {
		return matchTriggerStatus;
	}

	public void setMatchTriggerStatus(MatchTriggerStatus matchTriggerStatus) {
		this.matchTriggerStatus = matchTriggerStatus;
	}

	public CreateEicrStatus getCreateEicrStatus() {
		return createEicrStatus;
	}

	public void setCreateEicrStatus(CreateEicrStatus createEicrStatus) {
		this.createEicrStatus = createEicrStatus;
	}

	public Set<PeriodicUpdateEicrStatus> getPeriodicUpdateStatus() {
		return periodicUpdateStatus;
	}

	public void setPeriodicUpdateStatus(Set<PeriodicUpdateEicrStatus> periodicUpdateStatus) {
		this.periodicUpdateStatus = periodicUpdateStatus;
	}

	public CloseOutEicrStatus getCloseOutEicrStatus() {
		return closeOutEicrStatus;
	}

	public void setCloseOutEicrStatus(CloseOutEicrStatus closeOutEicrStatus) {
		this.closeOutEicrStatus = closeOutEicrStatus;
	}

	public Set<ValidateEicrStatus> getValidateEicrStatus() {
		return validateEicrStatus;
	}

	public void setValidateEicrStatus(Set<ValidateEicrStatus> validateEicrStatus) {
		this.validateEicrStatus = validateEicrStatus;
	}

	public Set<SubmitEicrStatus> getSubmitEicrStatus() {
		return submitEicrStatus;
	}

	public void setSubmitEicrStatus(Set<SubmitEicrStatus> submitEicrStatus) {
		this.submitEicrStatus = submitEicrStatus;
	}
	
	public Boolean hasActionCompleted(String actionId) {
		
		if(actionId.contentEquals(matchTriggerStatus.actionId) && 
		   matchTriggerStatus.jobStatus == JobStatus.COMPLETED ) {
			
			// Add check to see if a trigger matched ...For testing because of lack of data the check is omitted.
			return true;
		}
		else if(actionId.contentEquals(createEicrStatus.actionId) && 
				   createEicrStatus.jobStatus == JobStatus.COMPLETED) {
			return true;
		}
		else if(actionId.contentEquals(closeOutEicrStatus.actionId) && 
				closeOutEicrStatus.jobStatus == JobStatus.COMPLETED) {
			return true;	
		}
		
		for(PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {
			
			if(actionId.contentEquals(pd.actionId) && 
					pd.jobStatus == JobStatus.COMPLETED) {
				return true;
			}	
		}
		
		for(ValidateEicrStatus vs : validateEicrStatus) {
				
				if(actionId.contentEquals(vs.actionId) && 
						vs.jobStatus == JobStatus.COMPLETED) {
					return true;
				}
		}
		
		for(SubmitEicrStatus ss : submitEicrStatus) {
			
			if(actionId.contentEquals(ss.actionId) && 
					ss.jobStatus == JobStatus.COMPLETED) {
				return true;
			}	
		}
				
		return false;
	}
	
	public Set<Integer> getEicrIdForCompletedActions(String actionId) {
		
		Set<Integer> ids = new HashSet<Integer>();
		
		if(actionId.contentEquals(createEicrStatus.actionId) && 
				   createEicrStatus.jobStatus == JobStatus.COMPLETED) {
			
			ids.add(Integer.valueOf(createEicrStatus.eICRId));
		}
		
		if(actionId.contentEquals(closeOutEicrStatus.actionId) && 
				closeOutEicrStatus.jobStatus == JobStatus.COMPLETED) {
			ids.add(Integer.valueOf(closeOutEicrStatus.eICRId));	
		}
		
		for(PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {
			
			if(actionId.contentEquals(pd.actionId) && 
					pd.jobStatus == JobStatus.COMPLETED) {
				ids.add(Integer.valueOf(pd.eICRId));
			}	
		}
				
		return ids;
	}
	
	public Set<Integer> getEicrsReadyForValidation() {
		
		Set<Integer> ids = new HashSet<Integer>();
		
		// Get the EICRs already validated. 
		Set<Integer> valIds = new HashSet<Integer>();
		Set<ValidateEicrStatus> vals = this.getValidateEicrStatus();
		for(ValidateEicrStatus val : vals) {
			
			// Collect the Ids.
			valIds.add(Integer.valueOf(val.geteICRId()));
		}
		
		if(this.getCreateEicrStatus().getJobStatus() == JobStatus.COMPLETED && 
		   !valIds.contains(Integer.valueOf(this.getCreateEicrStatus().geteICRId()))) {
			ids.add(Integer.valueOf(this.getCreateEicrStatus().geteICRId()));
		}
			
		if(this.getCloseOutEicrStatus().getJobStatus() == JobStatus.COMPLETED && 
				   !valIds.contains(Integer.valueOf(this.getCloseOutEicrStatus().geteICRId()))) {
			ids.add(Integer.valueOf(getCloseOutEicrStatus().eICRId));	
		}
		
		for(PeriodicUpdateEicrStatus pd : periodicUpdateStatus) {
			
			if(	pd.jobStatus == JobStatus.COMPLETED && 
					   !valIds.contains(Integer.valueOf(pd.geteICRId()))) {
				ids.add(Integer.valueOf(pd.eICRId));
			}	
		}
		
		return ids;
	}
	
}
