package com.drajer.eca.model;

public final class EventTypes {
	
	public static enum WorkflowEvent {
		
		// These are the two ways that the app can be initiated currently.
		SOF_LAUNCH,
		SUBSCRIPTION_NOTIFICATION
	}
	
	public static enum JobStatus {
		
		NOT_STARTED,
		SCHEDULED,
		IN_PROGRESS,
		COMPLETED,
		ABORTED
	}
	
/*	public static enum ActionTriggerTypes {
		
		// These are based on FHIR Trigger Types
		// named-event | periodic | data-changed | data-added | data-modified | data-removed | data-accessed | data-access-ended
		NAMED_EVENT,
		PERIODIC,
		DATA_CHANGED,
		DATA_ADDED,
		DATA_MODIFIED,
		DATA_REMOVED,
		DATA_ACCESSED,
		DATA_ACCESS_ENDED
	} */
	
	public static enum EcrActionTypes {
		
		// These are the actions that can happen for eCR reporting.
		MATCH_TRIGGER,
		CREATE_EICR,
		PERIODIC_UPDATE_EICR,
		CLOSE_OUT_EICR,
		VALIDATE_EICR,
		SUBMIT_EICR,
		UNKNOWN
	}
	
/*	public static enum ActionRelationshipType {
		
		// Maps to FHIR Related Actions 
		// before-start | before | before-end | concurrent-with-start | concurrent | concurrent-with-end | after-start | after | after-end
		
		BEFORE_START,
		BEFORE,
		BEFORE_END,
		CONCURRENT_WITH_START,
		CONCURRENT,
		CONCURRENT_WITH_END,
		AFTER_START,
		AFTER,
		AFTER_END
	}*/
	
	public static enum ConditionType {
		
		// condition types in FHIR Plan Definition
		APPLICABILITY,
		START,
		STOP
	}
	
	public EcrActionTypes getEcrActionTypes(String s) {
		
		if(s.equalsIgnoreCase("match-trigger"))
				return EcrActionTypes.MATCH_TRIGGER;
		else if(s.equalsIgnoreCase("create-eicr"))
			return EcrActionTypes.CREATE_EICR;
		else if(s.equalsIgnoreCase("periodic-update-eicr"))
			return EcrActionTypes.PERIODIC_UPDATE_EICR;
		else if(s.equalsIgnoreCase("close-out-eicr"))
			return EcrActionTypes.CLOSE_OUT_EICR;
		else if(s.equalsIgnoreCase("validate-eicr"))
			return EcrActionTypes.VALIDATE_EICR;
		else if(s.equalsIgnoreCase("route-and-send-eicr"))
			return EcrActionTypes.SUBMIT_EICR;
		else 
			return EcrActionTypes.UNKNOWN;
				
	}

}
