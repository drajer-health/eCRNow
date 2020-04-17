package com.drajer.sof.model;

import java.util.List;

import org.hl7.fhir.r4.model.CodeableConcept;

public abstract class FhirData {

	List<CodeableConcept>   conditionCodes;
	List<CodeableConcept>	labResultCodes;
	List<CodeableConcept>	medicationCodes;
	List<CodeableConcept>	diagnosticOrderCodes;
	List<CodeableConcept> 	encounterCodes;
	
	public List<CodeableConcept> getConditionCodes() {
		return conditionCodes;
	}
	public void setConditionCodes(List<CodeableConcept> conditionCodes) {
		this.conditionCodes = conditionCodes;
	}
	public List<CodeableConcept> getLabResultCodes() {
		return labResultCodes;
	}
	public void setLabResultCodes(List<CodeableConcept> labResultCodes) {
		this.labResultCodes = labResultCodes;
	}
	public List<CodeableConcept> getMedicationCodes() {
		return medicationCodes;
	}
	public void setMedicationCodes(List<CodeableConcept> medicationCodes) {
		this.medicationCodes = medicationCodes;
	}
	public List<CodeableConcept> getDiagnosticOrderCodes() {
		return diagnosticOrderCodes;
	}
	public void setDiagnosticOrderCodes(List<CodeableConcept> diagnosticOrderCodes) {
		this.diagnosticOrderCodes = diagnosticOrderCodes;
	}
	public List<CodeableConcept> getEncounterCodes() {
		return encounterCodes;
	}
	public void setEncounterCodes(List<CodeableConcept> encounterCodes) {
		this.encounterCodes = encounterCodes;
	}
	
	
	
	
}
