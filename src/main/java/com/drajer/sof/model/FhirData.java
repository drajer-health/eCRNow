package com.drajer.sof.model;

import java.util.List;

import org.hl7.fhir.r4.model.CodeableConcept;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;

public abstract class FhirData {

	List<CodeableConceptDt> conditionCodes;
	List<CodeableConceptDt> labResultCodes;
	List<CodeableConceptDt> medicationCodes;
	List<CodeableConceptDt> diagnosticOrderCodes;
	List<CodeableConceptDt> encounterCodes;
	List<CodeableConceptDt> immuniationCodes;
	List<CodeableConceptDt> diagnosticReportCodes;
	
	List<CodeableConcept> r4ConditionCodes;
	List<CodeableConcept> r4LabResultCodes;
	List<CodeableConcept> r4MedicationCodes;
	List<CodeableConcept> r4DiagnosticOrderCodes;
	List<CodeableConcept> r4EncounterCodes;
	List<CodeableConcept> r4ImmunizationCodes;
	List<CodeableConcept> r4DiagnosticReportCodes;
	
	public List<CodeableConceptDt> getConditionCodes() {
		return conditionCodes;
	}

	public void setConditionCodes(List<CodeableConceptDt> conditionCodes) {
		this.conditionCodes = conditionCodes;
	}

	public List<CodeableConceptDt> getLabResultCodes() {
		return labResultCodes;
	}

	public void setLabResultCodes(List<CodeableConceptDt> labResultCodes) {
		this.labResultCodes = labResultCodes;
	}

	public List<CodeableConceptDt> getMedicationCodes() {
		return medicationCodes;
	}

	public void setMedicationCodes(List<CodeableConceptDt> medicationCodes) {
		this.medicationCodes = medicationCodes;
	}

	public List<CodeableConceptDt> getDiagnosticOrderCodes() {
		return diagnosticOrderCodes;
	}

	public void setDiagnosticOrderCodes(List<CodeableConceptDt> diagnosticOrderCodes) {
		this.diagnosticOrderCodes = diagnosticOrderCodes;
	}

	public List<CodeableConceptDt> getEncounterCodes() {
		return encounterCodes;
	}

	public void setEncounterCodes(List<CodeableConceptDt> encounterCodes) {
		this.encounterCodes = encounterCodes;
	}

	public List<CodeableConceptDt> getImmuniationCodes() {
		return immuniationCodes;
	}

	public void setImmuniationCodes(List<CodeableConceptDt> immuniationCodes) {
		this.immuniationCodes = immuniationCodes;
	}

	public List<CodeableConceptDt> getDiagnosticReportCodes() {
		return diagnosticReportCodes;
	}

	public void setDiagnosticReportCodes(List<CodeableConceptDt> diagnosticReportCodes) {
		this.diagnosticReportCodes = diagnosticReportCodes;
	}

	public List<CodeableConceptDt> getCodesForExpression(String expression) {
		
		if(expression.contains("Condition")) {
			return conditionCodes;
		}
		else if(expression.contains("Medication")) {
			return medicationCodes;
		}
		else if(expression.contains("Observation")) {
			return labResultCodes;
		}
		else if(expression.contains("Immunization")) {
			return immuniationCodes;
		}
		else if(expression.contains("ServiceRequest")) {
			return diagnosticOrderCodes;
		}
		else {
			return null;
		}
		
	}

	public List<CodeableConcept> getR4ConditionCodes() {
		return r4ConditionCodes;
	}

	public void setR4ConditionCodes(List<CodeableConcept> r4ConditionCodes) {
		this.r4ConditionCodes = r4ConditionCodes;
	}

	public List<CodeableConcept> getR4LabResultCodes() {
		return r4LabResultCodes;
	}

	public void setR4LabResultCodes(List<CodeableConcept> r4LabResultCodes) {
		this.r4LabResultCodes = r4LabResultCodes;
	}

	public List<CodeableConcept> getR4MedicationCodes() {
		return r4MedicationCodes;
	}

	public void setR4MedicationCodes(List<CodeableConcept> r4MedicationCodes) {
		this.r4MedicationCodes = r4MedicationCodes;
	}

	public List<CodeableConcept> getR4DiagnosticOrderCodes() {
		return r4DiagnosticOrderCodes;
	}

	public void setR4DiagnosticOrderCodes(List<CodeableConcept> r4DiagnosticOrderCodes) {
		this.r4DiagnosticOrderCodes = r4DiagnosticOrderCodes;
	}

	public List<CodeableConcept> getR4EncounterCodes() {
		return r4EncounterCodes;
	}

	public void setR4EncounterCodes(List<CodeableConcept> r4EncounterCodes) {
		this.r4EncounterCodes = r4EncounterCodes;
	}

	public List<CodeableConcept> getR4ImmunizationCodes() {
		return r4ImmunizationCodes;
	}

	public void setR4ImmunizationCodes(List<CodeableConcept> r4ImmunizationCodes) {
		this.r4ImmunizationCodes = r4ImmunizationCodes;
	}

	public List<CodeableConcept> getR4DiagnosticReportCodes() {
		return r4DiagnosticReportCodes;
	}

	public void setR4DiagnosticReportCodes(List<CodeableConcept> r4DiagnosticReportCodes) {
		this.r4DiagnosticReportCodes = r4DiagnosticReportCodes;
	}
}
