package com.drajer.sof.model;

import java.util.List;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;

public abstract class FhirData {

	List<CodeableConceptDt> conditionCodes;
	List<CodeableConceptDt> labResultCodes;
	List<CodeableConceptDt> medicationCodes;
	List<CodeableConceptDt> diagnosticOrderCodes;
	List<CodeableConceptDt> encounterCodes;
	List<CodeableConceptDt> immuniationCodes;
	List<CodeableConceptDt> diagnosticReportCodes;

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

}
