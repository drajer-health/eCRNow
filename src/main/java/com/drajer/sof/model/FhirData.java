package com.drajer.sof.model;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.hl7.fhir.r4.model.CodeableConcept;

public abstract class FhirData {

  List<CodeableConceptDt> conditionCodes;
  List<CodeableConceptDt> labResultCodes;
  List<CodeableConceptDt> labResultValues;
  List<CodeableConceptDt> medicationCodes;
  List<CodeableConceptDt> diagnosticOrderCodes;
  List<CodeableConceptDt> encounterCodes;
  List<CodeableConceptDt> immuniationCodes;
  List<CodeableConceptDt> diagnosticReportCodes;

  List<CodeableConcept> r4ConditionCodes;
  List<CodeableConcept> r4LabResultCodes;
  List<CodeableConcept> r4LabResultValues;
  List<CodeableConcept> r4MedicationCodes;
  List<CodeableConcept> r4ServiceRequestCodes;
  List<CodeableConcept> r4EncounterCodes;
  List<CodeableConcept> r4ImmunizationCodes;
  List<CodeableConcept> r4DiagnosticReportCodes;

  public FhirData() {

    conditionCodes = new ArrayList<>();
    labResultCodes = new ArrayList<>();
    labResultValues = new ArrayList<>();
    medicationCodes = new ArrayList<>();
    diagnosticOrderCodes = new ArrayList<>();
    encounterCodes = new ArrayList<>();
    immuniationCodes = new ArrayList<>();
    diagnosticReportCodes = new ArrayList<>();

    r4ConditionCodes = new ArrayList<>();
    r4LabResultCodes = new ArrayList<>();
    r4LabResultValues = new ArrayList<>();
    r4MedicationCodes = new ArrayList<>();
    r4ServiceRequestCodes = new ArrayList<>();
    r4EncounterCodes = new ArrayList<>();
    r4ImmunizationCodes = new ArrayList<>();
    r4DiagnosticReportCodes = new ArrayList<>();
  }

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

    if (expression.contains("Condition.code")) {
      List<CodeableConceptDt> returnCodes = new ArrayList<>();
      returnCodes.addAll(conditionCodes);
      returnCodes.addAll(encounterCodes);
      return returnCodes;
    } else if (expression.contains("MedicationAdministration.code")) {
      return medicationCodes;
    } else if (expression.contains("Observation.code")) {
      List<CodeableConceptDt> returnCodes = new ArrayList<>();
      returnCodes.addAll(labResultCodes);
      returnCodes.addAll(diagnosticReportCodes);
      return returnCodes;
    } else if (expression.contains("Immunization.vaccineCode")) {
      return immuniationCodes;
    } else if (expression.contains("ServiceRequest.code")) {
      List<CodeableConceptDt> returnCodes = new ArrayList<>();
      returnCodes.addAll(diagnosticOrderCodes);
      returnCodes.addAll(diagnosticReportCodes);
      returnCodes.addAll(labResultCodes);
      return returnCodes;
    } else if (expression.contains("Observation.value")) {
      return labResultValues;
    } else {
      return Collections.emptyList();
    }
  }

  public List<CodeableConcept> getR4CodesForExpression(String expression) {

    if (expression.contains("Condition.code")) {
      List<CodeableConcept> returnCodes = new ArrayList<>();
      returnCodes.addAll(r4ConditionCodes);
      returnCodes.addAll(r4EncounterCodes);
      return returnCodes;
    } else if (expression.contains("MedicationAdministration.code")) {
      return r4MedicationCodes;
    } else if (expression.contains("Observation.code")) {
      List<CodeableConcept> returnCodes = new ArrayList<>();
      returnCodes.addAll(r4LabResultCodes);
      returnCodes.addAll(r4DiagnosticReportCodes);
      return returnCodes;
    } else if (expression.contains("Immunization.vaccineCode")) {
      return r4ImmunizationCodes;
    } else if (expression.contains("ServiceRequest.code")) {
      List<CodeableConcept> returnCodes = new ArrayList<>();
      returnCodes.addAll(r4ServiceRequestCodes);
      returnCodes.addAll(r4DiagnosticReportCodes);
      returnCodes.addAll(r4LabResultCodes);
      return returnCodes;
    } else if (expression.contains("Observation.value")) {
      return r4LabResultValues;
    } else {
      return Collections.emptyList();
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

  public List<CodeableConcept> getR4ServiceRequestCodes() {
    return r4ServiceRequestCodes;
  }

  public void setR4ServiceRequestCodes(List<CodeableConcept> r4ServiceRequestCodes) {
    this.r4ServiceRequestCodes = r4ServiceRequestCodes;
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

  public List<CodeableConceptDt> getLabResultValues() {
    return labResultValues;
  }

  public void setLabResultValues(List<CodeableConceptDt> labResultValues) {
    this.labResultValues = labResultValues;
  }

  public List<CodeableConcept> getR4LabResultValues() {
    return r4LabResultValues;
  }

  public void setR4LabResultValues(List<CodeableConcept> r4LabResultValues) {
    this.r4LabResultValues = r4LabResultValues;
  }
}
