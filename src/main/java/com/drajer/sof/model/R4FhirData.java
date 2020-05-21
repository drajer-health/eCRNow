package com.drajer.sof.model;

import java.util.List;

import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;

public class R4FhirData extends FhirData {

	private Bundle data;
	
	private Patient patient;
	private Practitioner practitioner;
	private Encounter encounter;
	private Location  location;
	private Organization organization;
	private List<Condition> conditions;
	private List<DiagnosticReport> diagReports;
	private List<Observation> 	   labResults;
	private List<Observation> 	   travelObs;
	private List<Observation>      pregnancyObs;
	private List<Immunization> 	   immunizations;
	private List<MedicationStatement>  medications;
	// private List<DiagnosticOrder>  diagOrders;

	public Bundle getData() {
		return data;
	}

	public void setData(Bundle data) {
		this.data = data;
	}

	public Patient getPatient() {
		return patient;
	}

	public void setPatient(Patient patient) {
		this.patient = patient;
	}

	public Practitioner getPractitioner() {
		return practitioner;
	}

	public void setPractitioner(Practitioner practitioner) {
		this.practitioner = practitioner;
	}

	public Encounter getEncounter() {
		return encounter;
	}

	public void setEncounter(Encounter encounter) {
		this.encounter = encounter;
	}

	public Location getLocation() {
		return location;
	}

	public void setLocation(Location location) {
		this.location = location;
	}

	public Organization getOrganization() {
		return organization;
	}

	public void setOrganization(Organization organization) {
		this.organization = organization;
	}

	public List<Condition> getConditions() {
		return conditions;
	}

	public void setConditions(List<Condition> conditions) {
		this.conditions = conditions;
	}

	public List<DiagnosticReport> getDiagReports() {
		return diagReports;
	}

	public void setDiagReports(List<DiagnosticReport> diagReports) {
		this.diagReports = diagReports;
	}

	public List<Observation> getLabResults() {
		return labResults;
	}

	public void setLabResults(List<Observation> labResults) {
		this.labResults = labResults;
	}

	public List<Observation> getTravelObs() {
		return travelObs;
	}

	public void setTravelObs(List<Observation> travelObs) {
		this.travelObs = travelObs;
	}

	public List<Observation> getPregnancyObs() {
		return pregnancyObs;
	}

	public void setPregnancyObs(List<Observation> pregnancyObs) {
		this.pregnancyObs = pregnancyObs;
	}

	public List<Immunization> getImmunizations() {
		return immunizations;
	}

	public void setImmunizations(List<Immunization> immunizations) {
		this.immunizations = immunizations;
	}

	public List<MedicationStatement> getMedications() {
		return medications;
	}

	public void setMedications(List<MedicationStatement> medications) {
		this.medications = medications;
	}
}
