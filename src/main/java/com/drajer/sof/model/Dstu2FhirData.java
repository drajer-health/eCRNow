package com.drajer.sof.model;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.dstu2.resource.Location;
import ca.uhn.fhir.model.dstu2.resource.Medication;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.dstu2.resource.Organization;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import java.util.ArrayList;
import java.util.List;

public class Dstu2FhirData extends FhirData {

  private Bundle data;

  private Patient patient;
  private Practitioner practitioner;
  private Encounter encounter;
  private Location location;
  private Organization organization;
  private List<Condition> conditions;
  private List<DiagnosticReport> diagReports;
  private List<DiagnosticOrder> diagOrders;
  private List<Observation> labResults;
  private List<Observation> travelObs;
  private List<Observation> pregnancyObs;
  private List<Immunization> immunizations;
  private List<MedicationStatement> medications;
  private List<MedicationAdministration> medicationAdministrations;
  private List<Medication> medicationList;

  public Dstu2FhirData() {

    conditions = new ArrayList<>();
    diagReports = new ArrayList<>();
    diagOrders = new ArrayList<>();
    labResults = new ArrayList<>();
    travelObs = new ArrayList<>();
    pregnancyObs = new ArrayList<>();
    immunizations = new ArrayList<>();
    medications = new ArrayList<>();
    medicationAdministrations = new ArrayList<>();
    medicationList = new ArrayList<>();
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

  public List<DiagnosticOrder> getDiagOrders() {
    return diagOrders;
  }

  public void setDiagOrders(List<DiagnosticOrder> diagOrders) {
    this.diagOrders = diagOrders;
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

  public List<MedicationAdministration> getMedicationAdministrations() {
    return medicationAdministrations;
  }

  public void setMedicationAdministrations(
      List<MedicationAdministration> medicationAdministrations) {
    this.medicationAdministrations = medicationAdministrations;
  }

  public List<Medication> getMedicationList() {
    return medicationList;
  }

  public void setMedicationList(List<Medication> medicationList) {
    this.medicationList = medicationList;
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

  public Bundle getData() {
    return data;
  }

  public void setData(Bundle data) {
    this.data = data;
  }
}
