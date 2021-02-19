package com.drajer.sof.model;

import java.util.ArrayList;
import java.util.List;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class R4FhirData extends FhirData {

  private static final Logger logger = LoggerFactory.getLogger(R4FhirData.class);

  private Bundle data;

  private Patient patient;
  private Practitioner practitioner;
  private List<Practitioner> practitionersList;
  private Encounter encounter;
  private Location location;
  private List<Location> locationList;
  private Organization organization;
  private List<Condition> conditions;
  private List<Condition> encounterDiagnosisConditions;
  private List<Condition> pregnancyConditions;
  private List<DiagnosticReport> diagReports;
  private List<Observation> labResults;
  private List<Observation> travelObs;
  private List<Observation> pregnancyObs;
  private List<Observation> occupationObs;
  private List<Immunization> immunizations;
  private List<MedicationStatement> medications;
  private List<MedicationRequest> medicationRequests;
  private List<MedicationAdministration> medicationAdministrations;
  private List<Medication> medicationList;
  private List<ServiceRequest> serviceRequests;

  private Address jurisdiction;

  public void prepareJurisdicationData() {

    if (location != null && location.getAddress() != null) {
      logger.info(" Using Location information from the Encounter for Jurisdiction");
      jurisdiction = location.getAddress();
    } else if (organization != null && organization.getAddressFirstRep() != null) {
      logger.info(
          " Using Organization information from Encounter.serviceProvider as Jurisdiction ");
      jurisdiction = organization.getAddressFirstRep();
    }
  }

  public Boolean hasRequiredDataForEicr() {

    Boolean requiredDataPresent = true;

    // Check Patient and Encounter.
    if (patient == null) {
      logger.warn(" Patient is null, cannot generate eICR ");
      requiredDataPresent = false;
    }

    if (encounter == null) {
      logger.warn(" Encounter is null, cannot generate eICR ");
      requiredDataPresent = false;
    }

    if (jurisdiction == null) {
      logger.warn(" Jurisdiction is null, cannot generate eICR ");
      requiredDataPresent = false;
    }

    return requiredDataPresent;
  }

  public Practitioner getPractitionerById(String id) {

    logger.info(" Id for retrieving practitioner = {}", id);
    for (Practitioner pr : practitionersList) {

      if (pr.getId().contains(id)) return pr;
    }

    return null;
  }

  public R4FhirData() {

    locationList = new ArrayList<>();
    practitionersList = new ArrayList<>();
    conditions = new ArrayList<>();
    encounterDiagnosisConditions = new ArrayList<>();
    pregnancyConditions = new ArrayList<>();
    diagReports = new ArrayList<>();
    labResults = new ArrayList<>();
    travelObs = new ArrayList<>();
    pregnancyObs = new ArrayList<>();
    occupationObs = new ArrayList<>();
    immunizations = new ArrayList<>();
    medications = new ArrayList<>();
    medicationRequests = new ArrayList<>();
    medicationAdministrations = new ArrayList<>();
    serviceRequests = new ArrayList<>();
    medicationList = new ArrayList<>();
  }

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

  public List<Practitioner> getPractitionersList() {
    return practitionersList;
  }

  public void setPractitionersList(List<Practitioner> practitionersList) {
    this.practitionersList = practitionersList;
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

  public List<Location> getLocationList() {
    return locationList;
  }

  public void setLocationList(List<Location> locationList) {
    this.locationList = locationList;
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

  public List<Condition> getEncounterDiagnosisConditions() {
    return encounterDiagnosisConditions;
  }

  public void setEncounterDiagnosisConditions(List<Condition> encounterDiagnosisConditions) {
    this.encounterDiagnosisConditions = encounterDiagnosisConditions;
  }

  public List<Condition> getPregnancyConditions() {
    return pregnancyConditions;
  }

  public void setPregnancyConditions(List<Condition> pregnancyConditions) {
    this.pregnancyConditions = pregnancyConditions;
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

  public List<Observation> getOccupationObs() {
    return occupationObs;
  }

  public void setOccupationObs(List<Observation> occupationObs) {
    this.occupationObs = occupationObs;
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

  public List<MedicationRequest> getMedicationRequests() {
    return medicationRequests;
  }

  public void setMedicationRequests(List<MedicationRequest> medicationRequests) {
    this.medicationRequests = medicationRequests;
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

  public List<ServiceRequest> getServiceRequests() {
    return serviceRequests;
  }

  public void setServiceRequests(List<ServiceRequest> serviceRequests) {
    this.serviceRequests = serviceRequests;
  }
}
