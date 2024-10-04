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
import org.hl7.fhir.r4.model.Procedure;
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
  private List<Observation> diagReportObservations;
  private List<Observation> labResults;
  private List<Observation> labResultValueObservations;
  private List<Observation> vitalObs;
  private List<Observation> travelObs;
  private List<Observation> pregnancyObs;
  private List<Observation> occupationObs;
  private List<Observation> pregnancyStatusObs;
  private List<Observation> lmpObs;
  private List<Observation> postPartumObs;
  private List<Observation> pregnancyOutcomeObs;
  private List<Observation> homelessObs;
  private List<Observation> disabilityObs;
  private List<Observation> vaccineCredObs;
  private List<Observation> residencyObs;
  private List<Observation> nationalityObs;
  private List<Immunization> immunizations;
  private List<MedicationStatement> medications;
  private List<MedicationRequest> medicationRequests;
  private List<MedicationAdministration> medicationAdministrations;
  private List<Medication> medicationList;
  private List<ServiceRequest> serviceRequests;
  private List<Procedure> procedureList;

  private Address jurisdiction;
  private List<Address> jurisdictions;

  public void prepareJurisdicationData() {

    if (location != null && location.getAddress() != null) {
      logger.info(" Using Location information from the Encounter for Jurisdiction");
      jurisdiction = location.getAddress();
    } else if (organization != null && organization.getAddressFirstRep() != null) {
      logger.info(
          " Using Organization information from Encounter.serviceProvider as Jurisdiction ");
      jurisdiction = organization.getAddressFirstRep();
    } else if (patient != null && patient.getAddress() != null) {
      logger.info(
          " Using Patient address information for Jurisdiction assuming first entry of list.");
      jurisdictions = patient.getAddress();

      jurisdiction = patient.getAddressFirstRep();
    }
  }

  public Boolean hasRequiredDataForEicr() {

    Boolean requiredDataPresent = true;

    // Check Patient and Encounter.
    if (patient == null) {
      logger.warn("Patient is null, cannot generate eICR ");
      requiredDataPresent = false;
    }

    if (encounter == null) {
      logger.warn("Encounter is null, cannot generate eICR ");
      requiredDataPresent = false;
    }

    if (jurisdiction == null) {
      logger.warn("Jurisdiction is null, cannot generate eICR ");
      requiredDataPresent = false;
    }

    return requiredDataPresent;
  }

  public Practitioner getPractitionerById(String id) {

    logger.debug("Id for retrieving practitioner = {}", id);
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
    diagReportObservations = new ArrayList<>();
    labResults = new ArrayList<>();
    labResultValueObservations = new ArrayList<>();
    vitalObs = new ArrayList<>();
    travelObs = new ArrayList<>();
    pregnancyObs = new ArrayList<>();
    occupationObs = new ArrayList<>();
    immunizations = new ArrayList<>();
    medications = new ArrayList<>();
    medicationRequests = new ArrayList<>();
    medicationAdministrations = new ArrayList<>();
    serviceRequests = new ArrayList<>();
    medicationList = new ArrayList<>();
    procedureList = new ArrayList<>();

    // 3.1 and FHIR Related data.
    pregnancyStatusObs = new ArrayList<>();
    lmpObs = new ArrayList<>();
    postPartumObs = new ArrayList<>();
    pregnancyOutcomeObs = new ArrayList<>();
    homelessObs = new ArrayList<>();
    disabilityObs = new ArrayList<>();
    vaccineCredObs = new ArrayList<>();
    residencyObs = new ArrayList<>();
    nationalityObs = new ArrayList<>();
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

  public void addPractitionersList(List<Practitioner> practitionersList) {
    this.practitionersList.addAll(practitionersList);
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

  public void addConditions(List<Condition> conditions) {
    this.conditions.addAll(conditions);
  }

  public List<Condition> getEncounterDiagnosisConditions() {
    return encounterDiagnosisConditions;
  }

  public void setEncounterDiagnosisConditions(List<Condition> encounterDiagnosisConditions) {
    this.encounterDiagnosisConditions = encounterDiagnosisConditions;
  }

  public void addEncounterDiagnosisConditions(List<Condition> encounterDiagnosisConditions) {
    this.encounterDiagnosisConditions.addAll(encounterDiagnosisConditions);
  }

  public List<Condition> getPregnancyConditions() {
    return pregnancyConditions;
  }

  public void setPregnancyConditions(List<Condition> pregnancyConditions) {
    this.pregnancyConditions = pregnancyConditions;
  }

  public void addPregnancyConditions(List<Condition> pregnancyConditions) {
    this.pregnancyConditions.addAll(pregnancyConditions);
  }

  public List<DiagnosticReport> getDiagReports() {
    return diagReports;
  }

  public void setDiagReports(List<DiagnosticReport> diagReports) {
    this.diagReports = diagReports;
  }

  public void addDiagReports(List<DiagnosticReport> diagReports) {
    this.diagReports.addAll(diagReports);
  }

  public List<Observation> getLabResults() {
    return labResults;
  }

  public void setLabResults(List<Observation> labResults) {
    this.labResults = labResults;
  }

  public void addLabResults(List<Observation> labRes) {
    this.labResults.addAll(labRes);
  }

  public List<Observation> getTravelObs() {
    return travelObs;
  }

  public void setTravelObs(List<Observation> travelObs) {
    this.travelObs = travelObs;
  }

  public void addTravelObs(List<Observation> travelObs) {
    this.travelObs.addAll(travelObs);
  }

  public List<Observation> getPregnancyObs() {
    return pregnancyObs;
  }

  public void setPregnancyObs(List<Observation> pregnancyObs) {
    this.pregnancyObs = pregnancyObs;
  }

  public void addPregnancyObs(List<Observation> pregnancyObs) {
    this.pregnancyObs.addAll(pregnancyObs);
  }

  public List<Observation> getOccupationObs() {
    return occupationObs;
  }

  public void setOccupationObs(List<Observation> occupationObs) {
    this.occupationObs = occupationObs;
  }

  public void addOccupationObs(List<Observation> occupationObs) {
    this.occupationObs.addAll(occupationObs);
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

  public void addMedicationList(List<Medication> medicationList) {
    this.medicationList.addAll(medicationList);
  }

  public List<ServiceRequest> getServiceRequests() {
    return serviceRequests;
  }

  public void setServiceRequests(List<ServiceRequest> serviceRequests) {
    this.serviceRequests = serviceRequests;
  }

  public void addServiceRequests(List<ServiceRequest> serviceRequests) {
    this.serviceRequests.addAll(serviceRequests);
  }

  public List<Observation> getLabResultValueObservations() {
    return labResultValueObservations;
  }

  public void setLabResultValueObservations(List<Observation> labResultValueObservations) {
    this.labResultValueObservations = labResultValueObservations;
  }

  public List<Procedure> getProcedureList() {
    return procedureList;
  }

  public void setProcedureList(List<Procedure> procedureList) {
    this.procedureList = procedureList;
  }

  public List<Observation> getDiagReportObservations() {
    return diagReportObservations;
  }

  public void setDiagReportObservations(List<Observation> diagReportObservations) {
    this.diagReportObservations = diagReportObservations;
  }

  public List<Observation> getPregnancyStatusObs() {
    return pregnancyStatusObs;
  }

  public void setPregnancyStatusObs(List<Observation> pregnancyStatusObs) {
    this.pregnancyStatusObs = pregnancyStatusObs;
  }

  public void addPregnancyStatusObs(List<Observation> pregnancyStatusObs) {
    this.pregnancyStatusObs.addAll(pregnancyStatusObs);
  }

  public List<Observation> getLmpObs() {
    return lmpObs;
  }

  public void setLmpObs(List<Observation> lmpObs) {
    this.lmpObs = lmpObs;
  }

  public void addLmpObs(List<Observation> lmpObs) {
    this.lmpObs.addAll(lmpObs);
  }

  public List<Observation> getPostPartumObs() {
    return postPartumObs;
  }

  public void setPostPartumObs(List<Observation> postPartumObs) {
    this.postPartumObs = postPartumObs;
  }

  public void addPostPartumObs(List<Observation> postPartumObs) {
    this.postPartumObs.addAll(postPartumObs);
  }

  public List<Observation> getPregnancyOutcomeObs() {
    return pregnancyOutcomeObs;
  }

  public void setPregnancyOutcomeObs(List<Observation> pregnancyOutcomeObs) {
    this.pregnancyOutcomeObs = pregnancyOutcomeObs;
  }

  public void addPregnancyOutcomeObs(List<Observation> pregnancyOutcomeObs) {
    this.pregnancyOutcomeObs.addAll(pregnancyOutcomeObs);
  }

  public List<Observation> getHomelessObs() {
    return homelessObs;
  }

  public void setHomelessObs(List<Observation> homelessObs) {
    this.homelessObs = homelessObs;
  }

  public void addHomelessObs(List<Observation> homelessObs) {
    this.homelessObs.addAll(homelessObs);
  }

  public List<Observation> getDisabilityObs() {
    return disabilityObs;
  }

  public void setDisabilityObs(List<Observation> disabilityObs) {
    this.disabilityObs = disabilityObs;
  }

  public void addDisabilityObs(List<Observation> disabilityObs) {
    this.disabilityObs.addAll(disabilityObs);
  }

  public List<Observation> getVaccineCredObs() {
    return vaccineCredObs;
  }

  public void setVaccineCredObs(List<Observation> vaccineCredObs) {
    this.vaccineCredObs = vaccineCredObs;
  }

  public void addVaccineCredObs(List<Observation> vaccineCredObs) {
    this.vaccineCredObs.addAll(vaccineCredObs);
  }

  public List<Observation> getResidencyObs() {
    return residencyObs;
  }

  public void setResidencyObs(List<Observation> residencyObs) {
    this.residencyObs = residencyObs;
  }

  public void addResidencyObs(List<Observation> residencyObs) {
    this.residencyObs.addAll(residencyObs);
  }

  public List<Observation> getNationalityObs() {
    return nationalityObs;
  }

  public void setNationalityObs(List<Observation> nationalityObs) {
    this.nationalityObs = nationalityObs;
  }

  public void addNationalityObs(List<Observation> nationalityObs) {
    this.nationalityObs.addAll(nationalityObs);
  }

  public Address getJurisdiction() {
    return jurisdiction;
  }

  public void setJurisdiction(Address jurisdiction) {
    this.jurisdiction = jurisdiction;
  }

  public List<Address> getJurisdictions() {
    return jurisdictions;
  }

  public void setJurisdictions(List<Address> jurisdictions) {
    this.jurisdictions = jurisdictions;
  }

  public List<Observation> getVitalObs() {
    return vitalObs;
  }

  public void setVitalObs(List<Observation> vitalObs) {
    this.vitalObs = vitalObs;
  }

  @Override
  public String toString() {
    return "R4FhirData [data="
        + data
        + ", patient="
        + patient
        + ", practitioner="
        + practitioner
        + ", practitionersList="
        + practitionersList
        + ", encounter="
        + encounter
        + ", location="
        + location
        + ", locationList="
        + locationList
        + ", organization="
        + organization
        + ", conditions="
        + conditions
        + ", encounterDiagnosisConditions="
        + encounterDiagnosisConditions
        + ", pregnancyConditions="
        + pregnancyConditions
        + ", diagReports="
        + diagReports
        + ", diagReportObservations="
        + diagReportObservations
        + ", labResults="
        + labResults
        + ", labResultValueObservations="
        + labResultValueObservations
        + ", travelObs="
        + travelObs
        + ", pregnancyObs="
        + pregnancyObs
        + ", occupationObs="
        + occupationObs
        + ", immunizations="
        + immunizations
        + ", medications="
        + medications
        + ", medicationRequests="
        + medicationRequests
        + ", medicationAdministrations="
        + medicationAdministrations
        + ", medicationList="
        + medicationList
        + ", serviceRequests="
        + serviceRequests
        + ", procedureList="
        + procedureList
        + ", jurisdiction="
        + jurisdiction
        + ", jurisdictions="
        + jurisdictions
        + "]";
  }
}
