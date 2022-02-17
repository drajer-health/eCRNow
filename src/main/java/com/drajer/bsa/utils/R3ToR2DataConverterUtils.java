package com.drajer.bsa.utils;

import com.drajer.bsa.model.KarProcessingData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class R3ToR2DataConverterUtils {

  private static final Logger logger = LoggerFactory.getLogger(R3ToR2DataConverterUtils.class);

  public static Pair<R4FhirData, LaunchDetails> convertKarProcessingDataForCdaGeneration(
      KarProcessingData kd) {

    R4FhirData r4FhirData = new R4FhirData();
    ;
    LaunchDetails details = new LaunchDetails();
    Bundle data = new Bundle();

    if (kd != null) {

      logger.debug(" KarProcessingData is not null, to be converted ");

      details.setEhrServerURL(kd.getNotificationContext().getFhirServerBaseUrl());

      Set<Resource> patients = kd.getResourcesByType(ResourceType.Patient.toString());
      if (patients != null && patients.size() >= 1) {

        logger.info(" Setting up the patient for R4FhirData ");
        Resource patient = patients.iterator().next();
        r4FhirData.setPatient((Patient) patient);
        details.setLaunchPatientId(patient.getIdElement().getIdPart());
        data.addEntry(new BundleEntryComponent().setResource(patient));
      }

      Set<Resource> encounters = kd.getResourcesByType(ResourceType.Encounter.toString());
      if (encounters != null && encounters.size() >= 1) {

        logger.info(" Setting up the encounter for R4FhirData ");
        Resource encounter = encounters.iterator().next();
        r4FhirData.setEncounter((Encounter) encounter);
        details.setEncounterId(encounter.getIdElement().getIdPart());
        data.addEntry(new BundleEntryComponent().setResource(encounter));
      }

      Set<Resource> locations = kd.getResourcesByType(ResourceType.Location.toString());
      if (locations != null && locations.size() >= 1) {

        logger.info(" Setting up the location for R4FhirData ");
        Resource location = locations.iterator().next();
        r4FhirData.setLocation((Location) location);
        data.addEntry(new BundleEntryComponent().setResource(location));
      }

      Set<Resource> orgs = kd.getResourcesByType(ResourceType.Organization.toString());
      if (orgs != null && orgs.size() >= 1) {

        logger.info(" Setting up the organization for R4FhirData ");
        Resource organization = orgs.iterator().next();
        r4FhirData.setOrganization((Organization) organization);
        data.addEntry(new BundleEntryComponent().setResource(organization));
      }

      Set<Resource> conditions = kd.getResourcesByType(ResourceType.Condition.toString());
      ArrayList<Condition> conditionList = new ArrayList<Condition>();
      if (conditions != null && conditions.size() >= 1) {

        for (Resource r : conditions) {
          conditionList.add((Condition) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setConditions(conditionList);
      }

      Set<Resource> imms = kd.getResourcesByType(ResourceType.Immunization.toString());
      ArrayList<Immunization> immList = new ArrayList<Immunization>();
      if (imms != null && imms.size() >= 1) {

        for (Resource r : imms) {
          immList.add((Immunization) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setImmunizations(immList);
      }

      Set<Resource> procedures = kd.getResourcesByType(ResourceType.Procedure.toString());
      ArrayList<Procedure> procList = new ArrayList<Procedure>();
      if (procedures != null && procedures.size() >= 1) {

        for (Resource r : procedures) {
          procList.add((Procedure) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
      }

      Set<Resource> medReqs = kd.getResourcesByType(ResourceType.MedicationRequest.toString());
      ArrayList<MedicationRequest> medReqList = new ArrayList<MedicationRequest>();
      if (medReqs != null && medReqs.size() >= 1) {

        for (Resource r : medReqs) {
          medReqList.add((MedicationRequest) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setMedicationRequests(medReqList);
      }

      Set<Resource> medAdms =
          kd.getResourcesByType(ResourceType.MedicationAdministration.toString());
      ArrayList<MedicationAdministration> medAdmList = new ArrayList<MedicationAdministration>();
      if (medAdms != null && medAdms.size() >= 1) {

        for (Resource r : medAdms) {
          medAdmList.add((MedicationAdministration) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setMedicationAdministrations(medAdmList);
      }

      Set<Resource> meds = kd.getResourcesByType(ResourceType.Medication.toString());
      ArrayList<Medication> medList = new ArrayList<Medication>();
      if (meds != null && meds.size() >= 1) {

        for (Resource r : meds) {
          medList.add((Medication) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setMedicationList(medList);
      }

      Set<Resource> servReqs = kd.getResourcesByType(ResourceType.ServiceRequest.toString());
      ArrayList<ServiceRequest> servReqList = new ArrayList<ServiceRequest>();
      if (servReqs != null && servReqs.size() >= 1) {

        for (Resource r : servReqs) {
          servReqList.add((ServiceRequest) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setServiceRequests(servReqList);
      }

      Set<Resource> observations = kd.getResourcesByType(ResourceType.Observation.toString());

      Set<Resource> labObs =
          ReportGenerationUtils.filterObservationsByCategory(
              observations, ObservationCategory.LABORATORY.toCode());
      ArrayList<Observation> labObsList = new ArrayList<Observation>();
      if (labObs != null && labObs.size() >= 1) {

        for (Resource r : labObs) {
          labObsList.add((Observation) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setLabResults(labObsList);
      }

      Set<Resource> vitalObs =
          ReportGenerationUtils.filterObservationsByCategory(
              observations, ObservationCategory.VITALSIGNS.toCode());
      ArrayList<Observation> vitalObsList = new ArrayList<Observation>();
      if (vitalObs != null && vitalObs.size() >= 1) {

        for (Resource r : labObs) {
          vitalObsList.add((Observation) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
      }

      Set<Resource> socObs =
          ReportGenerationUtils.filterObservationsByCategory(
              observations, ObservationCategory.SOCIALHISTORY.toCode());
      ArrayList<Observation> socObsList = new ArrayList<Observation>();
      if (socObs != null && socObs.size() >= 1) {

        for (Resource r : socObs) {
          socObsList.add((Observation) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
      }

    } else {
      logger.error(" Cannot convert Null Kar Processing Data For Cda Generation ");
    }

    return new Pair<R4FhirData, LaunchDetails>(r4FhirData, details);
  }
}
