package com.drajer.bsa.utils;

import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.kar.action.CheckTriggerCodeStatus;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.MatchTriggerStatus;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.List;
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

/**
 * The class is used to convert from Release3.0 data structures to Release2.0 data structures so
 * that common code such as CDA generation, validation and routing can be reused from Release2.0.
 *
 * @author nbashyam
 */
public class R3ToR2DataConverterUtils {

  private static final Logger logger = LoggerFactory.getLogger(R3ToR2DataConverterUtils.class);

  private R3ToR2DataConverterUtils() {
    super();
  }

  /**
   * The method creates the Release2.0 R4FhirData and LaunchDetails data structures from
   * KarProcessingData.
   *
   * @param kd
   * @return
   */
  public static Pair<R4FhirData, LaunchDetails> convertKarProcessingDataForCdaGeneration(
      KarProcessingData kd) {

    R4FhirData r4FhirData = new R4FhirData();
    LaunchDetails details = new LaunchDetails();
    Bundle data = new Bundle();
    setPatientStateInLaunchDetails(kd, details);

    if (kd != null) {

      logger.debug(" KarProcessingData is not null, to be converted ");

      details.setEhrServerURL(kd.getNotificationContext().getFhirServerBaseUrl());

      Set<Resource> patients = kd.getResourcesByType(ResourceType.Patient.toString());
      if (patients != null && !patients.isEmpty()) {

        logger.info(" Setting up the patient for R4FhirData ");
        Resource patient = patients.iterator().next();
        r4FhirData.setPatient((Patient) patient);
        details.setLaunchPatientId(patient.getIdElement().getIdPart());
        data.addEntry(new BundleEntryComponent().setResource(patient));
      }

      Set<Resource> encounters = kd.getResourcesByType(ResourceType.Encounter.toString());
      if (encounters != null && !encounters.isEmpty()) {

        logger.info(" Setting up the encounter for R4FhirData ");
        Resource encounter = encounters.iterator().next();
        r4FhirData.setEncounter((Encounter) encounter);
        details.setEncounterId(encounter.getIdElement().getIdPart());
        data.addEntry(new BundleEntryComponent().setResource(encounter));
      }

      Set<Resource> locations = kd.getResourcesByType(ResourceType.Location.toString());
      if (locations != null && !locations.isEmpty()) {

        logger.info(" Setting up the location for R4FhirData ");
        Resource location = locations.iterator().next();
        r4FhirData.setLocation((Location) location);
        data.addEntry(new BundleEntryComponent().setResource(location));
      }

      Set<Resource> orgs = kd.getResourcesByType(ResourceType.Organization.toString());
      if (orgs != null && !orgs.isEmpty()) {

        logger.info(" Setting up the organization for R4FhirData ");
        Resource organization = orgs.iterator().next();
        r4FhirData.setOrganization((Organization) organization);
        data.addEntry(new BundleEntryComponent().setResource(organization));
      }

      Set<Resource> conditions = kd.getResourcesByType(ResourceType.Condition.toString());
      ArrayList<Condition> conditionList = new ArrayList<>();
      if (conditions != null && !conditions.isEmpty()) {

        for (Resource r : conditions) {
          conditionList.add((Condition) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setConditions(conditionList);
      }

      Set<Resource> imms = kd.getResourcesByType(ResourceType.Immunization.toString());
      ArrayList<Immunization> immList = new ArrayList<>();
      if (imms != null && !imms.isEmpty()) {

        for (Resource r : imms) {
          immList.add((Immunization) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setImmunizations(immList);
      }

      Set<Resource> procedures = kd.getResourcesByType(ResourceType.Procedure.toString());
      ArrayList<Procedure> procList = new ArrayList<>();
      if (procedures != null && !procedures.isEmpty()) {

        for (Resource r : procedures) {
          procList.add((Procedure) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
      }

      Set<Resource> medReqs = kd.getResourcesByType(ResourceType.MedicationRequest.toString());
      ArrayList<MedicationRequest> medReqList = new ArrayList<>();
      if (medReqs != null && !medReqs.isEmpty()) {

        for (Resource r : medReqs) {
          medReqList.add((MedicationRequest) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setMedicationRequests(medReqList);
      }

      Set<Resource> medAdms =
          kd.getResourcesByType(ResourceType.MedicationAdministration.toString());
      ArrayList<MedicationAdministration> medAdmList = new ArrayList<>();
      if (medAdms != null && !medAdms.isEmpty()) {

        for (Resource r : medAdms) {
          medAdmList.add((MedicationAdministration) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setMedicationAdministrations(medAdmList);
      }

      Set<Resource> meds = kd.getResourcesByType(ResourceType.Medication.toString());
      ArrayList<Medication> medList = new ArrayList<>();
      if (meds != null && !meds.isEmpty()) {

        for (Resource r : meds) {
          medList.add((Medication) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setMedicationList(medList);
      }

      Set<Resource> servReqs = kd.getResourcesByType(ResourceType.ServiceRequest.toString());
      ArrayList<ServiceRequest> servReqList = new ArrayList<>();
      if (servReqs != null && !servReqs.isEmpty()) {

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
      ArrayList<Observation> labObsList = new ArrayList<>();
      if (labObs != null && !labObs.isEmpty()) {

        for (Resource r : labObs) {
          labObsList.add((Observation) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
        r4FhirData.setLabResults(labObsList);
      }

      Set<Resource> vitalObs =
          ReportGenerationUtils.filterObservationsByCategory(
              observations, ObservationCategory.VITALSIGNS.toCode());
      ArrayList<Observation> vitalObsList = new ArrayList<>();
      if (vitalObs != null && !vitalObs.isEmpty()) {

        for (Resource r : labObs) {
          vitalObsList.add((Observation) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
      }

      Set<Resource> socObs =
          ReportGenerationUtils.filterObservationsByCategory(
              observations, ObservationCategory.SOCIALHISTORY.toCode());
      ArrayList<Observation> socObsList = new ArrayList<>();
      if (socObs != null && !socObs.isEmpty()) {

        for (Resource r : socObs) {
          socObsList.add((Observation) r);
          data.addEntry(new BundleEntryComponent().setResource(r));
        }
      }

      r4FhirData.setData(data);

    } else {
      logger.error(" Cannot convert Null Kar Processing Data For Cda Generation ");
    }

    return new Pair<>(r4FhirData, details);
  }

  /**
   * The method updates the LaunchDetails.status field with the right PatientExecutionState which is
   * setup from KarProcessingData.actionStatus field.
   *
   * @param data
   * @param details
   */
  private static void setPatientStateInLaunchDetails(
      KarProcessingData data, LaunchDetails details) {

    PatientExecutionState state = new PatientExecutionState();

    // Set Trigger codes.
    List<BsaActionStatus> statuses = data.getActionStatusByType(ActionType.CHECK_TRIGGER_CODES);

    for (BsaActionStatus entry : statuses) {

      CheckTriggerCodeStatus ctcs = (CheckTriggerCodeStatus) entry;
      MatchTriggerStatus mts = new MatchTriggerStatus();

      mts.setActionId(ctcs.getActionId());
      mts.setJobStatus(getJobStatusForActionStatus(ctcs.getActionStatus()));
      mts.setTriggerMatchStatus(ctcs.getTriggerMatchStatus());
      mts.setMatchedCodes(ctcs.getMatchedCodes());
      state.setMatchTriggerStatus(mts);
    }

    ObjectMapper mapper = new ObjectMapper();

    try {

      details.setStatus(mapper.writeValueAsString(state));

    } catch (JsonProcessingException e) {

      String msg = "Unable to update execution state";
      logger.error(msg, e);
      throw new RuntimeException(msg, e);
    }
  }

  private static JobStatus getJobStatusForActionStatus(BsaActionStatusType status) {

    if (status == BsaActionStatusType.COMPLETED) return JobStatus.COMPLETED;
    else if (status == BsaActionStatusType.ABORTED) return JobStatus.ABORTED;
    else if (status == BsaActionStatusType.FAILED) return JobStatus.ABORTED;
    else if (status == BsaActionStatusType.IN_PROGRESS) return JobStatus.IN_PROGRESS;
    else if (status == BsaActionStatusType.NOT_STARTED) return JobStatus.NOT_STARTED;
    else if (status == BsaActionStatusType.SCHEDULED) return JobStatus.SCHEDULED;
    else return JobStatus.ABORTED;
  }
}
