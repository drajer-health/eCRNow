package com.drajer.sof.service;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.base.composite.BaseContainedDt;
import ca.uhn.fhir.model.base.composite.BaseResourceReferenceDt;
import ca.uhn.fhir.model.dstu2.composite.ResourceReferenceDt;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Encounter.Location;
import ca.uhn.fhir.model.dstu2.resource.Encounter.Participant;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.dstu2.resource.Medication;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.dstu2.resource.Organization;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.Dstu2ResourcesData;
import com.drajer.sof.utils.FhirContextInitializer;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadingQueryDstu2Bundle {

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired Dstu2ResourcesData dstu2ResourcesData;

  private final Logger logger = LoggerFactory.getLogger(LoadingQueryDstu2Bundle.class);

  public Bundle createDSTU2Bundle(
      LaunchDetails launchDetails, Dstu2FhirData dstu2FhirData, Date start, Date end) {
    Bundle bundle = new Bundle();
    logger.info("Initializing FHIR Context for Version:::: {}", launchDetails.getFhirVersion());
    FhirContext context = fhirContextInitializer.getFhirContext(launchDetails.getFhirVersion());
    logger.info("Initializing Client");
    IGenericClient client =
        fhirContextInitializer.createClient(
            context, launchDetails.getEhrServerURL(), launchDetails.getAccessToken());

    // GET Patient Details and Add to Bundle
    try {
      logger.info("Get Patient Data");
      Patient patient =
          (Patient)
              fhirContextInitializer.getResouceById(
                  launchDetails, client, context, "Patient", launchDetails.getLaunchPatientId());
      Entry patientEntry = new Entry();
      patientEntry.setResource(patient);
      bundle.addEntry(patientEntry);
    } catch (Exception e) {
      logger.error("Error in getting Patient Data", e);
    }

    // Step 1: Get Encounters for Patient based on encId. (Create a method to get
    // encounters)
    // If encId is null, find encounters for patient within the start and end time
    // provided.
    // Add to the bundle.
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of encounterCodes.
    Encounter encounter = null;
    try {
      logger.info("Get Encounter Data");
      encounter =
          dstu2ResourcesData.getEncounterData(
              context, client, launchDetails, dstu2FhirData, start, end);
      if (encounter.getParticipant() != null) {
        List<Participant> participants = encounter.getParticipant();
        for (Participant participant : participants) {
          if (participant.getIndividual() != null) {
            ResourceReferenceDt practitionerReference = participant.getIndividual();
            Practitioner practitioner =
                (Practitioner)
                    fhirContextInitializer.getResouceById(
                        launchDetails,
                        client,
                        context,
                        "Practitioner",
                        practitionerReference.getReference().getIdPart());
            Entry practitionerEntry = new Entry().setResource(practitioner);
            bundle.addEntry(practitionerEntry);
          }
        }
      }
      if (encounter.getServiceProvider() != null) {
        ResourceReferenceDt organizationReference = encounter.getServiceProvider();
        Organization organization =
            (Organization)
                fhirContextInitializer.getResouceById(
                    launchDetails,
                    client,
                    context,
                    "Organization",
                    organizationReference.getReference().getIdPart());
        Entry organizationEntry = new Entry().setResource(organization);
        bundle.addEntry(organizationEntry);
      }
      if (encounter.getLocation() != null) {
        List<Location> enocunterLocations = encounter.getLocation();
        for (Location location : enocunterLocations) {
          if (location.getLocation() != null) {
            ResourceReferenceDt locationReference = location.getLocation();
            ca.uhn.fhir.model.dstu2.resource.Location locationResource =
                (ca.uhn.fhir.model.dstu2.resource.Location)
                    fhirContextInitializer.getResouceById(
                        launchDetails,
                        client,
                        context,
                        "Location",
                        locationReference.getReference().getIdPart());
            Entry locationEntry = new Entry().setResource(locationResource);
            bundle.addEntry(locationEntry);
          }
        }
      }
      Entry encounterEntry = new Entry().setResource(encounter);
      bundle.addEntry(encounterEntry);
    } catch (Exception e) {
      logger.error("Error in getting Encounter Data", e);
    }

    // Step 2: Get Conditions for Patient (Write a method)
    // Filter the conditions based on encounter Reference if Encounter Reference is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // Condition time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of ConditionCodes.
    try {
      logger.info("Get Condition Data");
      List<Condition> conditionsList =
          dstu2ResourcesData.getConditionData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered ConditionsList----> {}", conditionsList.size());
      dstu2FhirData.setConditions(conditionsList);
      for (Condition condition : conditionsList) {
        Entry conditionsEntry = new Entry().setResource(condition);
        bundle.addEntry(conditionsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Condition Data", e);
    }

    // Get Observations for Patients and laboratory category (Write a method).
    // Filter the observations based on encounter Reference if encounter is present.
    // If encounter is not present, then filter based on times (Start and end, if
    // observation time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of labResultCodes.
    try {
      logger.info("Get Observation Data");
      List<Observation> observationList =
          dstu2ResourcesData.getObservationData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered Observations----> {}", observationList.size());
      dstu2FhirData.setLabResults(observationList);
      for (Observation observation : observationList) {
        Entry observationsEntry = new Entry().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Observation Data", e);
    }

    // Get Pregnancy Observations
    try {
      logger.info("Get Pregnancy Observation Data");
      List<Observation> observationList =
          dstu2ResourcesData.getPregnancyObservationData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered Observations----> {}", observationList.size());
      dstu2FhirData.setPregnancyObs(observationList);
      for (Observation observation : observationList) {
        Entry observationsEntry = new Entry().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Pregnancy Observation Data", e);
    }

    // Get Travel Observations
    try {
      logger.info("Get Travel Observation Data");
      List<Observation> observationList =
          dstu2ResourcesData.getTravelObservationData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered Observations----> {}", observationList.size());
      dstu2FhirData.setTravelObs(observationList);
      for (Observation observation : observationList) {
        Entry observationsEntry = new Entry().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Travel Observation Data", e);
    }

    // Get MedicationAdministration for Patients and laboratory category (Write a
    // method).
    // Filter the MedicationAdministrations based on encounter Reference if
    // encounter is present.
    // If encounter is not present, then filter based on times (Start and end, if
    // medicationadministration time is between start and end times) -- Do this
    // later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of medicationCodes.
    try {
      logger.info("Get MedicationAdministration Data");
      List<MedicationAdministration> medAdministrationsList =
          dstu2ResourcesData.getMedicationAdministrationData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info(
          "Filtered MedicationAdministration-----------> {}", medAdministrationsList.size());
      dstu2FhirData.setMedicationAdministrations(medAdministrationsList);
      for (MedicationAdministration medAdministration : medAdministrationsList) {
        if (medAdministration.getMedication() != null
            && !medAdministration.getMedication().isEmpty()
            && medAdministration.getMedication() instanceof ResourceReferenceDt) {
          BaseResourceReferenceDt medRef =
              (BaseResourceReferenceDt) medAdministration.getMedication();
          String medReference = medRef.getReference().getValue();
          if (medReference.startsWith("#")) {
            BaseContainedDt medAdministrationContained = medAdministration.getContained();
            List<Medication> containedResources =
                (List<Medication>) medAdministrationContained.getContainedResources();
            if (containedResources
                .stream()
                .anyMatch(resource -> resource.getIdElement().getValue().equals(medReference))) {
              logger.info(
                  "Medication Resource exists in MedicationAdministration.contained. So no need to add again in Bundle.");
            }
          } else {
            logger.info("Medication Reference Found=============>");
            Medication medication =
                dstu2ResourcesData.getMedicationData(
                    context, client, launchDetails, dstu2FhirData, medReference);
            Entry medicationEntry = new Entry().setResource(medication);
            bundle.addEntry(medicationEntry);
            if (medication != null) {
              List<Medication> medicationList = new ArrayList<>();
              medicationList.add(medication);
              dstu2FhirData.setMedicationList(medicationList);
            }
          }
        }
        Entry medAdministrationEntry = new Entry().setResource(medAdministration);
        bundle.addEntry(medAdministrationEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationAdministration Data", e);
    }

    try {
      logger.info("Get MedicationStatement Data");
      List<MedicationStatement> medStatementsList =
          dstu2ResourcesData.getMedicationStatementData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered MedicationStatement-----------> {}", medStatementsList.size());
      for (MedicationStatement medStatement : medStatementsList) {
        Entry medStatementEntry = new Entry().setResource(medStatement);
        bundle.addEntry(medStatementEntry);
      }
      dstu2FhirData.setMedications(medStatementsList);
    } catch (Exception e) {
      logger.error("Error in getting the MedicationStatement Data", e);
    }

    // Get DiagnosticOrders for Patients (Write a method).
    // Filter the Diagnostic Orders based on encounter Reference if encounter is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // diagnostic order time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of diagnosticOrderCodes.

    try {
      logger.info("Get DiagnosticOrder Data");
      List<DiagnosticOrder> diagnosticOrdersList =
          dstu2ResourcesData.getDiagnosticOrderData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered DiagnosticOrders-----------> {}", diagnosticOrdersList.size());
      dstu2FhirData.setDiagOrders(diagnosticOrdersList);
      for (DiagnosticOrder diagnosticOrder : diagnosticOrdersList) {
        Entry diagnosticOrderEntry = new Entry().setResource(diagnosticOrder);
        bundle.addEntry(diagnosticOrderEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the DiagnosticOrder Data", e);
    }

    // Get Immunizations for Patients and laboratory category (Write a method).
    // Filter the Immunizations based on encounter Reference if encounter is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // Immunizations time is between start and end times) -- Do this later.
    // Add to the bundle
    try {
      List<Immunization> immunizationsList =
          dstu2ResourcesData.getImmunizationData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered Immunizations-----------> {}", immunizationsList.size());
      dstu2FhirData.setImmunizations(immunizationsList);
      for (Immunization immunization : immunizationsList) {
        Entry immunizationEntry = new Entry().setResource(immunization);
        bundle.addEntry(immunizationEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the Immunization Data", e);
    }

    // Get Diagnostic Reports for Patients (Write a method).
    // Filter the Diagnostic Reports based on encounter Reference if encounter is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // diagnostic Reports time is between start and end times) -- Do this later.
    // Add to the bundle
    try {
      List<DiagnosticReport> diagnosticReportList =
          dstu2ResourcesData.getDiagnosticReportData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered DiagnosticReports-----------> {}", diagnosticReportList.size());
      dstu2FhirData.setDiagReports(diagnosticReportList);
      for (DiagnosticReport diagnosticReport : diagnosticReportList) {
        Entry diagnosticReportEntry = new Entry().setResource(diagnosticReport);
        bundle.addEntry(diagnosticReportEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the DiagnosticReport Data", e);
    }

    // Setting bundle to FHIR Data
    logger.info(
        "------------------------------CodeableConcept Codes------------------------------");
    logger.info("Encounter Codes Size=====> {}", dstu2FhirData.getEncounterCodes().size());
    logger.info("Conditions Codes Size=====> {}", dstu2FhirData.getConditionCodes().size());
    logger.info("Observation Codes Size=====> {}", dstu2FhirData.getLabResultCodes().size());
    logger.info("Medication Codes Size=====> {}", dstu2FhirData.getMedicationCodes().size());
    logger.info("Immunization Codes Size=====> {}", dstu2FhirData.getImmuniationCodes().size());
    logger.info(
        "DiagnosticReport Codes Size=====> {}", dstu2FhirData.getDiagnosticReportCodes().size());

    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/LoadingQueryDSTU2Bundle-"
            + launchDetails.getLaunchPatientId()
            + ".json";
    ApplicationUtils.saveDataToFile(
        context.newJsonParser().encodeResourceToString(bundle), fileName);
    return bundle;
  }
}
