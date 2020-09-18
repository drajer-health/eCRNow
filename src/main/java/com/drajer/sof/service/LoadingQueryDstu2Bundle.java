package com.drajer.sof.service;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.eca.model.ActionRepo;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.Dstu2ResourcesData;
import com.drajer.sof.utils.FhirContextInitializer;
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

  @Autowired TriggerQueryDstu2Bundle triggerQueryDstu2Bundle;

  private final Logger logger = LoggerFactory.getLogger(LoadingQueryDstu2Bundle.class);

  public Bundle createDSTU2Bundle(
      LaunchDetails launchDetails, Dstu2FhirData dstu2FhirData, Date start, Date end) {
    Bundle bundle = new Bundle();
    logger.info("Initializing FHIR Context for Version::::" + launchDetails.getFhirVersion());
    FhirContext context = fhirContextInitializer.getFhirContext(launchDetails.getFhirVersion());
    logger.info("Initializing Client");
    IGenericClient client =
        fhirContextInitializer.createClient(
            context, launchDetails.getEhrServerURL(), launchDetails.getAccessToken());

    // GET Patient Details and Add to Bundle
    bundle = triggerQueryDstu2Bundle.getPatientData(launchDetails, client, context, bundle);

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
      bundle =
          triggerQueryDstu2Bundle.getEncounterData(
              encounter, launchDetails, client, context, bundle);
    } catch (Exception e) {
      logger.error("Error in getting Encounter Data");
    }

    bundle =
        triggerQueryDstu2Bundle.getConditionObservationData(
            encounter, dstu2FhirData, start, end, launchDetails, client, context, bundle);

    // Get Pregnancy Observations
    try {
      logger.info("Get Pregnancy Observation Data");
      List<Observation> observationList =
          dstu2ResourcesData.getPregnancyObservationData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered Observations---->" + observationList.size());
      dstu2FhirData.setPregnancyObs(observationList);
      for (Observation observation : observationList) {
        Entry observationsEntry = new Entry().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Pregnancy Observation Data");
    }

    // Get Travel Observations
    try {
      logger.info("Get Travel Observation Data");
      List<Observation> observationList =
          dstu2ResourcesData.getTravelObservationData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered Observations---->" + observationList.size());
      dstu2FhirData.setTravelObs(observationList);
      for (Observation observation : observationList) {
        Entry observationsEntry = new Entry().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Travel Observation Data");
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
    bundle =
        triggerQueryDstu2Bundle.getMedicationAdministrationData(
            encounter, dstu2FhirData, start, end, launchDetails, client, context, bundle);

    try {
      logger.info("Get MedicationStatement Data");
      List<MedicationStatement> medStatementsList =
          dstu2ResourcesData.getMedicationStatementData(
              context, client, launchDetails, dstu2FhirData, encounter, start, end);
      logger.info("Filtered MedicationStatement----------->" + medStatementsList.size());
      for (MedicationStatement medStatement : medStatementsList) {
        Entry medStatementEntry = new Entry().setResource(medStatement);
        bundle.addEntry(medStatementEntry);
      }
      dstu2FhirData.setMedications(medStatementsList);
    } catch (Exception e) {
      logger.error("Error in getting the MedicationStatement Data");
    }

    // Get DiagnosticOrders for Patients (Write a method).
    // Filter the Diagnostic Orders based on encounter Reference if encounter is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // diagnostic order time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of diagnosticOrderCodes.
    bundle =
        triggerQueryDstu2Bundle.getDiagnosticOrderData(
            encounter, dstu2FhirData, start, end, launchDetails, client, context, bundle);

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
      logger.info("Filtered Immunizations----------->" + immunizationsList.size());
      dstu2FhirData.setImmunizations(immunizationsList);
      for (Immunization immunization : immunizationsList) {
        Entry immunizationEntry = new Entry().setResource(immunization);
        bundle.addEntry(immunizationEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the Immunization Data");
    }

    // Setting bundle to FHIR Data
    logger.info(
        "------------------------------CodeableConcept Codes------------------------------");
    logger.info("Encounter Codes Size=====>" + dstu2FhirData.getEncounterCodes().size());
    logger.info("Conditions Codes Size=====>" + dstu2FhirData.getConditionCodes().size());
    logger.info("Observation Codes Size=====>" + dstu2FhirData.getLabResultCodes().size());
    logger.info("Medication Codes Size=====>" + dstu2FhirData.getMedicationCodes().size());
    logger.info("Immunization Codes Size=====>" + dstu2FhirData.getImmuniationCodes().size());
    logger.info(
        "DiagnosticReport Codes Size=====>" + dstu2FhirData.getDiagnosticReportCodes().size());
    // logger.info("DiagnosticOrders Codes Size=====>" +
    // dstu2FhirData.getDiagnosticOrderCodes().size());

    // logger.info(context.newJsonParser().encodeResourceToString(bundle));

    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/LoadingQueryDSTU2Bundle-"
            + launchDetails.getLaunchPatientId()
            + ".json";
    FhirContextInitializer.saveBundleToFile(
        context.newJsonParser().encodeResourceToString(bundle), fileName);
    return bundle;
  }
}
