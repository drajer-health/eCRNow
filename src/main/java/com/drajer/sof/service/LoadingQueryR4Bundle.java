package com.drajer.sof.service;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.EventTypes;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import java.util.Date;
import java.util.List;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadingQueryR4Bundle {

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired R4ResourcesData r4ResourcesData;

  private final Logger logger = LoggerFactory.getLogger(LoadingQueryR4Bundle.class);

  public Bundle createR4Bundle(
      LaunchDetails launchDetails, R4FhirData r4FhirData, Date start, Date end) {

    logger.trace("Initializing FHIR Context for Version:::: {}", launchDetails.getFhirVersion());
    FhirContext context = fhirContextInitializer.getFhirContext(launchDetails.getFhirVersion());
    IGenericClient client =
        fhirContextInitializer.createClient(
            context, launchDetails, EventTypes.QueryType.LOADING_QUERY);

    Bundle bundle =
        r4ResourcesData.getCommonResources(r4FhirData, start, end, launchDetails, client, context);

    Encounter encounter =
        (Encounter) r4ResourcesData.getResourceFromBundle(bundle, Encounter.class);

    r4ResourcesData.loadPractitionersLocationAndOrganization(
        context, client, launchDetails, r4FhirData, encounter, bundle, start, end);

    r4ResourcesData.loadMedicationsData(
        context, client, launchDetails, r4FhirData, encounter, bundle, start, end);

    // Get Pregnancy Observations, will be used once support of pregnancy observation is added in
    // Social History section.
    //    try {
    //      List<Observation> observationList =
    //          r4ResourcesData.getPregnancyObservationData(
    //              context, client, launchDetails, r4FhirData, encounter, start, end);
    //      r4FhirData.setPregnancyObs(observationList);
    //      for (Observation observation : observationList) {
    //        BundleEntryComponent observationsEntry =
    //            new BundleEntryComponent().setResource(observation);
    //        bundle.addEntry(observationsEntry);
    //      }
    //    } catch (Exception e) {
    //      logger.error("Error in getting Pregnancy Observation Data", e);
    //    }

    // Get Travel Observations
    try {
      List<Observation> observationList =
          r4ResourcesData.getTravelObservationData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      r4FhirData.setTravelObs(observationList);
      for (Observation observation : observationList) {
        BundleEntryComponent observationsEntry =
            new BundleEntryComponent().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Travel Observation Data", e);
    }

    // Get Social History Observations (Occupation)
    try {
      List<Observation> observationList =
          r4ResourcesData.getSocialHistoryObservationDataOccupation(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      r4FhirData.setOccupationObs(observationList);
      for (Observation observation : observationList) {
        BundleEntryComponent observationsEntry =
            new BundleEntryComponent().setResource(observation);
        bundle.addEntry(observationsEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Social History Observation(Occupation) Data", e);
    }

    // Get Pregnancy Conditions
    try {
      List<Condition> conditionList =
          r4ResourcesData.getPregnancyConditions(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      r4FhirData.setPregnancyConditions(conditionList);
      for (Condition condition : conditionList) {
        BundleEntryComponent conditionEntry = new BundleEntryComponent().setResource(condition);
        bundle.addEntry(conditionEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting Pregnancy Conditions", e);
    }

    try {
      List<MedicationStatement> medStatementsList =
          r4ResourcesData.getMedicationStatementData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      r4FhirData.setMedicationStatements(medStatementsList);
      for (MedicationStatement medStatement : medStatementsList) {
        BundleEntryComponent medStatementEntry =
            new BundleEntryComponent().setResource(medStatement);
        bundle.addEntry(medStatementEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the MedicationStatement Data", e);
    }

    // Get Immunizations for Patients and laboratory category (Write a method).
    // Filter the Immunizations based on encounter Reference if encounter is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // Immunizations time is between start and end times) -- Do this later.
    // Add to the bundle
    try {
      List<Immunization> immunizationsList =
          r4ResourcesData.getImmunizationData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      r4FhirData.setImmunizations(immunizationsList);
      for (Immunization immunization : immunizationsList) {
        BundleEntryComponent immunizationEntry =
            new BundleEntryComponent().setResource(immunization);
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
          r4ResourcesData.getDiagnosticReportData(
              context, client, launchDetails, r4FhirData, encounter, start, end);
      r4FhirData.setDiagReports(diagnosticReportList);
      for (DiagnosticReport diagnosticReport : diagnosticReportList) {
        BundleEntryComponent diagnosticReportEntry =
            new BundleEntryComponent().setResource(diagnosticReport);
        bundle.addEntry(diagnosticReportEntry);
      }
    } catch (Exception e) {
      logger.error("Error in getting the DiagnosticReport Data", e);
    }

    // Setting bundle to FHIR Data
    logger.info(
        "------------------------------CodeableConcept Codes------------------------------\n"
            + "Encounter Codes Size=====> {} \n"
            + "Conditions Codes Size=====> {} \n"
            + "Observation Codes Size=====> {}\n"
            + "Medication Codes Size=====> {}\n"
            + "Immunization Codes Size=====> {}\n"
            + "DiagnosticReport Codes Size=====> {}\n"
            + "ServiceRequests Codes Size=====> {}",
        r4FhirData.getR4EncounterCodes().size(),
        r4FhirData.getR4ConditionCodes().size(),
        r4FhirData.getR4LabResultCodes().size(),
        r4FhirData.getR4MedicationCodes().size(),
        r4FhirData.getR4ImmunizationCodes().size(),
        r4FhirData.getR4DiagnosticReportCodes().size(),
        r4FhirData.getR4ServiceRequestCodes().size());

    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/LoadingQueryR4Bundle-"
            + launchDetails.getLaunchPatientId()
            + ".json";
    ApplicationUtils.saveDataToFile(
        context.newJsonParser().encodeResourceToString(bundle), fileName);
    return bundle;
  }
}
