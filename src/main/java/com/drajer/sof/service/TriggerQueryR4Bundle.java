package com.drajer.sof.service;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.R4ResourcesData;
import java.util.Date;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Encounter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TriggerQueryR4Bundle {

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired R4ResourcesData r4ResourcesData;

  private static final Logger logger = LoggerFactory.getLogger(TriggerQueryR4Bundle.class);

  public Bundle createR4Bundle(
      LaunchDetails launchDetails, R4FhirData r4FhirData, Date start, Date end) {
    Bundle bundle = new Bundle();

    logger.info("Initializing FHIR Context for Version::::" + launchDetails.getFhirVersion());
    FhirContext context = fhirContextInitializer.getFhirContext(launchDetails.getFhirVersion());
    logger.info("Initializing Client");
    IGenericClient client =
        fhirContextInitializer.createClient(
            context, launchDetails.getEhrServerURL(), launchDetails.getAccessToken());

    // GET Patient Details and Add to Bundle
    bundle = r4ResourcesData.getPatientData(launchDetails, client, context, bundle);

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
          r4ResourcesData.getEncounterData(context, client, launchDetails, r4FhirData, start, end);
      bundle =
          r4ResourcesData.getPractitionerEntry(encounter, launchDetails, client, context, bundle);
      bundle =
          r4ResourcesData.getOrganizationEntry(encounter, launchDetails, client, context, bundle);
      bundle = r4ResourcesData.getLocationEntry(encounter, launchDetails, client, context, bundle);
      BundleEntryComponent encounterEntry = new BundleEntryComponent().setResource(encounter);
      bundle.addEntry(encounterEntry);
    } catch (Exception e) {
      logger.error("Error in getting Encounter Data");
    }

    // Step 2: Get Conditions for Patient (Write a method)
    // Filter the conditions based on encounter Reference if Encounter Reference is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // Condition time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of ConditionCodes.
    bundle =
        r4ResourcesData.getConditionData(
            encounter, r4FhirData, start, end, launchDetails, client, context, bundle);

    // Get Observations for Patients and laboratory category (Write a method).
    // Filter the observations based on encounter Reference if encounter is present.
    // If encounter is not present, then filter based on times (Start and end, if
    // observation time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of labResultCodes.
    bundle =
        r4ResourcesData.getObservationData(
            encounter, r4FhirData, start, end, launchDetails, client, context, bundle);

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
        r4ResourcesData.getMedicationAdministrationData(
            encounter, r4FhirData, start, end, launchDetails, client, context, bundle);

    // Get ServiceRequest for Patients (Write a method).
    // Filter the ServiceReques based on encounter Reference if encounter is
    // present.
    // If encounter is not present, then filter based on times (Start and end, if
    // ServiceRequest time is between start and end times) -- Do this later.
    // Add to the bundle
    // As you are adding to the bundle within Fhir Data, add the codeable concept
    // also to the list of ServiceRequestCodes.
    bundle =
        r4ResourcesData.getServiceRequestData(
            encounter, r4FhirData, start, end, launchDetails, client, context, bundle);

    // Setting bundle to FHIR Data
    logger.info(
        "------------------------------CodeableConcept Codes------------------------------");
    logger.info("Encounter Codes Size=====>" + r4FhirData.getR4EncounterCodes().size());
    logger.info("Conditions Codes Size=====>" + r4FhirData.getR4ConditionCodes().size());
    logger.info("Observation Codes Size=====>" + r4FhirData.getR4LabResultCodes().size());
    logger.info("Medication Codes Size=====>" + r4FhirData.getR4MedicationCodes().size());
    logger.info("ServiceRequests Codes Size=====>" + r4FhirData.getR4ServiceRequestCodes().size());

    // logger.info(context.newJsonParser().encodeResourceToString(bundle));

    String fileName =
        ActionRepo.getInstance().getLogFileDirectory()
            + "/TriggerQueryR4Bundle-"
            + launchDetails.getLaunchPatientId()
            + ".json";
    ApplicationUtils.saveDataToFile(
        context.newJsonParser().encodeResourceToString(bundle), fileName);
    return bundle;
  }
}
