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

    logger.info("Initializing FHIR Context for Version::::" + launchDetails.getFhirVersion());
    FhirContext context = fhirContextInitializer.getFhirContext(launchDetails.getFhirVersion());
    logger.info("Initializing Client");
    IGenericClient client =
        fhirContextInitializer.createClient(
            context, launchDetails.getEhrServerURL(), launchDetails.getAccessToken());

    Bundle bundle =
        r4ResourcesData.getCommonResources(r4FhirData, start, end, launchDetails, client, context);

    // Setting bundle to FHIR Data
    if (logger.isInfoEnabled()) {
      logger.info(
          "------------------------------CodeableConcept Codes------------------------------");
      logger.info("Encounter Codes Size=====> {}", r4FhirData.getR4EncounterCodes().size());
      logger.info("Conditions Codes Size=====> {}", r4FhirData.getR4ConditionCodes().size());
      logger.info("Observation Codes Size=====> {}", r4FhirData.getR4LabResultCodes().size());
      logger.info("Medication Codes Size=====> {}", r4FhirData.getR4MedicationCodes().size());
      logger.info(
          "ServiceRequests Codes Size=====> {}", r4FhirData.getR4ServiceRequestCodes().size());
    }

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
