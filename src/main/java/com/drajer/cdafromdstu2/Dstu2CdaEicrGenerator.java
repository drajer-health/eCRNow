package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import ca.uhn.fhir.model.dstu2.resource.Location;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.dstu2.resource.Organization;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import ca.uhn.fhir.model.dstu2.resource.Practitioner;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaEicrGenerator {

  private static final Logger logger = LoggerFactory.getLogger(Dstu2CdaEicrGenerator.class);

  public static String convertDstu2FhirBundletoCdaEicr(
      Dstu2FhirData data, LaunchDetails details, Eicr ecr) {

    StringBuilder eICR = new StringBuilder();

    if (data != null) {

      Bundle bundle = data.getData();
      if (bundle != null) {

        List<Entry> entries = bundle.getEntry();

        for (Entry ent : entries) {

          // Populate data ..this can be moved to the APIs where the bundle is getting created.
          if (ent.getResource() instanceof Patient) {
            logger.info(" Bundle contains Patient ");
            data.setPatient((Patient) ent.getResource());
          } else if (ent.getResource() instanceof Practitioner) {
            logger.info(" Bundle contains Practitioner ");
            data.setPractitioner((Practitioner) ent.getResource());
          } else if (ent.getResource() instanceof Encounter) {
            logger.info(" Bundle contains Encounter ");
            data.setEncounter((Encounter) ent.getResource());
          } else if (ent.getResource() instanceof Location) {
            logger.info(" Bundle contains Location ");
            data.setLocation((Location) ent.getResource());
          } else if (ent.getResource() instanceof Organization) {
            logger.info(" Bundle contains Organization ");
            data.setOrganization((Organization) ent.getResource());
          } else if (ent.getResource() instanceof Condition) {
            logger.info(" Bundle contains Condition ");
            data.getConditions().add((Condition) ent.getResource());
          } else if (ent.getResource() instanceof Observation) {

            Observation obs = (Observation) ent.getResource();
            if (obs.getCategory() != null
                && obs.getCategory().getCodingFirstRep() != null
                && obs.getCategory().getCodingFirstRep().getCode() != null
                && obs.getCategory()
                    .getCodingFirstRep()
                    .getCode()
                    .contentEquals(CdaGeneratorConstants.FHIR_LAB_RESULT_CATEGORY)) {

              logger.info(" Bundle contains Lab Results ");
              data.getLabResults().add((Observation) ent.getResource());
            } else if (obs.getCategory() != null
                && obs.getCategory().getCodingFirstRep() != null
                && obs.getCategory().getCodingFirstRep().getCode() != null) {
              logger.info(
                  "Code for Observation Category =  "
                      + obs.getCategory().getCodingFirstRep().getCode());
            }

            // Compare Code for Travel Obs

            // Compare Codes for Pregnancy Obs and sort it out.

          } else if (ent.getResource() instanceof DiagnosticReport) {
            logger.info(" Bundle contains Diagnostic Report ");
            data.getDiagReports().add((DiagnosticReport) ent.getResource());
          } else if (ent.getResource() instanceof DiagnosticOrder) {
            logger.info(" Bundle contains Diagnostic Order ");
            data.getDiagOrders().add((DiagnosticOrder) ent.getResource());
          } else if (ent.getResource() instanceof MedicationStatement) {
            logger.info(" Bundle contains MedicationStatement ");
            data.getMedications().add((MedicationStatement) ent.getResource());
          } else if (ent.getResource() instanceof Immunization) {
            logger.info(" Bundle contains Immunization ");
            data.getImmunizations().add((Immunization) ent.getResource());
          }
        }

        eICR.append(Dstu2CdaHeaderGenerator.createCdaHeader(data, details));
        eICR.append(Dstu2CdaBodyGenerator.generateCdaBody(data, details));
        eICR.append(CdaGeneratorUtils.getEndXMLHeaderForCdaDocument());
      }

    } else {

      logger.error(" No Fhir Bundle Available to create CDA Documents ");
    }

    return eICR.toString();
  }
}
