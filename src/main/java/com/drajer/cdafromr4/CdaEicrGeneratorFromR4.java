package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.List;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Practitioner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaEicrGeneratorFromR4 {

  private static final Logger logger = LoggerFactory.getLogger(CdaEicrGeneratorFromR4.class);

  private CdaEicrGeneratorFromR4() {}

  public static String convertR4FhirBundletoCdaEicr(R4FhirData data, LaunchDetails details) {

    StringBuilder eICR = new StringBuilder();

    if (data != null) {

      Bundle bundle = data.getData();
      if (bundle != null) {

        List<BundleEntryComponent> entries = bundle.getEntry();

        for (BundleEntryComponent ent : entries) {

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

            // Problem List

            // Encounter Diagnosis List

            // Pregnancy Code
            data.getConditions().add((Condition) ent.getResource());

            // Compare Codes for Pregnancy Condition  and sort it out.

          } else if (ent.getResource() instanceof Observation) {

            Observation obs = (Observation) ent.getResource();

            // Split into Lab Results.

            // Compare Code for Travel Obs

            // Compare Occupational data.

          } else if (ent.getResource() instanceof DiagnosticReport) {
            logger.info(" Bundle contains Diagnostic Report ");
            data.getDiagReports().add((DiagnosticReport) ent.getResource());
          } else if (ent.getResource() instanceof MedicationStatement) {
            logger.info(" Bundle contains MedicationStatement ");
            data.getMedications().add((MedicationStatement) ent.getResource());
          } else if (ent.getResource() instanceof Immunization) {
            logger.info(" Bundle contains Immunization ");
            data.getImmunizations().add((Immunization) ent.getResource());
          }
        }

        eICR.append(CdaHeaderGenerator.createCdaHeader(data, details));
        eICR.append(CdaBodyGenerator.generateCdaBody(data, details));
        eICR.append(CdaGeneratorUtils.getEndXMLHeaderForCdaDocument());
      }

    } else {

      logger.error(" No Fhir Bundle Available to create CDA Documents ");
    }

    return eICR.toString();
  }
}
