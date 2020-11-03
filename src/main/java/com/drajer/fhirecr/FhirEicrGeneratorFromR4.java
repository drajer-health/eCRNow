package com.drajer.fhirecr;

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

public class FhirEicrGeneratorFromR4 {

  private FhirEicrGeneratorFromR4() {}

  private static final Logger logger = LoggerFactory.getLogger(FhirEicrGeneratorFromR4.class);

  public static String convertR4FhirBundletoCdaEicr(R4FhirData data) {

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
            data.getConditions().add((Condition) ent.getResource());
          } else if (ent.getResource() instanceof Observation) {
            logger.info(" Bundle constains Observation ");
            Observation obs = (Observation) ent.getResource();
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
      }

    } else {

      logger.error(" No Fhir Bundle Available to create CDA Documents ");
    }

    return eICR.toString();
  }
}
