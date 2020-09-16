package com.drajer.cdafromR4;

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
            data.getConditions().add((Condition) ent.getResource());
          } else if (ent.getResource() instanceof Observation) {

            Observation obs = (Observation) ent.getResource();
            /*	if(obs.getCategory() != null &&
               obs.getCategory().getCodingFirstRep() != null &&
               obs.getCategory().getCodingFirstRep().getCode() != null &&
               obs.getCategory().getCodingFirstRep().getCode().contentEquals(CdaGeneratorConstants.FHIR_LAB_RESULT_CATEGORY)) {

            	logger.info(" Bundle contains Lab Results ");
            	data.getLabResults().add((Observation)ent.getResource());
            }
            else if(obs.getCategory() != null &&
               obs.getCategory().getCodingFirstRep() != null &&
               obs.getCategory().getCodingFirstRep().getCode() != null ) {
            	logger.info( "Code for Observation Category =  " + obs.getCategory().getCodingFirstRep().getCode());

            }*/

            // Compare Code for Travel Obs

            // Compare Codes for Pregnancy Obs and sort it out.

          } else if (ent.getResource() instanceof DiagnosticReport) {
            logger.info(" Bundle contains Diagnostic Report ");
            data.getDiagReports().add((DiagnosticReport) ent.getResource());
          }
          /*	else if(ent.getResource() instanceof DiagnosticOrder) {
          	logger.info(" Bundle contains Diagnostic Order ");
          	data.getDiagOrders().add((DiagnosticOrder)ent.getResource());
          }*/
          else if (ent.getResource() instanceof MedicationStatement) {
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
