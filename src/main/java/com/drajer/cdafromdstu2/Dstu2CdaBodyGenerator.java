package com.drajer.cdafromdstu2;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaBodyGenerator {

  private static final Logger logger = LoggerFactory.getLogger(Dstu2CdaBodyGenerator.class);

  public static String generateCdaBody(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder eICRBody = new StringBuilder(200);

    eICRBody.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    eICRBody.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));

    if (data != null) {

      logger.info("Starting Problem generation ");
      eICRBody.append(Dstu2CdaProblemGenerator.generateProblemSection(data, details));

      logger.info("Starting Encounter generation ");
      eICRBody.append(Dstu2CdaEncounterGenerator.generateEncounterSection(data, details));

      logger.info("Starting Result Section generation ");
      eICRBody.append(Dstu2CdaResultGenerator.generateResultsSection(data, details));

      logger.info("Starting Medication Administered Section generation ");
      eICRBody.append(Dstu2CdaMedicationGenerator.generateMedicationSection(data, details));

      logger.info("Starting Immunization Section generation ");
      eICRBody.append(Dstu2CdaImmunizationGenerator.generateImmunizationSection(data, details));

      logger.info("Starting Social History Section generation ");
      eICRBody.append(Dstu2CdaSocialHistoryGenerator.generateSocialHistorySection(data, details));

      logger.info("Starting Plan Of Treatment Section generation ");
      eICRBody.append(
          Dstu2CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(data, details));

      logger.info("Starting History of Present Illness Section generation ");
      eICRBody.append(
          Dstu2CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
              data, details));

      logger.info("Starting Reason For Visit Section generation ");
      eICRBody.append(Dstu2CdaReasonForVisitGenerator.generateReasonForVisitSection(data, details));
    }

    eICRBody.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));
    eICRBody.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return eICRBody.toString();
  }
}
