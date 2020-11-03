package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaBodyGenerator {

  private CdaBodyGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaBodyGenerator.class);

  public static String generateCdaBody(R4FhirData data, LaunchDetails details) {

    StringBuilder eICRBody = new StringBuilder();

    eICRBody.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    eICRBody.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));

    if (data != null) {

      logger.info("Starting Problem generation ");
      eICRBody.append(CdaProblemGenerator.generateProblemSection(data, details));

      logger.info("Starting Encounter generation ");
      eICRBody.append(CdaEncounterGenerator.generateEncounterSection(data, details));

      logger.info("Starting Result Section generation ");
      eICRBody.append(CdaResultGenerator.generateResultsSection(data, details));

      logger.info("Starting Medication Administered Section generation ");
      eICRBody.append(CdaMedicationGenerator.generateMedicationSection(data, details));

      logger.info("Starting Immunization Section generation ");
      eICRBody.append(CdaImmunizationGenerator.generateImmunizationSection(data, details));

      logger.info("Starting Social History Section generation ");
      eICRBody.append(CdaSocialHistoryGenerator.generateSocialHistorySection(data, details));

      logger.info("Starting Plan Of Treatment Section generation ");
      eICRBody.append(CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(data, details));

      logger.info("Starting History of Present Illness Section generation ");
      eICRBody.append(
          CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(data));

      logger.info("Starting Reason For Visit Section generation ");
      eICRBody.append(CdaReasonForVisitGenerator.generateReasonForVisitSection(data));
    }

    eICRBody.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));
    eICRBody.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return eICRBody.toString();
  }
}
