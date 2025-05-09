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

  public static String generateCdaBody(R4FhirData data, LaunchDetails details, String version) {

    StringBuilder eICRBody = new StringBuilder();

    eICRBody.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    eICRBody.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));

    if (data != null) {

      if (version.contentEquals("CDA_R31")) {

        logger.info("Starting Problem generation ");
        eICRBody.append(CdaProblemGenerator.generateProblemSection(data, details, version));

        logger.info("Starting Encounter generation ");
        eICRBody.append(CdaEncounterGenerator.generateEncounterSection(data, details, version));

        logger.info("Starting R31 Medication Administered Section generation ");
        eICRBody.append(
            CdaMedicationGenerator.generateR31MedicationsAdministeredSection(
                data, details, version));

        logger.info("Starting R31 Medications Section generation ");
        eICRBody.append(
            CdaMedicationGenerator.generateR31MedicationsSection(data, details, version));

        logger.info("Starting Immunization Section generation ");
        eICRBody.append(
            CdaImmunizationGenerator.generateImmunizationSection(data, details, version));

        logger.info("Starting R31 Result Section generation ");
        eICRBody.append(CdaResultGenerator.generateResultsSection(data, details, version));

        logger.info("Starting R31 Chief Complaint Section generation ");
        eICRBody.append(
            CdaChiefComplaintGenerator.generateChiefComplaintSection(data, details, version));

        logger.info("Starting R31 Plan Of Treatment Section generation ");
        eICRBody.append(
            CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(data, details, version));

        logger.info("Starting R31 Social History Section generation ");
        eICRBody.append(
            CdaSocialHistoryGenerator.generateR31SocialHistorySection(data, details, version));

        logger.info("Starting R31 Pregnancy Section generation ");
        eICRBody.append(CdaPregnancyGenerator.generatePregnancySection(data, details, version));

        logger.info("Starting R31 ODH Data Section generation ");
        eICRBody.append(CdaOdhDataGenerator.generateOdhSection(data, details, version));

        logger.info("Starting R31 Procedure Section generation ");
        eICRBody.append(CdaProcedureGenerator.generateProcedureSection(data, details, version));

        logger.info("Starting R31 Vitals generation ");
        eICRBody.append(CdaVitalSignsGenerator.generateVitalsSection(data, details, version));

      } else {

        logger.info("Starting Problem generation ");
        eICRBody.append(CdaProblemGenerator.generateProblemSection(data, details, version));

        logger.info("Starting Encounter generation ");
        eICRBody.append(CdaEncounterGenerator.generateEncounterSection(data, details, version));

        logger.info("Starting Result Section generation ");
        eICRBody.append(CdaResultGenerator.generateResultsSection(data, details, version));

        logger.info("Starting Medication Administered Section generation ");
        eICRBody.append(CdaMedicationGenerator.generateMedicationSection(data, details));

        logger.info("Starting Immunization Section generation ");
        eICRBody.append(
            CdaImmunizationGenerator.generateImmunizationSection(data, details, version));

        logger.info("Starting Social History Section generation ");
        eICRBody.append(
            CdaSocialHistoryGenerator.generateSocialHistorySection(data, details, version));

        logger.info("Starting Plan Of Treatment Section generation ");
        eICRBody.append(
            CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(data, details, version));
      }

      logger.info("Starting History of Present Illness Section generation ");
      eICRBody.append(
          CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
              data, version));

      logger.info("Starting Reason For Visit Section generation ");
      eICRBody.append(CdaReasonForVisitGenerator.generateReasonForVisitSection(data, version));
    }

    eICRBody.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.STRUC_BODY_EL_NAME));
    eICRBody.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return eICRBody.toString();
  }
}
