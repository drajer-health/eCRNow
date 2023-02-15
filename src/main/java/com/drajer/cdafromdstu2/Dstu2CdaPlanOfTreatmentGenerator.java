package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticOrder;
import ca.uhn.fhir.model.dstu2.resource.DiagnosticReport;
import ca.uhn.fhir.model.dstu2.valueset.DiagnosticOrderStatusEnum;
import ca.uhn.fhir.model.dstu2.valueset.DiagnosticReportStatusEnum;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.cdafromr4.CdaFhirUtilities;
import com.drajer.eca.model.ActionRepo;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaPlanOfTreatmentGenerator {

  private static final Logger logger =
      LoggerFactory.getLogger(Dstu2CdaPlanOfTreatmentGenerator.class);

  public static String generatePlanOfTreatmentSection(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    List<DiagnosticReport> drs = getValidDiagnosticReports(data);
    List<DiagnosticOrder> dors = getValidDiagnosticOrders(data);
    logger.info("DiagnosticOrder List in generatePlanOfTreatmentSection :{}", dors);

    if ((drs != null && !drs.isEmpty())) {
      logger.info("Found {} Diagnostic Report objects to translate to CDA.", drs.size());

      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID_EXT));

      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.CAREPLAN_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.CAREPLAN_SEC_NAME));

      // Add Title
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.CAREPLAN_SEC_TITLE));

      // Add Narrative Text
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.POT_OBS_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.POT_OBS_TABLE_COL_2_TITLE);
      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int rowNum = 1;
      StringBuilder potObsXml = new StringBuilder();

      for (DiagnosticReport dr : drs) {

        Pair<String, Boolean> srDisplayName =
            Dstu2CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
                dr.getCode(), CdaGeneratorConstants.FHIR_LOINC_URL, false);

        if (srDisplayName.getValue0().isEmpty()) {
          srDisplayName.setAt0(CdaGeneratorConstants.UNKNOWN_VALUE);
        }

        String serviceDate = Dstu2CdaFhirUtilities.getStringForIDataType(dr.getEffective());
        logger.debug("Service Date for display {} ", serviceDate);

        if (serviceDate.isEmpty() && dr.getIssued() != null) {
          serviceDate = CdaGeneratorUtils.getStringForDateTime(dr.getIssued(), null);
        }

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(
            CdaGeneratorConstants.POT_OBS_TABLE_COL_1_BODY_CONTENT, srDisplayName.getValue0());
        bodyvals.put(CdaGeneratorConstants.POT_OBS_TABLE_COL_2_BODY_CONTENT, serviceDate);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        String contentRef =
            CdaGeneratorConstants.POT_OBS_TABLE_COL_1_BODY_CONTENT + Integer.toString(rowNum);

        rowNum++;

        potObsXml.append(getPlannedObservationXml(dr, details, contentRef));
      }

      // Close the Text Element
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add Entries
      sb.append(potObsXml);

      // Complete the section end tags.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {

      sb.append(generateEmptyPlanOfTreatmentSection());
    }

    return sb.toString();
  }

  public static String getPlannedObservationXml(
      DiagnosticReport dr, LaunchDetails details, String contentRef) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_RQO));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID_EXT));

    List<String> matchedTriggerCodes =
        CdaFhirUtilities.getMatchedCodesForResourceAndUrl(
            details, "ServiceRequest", CdaGeneratorConstants.FHIR_LOINC_URL);

    String codeXml = "";
    // Add Trigger code template if the code matched the Url in the DiagnosticReport.
    if (matchedTriggerCodes != null && !matchedTriggerCodes.isEmpty()) {
      logger.info("Found a Matched Code that is for DiagnosticReport");

      String mCd =
          Dstu2CdaFhirUtilities.getMatchingCodeFromCodeableConceptForCodeSystem(
              matchedTriggerCodes, dr.getCode(), CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty()) {

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE,
                CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT));

        codeXml =
            CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                mCd,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.RCTC_OID,
                ActionRepo.getInstance().getRctcVersion(),
                "",
                contentRef);
      } else {
        logger.debug(
            "Did not find the code value in the matched codes, make it a regular Planned Observation");
      }
    }

    // add Id
    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    if (codeXml.isEmpty()) {
      List<CodeableConceptDt> ccs = new ArrayList<>();
      ccs.add(dr.getCode());
      codeXml =
          Dstu2CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
              ccs,
              CdaGeneratorConstants.CODE_EL_NAME,
              false,
              CdaGeneratorConstants.FHIR_LOINC_URL,
              false);

      if (!codeXml.isEmpty()) {
        sb.append(codeXml);
      } else {
        sb.append(
            Dstu2CdaFhirUtilities.getCodeableConceptXml(
                ccs, CdaGeneratorConstants.CODE_EL_NAME, false));
      }
    } else {
      sb.append(codeXml);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    Pair<Date, TimeZone> effDate = Dstu2CdaFhirUtilities.getActualDate(dr.getEffective());
    if (effDate.getValue0() == null) {
      logger.debug("Use Issued Date");

      effDate.setAt0(dr.getIssued());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate.getValue0(), effDate.getValue1()));

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static List<DiagnosticReport> getValidDiagnosticReports(Dstu2FhirData data) {

    List<DiagnosticReport> drs = new ArrayList<>();

    if (data.getDiagReports() != null && !data.getDiagReports().isEmpty()) {

      logger.info(
          "Total num of DiagnosticReports available for Patient {}", data.getDiagReports().size());

      for (DiagnosticReport dr : data.getDiagReports()) {

        if (dr.getCode() != null
            && dr.getCode().getCoding() != null
            && !dr.getCode().getCoding().isEmpty()
            && dr.getStatus() != null
            && dr.getStatus().contentEquals(DiagnosticReportStatusEnum.FINAL.getCode())
            && dr.getResult() != null
            && !dr.getResult().isEmpty()) {

          logger.info("Found a Diagnostic Report");
          drs.add(dr);
        }
      }
    } else {
      logger.info("No Valid DiagnosticReports in the bundle to process");
    }

    logger.info("Total num of DiagnosticReports after filtering for Patient {}", drs.size());
    return drs;
  }

  public static List<DiagnosticOrder> getValidDiagnosticOrders(Dstu2FhirData data) {

    List<DiagnosticOrder> drs = new ArrayList<>();

    if (data.getDiagOrders() != null && !data.getDiagOrders().isEmpty()) {

      logger.debug(
          "Total num of DiagnosticOrders available for Patient {}", data.getDiagOrders().size());

      for (DiagnosticOrder dr : data.getDiagOrders()) {

        if (dr.getItemFirstRep() != null
            && dr.getItemFirstRep().getCode() != null
            && dr.getItemFirstRep().getCode().getCoding() != null
            && !dr.getItemFirstRep().getCode().getCoding().isEmpty()
            && dr.getStatus() != null
            && Objects.equals(dr.getStatus(), DiagnosticOrderStatusEnum.ACCEPTED.getCode())) {

          logger.debug("Found a Diagnostic Order");
          drs.add(dr);
        }
      }
    } else {
      logger.debug("No Valid DiagnosticOrders in the bundle to process");
    }

    return drs;
  }

  public static String generateEmptyPlanOfTreatmentSection() {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    // Add Plan of Treatment Template Id
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.CAREPLAN_SEC_TITLE));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Plan Of Treatment Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }
}
