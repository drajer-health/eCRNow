package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Procedure;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaProcedureGenerator {

  private static final Logger logger = LoggerFactory.getLogger(CdaProcedureGenerator.class);

  public static String generateProcedureSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder();

    logger.info("Start Creating Procedure Section");

    List<Procedure> procList = data.getProcedureList();

    if ((procList != null && !procList.isEmpty())) {

      logger.info("Found Procedures ");
      sb.append(generateProcedureSectionHeader(""));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.PROC_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.PROC_TABLE_COL_2_TITLE);
      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int rowNum = 0;
      StringBuilder procEntries = new StringBuilder();
      for (Procedure proc : procList) {

        if (isProcedureActivityProcedure(proc)) {

          generateProcedureActivityProcedure(proc, details, sb, procEntries, rowNum);
          rowNum++;

        } else if (isProcedureActivityObservation(proc)) {
          generateProcedureActivityObservation(proc, details, sb, procEntries, rowNum);
          rowNum++;
        } else {
          logger.info(
              "Ignoring Procedure {} since it is not what is expected for Procedure section",
              proc.getIdElement().getIdPart());
        }
      }

      // Close the Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add entry
      if (!StringUtils.isEmpty(procEntries)) {
        sb.append(procEntries);
      }

      sb.append(generateProcedureSectionEndHeader());

    } else {
      sb.append(generateEmptyProcedureSection());
    }
    return sb.toString();
  }

  private static void generateProcedureActivityObservation(
      Procedure proc,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder procEntries,
      int rowNum) {

    StringBuilder sb = new StringBuilder();
    String display = CdaGeneratorConstants.UNKNOWN_VALUE;
    Map<String, String> bodyvals = new LinkedHashMap<>();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROC_ACT_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PROC_ACT_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PROC_ACT_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), proc.getIdElement().getIdPart()));

    display = CdaFhirUtilities.getDisplayStringForCodeableConcept(proc.getCode());
    bodyvals.put(CdaGeneratorConstants.PROC_TABLE_COL_1_BODY_CONTENT, display);
    sb.append(
        CdaFhirUtilities.getCodeableConceptXml(
            proc.getCode(), CdaGeneratorConstants.CODE_EL_NAME, Integer.toString(rowNum)));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    bodyvals.put(
        CdaGeneratorConstants.PROC_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getStringForType(proc.getPerformed()));
    sb.append(
        CdaFhirUtilities.getXmlForType(
            proc.getPerformed(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    sb.append(CdaGeneratorUtils.getNFXMLForValue(CdaGeneratorConstants.NF_NI));
    // TargetSite Code
    if (proc.hasBodySite()) {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(
              proc.getBodySite(), CdaGeneratorConstants.TARGET_SITE_CODE_EL_NAME, false));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
    procEntries.append(sb.toString());
  }

  private static Boolean isProcedureActivityObservation(Procedure proc) {

    if (proc != null && proc.hasCode()) {

      if (CdaFhirUtilities.isCodeableConceptPresentForCodeSystem(
          proc.getCode(), CdaGeneratorConstants.FHIR_LOINC_URL)) {
        return true;
      }
    }

    return false;
  }

  private static void generateProcedureActivityProcedure(
      Procedure proc,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder procEntries,
      int rowNum) {

    StringBuilder sb = new StringBuilder();
    String display = CdaGeneratorConstants.UNKNOWN_VALUE;
    Map<String, String> bodyvals = new LinkedHashMap<>();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.PROC_ACT_EL_NAME,
            CdaGeneratorConstants.PROC_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROC_ENTRY_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PROC_ENTRY_TEMPLATE_ID,
            CdaGeneratorConstants.PROC_ENTRY_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), proc.getIdElement().getIdPart()));

    display = CdaFhirUtilities.getDisplayStringForCodeableConcept(proc.getCode());
    bodyvals.put(CdaGeneratorConstants.PROC_TABLE_COL_1_BODY_CONTENT, display);
    sb.append(
        CdaFhirUtilities.getCodeableConceptXml(
            proc.getCode(), CdaGeneratorConstants.CODE_EL_NAME, Integer.toString(rowNum)));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    bodyvals.put(
        CdaGeneratorConstants.PROC_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getStringForType(proc.getPerformed()));
    sb.append(
        CdaFhirUtilities.getXmlForType(
            proc.getPerformed(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    // TargetSite Code
    if (proc.hasBodySite()) {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(
              proc.getBodySite(), CdaGeneratorConstants.TARGET_SITE_CODE_EL_NAME, false));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PROC_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
    procEntries.append(sb.toString());
  }

  private static Boolean isProcedureActivityProcedure(Procedure proc) {

    if (proc != null && proc.hasCode()) {

      if (CdaFhirUtilities.isCodeableConceptPresentForCodeSystem(
          proc.getCode(), CdaGeneratorConstants.FHIR_CPT_URL)) {
        return true;
      } else if (CdaFhirUtilities.isCodeableConceptPresentForCodeSystem(
          proc.getCode(), CdaGeneratorConstants.FHIR_SNOMED_URL)) {
        return true;
      }
      // Add HCPCS and CDT URLs in the future.

    }

    return false;
  }

  private static String generateProcedureSectionEndHeader() {
    StringBuilder sb = new StringBuilder();

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  private static String generateEmptyProcedureSection() {
    StringBuilder sb = new StringBuilder();

    sb.append(generateProcedureSectionHeader(CdaGeneratorConstants.NF_NI));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Procedure Information"));

    sb.append(generateProcedureSectionEndHeader());

    return sb.toString();
  }

  private static String generateProcedureSectionHeader(String nf) {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));

    if (!StringUtils.isEmpty(nf)) {
      sb.append(
          CdaGeneratorUtils.getXmlForNFSection(
              CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));
    } else {
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));
    }

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROC_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PROC_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.PROC_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PROC_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PROC_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.PROC_SEC_TITLE));

    return sb.toString();
  }
}
