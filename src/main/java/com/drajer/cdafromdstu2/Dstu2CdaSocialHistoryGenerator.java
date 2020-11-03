package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.primitive.CodeDt;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaSocialHistoryGenerator {

  private static final Logger logger =
      LoggerFactory.getLogger(Dstu2CdaSocialHistoryGenerator.class);

  public static String generateSocialHistorySection(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    // Will have to wait to discuss with vendors on Travel History, Pregnancy, and Birth Sex
    // Observations.
    // Then we can generte the entries. Till then it will be empty section.
    CodeDt birthSex =
        Dstu2CdaFhirUtilities.getCodeExtension(
            data.getPatient().getUndeclaredExtensions(),
            CdaGeneratorConstants.FHIR_ARGO_BIRTHSEX_EXT_URL);
    List<Observation> pregObs = data.getPregnancyObs();
    List<Observation> travelHistory = data.getTravelObs();

    if (birthSex != null
        || (pregObs != null && pregObs.size() > 0)
        || (travelHistory != null && travelHistory.size() > 0)) {

      sb.append(generateSocialHistorySectionHeader(""));

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<String>();
      list.add(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_TITLE);
      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      String birthSexXml = "";
      String pregObsXml = "";
      String travelHistoryXml = "";

      if (birthSex != null) {

        Map<String, String> bodyvals = new HashMap<String, String>();
        bodyvals.put(
            CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
            CdaGeneratorConstants.BIRTH_SEX_DISPLAY);
        bodyvals.put(
            CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT,
            Dstu2CdaFhirUtilities.getStringForIDataType(birthSex));

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, 0));

        birthSexXml = generateBirthSexEntry(data, details, birthSex);
      }

      if (pregObs != null && pregObs.size() > 0) {

        logger.error(" Pregnancy Status Observation Found , translation not implemented ");
        // These are not available in FHIR right now reliably, so nothing to process until further
        // discussion with vendors.

        // Setup Table Text Entries

        // Setup XML Entries
      }

      if (travelHistory != null && travelHistory.size() > 0) {

        logger.error(" Pregnancy Status Observation Found , translation not implemented ");
        // These are not available in FHIR right now reliably, so nothing to process until further
        // discussion with vendors.

        // Setup Table Text Entries

        // Setup XML Entries
      }

      // Close the Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add entry
      if (!StringUtils.isEmpty(birthSexXml)) {
        sb.append(birthSexXml);
      }

      if (!StringUtils.isEmpty(pregObsXml)) {
        sb.append(birthSexXml);
      }

      if (!StringUtils.isEmpty(travelHistoryXml)) {
        sb.append(birthSexXml);
      }

      sb.append(generateSocialHistorySectionEndHeader());

    } else {
      sb.append(generateEmptySocialHistorySection());
    }

    return sb.toString();
  }

  public static String generateBirthSexEntry(
      Dstu2FhirData data, LaunchDetails details, CodeDt birthSex) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.BIRTH_SEX_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.BIRTH_SEX_OBS_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.BIRTH_SEX_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.BIRTH_SEX_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(
        CdaGeneratorUtils.getXmlForValueCD(
            Dstu2CdaFhirUtilities.getStringForIDataType(birthSex),
            CdaGeneratorConstants.BIRTH_SEX_CODESYSTEM_OID,
            CdaGeneratorConstants.BIRTH_SEX_CODESYSTEM_NAME,
            ""));

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String generateSocialHistorySectionHeader(String nf) {

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

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.SOC_HISTORY_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.SOC_HISTORY_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.SOC_HISTORY_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.SOC_HISTORY_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.SOC_HISTORY_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.SOC_HISTORY_SEC_TITLE));

    return sb.toString();
  }

  public static String generateSocialHistorySectionEndHeader() {

    StringBuilder sb = new StringBuilder();

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static String generateEmptySocialHistorySection() {

    StringBuilder sb = new StringBuilder();

    sb.append(generateSocialHistorySectionHeader(CdaGeneratorConstants.NF_NI));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Social History Information"));

    sb.append(generateSocialHistorySectionEndHeader());

    return sb.toString();
  }
}
