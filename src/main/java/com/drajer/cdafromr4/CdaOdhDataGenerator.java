package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaOdhDataGenerator {

  private static final Logger logger = LoggerFactory.getLogger(CdaOdhDataGenerator.class);

  public static String generateOdhSection(R4FhirData data, LaunchDetails details, String version) {
    StringBuilder sb = new StringBuilder();

    logger.info("Start Creating ODH Section");

    List<Observation> occObs = data.getOccupationObs();

    if ((occObs != null && !occObs.isEmpty())) {

      logger.info("Found Occupation  Observations ");
      sb.append(generateOdhSectionHeader(""));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.ODH_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.ODH_TABLE_COL_2_TITLE);
      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int rowNum = 0;
      StringBuilder occEntries = new StringBuilder();
      for (Observation obs : occObs) {

        if (isPastOrPresentOccupation(obs)) {

          generatePastOrPresentEntry(obs, details, sb, occEntries, rowNum);
          rowNum++;

        } else if (isUsualOccupation(obs)) {
          generateUsualOccupationEntry(obs, details, sb, occEntries, rowNum);
          rowNum++;

        } else if (isEmploymentStatusObservation(obs)) {
          generateEmploymentStatusObservation(obs, details, sb, occEntries, rowNum);
          rowNum++;
        } else {
          logger.info(
              "Ignoring Observation {} since it is not what is expected for ODH",
              obs.getIdElement().getIdPart());
        }
      }

      // Close the Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add entry
      if (!StringUtils.isEmpty(occEntries)) {
        sb.append(occEntries);
      }

      sb.append(generateOdhSectionEndHeader());

    } else {
      sb.append(generateEmptyOdhSection());
    }
    return sb.toString();
  }

  private static void generateEmploymentStatusObservation(
      Observation obs,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder occEntries,
      int rowNum) {
    StringBuilder sb = new StringBuilder();
    String display = CdaGeneratorConstants.UNKNOWN_VALUE;
    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(
        CdaGeneratorConstants.ODH_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.EMPLOYMENT_STATUS_CODE_DISPLAY);

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.EMPLOYMENT_STATUS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.EMPLOYMENT_STATUS_TEMPLATE_ID,
            CdaGeneratorConstants.EMPLOYMENT_STATUS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.EMPLOYMENT_STATUS_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.EMPLOYMENT_STATUS_CODE_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(
        CdaFhirUtilities.getXmlForTypeForValueIvlTsEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, obs.getEffective()));

    if (obs.hasValueCodeableConcept()) {

      display = CdaFhirUtilities.getDisplayStringForCodeableConcept(obs.getValueCodeableConcept());
      bodyvals.put(CdaGeneratorConstants.ODH_TABLE_COL_2_BODY_CONTENT, display);

      sb.append(
          CdaFhirUtilities.getXmlForType(
              obs.getValueCodeableConcept(), CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    occEntries.append(sb.toString());
  }

  private static void generateUsualOccupationEntry(
      Observation obs,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder occEntries,
      int rowNum) {

    StringBuilder sb = new StringBuilder();
    String display = CdaGeneratorConstants.UNKNOWN_VALUE;
    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(
        CdaGeneratorConstants.ODH_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.USUAL_OCCUPATION_CODE_DISPLAY);

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.USUAL_OCCUPATION_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.USUAL_OCCUPATION_TEMPLATE_ID,
            CdaGeneratorConstants.USUAL_OCCUPATION_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.USUAL_OCCUPATION_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.USUAL_OCCUPATION_CODE_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(
        CdaFhirUtilities.getXmlForTypeForValueIvlTsEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, obs.getEffective()));

    if (obs.hasValueCodeableConcept()) {

      display = CdaFhirUtilities.getDisplayStringForCodeableConcept(obs.getValueCodeableConcept());
      bodyvals.put(CdaGeneratorConstants.ODH_TABLE_COL_2_BODY_CONTENT, display);

      sb.append(
          CdaFhirUtilities.getXmlForType(
              obs.getValueCodeableConcept(), CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    // Add Usual Industry
    sb.append(addUsualIndustryObservation(obs, details));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    occEntries.append(sb.toString());
  }

  private static String addUsualIndustryObservation(Observation obs, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    sb.append(
        CdaGeneratorUtils.getXmlForEntryRelationship(CdaGeneratorConstants.ENTRY_REL_REFR_CODE));

    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.USUAL_INDUSTRY_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.USUAL_INDUSTRY_TEMPLATE_ID,
            CdaGeneratorConstants.USUAL_INDUSTRY_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.USUAL_INDUSTRY_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.USUAL_INDUSTRY_CODE_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    ObservationComponentComponent oc = getUsualIndustryComponent(obs);

    if (oc != null && oc.hasValueCodeableConcept()) {
      sb.append(
          CdaFhirUtilities.getXmlForType(
              oc.getValueCodeableConcept(), CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));

    return sb.toString();
  }

  private static ObservationComponentComponent getUsualIndustryComponent(Observation obs) {
    if (obs.hasComponent()) {

      List<ObservationComponentComponent> comps = obs.getComponent();

      for (ObservationComponentComponent co : comps) {

        if (co.hasCode() && isUsualIndustryCode(co.getCode())) {
          return co;
        }
      }
    }
    return null;
  }

  private static Boolean isUsualIndustryCode(CodeableConcept code) {
    if (code != null) {
      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(code);
      if (CdaFhirUtilities.isCodePresent(
          cds, CdaGeneratorConstants.USUAL_INDUSTRY_CODE, CdaGeneratorConstants.FHIR_LOINC_URL))
        return true;
    }

    return false;
  }

  private static void generatePastOrPresentEntry(
      Observation obs,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder occEntries,
      int rowNum) {

    StringBuilder sb = new StringBuilder();
    String display = CdaGeneratorConstants.UNKNOWN_VALUE;
    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(
        CdaGeneratorConstants.ODH_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE_DISPLAY);

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_TEMPLATE_ID,
            CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE_DISPLAY));

    if (isCurrentJob(obs)) {
      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));
    }

    sb.append(
        CdaFhirUtilities.getXmlForTypeForValueIvlTsEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, obs.getEffective()));

    if (obs.hasValueCodeableConcept()) {

      display = CdaFhirUtilities.getDisplayStringForCodeableConcept(obs.getValueCodeableConcept());
      bodyvals.put(CdaGeneratorConstants.ODH_TABLE_COL_2_BODY_CONTENT, display);

      sb.append(
          CdaFhirUtilities.getXmlForType(
              obs.getValueCodeableConcept(), CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    // Add participant
    sb.append(
        CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
            CdaGeneratorConstants.PARTICIPANT_EL_NAME, CdaGeneratorConstants.TYPE_CODE_IND));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElementWithClassCode(
            CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME, CdaGeneratorConstants.ROL_CLASS_CODE));
    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    // Figure out how to add address in the future
    sb.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_EL_NAME));

    sb.append(addPastOrPresentIndustryObservation(obs, details));
    sb.append(addOccupationHazardObservation(obs, details));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    occEntries.append(sb.toString());
  }

  private static String addOccupationHazardObservation(Observation obs, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    sb.append(
        CdaGeneratorUtils.getXmlForEntryRelationship(CdaGeneratorConstants.ENTRY_REL_REFR_CODE));

    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.OCC_HAZARD_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.OCC_HAZARD_TEMPLATE_ID,
            CdaGeneratorConstants.OCC_HAZARD_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.OCC_HAZARD_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.OCC_HAZARD_CODE_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    ObservationComponentComponent oc = getOccupationalHazardComponent(obs);

    if (oc != null && oc.hasValue()) {
      String textVal = CdaFhirUtilities.getStringForType(oc.getValue());
      sb.append(
          StringUtils.isNotBlank(textVal)
              ? CdaGeneratorUtils.getXmlForValueString(textVal)
              : CdaGeneratorUtils.getNFXmlForValueString(CdaGeneratorConstants.NF_NI));
    } else {
      sb.append(CdaGeneratorUtils.getNFXmlForValueString(CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));

    return sb.toString();
  }

  private static ObservationComponentComponent getOccupationalHazardComponent(Observation obs) {
    if (obs.hasComponent()) {

      List<ObservationComponentComponent> comps = obs.getComponent();

      for (ObservationComponentComponent co : comps) {

        if (co.hasCode() && isOccupationalHazardCode(co.getCode())) {
          return co;
        }
      }
    }
    return null;
  }

  private static Boolean isOccupationalHazardCode(CodeableConcept code) {
    if (code != null) {
      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(code);
      if (CdaFhirUtilities.isCodePresent(
          cds, CdaGeneratorConstants.OCC_HAZARD_CODE, CdaGeneratorConstants.FHIR_LOINC_URL))
        return true;
    }

    return false;
  }

  public static String addPastOrPresentIndustryObservation(Observation obs, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    sb.append(
        CdaGeneratorUtils.getXmlForEntryRelationship(CdaGeneratorConstants.ENTRY_REL_REFR_CODE));

    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PAST_PRESENT_INDUSTRY_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PAST_PRESENT_INDUSTRY_TEMPLATE_ID,
            CdaGeneratorConstants.PAST_PRESENT_INDUSTRY_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PAST_PRESENT_INDUSTRY_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PAST_PRESENT_INDUSTRY_CODE));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    ObservationComponentComponent oc = getIndustryComponent(obs);

    if (oc != null && oc.hasValueCodeableConcept()) {
      sb.append(
          CdaFhirUtilities.getXmlForType(
              oc.getValueCodeableConcept(), CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueCD(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));

    return sb.toString();
  }

  private static ObservationComponentComponent getIndustryComponent(Observation obs) {

    if (obs.hasComponent()) {

      List<ObservationComponentComponent> comps = obs.getComponent();

      for (ObservationComponentComponent co : comps) {

        if (co.hasCode() && isPastOrPresentIndustryCode(co.getCode())) {
          return co;
        }
      }
    }
    return null;
  }

  private static Boolean isPastOrPresentIndustryCode(CodeableConcept code) {

    if (code != null) {
      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(code);
      if (CdaFhirUtilities.isCodePresent(
          cds,
          CdaGeneratorConstants.PAST_PRESENT_INDUSTRY_CODE,
          CdaGeneratorConstants.FHIR_LOINC_URL)) return true;
    }

    return false;
  }

  private static Boolean isCurrentJob(Observation obs) {

    return CdaFhirUtilities.getBooleanExtensionValue(
        obs.getExtension(), CdaGeneratorConstants.OdhCurrentJobExtension);
  }

  private static Boolean isEmploymentStatusObservation(Observation obs) {

    if (obs != null && obs.hasCode()) {

      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(obs.getCode());
      if (CdaFhirUtilities.isCodePresent(
          cds, CdaGeneratorConstants.EMPLOYMENT_STATUS_CODE, CdaGeneratorConstants.FHIR_LOINC_URL))
        return true;
    }
    return false;
  }

  private static boolean isUsualOccupation(Observation obs) {

    if (obs != null && obs.hasCode()) {

      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(obs.getCode());
      if (CdaFhirUtilities.isCodePresent(
          cds, CdaGeneratorConstants.USUAL_OCCUPATION_CODE, CdaGeneratorConstants.FHIR_LOINC_URL))
        return true;
    }
    return false;
  }

  private static boolean isPastOrPresentOccupation(Observation obs) {
    if (obs != null && obs.hasCode()) {

      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(obs.getCode());
      if (CdaFhirUtilities.isCodePresent(
          cds,
          CdaGeneratorConstants.PAST_PRESENT_OCCUPATION_CODE,
          CdaGeneratorConstants.FHIR_LOINC_URL)) return true;
    }
    return false;
  }

  public static String generateOccHistoryEntry(Observation obs, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.SOC_HISTORY_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.SOC_HISTORY_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.SOC_HISTORY_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCDWithoutEndTag(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.SNOMED_OCC_HISTORY_CODE,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_OID,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_NAME,
            CdaGeneratorConstants.SNOMED_OCC_HISTORY_CODE_DISPLAY));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.TRANSLATION_EL_NAME,
            CdaGeneratorConstants.LOINC_OCC_HISTORY_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.LOINC_OCC_HISTORY_CODE_DISPLAY));

    List<CodeableConcept> cds = new ArrayList<>();
    cds.add(obs.getCode());
    sb.append(
        CdaFhirUtilities.getCodeableConceptXml(
            cds, CdaGeneratorConstants.TRANSLATION_EL_NAME, false));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CODE_EL_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> effDate = CdaFhirUtilities.getActualDate(obs.getEffective());

    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate.getValue0(), effDate.getValue1()));

    sb.append(
        CdaFhirUtilities.getXmlForType(obs.getValue(), CdaGeneratorConstants.VAL_EL_NAME, true));

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String generateEmptyOdhSection() {

    StringBuilder sb = new StringBuilder();

    sb.append(generateOdhSectionHeader(CdaGeneratorConstants.NF_NI));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Occupational Data Information"));

    sb.append(generateOdhSectionEndHeader());

    return sb.toString();
  }

  public static String generateOdhSectionHeader(String nf) {

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
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.ODH_SECTION_TEMPLATE_ID,
            CdaGeneratorConstants.ODH_SECTION_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.ODH_SECTION_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.ODH_SECTION_CODE_DISPLAY));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.ODH_SECTION_TITLE));

    return sb.toString();
  }

  public static String generateOdhSectionEndHeader() {

    StringBuilder sb = new StringBuilder();

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }
}
