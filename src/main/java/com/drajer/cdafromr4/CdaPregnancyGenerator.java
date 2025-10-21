package com.drajer.cdafromr4;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
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
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaPregnancyGenerator {

  private static final Logger logger = LoggerFactory.getLogger(CdaPregnancyGenerator.class);

  public static String generatePregnancySection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder();

    logger.info("Start Creating Pregnancy Section");

    List<Observation> pregObs = data.getPregnancyObs();
    pregObs.addAll(data.getPregnancyStatusObs());
    List<Condition> pregConds = data.getPregnancyConditions();
    List<Observation> lmpObs = data.getLmpObs();
    List<Observation> postPartumObs = data.getPostPartumObs();
    List<Observation> pregOutcomeObs = data.getPregnancyOutcomeObs();
    List<Observation> pregnancyIntentionObs = data.getPregnancyIntentionObservations();

    if ((pregObs != null && !pregObs.isEmpty())
        || (pregConds != null && !pregConds.isEmpty())
        || (lmpObs != null && !lmpObs.isEmpty())
        || (postPartumObs != null && !postPartumObs.isEmpty())
        || (pregnancyIntentionObs != null && !pregnancyIntentionObs.isEmpty())
        || (pregOutcomeObs != null && !pregOutcomeObs.isEmpty())) {

      logger.info("Found Pregnancy Conditions or Observations ");
      sb.append(generatePregnancySectionHeader(""));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_2_TITLE);
      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      StringBuilder pregObsXml = new StringBuilder();
      StringBuilder pregCondXml = new StringBuilder();
      int rowNum = 0;

      for (Condition c : pregConds) {

        processCondition(c, details, sb, pregCondXml, rowNum);
        rowNum++;
      }

      for (Observation obs : pregObs) {

        processObservation(obs, details, sb, pregObsXml, rowNum);
        rowNum++;
      }

      for (Observation lmp : lmpObs) {

        processLmpObservation(lmp, details, sb, pregObsXml, rowNum);
        rowNum++;
      }

      for (Observation pp : postPartumObs) {

        processPostPartumObservation(pp, details, sb, pregObsXml, rowNum);
        rowNum++;
      }

      for (Observation po : pregOutcomeObs) {

        processPregOutcomeObservation(po, details, sb, pregObsXml, rowNum);
        rowNum++;
      }

      for (Observation pi : pregnancyIntentionObs) {
        processPregnancyIntentionObservation(pi, details, sb, pregObsXml, rowNum);
        rowNum++;
      }

      // Close the Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add entry
      if (!StringUtils.isEmpty(pregCondXml)) {
        sb.append(pregCondXml);
      }

      if (!StringUtils.isEmpty(pregObsXml)) {
        sb.append(pregObsXml);
      }

      sb.append(generatePregnancySectionEndHeader());

    } else {

      return sb.toString();
      // sb.append(generateEmptyPregnancySection());
    }

    return sb.toString();
  }

  private static void processPregnancyIntentionObservation(
      Observation pi,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder pregObsXml,
      int rowNum) {

    StringBuilder sb = new StringBuilder();

    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.PREGNANCY_INTENTION_CODE_DISPLAY);

    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getStringForType(pi.getValue()));
    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_INT));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_INTENTION_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_INTENTION_TEMPLATE_ID,
            CdaGeneratorConstants.PREGNANCY_INTENTION_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForII(pi.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PREGNANCY_INTENTION_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PREGNANCY_INTENTION_CODE_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> start = null;
    Pair<Date, TimeZone> end = null;
    if (pi.hasEffective() && pi.getEffective() instanceof Period) {

      Period p = (Period) pi.getEffective();
      start = CdaFhirUtilities.getActualDate(p.getStartElement());
      end = CdaFhirUtilities.getActualDate(p.getEndElement());

    } else if (pi.hasEffective() && pi.getEffective() instanceof DateTimeType) {
      start = CdaFhirUtilities.getActualDate(pi.getEffective());
      end = new Pair<>(null, null);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForIVLWithTS(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, start, end, false));

    String valXml = "";
    valXml = CdaFhirUtilities.getXmlForType(pi.getValue(), CdaGeneratorConstants.VAL_EL_NAME, true);

    if (!valXml.isEmpty()) {
      sb.append(valXml);
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueEffectiveTime(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    pregObsXml.append(sb.toString());
  }

  private static void processPregOutcomeObservation(
      Observation po,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder pregObsXml,
      int rowNum) {

    StringBuilder sb = new StringBuilder();

    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.PREG_OUTCOME_DISPLAY);

    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getStringForType(po.getValue()));
    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PREG_OUTCOME_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREG_OUTCOME_TEMPLATE_ID,
            CdaGeneratorConstants.PREG_OUTCOME_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForII(po.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PREG_OUTCOME_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PREG_OUTCOME_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> start = null;
    Pair<Date, TimeZone> end = null;
    if (po.hasEffective() && po.getEffective() instanceof Period) {

      Period p = (Period) po.getEffective();
      start = CdaFhirUtilities.getActualDate(p.getStartElement());
      end = CdaFhirUtilities.getActualDate(p.getEndElement());

    } else if (po.hasEffective() && po.getEffective() instanceof DateTimeType) {
      start = CdaFhirUtilities.getActualDate(po.getEffective());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForIVLWithTS(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, start, end, false));

    String valXml = "";
    valXml = CdaFhirUtilities.getXmlForType(po.getValue(), CdaGeneratorConstants.VAL_EL_NAME, true);

    if (!valXml.isEmpty()) {
      sb.append(valXml);
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueEffectiveTime(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    pregObsXml.append(sb.toString());
  }

  public static void processPostPartumObservation(
      Observation pp,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder pregObsXml,
      int rowNum) {

    StringBuilder sb = new StringBuilder();

    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.PP_DISPLAY);

    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getStringForType(pp.getValue()));
    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PP_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PP_TEMPLATE_ID, CdaGeneratorConstants.PP_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForII(pp.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PP_CODE,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_OID,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_NAME,
            CdaGeneratorConstants.PP_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> start = null;
    Pair<Date, TimeZone> end = null;
    if (pp.hasEffective() && pp.getEffective() instanceof Period) {

      Period p = (Period) pp.getEffective();
      start = CdaFhirUtilities.getActualDate(p.getStartElement());
      end = CdaFhirUtilities.getActualDate(p.getEndElement());

    } else if (pp.hasEffective() && pp.getEffective() instanceof DateTimeType) {
      start = CdaFhirUtilities.getActualDate(pp.getEffective());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForIVLWithTS(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, start, end, false));

    String valXml = "";
    valXml = CdaFhirUtilities.getXmlForType(pp.getValue(), CdaGeneratorConstants.VAL_EL_NAME, true);

    if (!valXml.isEmpty()) {
      sb.append(valXml);
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueEffectiveTime(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    pregObsXml.append(sb.toString());
  }

  public static void processLmpObservation(
      Observation obs,
      LaunchDetails details,
      StringBuilder sb,
      StringBuilder pregObsXml,
      int rowNum) {

    pregObsXml.append(generateLmpEntry(obs, details, sb, rowNum, obs.getIdElement().getIdPart()));
  }

  public static void processObservation(
      Observation obs,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder pregEntries,
      Integer rowNum) {

    Map<String, String> bodyvals = new LinkedHashMap<>();

    String dispName = CdaFhirUtilities.getDisplayStringForCodeableConcept(obs.getCode());
    bodyvals.put(CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_1_BODY_CONTENT, dispName);

    String displayValue = "";

    if (obs.hasValue() && obs.getValue() instanceof CodeableConcept) {
      Pair<String, Boolean> resultPair =
          CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
              obs.getValueCodeableConcept(), CdaGeneratorConstants.FHIR_SNOMED_URL, true);
      displayValue =
          resultPair != null ? resultPair.getValue0() : CdaGeneratorConstants.UNKNOWN_VALUE;
    }
    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_2_BODY_CONTENT, displayValue);

    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    pregEntries.append(generatePregnancyObservationEntry(obs, details));

    pregEntries.append(generateEddEntryRelationship(obs));

    pregEntries.append(generateGestationalAgeEntryRelationship(obs, details));

    pregEntries.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    pregEntries.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    // Add Last Menstrual Period
    pregEntries.append(generateLmpEntry(obs, details, table, rowNum, CdaGeneratorUtils.getGuid()));
  }

  public static String generateLmpEntry(
      Observation obs, LaunchDetails details, StringBuilder table, Integer rowNum, String id) {

    StringBuilder sb = new StringBuilder();

    if (obs.hasComponent()) {
      List<ObservationComponentComponent> comps = obs.getComponent();

      Boolean found = false;
      for (ObservationComponentComponent comp : comps) {

        if (comp.hasCode()
            && R3ToR2DataConverterUtils.isLastMenstrualPeriodObservation(comp.getCode())
            && comp.hasValue()) {

          Map<String, String> bodyvals = new LinkedHashMap<>();
          bodyvals.put(
              CdaGeneratorConstants.PREGNANCY_LMP_OBSERVATION_TABLE_COL_1_BODY_CONTENT,
              CdaGeneratorConstants.LAST_MENSTRUAL_PERIOD_DISPLAY);

          bodyvals.put(
              CdaGeneratorConstants.PREGNANCY_LMP_OBSERVATION_TABLE_COL_2_BODY_CONTENT,
              CdaFhirUtilities.getStringForType(comp.getValue()));

          rowNum++;
          table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

          sb.append(generateLmpEntryFromComponent(id, obs.getEffective(), obs.getValue()));
        }
      }
    }

    return sb.toString();
  }

  public static String generateLmpEntryFromComponent(String id, Type effTime, Type val) {

    StringBuilder sb = new StringBuilder();

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LMP_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.LMP_TEMPLATE_ID, CdaGeneratorConstants.LMP_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForII(id));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.LMP_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.LMP_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> start = null;
    Pair<Date, TimeZone> end = null;
    if (effTime != null && effTime instanceof Period) {

      Period p = (Period) effTime;
      start = CdaFhirUtilities.getActualDate(p.getStartElement());
      end = CdaFhirUtilities.getActualDate(p.getEndElement());

    } else if (effTime != null && effTime instanceof DateTimeType) {
      start = CdaFhirUtilities.getActualDate(effTime);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForIVLWithTS(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, start, end, false));

    String valXml = "";
    valXml = CdaFhirUtilities.getXmlForType(val, CdaGeneratorConstants.VAL_EL_NAME, true);

    if (!valXml.isEmpty()) {
      sb.append(valXml);
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullValueEffectiveTime(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    return sb.toString();
  }

  public static String generateGestationalAgeEntryRelationship(
      Observation obs, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    if (obs.hasComponent()) {
      List<ObservationComponentComponent> comps = obs.getComponent();

      Boolean found = false;
      for (ObservationComponentComponent comp : comps) {

        if (comp.hasCode()
            && CdaFhirUtilities.isCodeableConceptPresentInValueSet(
                CdaGeneratorConstants.GestationalAgeEstimatedValueset, comp.getCode())
            && comp.hasValueQuantity()
            && comp.getValueQuantity().hasCode()) {

          sb.append(
              CdaGeneratorUtils.getXmlForEntryRelationship(
                  CdaGeneratorConstants.ENTRY_REL_REFR_CODE));
          sb.append(
              generateXmlforGestationalAge(obs, details, comp.getValueQuantity(), comp.getCode()));
          sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));
        }
      }
    }

    return sb.toString();
  }

  public static String generateXmlforGestationalAge(
      Observation obs, LaunchDetails details, Quantity quantity, CodeableConcept code) {

    StringBuilder sb = new StringBuilder();

    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_GEST_AGE_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_GEST_AGE_TEMPLATE_ID,
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_GEST_AGE_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    List<CodeableConcept> cds = new ArrayList<>();
    cds.add(code);
    sb.append(
        CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.CODE_EL_NAME, false));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(CdaFhirUtilities.getXmlForType(quantity, CdaGeneratorConstants.VAL_EL_NAME, true));

    return sb.toString();
  }

  public static void processCondition(
      Condition cond,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder pregEntries,
      Integer rowNum) {

    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.PREGNANCY_CONDITION_DISPLAY);

    bodyvals.put(
        CdaGeneratorConstants.PREGNANCY_OBSERVATION_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
                cond.getCode(), CdaGeneratorConstants.FHIR_SNOMED_URL, true)
            .getValue0());

    table.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

    pregEntries.append(generatePregnancyObservationEntry(cond, details));

    pregEntries.append(generateEddEntryRelationship(cond));

    pregEntries.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    pregEntries.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));
  }

  public static String generatePregnancySectionHeader(String nf) {

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
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PREGNANCY_SECTION_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SECTION_TEMPLATE_ID,
            CdaGeneratorConstants.PREGNANCY_SECTION_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PREGNANCY_SECTION_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PREGNANCY_SECTION_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.PREGNANCY_SECTION_TITLE));

    return sb.toString();
  }

  public static String generateEmptyPregnancySection() {

    StringBuilder sb = new StringBuilder();

    sb.append(generatePregnancySectionHeader(CdaGeneratorConstants.NF_NI));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Social History Information"));

    sb.append(generatePregnancySectionEndHeader());

    return sb.toString();
  }

  public static String generatePregnancySectionEndHeader() {

    StringBuilder sb = new StringBuilder();

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static String generatePregnancyObservationEntry(Condition cond, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PREGNANCY_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), cond.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.OBS_ASSERTION,
            CdaGeneratorConstants.OBS_ASSERTION_CODESYSTEM,
            CdaGeneratorConstants.OBS_ASSERTION_CODESYSTEM_NAME,
            CdaGeneratorConstants.OBS_ASSERTION_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> onset = CdaFhirUtilities.getActualDate(cond.getOnset());
    Pair<Date, TimeZone> abatement = CdaFhirUtilities.getActualDate(cond.getAbatement());

    sb.append(
        CdaGeneratorUtils.getXmlForIVLWithTS(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, onset, abatement, false));

    List<CodeableConcept> cds = new ArrayList<>();
    cds.add(cond.getCode());

    String codeXml =
        CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
            cds,
            CdaGeneratorConstants.VAL_EL_NAME,
            true,
            CdaGeneratorConstants.FHIR_SNOMED_URL,
            false,
            "");

    if (!codeXml.isEmpty()) {
      sb.append(codeXml);
    } else {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.VAL_EL_NAME, true));
    }

    sb.append(CdaFhirUtilities.getXmlForAuthorTime(cond.getRecordedDateElement()));

    return sb.toString();
  }

  public static String generatePregnancyObservationEntry(Observation obs, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PREGNANCY_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.OBS_ASSERTION,
            CdaGeneratorConstants.OBS_ASSERTION_CODESYSTEM,
            CdaGeneratorConstants.OBS_ASSERTION_CODESYSTEM_NAME,
            CdaGeneratorConstants.OBS_ASSERTION_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> start = null;
    Pair<Date, TimeZone> end = null;
    if (obs.hasEffective() && (obs.getEffective() instanceof Period)) {

      Period p = (Period) obs.getEffectivePeriod();
      start = CdaFhirUtilities.getActualDate(p.getStartElement());
      end = CdaFhirUtilities.getActualDate(p.getEndElement());

    } else if (obs.hasEffective() && (obs.getEffective() instanceof DateTimeType)) {
      start = CdaFhirUtilities.getActualDate(obs.getEffective());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForIVLWithTS(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, start, end, false));

    List<CodeableConcept> cds = new ArrayList<>();
    String codeXml = "";

    if (obs.hasValue() && (obs.getValue() instanceof CodeableConcept)) {
      cds.add(obs.getValueCodeableConcept());
      codeXml =
          CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
              cds,
              CdaGeneratorConstants.VAL_EL_NAME,
              true,
              CdaGeneratorConstants.FHIR_SNOMED_URL,
              false,
              "");
    } else {
      cds.add(obs.getCode());

      codeXml =
          CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
              cds,
              CdaGeneratorConstants.VAL_EL_NAME,
              true,
              CdaGeneratorConstants.FHIR_SNOMED_URL,
              false,
              "");
    }

    if (!codeXml.isEmpty()) {
      sb.append(codeXml);
    } else {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.VAL_EL_NAME, true));
    }

    if (obs.hasMethod()) {
      List<CodeableConcept> ms = new ArrayList<>();
      ms.add(obs.getMethod());

      String methodCodeXml =
          CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
              cds,
              CdaGeneratorConstants.METHOD_CODE_EL_NAME,
              true,
              CdaGeneratorConstants.FHIR_SNOMED_URL,
              false,
              "");

      if (!methodCodeXml.isEmpty()) {
        sb.append(codeXml);
      }
    }

    sb.append(CdaFhirUtilities.getXmlForAuthorTime(obs.getIssuedElement()));

    return sb.toString();
  }

  public static String generatePregnancyEntry(Condition cond, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    sb.append(generatePregnancyObservationEntry(cond, details));

    sb.append(generateEddEntryRelationship(cond));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    return sb.toString();
  }

  public static String generateEddEntryRelationship(Condition cond) {

    StringBuilder sb = new StringBuilder();

    DateTimeType estimatedDate = null;

    if (cond.getOnset() instanceof Period) {
      Period onsetPeriod = (Period) cond.getOnset();
      if (onsetPeriod.hasEnd()) {
        estimatedDate = onsetPeriod.getEndElement();
      }
    } else if (cond.hasAbatement()) {
      if (cond.getAbatement() instanceof DateTimeType) {
        estimatedDate = (DateTimeType) cond.getAbatement();
      }
    }

    if (estimatedDate != null) {
      // Generate the Estimate Delivery entry
      sb.append(
          CdaGeneratorUtils.getXmlForEntryRelationship(CdaGeneratorConstants.ENTRY_REL_REFR_CODE));
      sb.append(generateXmlforEstimatedDeliveryDate(estimatedDate, null));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));
    }

    return sb.toString();
  }

  public static String generateEddEntryRelationship(Observation obs) {

    StringBuilder sb = new StringBuilder();
    DateTimeType estimatedDate = null;
    CodeableConcept methodCode = null;

    if (obs.hasComponent()) {

      List<ObservationComponentComponent> comps = obs.getComponent();

      Boolean found = false;
      for (ObservationComponentComponent comp : comps) {

        if (comp.hasCode()
            && CdaFhirUtilities.isCodeableConceptPresentInValueSet(
                CdaGeneratorConstants.EstimatedDateofDeliveryValueset, comp.getCode())) {

          estimatedDate =
              CdaFhirUtilities.getDateTimeExtensionValue(
                  comp.getExtension(), CdaGeneratorConstants.EstimatedDateofDeliveryExtUrl);

          if (estimatedDate != null) {
            found = true;
          } else if (comp.hasValue() && comp.getValue() instanceof Period) {
            Period p = comp.getValuePeriod();
            if (p.hasEnd()) {
              found = true;
              estimatedDate = p.getEndElement();
            }
          } else if (comp.hasValue() && comp.getValue() instanceof DateTimeType) {
            found = true;
            estimatedDate = comp.getValueDateTimeType();
          }
        }

        if (found) {

          if (comp.hasCode()) methodCode = comp.getCode();
          break;
        }
      }
    }

    if (estimatedDate != null) {
      // Generate the Estimate Delivery entry
      sb.append(
          CdaGeneratorUtils.getXmlForEntryRelationship(CdaGeneratorConstants.ENTRY_REL_REFR_CODE));
      sb.append(generateXmlforEstimatedDeliveryDate(estimatedDate, methodCode));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));
    }

    return sb.toString();
  }

  public static String generateXmlforEstimatedDeliveryDate(
      DateTimeType estimatedDate, CodeableConcept methodCode) {
    StringBuilder sb = new StringBuilder();

    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_EDOD_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_EDOD_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PREGNANCY_SUPPLEMENTAL_EDOD_OBS_TEMPLATE_ID_EXT));

    if (methodCode != null && methodCode.hasCoding()) {
      List<CodeableConcept> cds = new ArrayList<>();
      cds.add(methodCode);
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(cds, CdaGeneratorConstants.CODE_EL_NAME, false));

    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.PREGNANCY_ESTIMATED_DELIVERY_DATE_CODE,
              CdaGeneratorConstants.PREGNANCY_ESTIMATED_DELIVERY_DATE_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.PREGNANCY_ESTIMATED_DELIVERY_DATE_DISPLAY_NAME));
    }
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(
        CdaGeneratorUtils.getXmlForValueEffectiveTime(
            CdaGeneratorConstants.VAL_EL_NAME, estimatedDate.getValue(), null));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    return sb.toString();
  }
}
