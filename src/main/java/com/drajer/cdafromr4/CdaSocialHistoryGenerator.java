package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.BooleanType;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.Type;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaSocialHistoryGenerator {

  private CdaSocialHistoryGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaSocialHistoryGenerator.class);
  private static final String FALSE = "false";

  private static class SocialHistoryData {
    int index = 0;
    StringBuilder tableRows = new StringBuilder();
    String birthSexXml = "";
    StringBuilder pregCondXml = new StringBuilder();
    StringBuilder occHistoryXml = new StringBuilder();
    StringBuilder travelHistoryXml = new StringBuilder();
    String pregObsXml = "";
  }

  public static String generateSocialHistorySection(
      R4FhirData data, LaunchDetails details, String version) {
    logger.info("LaunchDetails in generateSocialHistorySection:{}", details);

    StringBuilder sb = new StringBuilder(2000);

    CodeType birthSex =
        CdaFhirUtilities.getCodeExtension(
            data.getPatient().getExtension(), CdaGeneratorConstants.FHIR_USCORE_BIRTHSEX_EXT_URL);
    List<Observation> pregObs = new ArrayList<>();
    List<Condition> pregCond = new ArrayList<>();
    List<Observation> occHistory = new ArrayList<>();

    if (version.contentEquals(CdaGeneratorConstants.CDA_EICR_VERSION_R11)) {
      pregCond = data.getPregnancyConditions();
      pregObs = data.getPregnancyObs();
      occHistory = data.getOccupationObs();
    }

    List<Observation> travelHistory = data.getTravelObs();

    if (!hasSocialHistoryData(birthSex, pregObs, pregCond, occHistory, travelHistory)) {
      sb.append(generateEmptySocialHistorySection(version));
      return sb.toString();
    }

    sb.append(generateSocialHistorySectionHeader("", version));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));
    appendTableHeader(sb);
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    SocialHistoryData shData = new SocialHistoryData();
    Map<String, String> bodyvals = new LinkedHashMap<>();

    processBirthSex(birthSex, shData, bodyvals);
    processPregnancyConditions(pregCond, details, shData, bodyvals);
    processPregnancyObservations(pregObs, shData);
    processOccupationHistory(occHistory, details, shData, bodyvals);
    processTravelHistory(travelHistory, details, shData, bodyvals);

    sb.append(shData.tableRows);
    appendTableClosing(sb);
    appendSocialHistoryEntries(sb, shData);
    sb.append(generateSocialHistorySectionEndHeader());

    return sb.toString();
  }

  private static boolean hasSocialHistoryData(
      CodeType birthSex,
      List<Observation> pregObs,
      List<Condition> pregCond,
      List<Observation> occHistory,
      List<Observation> travelHistory) {
    return birthSex != null
        || (pregObs != null && !pregObs.isEmpty())
        || (pregCond != null && !pregCond.isEmpty())
        || (occHistory != null && !occHistory.isEmpty())
        || (travelHistory != null && !travelHistory.isEmpty());
  }

  private static void appendTableHeader(StringBuilder sb) {
    List<String> list = new ArrayList<>();
    list.add(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_TITLE);
    list.add(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_TITLE);
    sb.append(
        CdaGeneratorUtils.getXmlForTableHeader(
            list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));
  }

  private static void appendTableClosing(StringBuilder sb) {
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));
  }

  private static void processBirthSex(
      CodeType birthSex, SocialHistoryData shData, Map<String, String> bodyvals) {
    if (birthSex == null) return;
    logger.info("Found Birth Sex");
    bodyvals.put(
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.BIRTH_SEX_DISPLAY);
    bodyvals.put(
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getStringForType(birthSex));
    shData.tableRows.append(CdaGeneratorUtils.addTableRow(bodyvals, shData.index++));
    shData.birthSexXml = generateBirthSexEntry(birthSex);
  }

  private static void processPregnancyConditions(
      List<Condition> pregCond,
      LaunchDetails details,
      SocialHistoryData shData,
      Map<String, String> bodyvals) {
    if (pregCond == null || pregCond.isEmpty()) return;
    logger.info("Pregnancy Condition Found");
    for (Condition c : pregCond) {
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
          CdaGeneratorConstants.PREGNANCY_CONDITION_DISPLAY);
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT,
          CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
                  c.getCode(), CdaGeneratorConstants.FHIR_SNOMED_URL, true)
              .getValue0());
      shData.tableRows.append(CdaGeneratorUtils.addTableRow(bodyvals, shData.index++));
      shData.pregCondXml.append(generatePregnancyEntry(c, details));
    }
  }

  private static void processPregnancyObservations(
      List<Observation> pregObs, SocialHistoryData shData) {
    if (pregObs == null || pregObs.isEmpty()) return;
    logger.info("Pregnancy Status Observation Found - Will be added as needed.");
  }

  private static void processOccupationHistory(
      List<Observation> occHistory,
      LaunchDetails details,
      SocialHistoryData shData,
      Map<String, String> bodyvals) {
    if (occHistory == null || occHistory.isEmpty()) return;
    logger.info("Occupation History Observation Found");
    for (Observation obs : occHistory) {
      if (obs.getValue() != null
          && (obs.getValue() instanceof StringType || obs.getValue() instanceof CodeableConcept)) {
        bodyvals.put(
            CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
            CdaGeneratorConstants.OCCUPATION_HISTORY_DISPLAY);
        String display = formatOccupationDisplay(CdaFhirUtilities.getStringForType(obs.getValue()));
        bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
        shData.tableRows.append(CdaGeneratorUtils.addTableRow(bodyvals, shData.index++));
        shData.occHistoryXml.append(generateOccHistoryEntry(obs, details));
      }
    }
  }

  private static String formatOccupationDisplay(String display) {
    if (display.contentEquals("true")) return "Employed";
    if (display.contentEquals(FALSE)) return "Unemployed";
    return display;
  }

  private static void processTravelHistory(
      List<Observation> travelHistory,
      LaunchDetails details,
      SocialHistoryData shData,
      Map<String, String> bodyvals) {
    if (travelHistory == null || travelHistory.isEmpty()) return;
    logger.info("Travel History Observation Found ");
    for (Observation obs : travelHistory) {
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
          CdaGeneratorConstants.TRAVEL_HISTORY_DISPLAY);
      String display = CdaFhirUtilities.getStringForObservationsWithComponents(obs);
      bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
      shData.tableRows.append(CdaGeneratorUtils.addTableRow(bodyvals, shData.index++));
      shData.travelHistoryXml.append(generateTravelHistoryEntry(obs, display, details));
    }
  }

  private static void appendSocialHistoryEntries(StringBuilder sb, SocialHistoryData shData) {
    if (!StringUtils.isEmpty(shData.birthSexXml)) sb.append(shData.birthSexXml);
    if (!StringUtils.isEmpty(shData.pregCondXml)) sb.append(shData.pregCondXml);
    if (!StringUtils.isEmpty(shData.pregObsXml)) sb.append(shData.pregObsXml);
    if (!StringUtils.isEmpty(shData.occHistoryXml)) sb.append(shData.occHistoryXml);
    if (!StringUtils.isEmpty(shData.travelHistoryXml)) sb.append(shData.travelHistoryXml);
  }

  public static String generateTravelHistoryEntry(
      Observation obs, String display, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.ACT_EL_NAME,
            CdaGeneratorConstants.ACT_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.TRAVEL_HISTORY_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.TRAVEL_HISTORY_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.TRAVEL_HISTORY_SNOMED_CODE,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_OID,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_NAME,
            CdaGeneratorConstants.TRAVEL_HISTORY_SNOMED_CODE_DISPLAY));

    sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME, display));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    if (obs.hasEffective()) {
      Type effective = obs.getEffective();

      if (effective instanceof Period) {
        Period period = (Period) effective;
        sb.append(
            CdaFhirUtilities.getPeriodXml(period, CdaGeneratorConstants.EFF_TIME_EL_NAME, false));
      } else {
        Pair<Date, TimeZone> effDate = CdaFhirUtilities.getActualDate(effective);
        sb.append(
            CdaGeneratorUtils.getXmlForEffectiveTime(
                CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate.getValue0(), effDate.getValue1()));
      }
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    // Add components
    sb.append(generateParticipant(obs));

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String generateParticipant(Observation obs) {
    StringBuilder sb = new StringBuilder();

    if (obs != null && obs.hasComponent()) {
      for (ObservationComponentComponent oc : obs.getComponent()) {
        if (isValidObservationComponent(oc)) {
          sb.append(buildParticipantXml(oc));
        }
      }
    }

    return sb.toString();
  }

  private static boolean isValidObservationComponent(ObservationComponentComponent oc) {
    return (oc.hasCode() && oc.hasValue());
  }

  private static String buildParticipantXml(ObservationComponentComponent oc) {
    StringBuilder participantXml = new StringBuilder();

    participantXml
        .append(CdaGeneratorUtils.getXmlForParticipant(CdaGeneratorConstants.TYPE_CODE_LOC))
        .append(CdaGeneratorUtils.getXmlForParticipantRole(CdaGeneratorConstants.TERR));

    if (oc.hasValue() && oc.getValue() instanceof CodeableConcept) {

      CodeableConcept cc = (CodeableConcept) oc.getValue();

      if (cc.hasCoding()) {
        participantXml.append(
            CdaFhirUtilities.getSingleCodingXmlFromCodings(
                cc.getCoding(), CdaGeneratorConstants.CODE_EL_NAME));
      } else if (cc.hasText()) {
        participantXml.append(
            CdaGeneratorUtils.getXmlForNullCDWithText(
                CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_OTH, cc.getText()));
      }
    }

    if (oc.hasExtension()) {
      Address address =
          CdaFhirUtilities.getAddressExtensionValue(
              oc.getExtension(), CdaGeneratorConstants.FHIR_OBSERVATION_ADDRESS_EXTENSION_URL);
      if (address != null) {
        participantXml.append(CdaFhirUtilities.getTravelHistoryAddressXml(address));
      }
    }

    participantXml
        .append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME))
        .append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_EL_NAME));

    return participantXml.toString();
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

      sb.append(generateXmlforEstimatedDeliveryDate(estimatedDate));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));
    }

    return sb.toString();
  }

  public static String generateBirthSexEntry(CodeType birthSex) {

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

    String birsex = CdaFhirUtilities.getStringForType(birthSex);

    sb.append(CdaFhirUtilities.getBirthSexXml(birsex));

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String generateSocialHistorySectionHeader(String nf, String version) {

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

    if (version.contentEquals(CdaGeneratorConstants.CDA_EICR_VERSION_R31)) {
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.ODH_SECTION_TEMPLATE_ID,
              CdaGeneratorConstants.ODH_SECTION_TEMPLATE_ID_EXT));
    }

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

  private static String generateXmlforEstimatedDeliveryDate(DateTimeType estimatedDate) {
    StringBuilder sb = new StringBuilder();

    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PREGNANCY_EDOD_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PREGNANCY_ESTIMATED_DELIVERY_DATE_CODE,
            CdaGeneratorConstants.PREGNANCY_ESTIMATED_DELIVERY_DATE_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PREGNANCY_ESTIMATED_DELIVERY_DATE_DISPLAY_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(
        CdaGeneratorUtils.getXmlForValueEffectiveTime(
            CdaGeneratorConstants.VAL_EL_NAME, estimatedDate.getValue(), null));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    return sb.toString();
  }

  public static String generateSocialHistorySectionEndHeader() {

    StringBuilder sb = new StringBuilder();

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static String generateEmptySocialHistorySection(String version) {

    StringBuilder sb = new StringBuilder();

    sb.append(generateSocialHistorySectionHeader(CdaGeneratorConstants.NF_NI, version));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Social History Information"));

    sb.append(generateSocialHistorySectionEndHeader());

    return sb.toString();
  }

  private static class R31SocialHistoryData {
    int index = 0;
    String birthSexXml = "";
    String genderIdentityXml = "";
    String tribalAffiliationXml = "";
    StringBuilder nationalityDataXml = new StringBuilder();
    StringBuilder residencyDataXml = new StringBuilder();
    StringBuilder travelHistoryXml = new StringBuilder();
    StringBuilder homelessDataXml = new StringBuilder();
    StringBuilder disabilityDataXml = new StringBuilder();
    StringBuilder occEntries = new StringBuilder();
  }

  public static Object generateR31SocialHistorySection(
      R4FhirData data, LaunchDetails details, String version) {
    logger.info("LaunchDetails in generateSocialHistorySection:{}", details);

    CodeType birthSex =
        CdaFhirUtilities.getCodeExtension(
            data.getPatient().getExtension(), CdaGeneratorConstants.FHIR_USCORE_BIRTHSEX_EXT_URL);

    CodeableConcept genderIdentity =
        CdaFhirUtilities.getCodeableConceptFromExtension(
            data.getPatient().getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_GENDER_IDENTITY_EXT_URL);

    Extension tribalAffiliation =
        CdaFhirUtilities.getExtensionForUrl(
            data.getPatient().getExtension(),
            CdaGeneratorConstants.FHIR_USCORE_TRIBAL_AFFILIATION_EXT_URL);

    List<Observation> travelHistory = data.getTravelObs();
    List<Observation> residencyData = data.getResidencyObs();
    List<Observation> nationalityData = data.getNationalityObs();
    List<Observation> disabilityData = data.getDisabilityObs();
    List<Observation> homelessData = data.getHomelessObs();
    List<Observation> occObs = data.getOccupationObs();

    if (!hasR31SocialHistoryData(
        birthSex,
        genderIdentity,
        tribalAffiliation,
        nationalityData,
        residencyData,
        homelessData,
        disabilityData,
        travelHistory,
        occObs)) {
      return generateEmptySocialHistorySection(version);
    }

    StringBuilder sb = new StringBuilder(2000);
    sb.append(generateSocialHistorySectionHeader("", version));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));
    appendTableHeader(sb);
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    R31SocialHistoryData data31 = new R31SocialHistoryData();
    Map<String, String> bodyvals = new LinkedHashMap<>();

    processR31BirthSex(birthSex, data31, bodyvals, sb);
    processR31GenderIdentity(genderIdentity, data31, bodyvals, sb);
    processR31TribalAffiliation(tribalAffiliation, data31, bodyvals, sb);
    processR31TravelHistory(travelHistory, details, data31, bodyvals, sb);
    processR31NationalityData(nationalityData, details, data31, bodyvals, sb);
    processR31ResidencyData(residencyData, details, data31, bodyvals, sb);
    processR31HomelessData(homelessData, details, data31, bodyvals, sb);
    processR31DisabilityData(disabilityData, details, data31, bodyvals, sb);
    processR31OccupationData(occObs, details, data31, sb);

    appendTableClosing(sb);
    appendR31SocialHistoryEntries(sb, data31);
    sb.append(generateSocialHistorySectionEndHeader());

    return sb.toString();
  }

  private static boolean hasR31SocialHistoryData(
      CodeType birthSex,
      CodeableConcept genderIdentity,
      Extension tribalAffiliation,
      List<Observation> nationalityData,
      List<Observation> residencyData,
      List<Observation> homelessData,
      List<Observation> disabilityData,
      List<Observation> travelHistory,
      List<Observation> occObs) {
    return birthSex != null
        || genderIdentity != null
        || tribalAffiliation != null
        || (nationalityData != null && !nationalityData.isEmpty())
        || (residencyData != null && !residencyData.isEmpty())
        || (homelessData != null && !homelessData.isEmpty())
        || (disabilityData != null && !disabilityData.isEmpty())
        || (travelHistory != null && !travelHistory.isEmpty())
        || (occObs != null && !occObs.isEmpty());
  }

  private static void processR31BirthSex(
      CodeType birthSex,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (birthSex == null) return;
    logger.info("Found Birth Sex");
    bodyvals.put(
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.BIRTH_SEX_DISPLAY);
    bodyvals.put(
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT,
        CdaFhirUtilities.getStringForType(birthSex));
    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index++));
    data31.birthSexXml = generateBirthSexEntry(birthSex);
  }

  private static void processR31GenderIdentity(
      CodeableConcept genderIdentity,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (genderIdentity == null) return;
    logger.info(" Found Gender Identity ");
    bodyvals.put(
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.GENDER_IDENTITY_DISPLAY);
    String display = CdaFhirUtilities.getStringForType(genderIdentity);
    bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index));
    String contentRef =
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT + Integer.toString(data31.index);
    data31.genderIdentityXml = generateGenderIdentityEntry(genderIdentity, contentRef);
    data31.index++;
  }

  private static void processR31TribalAffiliation(
      Extension tribalAffiliation,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (tribalAffiliation == null) return;
    logger.info(" Found Tribal Affiliation ");
    bodyvals.put(
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
        CdaGeneratorConstants.TRIBAL_AFFILIATION_DISPLAY);
    String display = CdaFhirUtilities.getStringForType(tribalAffiliation);
    bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index));
    String contentRef =
        CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT + Integer.toString(data31.index);
    data31.tribalAffiliationXml = generateTribalAffiliationEntry(tribalAffiliation, contentRef);
    data31.index++;
  }

  private static void processR31TravelHistory(
      List<Observation> travelHistory,
      LaunchDetails details,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (travelHistory == null || travelHistory.isEmpty()) return;
    logger.info("Travel History Observation Found ");
    for (Observation obs : travelHistory) {
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
          CdaGeneratorConstants.TRAVEL_HISTORY_DISPLAY);
      String display = CdaFhirUtilities.getStringForObservationsWithComponents(obs);
      bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
      sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index));
      data31.travelHistoryXml.append(generateTravelHistoryEntry(obs, display, details));
      data31.index++;
    }
  }

  private static void processR31NationalityData(
      List<Observation> nationalityData,
      LaunchDetails details,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (nationalityData == null || nationalityData.isEmpty()) return;
    logger.info("Nationality Data Observation Found ");
    for (Observation obs : nationalityData) {
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
          CdaGeneratorConstants.NATIONALITY_DISPLAY);
      String display = CdaFhirUtilities.getStringForType(obs.getValue());
      bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
      sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index));
      String contentRef =
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT
              + Integer.toString(data31.index);
      data31.nationalityDataXml.append(generateNationalityEntry(obs, details, contentRef));
      data31.index++;
    }
  }

  private static void processR31ResidencyData(
      List<Observation> residencyData,
      LaunchDetails details,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (residencyData == null || residencyData.isEmpty()) return;
    logger.info("Residency Data Observation Found ");
    for (Observation obs : residencyData) {
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
          CdaGeneratorConstants.RESIDENCY_DISPLAY);
      String display = CdaFhirUtilities.getStringForType(obs.getValue());
      bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
      sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index));
      String contentRef =
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT
              + Integer.toString(data31.index);
      data31.residencyDataXml.append(generateResidencyEntry(obs, details, contentRef));
      data31.index++;
    }
  }

  private static void processR31HomelessData(
      List<Observation> homelessData,
      LaunchDetails details,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (homelessData == null || homelessData.isEmpty()) return;
    logger.info("Homeless Data Observation Found ");
    for (Observation obs : homelessData) {
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
          CdaGeneratorConstants.HOMELESS_TABLE_DISPLAY);
      String display = CdaFhirUtilities.getStringForType(obs.getValue());
      bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, display);
      sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index));
      String contentRef =
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT
              + Integer.toString(data31.index);
      data31.homelessDataXml.append(generateHomelessEntry(obs, details, contentRef));
      data31.index++;
    }
  }

  private static void processR31DisabilityData(
      List<Observation> disabilityData,
      LaunchDetails details,
      R31SocialHistoryData data31,
      Map<String, String> bodyvals,
      StringBuilder sb) {
    if (disabilityData == null || disabilityData.isEmpty()) return;
    logger.info("Disability Data Observation Found ");
    for (Observation obs : disabilityData) {
      bodyvals.put(
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT,
          CdaGeneratorConstants.DISABILITY_TABLE_DISPLAY);
      String display = CdaFhirUtilities.getStringForType(obs.getCode());
      String finalDisplay = display + "|value=" + CdaFhirUtilities.getStringForType(obs.getValue());
      bodyvals.put(CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_2_BODY_CONTENT, finalDisplay);
      sb.append(CdaGeneratorUtils.addTableRow(bodyvals, data31.index));
      String contentRef =
          CdaGeneratorConstants.SOC_HISTORY_TABLE_COL_1_BODY_CONTENT
              + Integer.toString(data31.index);
      data31.disabilityDataXml.append(generateDisabilityEntry(obs, details, contentRef));
      data31.index++;
    }
  }

  private static void processR31OccupationData(
      List<Observation> occObs,
      LaunchDetails details,
      R31SocialHistoryData data31,
      StringBuilder sb) {
    if (occObs == null) return;
    for (Observation obs : occObs) {
      if (obs == null) {
        continue;
      }

      if (isPastOrPresentOccupation(obs)) {
        generatePastOrPresentEntry(obs, details, sb, data31.occEntries, data31.index++);
      } else if (isUsualOccupation(obs)) {
        generateUsualOccupationEntry(obs, details, sb, data31.occEntries, data31.index++);
      } else if (isEmploymentStatusObservation(obs)) {
        generateEmploymentStatusObservation(obs, details, sb, data31.occEntries, data31.index++);
      } else {
        logger.info(
            "Ignoring Observation {} since it is not what is expected for ODH",
            obs.getIdElement().getIdPart());
      }
    }
  }

  private static void appendR31SocialHistoryEntries(StringBuilder sb, R31SocialHistoryData data31) {
    if (!StringUtils.isEmpty(data31.birthSexXml)) sb.append(data31.birthSexXml);
    if (!StringUtils.isEmpty(data31.genderIdentityXml)) sb.append(data31.genderIdentityXml);
    if (!StringUtils.isEmpty(data31.tribalAffiliationXml)) sb.append(data31.tribalAffiliationXml);
    if (!StringUtils.isEmpty(data31.travelHistoryXml)) sb.append(data31.travelHistoryXml);
    if (!StringUtils.isEmpty(data31.nationalityDataXml)) sb.append(data31.nationalityDataXml);
    if (!StringUtils.isEmpty(data31.residencyDataXml)) sb.append(data31.residencyDataXml);
    if (!StringUtils.isEmpty(data31.homelessDataXml)) sb.append(data31.homelessDataXml);
    if (!StringUtils.isEmpty(data31.disabilityDataXml)) sb.append(data31.disabilityDataXml);
    if (!StringUtils.isEmpty(data31.occEntries)) sb.append(data31.occEntries);
  }

  private static String generateDisabilityEntry(
      Observation obs, LaunchDetails details, String contentRef) {
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
            CdaGeneratorConstants.DISABILITY_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.DISABILITY_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));

    sb.append(
        CdaFhirUtilities.getCodeableConceptXml(
            obs.getCode(), CdaGeneratorConstants.CODE_EL_NAME, contentRef));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(
        CdaFhirUtilities.getXmlForType(
            obs.getEffective(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    sb.append(getDisabilityBooleanValue(obs.getValue()));

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  private static String generateHomelessEntry(
      Observation obs, LaunchDetails details, String contentRef) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.HOMELESS_OBS_TEMPLATE_ID));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.HOMELESS_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.HOMELESS_TABLE_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    if (obs.hasValue() && obs.getValue() instanceof CodeableConcept) {
      CodeableConcept cd = obs.getValueCodeableConcept();
      sb.append(
          CdaFhirUtilities.getCodeableConceptXmlForValue(
              cd, CdaGeneratorConstants.VAL_EL_NAME, contentRef));
    } else {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXmlForValue(
              null, CdaGeneratorConstants.VAL_EL_NAME, contentRef));
    }

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  private static Object generateResidencyEntry(
      Observation obs, LaunchDetails details, String contentRef) {
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
            CdaGeneratorConstants.RESIDENCY_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.RESIDENCY_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.RESIDENCY_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.RESIDENCY_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    if (obs.hasValue() && obs.getValue() instanceof CodeableConcept) {
      CodeableConcept cd = obs.getValueCodeableConcept();
      sb.append(
          CdaFhirUtilities.getCodeableConceptXmlForValue(
              cd, CdaGeneratorConstants.VAL_EL_NAME, contentRef));
    } else {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXmlForValue(
              null, CdaGeneratorConstants.VAL_EL_NAME, contentRef));
    }

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  private static Object generateNationalityEntry(
      Observation obs, LaunchDetails details, String contentRef) {

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
            CdaGeneratorConstants.NATIONALITY_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.NATIONALITY_OBS_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), obs.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.NATIONALITY_CODE,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_OID,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_NAME,
            CdaGeneratorConstants.NATIONALITY_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    if (obs.hasValue() && obs.getValue() instanceof CodeableConcept) {
      CodeableConcept cd = obs.getValueCodeableConcept();
      sb.append(
          CdaFhirUtilities.getCodeableConceptXmlForValue(
              cd, CdaGeneratorConstants.VAL_EL_NAME, contentRef));
    } else {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXmlForValue(
              null, CdaGeneratorConstants.VAL_EL_NAME, contentRef));
    }

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String generateTribalAffiliationEntry(
      Extension tribalAffiliation, String contentRef) {

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
            CdaGeneratorConstants.TRIBAL_AFFILIATION_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.TRIBAL_AFFILIATION_OBS_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    Extension tribalAff =
        tribalAffiliation.getExtensionByUrl(
            CdaGeneratorConstants.FHIR_USCORE_TRIBAL_AFFILIATION_CAT_URL);
    Extension tribalEnrolled =
        tribalAffiliation.getExtensionByUrl(
            CdaGeneratorConstants.FHIR_USCORE_TRIBAL_AFFILIATION_ENROLLED_URL);

    if (tribalAff != null
        && tribalAff.hasValue()
        && tribalAff.getValue() instanceof CodeableConcept) {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(
              (CodeableConcept) tribalAff.getValue(),
              CdaGeneratorConstants.CODE_EL_NAME,
              contentRef));
    } else {
      sb.append(
          CdaFhirUtilities.getCodeableConceptXml(
              null, CdaGeneratorConstants.CODE_EL_NAME, contentRef));
    }

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(
        CdaGeneratorUtils.getXmlForNullEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, CdaGeneratorConstants.NF_NI));

    if (tribalEnrolled != null
        && tribalEnrolled.hasValue()
        && tribalEnrolled.getValue() instanceof BooleanType) {
      String isEntrolledString = ((BooleanType) tribalEnrolled.getValue()).getValueAsString();
      sb.append(CdaGeneratorUtils.getXmlForValueBoolean(isEntrolledString));
    } else {
      sb.append(CdaGeneratorUtils.getXmlForValueBoolean(FALSE));
    }

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String generateGenderIdentityEntry(
      CodeableConcept genderIdentity, String contentRef) {

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
            CdaGeneratorConstants.SOC_HISTORY_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.SOC_HISTORY_OBS_TEMPLATE_ID_EXT));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.GENDER_IDENTITY_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.GENDER_IDENTITY_OBS_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.GENDER_IDENTITY_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.GENDER_IDENTITY_DISPLAY));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.EFF_TIME_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNullEffectiveTime(
            CdaGeneratorConstants.TIME_LOW_EL_NAME, CdaGeneratorConstants.NF_NI));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.EFF_TIME_EL_NAME));

    sb.append(
        CdaFhirUtilities.getCodeableConceptXmlForValue(
            genderIdentity, CdaGeneratorConstants.VAL_EL_NAME, contentRef));

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String getDisabilityBooleanValue(Type type) {
    if (type == null) {
      return CdaGeneratorUtils.getXmlForNullValueBL(CdaGeneratorConstants.NF_NI);
    }

    final String SNOMED_YES = "373066001";
    final String SNOMED_NO = "373067005";

    String valueCode =
        CdaFhirUtilities.getMatchingCodeFromTypeForCodeSystem(
            Arrays.asList(SNOMED_YES, SNOMED_NO), type, CdaGeneratorConstants.FHIR_SNOMED_URL);

    if (StringUtils.isNotEmpty(valueCode)) {
      if (SNOMED_YES.equals(valueCode)) {
        return CdaGeneratorUtils.getXmlForValueBoolean("true");
      }
      if (SNOMED_NO.equals(valueCode)) {
        return CdaGeneratorUtils.getXmlForValueBoolean(FALSE);
      }
    }

    String valueText = CdaFhirUtilities.getStringForType(type);
    if (StringUtils.isNotEmpty(valueText)) {
      if ("yes".equalsIgnoreCase(valueText)) {
        return CdaGeneratorUtils.getXmlForValueBoolean("true");
      }
      if ("no".equalsIgnoreCase(valueText)) {
        return CdaGeneratorUtils.getXmlForValueBoolean(FALSE);
      }
    }

    return CdaGeneratorUtils.getXmlForNullValueBL(CdaGeneratorConstants.NF_NI);
  }

  private static void generateEmploymentStatusObservation(
      Observation obs,
      LaunchDetails details,
      StringBuilder table,
      StringBuilder occEntries,
      int rowNum) {
    if (obs == null) {
      logger.error("Observation is null, cannot generate Employment Status Observation");
      return;
    }

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
      bodyvals.put(CdaGeneratorConstants.ODH_TABLE_COL_2_BODY_CONTENT, display);
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
      bodyvals.put(CdaGeneratorConstants.ODH_TABLE_COL_2_BODY_CONTENT, display);
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
      bodyvals.put(CdaGeneratorConstants.ODH_TABLE_COL_2_BODY_CONTENT, display);
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
            CdaGeneratorConstants.HISTORY_OF_OCCUPATION_INDUSTRY));

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
        obs.getExtension(), CdaGeneratorConstants.ODH_CURRENT_JOB_EXTENSION);
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

  public static String generateOdhSectionEndHeader() {

    StringBuilder sb = new StringBuilder();

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }
}
