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
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Identifier;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaEncounterGenerator {

  private static final Logger logger = LoggerFactory.getLogger(CdaEncounterGenerator.class);

  private CdaEncounterGenerator() {}

  public static String generateEncounterSection(R4FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);
    Encounter encounter = data.getEncounter();

    if (encounter == null) {
      sb.append(generateEmptyEncounterSection());
      return sb.toString();
    }

    logger.debug("Generating Encounter section");
    appendSectionHeader(sb);
    appendTableSection(sb, encounter);
    appendEntrySection(sb, encounter, details, data);
    appendSectionFooter(sb);

    return sb.toString();
  }

  private static void appendSectionHeader(StringBuilder sb) {
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID_EXT));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.ENC_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.ENC_SEC_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.ENC_SEC_TITLE));
  }

  private static void appendTableSection(StringBuilder sb, Encounter encounter) {
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));
    List<String> list = new ArrayList<>();
    list.add(CdaGeneratorConstants.ENC_TABLE_COL_1_TITLE);
    list.add(CdaGeneratorConstants.ENC_TABLE_COL_2_TITLE);
    sb.append(
        CdaGeneratorUtils.getXmlForTableHeader(
            list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    String actDisplayName = extractEncounterDisplayName(encounter);
    String dt = extractEncounterDateTime(encounter);

    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(CdaGeneratorConstants.ENC_TABLE_COL_1_BODY_CONTENT, actDisplayName);
    bodyvals.put(CdaGeneratorConstants.ENC_TABLE_COL_2_BODY_CONTENT, dt);
    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, 1));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));
  }

  private static void appendEntrySection(
      StringBuilder sb, Encounter encounter, LaunchDetails details, R4FhirData data) {
    sb.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.ENC_ACT_EL_NAME,
            CdaGeneratorConstants.ENC_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));
    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_ENTRY_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.ENC_ENTRY_TEMPLATE_ID,
            CdaGeneratorConstants.ENC_ENTRY_TEMPLATE_ID_EXT));
    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), encounter.getIdElement().getIdPart()));

    appendEncounterIdentifiers(sb, encounter, details);

    String contentRef = CdaGeneratorConstants.ENC_TABLE_COL_1_BODY_CONTENT + "1";
    sb.append(getEncounterCodeXml(encounter, contentRef));
    sb.append(
        CdaFhirUtilities.getPeriodXml(
            encounter.getPeriod(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    appendDischargeDisposition(sb, encounter);
    appendOptionalXmlEntry(
        sb,
        generateEncounterDiagnosisXml(data, details),
        "Adding Encounter Diagnosis to the Encounter Section");
    appendOptionalXmlEntry(
        sb,
        generateEncounterReasonXml(details, encounter),
        "Adding Encounter Reason Code Xml to the Encounter Section");

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENC_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));
  }

  private static void appendSectionFooter(StringBuilder sb) {
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));
  }

  private static String extractEncounterDisplayName(Encounter encounter) {
    if (encounter.getClass_() != null
        && encounter.getClass_().getCode() != null
        && !StringUtils.isEmpty(encounter.getClass_().getDisplay())) {
      logger.debug("Found Encounter Class value, so using it for display");
      return encounter.getClass_().getDisplay();
    }
    if (encounter.getTypeFirstRep() != null
        && encounter.getTypeFirstRep().getCodingFirstRep() != null
        && !StringUtils.isEmpty(encounter.getTypeFirstRep().getCodingFirstRep().getDisplay())) {
      logger.debug("Did not find Encounter class, but Found Encounter Type value, so using it");
      return encounter.getTypeFirstRep().getCodingFirstRep().getDisplay();
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  private static String extractEncounterDateTime(Encounter encounter) {
    if (encounter.getPeriod() != null && encounter.getPeriod().getStartElement() != null) {
      return CdaFhirUtilities.getDisplayStringForDateTimeType(
          encounter.getPeriod().getStartElement());
    }
    logger.error(
        " Period is either null or the Period.DateTime has a null value or null timezone value ");
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  private static void appendEncounterIdentifiers(
      StringBuilder sb, Encounter encounter, LaunchDetails details) {
    List<Identifier> ids = encounter.getIdentifier();
    if (ids == null) return;
    for (Identifier id : ids) {
      if (id.getSystem() != null && id.getValue() != null) {
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                CdaGeneratorUtils.getRootOid(id.getSystem(), details.getAssigningAuthorityId()),
                id.getValue()));
      }
    }
  }

  private static void appendDischargeDisposition(StringBuilder sb, Encounter encounter) {
    if (encounter.getHospitalization() == null
        || encounter.getHospitalization().getDischargeDisposition() == null) {
      return;
    }
    List<CodeableConcept> dds = new ArrayList<>();
    dds.add(encounter.getHospitalization().getDischargeDisposition());
    String ddxml =
        CdaFhirUtilities.getCodeableConceptXml(
            dds, CdaGeneratorConstants.SDTC_DISCHARGE_DISPOSITION, false);
    if (ddxml != null && !ddxml.isEmpty()) sb.append(ddxml);
  }

  private static void appendOptionalXmlEntry(
      StringBuilder sb, String xmlContent, String logMessage) {
    if (xmlContent == null || xmlContent.isEmpty()) return;
    logger.debug(logMessage);
    sb.append(xmlContent);
  }

  public static String generateEncounterDiagnosisXml(R4FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    if (data.getEncounterDiagnosisConditions() != null
        && !data.getEncounterDiagnosisConditions().isEmpty()) {

      List<Condition> encDiagnosis = data.getEncounterDiagnosisConditions();

      for (Condition c : encDiagnosis) {

        sb.append(
            CdaGeneratorUtils.getXmlForEntryRelationship(
                CdaGeneratorConstants.ENTRY_REL_RSON_CODE));
        sb.append(
            CdaGeneratorUtils.getXmlForAct(
                CdaGeneratorConstants.ACT_EL_NAME,
                CdaGeneratorConstants.ACT_CLASS_CODE,
                CdaGeneratorConstants.MOOD_CODE_DEF));

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_TEMPLATE_ID));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_TEMPLATE_ID,
                CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_TEMPLATE_ID_EXT));

        sb.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_CODE,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_CODE_DISPLAY_NAME));

        sb.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

        // Add problem observation
        sb.append(
            CdaGeneratorUtils.getXmlForEntryRelationship(
                CdaGeneratorConstants.ENTRY_REL_SUBJ_CODE));

        sb.append(
            CdaGeneratorUtils.getXmlForAct(
                CdaGeneratorConstants.OBS_ACT_EL_NAME,
                CdaGeneratorConstants.OBS_CLASS_CODE,
                CdaGeneratorConstants.MOOD_CODE_DEF));

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROB_OBS_TEMPLATE_ID));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.PROB_OBS_TEMPLATE_ID,
                CdaGeneratorConstants.PROB_OBS_TEMPALTE_ID_EXT));
        sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorUtils.getGuid()));

        // Add Code.
        sb.append(
            CdaGeneratorUtils.getXmlForCDWithoutEndTag(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.DIAGNOSIS_SNOMED,
                CdaGeneratorConstants.SNOMED_CODESYSTEM_OID,
                CdaGeneratorConstants.SNOMED_CODESYSTEM_NAME,
                CdaGeneratorConstants.DIAGNOSIS_DISPLAY_NAME));
        sb.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.TRANSLATION_EL_NAME,
                CdaGeneratorConstants.DIAGNOSIS_LOINC,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.DIAGNOSIS_DISPLAY_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CODE_EL_NAME));

        sb.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        Pair<Date, TimeZone> onset = CdaFhirUtilities.getActualDate(c.getOnset());
        Pair<Date, TimeZone> abatement = CdaFhirUtilities.getActualDate(c.getAbatement());

        sb.append(
            CdaGeneratorUtils.getXmlForIVLWithTS(
                CdaGeneratorConstants.EFF_TIME_EL_NAME, onset, abatement, true));

        List<CodeableConcept> cds = new ArrayList<>();
        cds.add(c.getCode());

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

        sb.append(
            CdaProblemGenerator.addTriggerCodes(
                details, c.getCode(), onset, abatement, c.getIdElement().getIdPart()));

        // End Tag for Entry Relationship
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));

        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ACT_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));
      }
    }

    return sb.toString();
  }

  public static String generateEncounterReasonXml(LaunchDetails details, Encounter encounter) {

    StringBuilder sb = new StringBuilder();
    CodeableConcept primaryCd = null;

    if (encounter.hasReasonCode()) {

      List<CodeableConcept> codes = encounter.getReasonCode();
      List<Coding> codings = new ArrayList<>();

      if (codes != null && !codes.isEmpty()) {

        for (CodeableConcept cd : codes) {

          Coding code =
              CdaFhirUtilities.getCodingForCodeSystem(cd, CdaGeneratorConstants.FHIR_SNOMED_URL);
          Coding code1 =
              CdaFhirUtilities.getCodingForCodeSystem(cd, CdaGeneratorConstants.FHIR_ICD10_CM_URL);
          Coding code2 =
              CdaFhirUtilities.getCodingForCodeSystem(cd, CdaGeneratorConstants.FHIR_ICD9_CM_URL);

          if (code != null) {
            primaryCd = cd;
            codings.add(code);
            break;
          }

          if (code1 != null) {
            primaryCd = cd;
            codings.add(code1);
            break;
          }

          if (code2 != null) {
            primaryCd = cd;
            codings.add(code2);
            break;
          }
        } // for all codeable concepts
      } // if codeable concept exists

      if (!codings.isEmpty()) {
        sb.append(
            addDiagnosisActAndObsXml(
                details, codings, primaryCd, encounter.getIdElement().getIdPart()));
      }
    } // if reason code is present

    return sb.toString();
  }

  public static String addDiagnosisActAndObsXml(
      LaunchDetails details, List<Coding> codes, CodeableConcept cd, String id) {

    StringBuilder sb = new StringBuilder(200);

    sb.append(
        CdaGeneratorUtils.getXmlForEntryRelationship(CdaGeneratorConstants.ENTRY_REL_RSON_CODE));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.ACT_EL_NAME,
            CdaGeneratorConstants.ACT_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_TEMPLATE_ID,
            CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.ENC_DIAGNOSIS_ACT_CODE_DISPLAY_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    // Add problem observation
    sb.append(
        CdaGeneratorUtils.getXmlForEntryRelationship(CdaGeneratorConstants.ENTRY_REL_SUBJ_CODE));

    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROB_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PROB_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PROB_OBS_TEMPALTE_ID_EXT));
    sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorUtils.getGuid()));

    // Add Code.
    sb.append(
        CdaGeneratorUtils.getXmlForCDWithoutEndTag(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.DIAGNOSIS_SNOMED,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_OID,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_NAME,
            CdaGeneratorConstants.DIAGNOSIS_DISPLAY_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.TRANSLATION_EL_NAME,
            CdaGeneratorConstants.DIAGNOSIS_LOINC,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.DIAGNOSIS_DISPLAY_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CODE_EL_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    Pair<Date, TimeZone> onset = null;
    Pair<Date, TimeZone> abatement = null;

    sb.append(
        CdaGeneratorUtils.getXmlForIVLWithTS(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, onset, abatement, true));

    String codeXml =
        CdaFhirUtilities.getCodingXmlForValue(codes, CdaGeneratorConstants.VAL_EL_NAME, "");

    if (!codeXml.isEmpty()) sb.append(codeXml);

    sb.append(CdaProblemGenerator.addTriggerCodes(details, cd, onset, abatement, id));

    // End Tag for Entry Relationship
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));

    return sb.toString();
  }

  public static String generateEmptyEncounterSection() {
    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.ENC_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.ENC_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.ENC_SEC_TITLE));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Encounter Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static String getEncounterCodeXml(Encounter encounter, String contentRef) {
    String codeXml = "";

    if (encounter != null) {
      if (encounter.hasClass_()) {
        Coding encClass = encounter.getClass_();
        List<Coding> codes = new ArrayList<>();
        codes.add(encClass);
        codeXml =
            CdaFhirUtilities.getCodingXmlForCodeSystem(
                codes,
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_ENCOUNTER_CLASS_URL,
                false,
                contentRef);
      }

      if (!codeXml.isEmpty()) {
        logger.debug("Found Encounter class information, so use it ");
        return codeXml;
      } else {

        List<CodeableConcept> cds = encounter.getType();

        // Try to see if there is a CPT code first, if not try ICD-10, if not try SNOMED.
        codeXml =
            CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
                cds,
                CdaGeneratorConstants.CODE_EL_NAME,
                false,
                CdaGeneratorConstants.FHIR_CPT_URL,
                false,
                contentRef);

        if (!codeXml.isEmpty()) {
          logger.debug("Found Encounter Information using CPT, so using it ");
          return codeXml;
        } else {
          codeXml =
              CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
                  cds,
                  CdaGeneratorConstants.CODE_EL_NAME,
                  false,
                  CdaGeneratorConstants.FHIR_ICD10_CM_URL,
                  false,
                  contentRef);

          if (!codeXml.isEmpty()) {
            logger.debug("Found Encounter Information using ICD10, so using it ");
            return codeXml;
          } else {

            codeXml =
                CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
                    cds,
                    CdaGeneratorConstants.CODE_EL_NAME,
                    false,
                    CdaGeneratorConstants.FHIR_SNOMED_URL,
                    false,
                    contentRef);

            if (!codeXml.isEmpty()) {
              logger.debug("Found Encounter Information using SNOMED, so using it ");
              return codeXml;
            } else {
              logger.debug(
                  "Append Encounter Information using other code systems if found or append null");
              codeXml =
                  CdaFhirUtilities.getCodeableConceptXml(
                      cds, CdaGeneratorConstants.CODE_EL_NAME, false);
              return codeXml;
            }
          }
        }
      }
    }
    return codeXml;
  }

  public static Object generate31EncounterSection() {
    // TODO Auto-generated method stub
    return null;
  }
}
