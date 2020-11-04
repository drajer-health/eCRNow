package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.valueset.ConditionClinicalStatusCodesEnum;
import ca.uhn.fhir.model.primitive.DateTimeDt;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

public class Dstu2CdaProblemGenerator {

  private static final Logger logger = LoggerFactory.getLogger(Dstu2CdaProblemGenerator.class);

  public static String generateProblemSection(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    List<Condition> conds = data.getConditions();

    if (conds != null && conds.size() > 0) {

      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROB_SEC_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.PROB_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.PROB_SEC_TEMPLATE_ID_EXT));

      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.PROB_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.PROB_SEC_NAME));

      // Add Title
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.PROB_SEC_TITLE));

      // Add Narrative Text
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<String>();
      list.add(CdaGeneratorConstants.PROB_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.PROB_TABLE_COL_2_TITLE);
      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // Add Body Rows
      int rowNum = 1;
      for (Condition prob : conds) {
        String probDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (prob.getCode() != null
            && prob.getCode().getCodingFirstRep() != null
            && !StringUtils.isEmpty(prob.getCode().getCodingFirstRep().getDisplay())) {

          probDisplayName = prob.getCode().getCodingFirstRep().getDisplay();
        }

        Map<String, String> bodyvals = new HashMap<String, String>();
        bodyvals.put(CdaGeneratorConstants.PROB_TABLE_COL_1_BODY_CONTENT, probDisplayName);

        if (prob.getClinicalStatus() != null
            && (prob.getClinicalStatus()
                    .contentEquals(ConditionClinicalStatusCodesEnum.ACTIVE.getCode())
                || prob.getClinicalStatus()
                    .contentEquals(ConditionClinicalStatusCodesEnum.RELAPSE.getCode()))) {
          bodyvals.put(
              CdaGeneratorConstants.PROB_TABLE_COL_2_BODY_CONTENT,
              CdaGeneratorConstants.TABLE_ACTIVE_STATUS);
        } else {
          bodyvals.put(
              CdaGeneratorConstants.PROB_TABLE_COL_2_BODY_CONTENT,
              CdaGeneratorConstants.TABLE_RESOLVED_STATUS);
        }

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
        ++rowNum; // TODO: ++rowNum or rowNum++
      }

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      Boolean triggerCodesAdded = false;
      for (Condition pr : conds) {
        // Add the Entries.
        sb.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));

        // Add the problem Concern Act
        sb.append(
            CdaGeneratorUtils.getXmlForAct(
                CdaGeneratorConstants.ACT_EL_NAME,
                CdaGeneratorConstants.ACT_CLASS_CODE,
                CdaGeneratorConstants.MOOD_CODE_DEF));

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROB_CONCERN_TEMPLATE_ID));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.PROB_CONCERN_TEMPLATE_ID,
                CdaGeneratorConstants.PROB_CONCERN_TEMPLATE_ID_EXT));

        sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
        sb.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.PROB_CONC_ACT_CODE,
                CdaGeneratorConstants.HL7_ACT_CLASS_OID,
                CdaGeneratorConstants.HL7_ACT_CLASS_NAME,
                CdaGeneratorConstants.PROB_CONC_ACT_NAME));

        if (pr.getClinicalStatus() != null
            && (pr.getClinicalStatus()
                    .contentEquals(ConditionClinicalStatusCodesEnum.ACTIVE.getCode())
                || pr.getClinicalStatus()
                    .contentEquals(ConditionClinicalStatusCodesEnum.RELAPSE.getCode()))) {

          sb.append(
              CdaGeneratorUtils.getXmlForCD(
                  CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));
        } else {
          sb.append(
              CdaGeneratorUtils.getXmlForCD(
                  CdaGeneratorConstants.STATUS_CODE_EL_NAME,
                  CdaGeneratorConstants.COMPLETED_STATUS));
        }

        Date onset = null;
        Date abatement = null;
        if (pr.getOnset() != null && pr.getOnset() instanceof DateTimeDt) {

          DateTimeDt dt = (DateTimeDt) pr.getOnset();
          onset = dt.getValue();
        }

        if (pr.getAbatement() != null && pr.getAbatement() instanceof DateTimeDt) {

          DateTimeDt dt = (DateTimeDt) pr.getAbatement();
          abatement = dt.getValue();
        }

        sb.append(
            CdaGeneratorUtils.getXmlForIVLWithTS(
                CdaGeneratorConstants.EFF_TIME_EL_NAME, onset, abatement));

        // Add the Problem Observation
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

        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), pr.getId().getValue()));

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

        sb.append(
            CdaGeneratorUtils.getXmlForIVLWithTS(
                CdaGeneratorConstants.EFF_TIME_EL_NAME, onset, abatement));

        List<CodeableConceptDt> cds = new ArrayList<CodeableConceptDt>();
        cds.add(pr.getCode());
        sb.append(
            Dstu2CdaFhirUtilities.getCodeableConceptXml(
                cds, CdaGeneratorConstants.VAL_EL_NAME, true));

        // End Tag for Entry Relationship
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));

        if (!triggerCodesAdded) {
          sb.append(addTriggerCodes(data, details, pr, onset, abatement));
          triggerCodesAdded = true;
        }

        // End Tags for Entries
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ACT_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));
      }

      // Complete the section end tags.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {

      sb.append(generateEmptyProblemSection());
    }

    return sb.toString();
  }

  public static String addTriggerCodes(
      Dstu2FhirData data, LaunchDetails details, Condition cond, Date onset, Date abatement) {

    StringBuilder sb = new StringBuilder();

    logger.info(" Adding Trigger Code Reason for Problem Observation ");

    ObjectMapper mapper = new ObjectMapper();
    PatientExecutionState state = null;

    try {
      state = mapper.readValue(details.getStatus(), PatientExecutionState.class);
    } catch (JsonMappingException e1) {

      String msg = "Unable to read/write execution state";
      logger.error(msg);
      throw new RuntimeException(msg);

    } catch (JsonProcessingException e1) {
      String msg = "Unable to read/write execution state";
      logger.error(msg);

      throw new RuntimeException(msg);
    }

    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {

      // Add each code as an entry relationship observation

      if (mtc.hasMatchedTriggerCodes("Condition")) {

        // Add the Problem Observation
        sb.append(
            CdaGeneratorUtils.getXmlForEntryRelationship(
                CdaGeneratorConstants.ENTRY_REL_RSON_CODE));
        sb.append(
            CdaGeneratorUtils.getXmlForActWithNegationInd(
                CdaGeneratorConstants.OBS_ACT_EL_NAME,
                CdaGeneratorConstants.OBS_CLASS_CODE,
                CdaGeneratorConstants.MOOD_CODE_DEF,
                "false",
                true));

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROB_OBS_TEMPLATE_ID));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.PROB_OBS_TEMPLATE_ID,
                CdaGeneratorConstants.PROB_OBS_TEMPALTE_ID_EXT));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.TRIGGER_CODE_PROB_OBS_TEMPLATE_ID,
                CdaGeneratorConstants.TRIGGER_CODE_PROB_OBS_TEMPLATE_ID_EXT));

        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), cond.getId().getIdPart()));

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

        sb.append(
            CdaGeneratorUtils.getXmlForIVLWithTS(
                CdaGeneratorConstants.EFF_TIME_EL_NAME, onset, abatement));

        Set<String> matchedCodes = mtc.getMatchedCodes();

        if (matchedCodes != null && matchedCodes.size() > 0) {

          // Split the system and code.
          matchedCodes
              .stream()
              .filter(Objects::nonNull)
              .findFirst()
              .ifPresent(
                  matchCode -> {
                    logger.info(" Starting to add trigger code that was matched " + matchCode);

                    String[] parts = matchCode.split("\\|");

                    Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(parts[0]);

                    // Add the right value set and version number.
                    String vs = "2.16.840.1.114222.4.11.7508";
                    String vsVersion = "19/05/2016";
                    sb.append(
                        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
                            parts[1], csd.getValue0(), csd.getValue1(), vs, vsVersion, ""));

                    // Adding one is sufficient, wait for feedback from connectathon.
                  });
        }

        // End Tag for Entry Relationship
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_REL_EL_NAME));
      } else {

        logger.info(" Not adding matched Trigger codes as they are not present ");
      }
    }

    return sb.toString();
  }

  public static String generateEmptyProblemSection() {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PROB_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PROB_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.PROB_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.PROB_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.PROB_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.PROB_SEC_TITLE));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Problem Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }
}
