package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.composite.CodingDt;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaResultGenerator {

  private static final Logger logger = LoggerFactory.getLogger(Dstu2CdaResultGenerator.class);

  public static String generateResultsSection(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder hsb = new StringBuilder(5000);
    StringBuilder sb = new StringBuilder(2000);
    StringBuilder resultEntries = new StringBuilder();

    List<Observation> results = data.getLabResults();

    if (results != null && !results.isEmpty()) {

      logger.info(" Number of Lab Result Observations to process {}", results.size());

      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      hsb.append(
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID));
      hsb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID_EXT));

      hsb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.LAB_RESULTS_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.LAB_RESULTS_SEC_NAME));

      // Add Title
      hsb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.LAB_RESULTS_SEC_TITLE));

      // Add Narrative Text
      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.LABTEST_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.LABTEST_TABLE_COL_2_TITLE);
      list.add(CdaGeneratorConstants.LABTEST_TABLE_COL_3_TITLE);
      hsb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int rowNum = 1;

      for (Observation obs : results) {

        String obsDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;
        List<CodingDt> cds = null;
        if (obs.getCode() != null && obs.getCode().getCodingFirstRep() != null) {

          cds = obs.getCode().getCoding();

          if (!StringUtils.isEmpty(obs.getCode().getCodingFirstRep().getDisplay())) {
            obsDisplayName = obs.getCode().getCodingFirstRep().getDisplay();
          } else if (!StringUtils.isEmpty(obs.getCode().getText())) {
            obsDisplayName = obs.getCode().getText();
          }
        } else if (obs.getCode() != null && obs.getCode().getText() != null) {
          obsDisplayName = obs.getCode().getText();
        }

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT, obsDisplayName);

        String val = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (obs.getValue() != null) {

          val = Dstu2CdaFhirUtilities.getStringForIDataType(obs.getValue());
        }

        bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_2_BODY_CONTENT, val);

        String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (obs.getEffective() != null) {

          dt = Dstu2CdaFhirUtilities.getStringForIDataType(obs.getEffective());
        }
        bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_3_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        // Setup the Organizer and Entries
        StringBuilder lrEntry = new StringBuilder();

        // Add the Entries.
        lrEntry.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));

        // Add the Organizer Act
        lrEntry.append(
            CdaGeneratorUtils.getXmlForAct(
                CdaGeneratorConstants.ORGANIZER_EL_NAME,
                CdaGeneratorConstants.ORGANIZER_CLASS_CODE_CLUSTER,
                CdaGeneratorConstants.MOOD_CODE_DEF));

        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID));
        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID,
                CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID_EXT));

        lrEntry.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

        // Fix the Code to be the same as the result code..
        lrEntry.append(Dstu2CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME));

        lrEntry.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        // Add the actual Result Observation
        lrEntry.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
        lrEntry.append(
            CdaGeneratorUtils.getXmlForAct(
                CdaGeneratorConstants.OBS_ACT_EL_NAME,
                CdaGeneratorConstants.OBS_CLASS_CODE,
                CdaGeneratorConstants.MOOD_CODE_DEF));

        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID));
        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID,
                CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID_EXT));

        lrEntry.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

        lrEntry.append(Dstu2CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME));

        lrEntry.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        lrEntry.append(
            Dstu2CdaFhirUtilities.getIDataTypeXml(
                obs.getEffective(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

        lrEntry.append(
            Dstu2CdaFhirUtilities.getIDataTypeXml(
                obs.getValue(), CdaGeneratorConstants.VAL_EL_NAME, true));

        // Add interpretation code.
        if ((obs.getInterpretation() != null)
            && (obs.getInterpretation().getCoding() != null)
            && (!obs.getInterpretation().getCoding().isEmpty())) {

          logger.info(" Adding Interpretation Code ");
          List<CodeableConceptDt> cdt = new ArrayList<>();
          cdt.add(obs.getInterpretation());
          lrEntry.append(
              Dstu2CdaFhirUtilities.getCodeableConceptXml(
                  cdt, CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME, false));
        }

        // End Tag for Entry Relationship
        lrEntry.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
        lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

        lrEntry.append(addTriggerCodes(details, obs));

        // End Tags for Entries
        lrEntry.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
        lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

        resultEntries.append(lrEntry);
        rowNum++;
      }

      // End the Sb string.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      hsb.append(sb);

      // Add lab results
      hsb.append(resultEntries);

      // Complete the section end tags.
      hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {
      hsb.append(generateEmptyLabResults());
    }

    return hsb.toString();
  }

  public static String addTriggerCodes(LaunchDetails details, Observation obs) {

    StringBuilder lrEntry = new StringBuilder();

    logger.info(" Adding Trigger Code Reason for Result Observation ");

    ObjectMapper mapper = new ObjectMapper();
    PatientExecutionState state = null;

    try {
      state = mapper.readValue(details.getStatus(), PatientExecutionState.class);
    } catch (JsonMappingException e1) {

      String msg = "Unable to read/write execution State";
      logger.error(msg);
      throw new RuntimeException(msg);

    } catch (JsonProcessingException e1) {
      String msg = "Unable to read/write execution state.";
      logger.error(msg);

      throw new RuntimeException(msg);
    }

    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {

      // Add each code as an entry relationship observation
      StringBuilder codeXml = new StringBuilder(200);
      StringBuilder valXml = new StringBuilder(200);

      if (Boolean.TRUE.equals(mtc.hasMatchedTriggerCodes("Observation"))) {

        // Check if match was found and then include the rest.
        Boolean matchFound = doesTriggerCodesMatchObservation(obs, mtc, codeXml, valXml);

        logger.info(" code Xml {}", codeXml);
        logger.info(" value Xml {}", valXml);

        if (Boolean.TRUE.equals(matchFound)) {

          lrEntry.append(addEntry(obs, details, codeXml.toString(), valXml.toString()));
          break;
        }
      }
    }

    return lrEntry.toString();
  }

  public static String addEntry(
      Observation obs, LaunchDetails details, String codeXml, String valXml) {

    StringBuilder lrEntry = new StringBuilder();

    // Add the actual Result Observation
    lrEntry.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    lrEntry.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    lrEntry.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID));
    lrEntry.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID,
            CdaGeneratorConstants.LAB_RESULTS_ENTRY_TEMPLATE_ID_EXT));
    lrEntry.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.TRIGGER_CODE_LAB_RESULT_TEMPLATE_ID,
            CdaGeneratorConstants.TRIGGER_CODE_LAB_RESULT_TEMPLATE_ID_EXT));

    lrEntry.append(
        CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), obs.getId().getIdPart()));

    if (!StringUtils.isEmpty(codeXml)) {
      lrEntry.append(codeXml);
    } else {

      lrEntry.append(Dstu2CdaFhirUtilities.getCodingXml(null, CdaGeneratorConstants.CODE_EL_NAME));
    }

    lrEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    lrEntry.append(
        Dstu2CdaFhirUtilities.getIDataTypeXml(
            obs.getEffective(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    if (!StringUtils.isEmpty(valXml)) {
      lrEntry.append(valXml);
    } else {
      lrEntry.append(
          Dstu2CdaFhirUtilities.getIDataTypeXml(
              obs.getValue(), CdaGeneratorConstants.VAL_EL_NAME, true));
    }

    // Add interpretation code.
    if ((obs.getInterpretation() != null)
        && (obs.getInterpretation().getCoding() != null)
        && (!obs.getInterpretation().getCoding().isEmpty())) {

      logger.info(" Adding Interpretaion Code ");
      List<CodeableConceptDt> cdt = new ArrayList<>();
      cdt.add(obs.getInterpretation());
      lrEntry.append(
          Dstu2CdaFhirUtilities.getCodeableConceptXml(
              cdt, CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME, false));
    }

    // End Tag for Entry Relationship
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return lrEntry.toString();
  }

  public static Boolean doesTriggerCodesMatchObservation(
      Observation obs, MatchedTriggerCodes mtc, StringBuilder codeXml, StringBuilder valXml) {

    Boolean matchFound = false;

    Set<String> matchedCodes = mtc.getMatchedCodes();

    List<CodingDt> codeElements = null;
    List<CodingDt> valElements = null;

    if (obs.getCode() != null
        && obs.getCode().getCoding() != null
        && !obs.getCode().getCoding().isEmpty()) {
      codeElements = obs.getCode().getCoding();
    }

    if (obs.getValue() instanceof CodeableConceptDt) {
      CodeableConceptDt valDt = (CodeableConceptDt) obs.getValue();

      if (valDt.getCoding() != null && !valDt.getCoding().isEmpty()) {
        valElements = valDt.getCoding();
      }
    }

    if (matchedCodes != null && !matchedCodes.isEmpty()) {

      logger.info(" Size of the Matched Codes {}", matchedCodes.size());

      for (String s : matchedCodes) {

        String[] parts = s.split("\\|");

        if (codeElements != null
            && Dstu2CdaFhirUtilities.isCodePresentInCoding(parts[1], codeElements)) {

          logger.info(" Code is present in the Codings {}", parts[1]);
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(parts[0]);

          // For Connectathon, until we get the right test data finish testing.
          codeXml.append(
              CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                  CdaGeneratorConstants.CODE_EL_NAME,
                  parts[1],
                  csd.getValue0(),
                  csd.getValue1(),
                  CdaGeneratorConstants.RCTC_OID,
                  ActionRepo.getInstance().getRctcVersion(),
                  "",
                  ""));
          matchFound = true;

        } else if (codeElements != null) {

          logger.info(" Setting code xml to just the value of the codings ");
          codeXml.append(
              Dstu2CdaFhirUtilities.getCodingXml(codeElements, CdaGeneratorConstants.CODE_EL_NAME));
        }

        // Check for values
        if (valElements != null
            && Dstu2CdaFhirUtilities.isCodePresentInCoding(parts[1], valElements)) {

          logger.info(" Matched Value Element for triggering {}", parts[1]);
          Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(parts[0]);

          // For Connectathon, until we get the right test data finish testing.
          valXml.append(
              CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
                  parts[1],
                  csd.getValue0(),
                  csd.getValue1(),
                  CdaGeneratorConstants.RCTC_OID,
                  ActionRepo.getInstance().getRctcVersion(),
                  ""));
          matchFound = true;

        } else if (valElements != null) {

          logger.info(" No Matched Value Element so just encoding value element ");
          valXml.append(
              Dstu2CdaFhirUtilities.getCodingXmlForValue(
                  valElements, CdaGeneratorConstants.VAL_EL_NAME));
        }

        if (Boolean.TRUE.equals(matchFound)) break;
      } // for
    }

    return matchFound;
  }

  public static String generateEmptyLabResults() {
    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.LAB_RESULTS_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.LAB_RESULTS_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.LAB_RESULTS_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.LAB_RESULTS_SEC_TITLE));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Lab Results Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }
}
