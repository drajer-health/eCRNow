package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Type;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaResultGenerator {

  public static final String OBSERVATION = "Observation";

  private CdaResultGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaResultGenerator.class);

  public static String generateResultsSection(R4FhirData data, LaunchDetails details) {

    StringBuilder hsb = new StringBuilder(5000);
    StringBuilder sb = new StringBuilder(2000);
    StringBuilder resultEntries = new StringBuilder();

    List<Observation> results = getValidLabResults(data);

    if (results != null && !results.isEmpty()) {

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
        List<Coding> cds = null;
        if (obs.getCode() != null && obs.getCode().getCodingFirstRep() != null) {

          cds = obs.getCode().getCoding();

          if (!StringUtils.isEmpty(obs.getCode().getCodingFirstRep().getDisplay())) {
            obsDisplayName = obs.getCode().getCodingFirstRep().getDisplay();
          } else if (!StringUtils.isEmpty(obs.getCode().getText())) {
            obsDisplayName = obs.getCode().getText();
          }
        }

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT, obsDisplayName);

        String val = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (obs.getValue() != null) {

          val = CdaFhirUtilities.getStringForType(obs.getValue());
        }

        bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_2_BODY_CONTENT, val);

        String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (obs.getEffective() != null) {

          dt = CdaFhirUtilities.getStringForType(obs.getEffective());
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
        logger.info("Find the Loinc Code as a priority first for Lab Results");
        String codeXml =
            CdaFhirUtilities.getCodingXmlForCodeSystem(
                cds,
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_LOINC_URL,
                false);

        logger.debug(" Code Xml = {}", codeXml);
        if (!codeXml.isEmpty()) {
          lrEntry.append(codeXml);
        } else {
          lrEntry.append(CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME));
        }

        lrEntry.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        lrEntry.append(getXmlForObservation(details, obs));

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

  public static String getXmlForObservation(LaunchDetails details, Observation obs) {

    StringBuilder lrEntry = new StringBuilder(2000);
    Boolean foundComponent = false;

    if (obs.getComponent() != null && !obs.getComponent().isEmpty()) {

      CodeableConcept cc = obs.getCode();
      Type val = obs.getValue();
      List<CodeableConcept> interpretation = obs.getInterpretation();
      StringBuilder id = new StringBuilder(200);
      id.append(obs.getId());
      int rowNum = 1;

      for (ObservationComponentComponent oc : obs.getComponent()) {

        logger.info(" Found Observation Components ");
        if (oc.getCode() != null) {
          cc = oc.getCode();
        }

        if (oc.getValue() != null) {
          val = oc.getValue();
        }

        if (oc.getInterpretation() != null) {
          interpretation = oc.getInterpretation();
        }

        id.append("-");
        id.append(Integer.toBinaryString(rowNum));

        String compString =
            getXmlForObservationComponent(
                details, cc, val, id.toString(), obs.getEffective(), interpretation);

        if (!compString.isEmpty() && !foundComponent) foundComponent = true;

        lrEntry.append(compString);

        rowNum++;
      }
    }

    if (!foundComponent) {

      logger.info(" No component found , so directly adding the observation code ");
      lrEntry.append(
          getXmlForObservationComponent(
              details,
              obs.getCode(),
              obs.getValue(),
              obs.getId(),
              obs.getEffective(),
              obs.getInterpretation()));
    }

    logger.debug(" Lr Entry = {}", lrEntry);

    return lrEntry.toString();
  }

  public static String getXmlForObservationComponent(
      LaunchDetails details,
      CodeableConcept cd,
      Type val,
      String id,
      Type effective,
      List<CodeableConcept> interpretation) {

    StringBuilder lrEntry = new StringBuilder(2000);

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

    List<String> matchedTriggerCodes =
        CdaFhirUtilities.getMatchedCodesForResourceAndUrl(
            details, OBSERVATION, CdaGeneratorConstants.FHIR_LOINC_URL);

    List<String> matchedTriggerValues =
        CdaFhirUtilities.getMatchedValuesForResourceAndUrl(
            details, OBSERVATION, CdaGeneratorConstants.FHIR_LOINC_URL);

    String obsCodeXml = "";
    String obsValueXml = "";
    // Add Trigger code template if the code matched the Url in the Service Request.
    if (matchedTriggerCodes != null && !matchedTriggerCodes.isEmpty()) {
      logger.info(" Found a Matched Code that is for Observation.code ");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromCodeableConceptForCodeSystem(
              matchedTriggerCodes, cd, CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty()) {

        logger.info(" Found exact code that matches the trigger codes ");
        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE,
                CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE_EXT));

        obsCodeXml =
            CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                mCd,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.RCTC_OID,
                ActionRepo.getInstance().getRctcVersion(),
                cd,
                CdaGeneratorConstants.FHIR_LOINC_URL);
      } else {
        logger.error(
            " Did not find the code value in the matched codes, make it a regular Observation ");
      }
    } else if (matchedTriggerValues != null && !matchedTriggerValues.isEmpty()) {
      logger.info(" Found a Matched Code that is for Observation.value ");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromTypeForCodeSystem(
              matchedTriggerValues, val, CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty() && val instanceof CodeableConcept) {

        logger.info(" Found exact Value Codeable concept that matches the trigger codes ");
        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE,
                CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE_EXT));

        CodeableConcept cc = (CodeableConcept) val;

        obsValueXml =
            CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
                CdaGeneratorConstants.VAL_EL_NAME,
                mCd,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.RCTC_OID,
                ActionRepo.getInstance().getRctcVersion(),
                cc,
                CdaGeneratorConstants.FHIR_LOINC_URL);
      } else {
        logger.error(
            " Did not find the code value in the matched values, make it a regular Observation ");
      }
    }

    lrEntry.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id));

    logger.info("Find the Loinc Code as priority for Lab Results");
    List<Coding> cds = null;
    if (cd != null && cd.getCodingFirstRep() != null) {
      cds = cd.getCoding();
    }

    if (obsCodeXml.isEmpty()) {

      logger.info(" Did not find the trigger code matches, adding XML based on code system");
      obsCodeXml =
          CdaFhirUtilities.getCodingXmlForCodeSystem(
              cds, CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.FHIR_LOINC_URL, false);

      if (!obsCodeXml.isEmpty()) {
        logger.info(" Did not find the trigger code matches, adding XML based on code system ");
        lrEntry.append(obsCodeXml);
      } else {
        logger.info(" Did not find the trigger code matches, creating default xml ");
        lrEntry.append(CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME));
      }

    } else {
      lrEntry.append(obsCodeXml);
    }

    lrEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    lrEntry.append(
        CdaFhirUtilities.getXmlForType(effective, CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    if (obsValueXml.isEmpty()) {
      lrEntry.append(CdaFhirUtilities.getXmlForType(val, CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      lrEntry.append(obsValueXml);
    }

    // Add interpretation code.
    if (interpretation != null) {

      logger.info(" Adding Interpretaion Code ");

      lrEntry.append(
          CdaFhirUtilities.getCodeableConceptXml(
              interpretation, CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME, false));
    }

    // End Tag for Entry Relationship
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return lrEntry.toString();
  }

  public static String addTriggerCodes(LaunchDetails details, Observation obs, List<Coding> cds) {

    StringBuilder lrEntry = new StringBuilder();

    logger.info(" Adding Trigger Code Reason for Result Observation ");

    PatientExecutionState state = null;

    state = ApplicationUtils.getDetailStatus(details);

    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {

      // Add each code as an entry relationship observation

      if (mtc.hasMatchedTriggerCodes(OBSERVATION)) {

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
        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.TRIGGER_CODE_LAB_RESULT_TEMPLATE_ID,
                CdaGeneratorConstants.TRIGGER_CODE_LAB_RESULT_TEMPLATE_ID_EXT));

        lrEntry.append(
            CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), obs.getId()));

        String codeXml =
            CdaFhirUtilities.getCodingXmlForCodeSystem(
                cds,
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_LOINC_URL,
                true);

        if (!codeXml.isEmpty()) {
          lrEntry.append(codeXml);
        } else {
          lrEntry.append(CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME));
        }

        lrEntry.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        lrEntry.append(
            CdaFhirUtilities.getXmlForType(
                obs.getEffective(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

        Set<String> matchedCodes = mtc.getMatchedCodes();

        if (matchedCodes != null && !matchedCodes.isEmpty()) {

          // Split the system and code.
          matchedCodes
              .stream()
              .filter(Objects::nonNull)
              .findFirst()
              .ifPresent(
                  matchCode -> {
                    String[] parts = matchCode.split("\\|");

                    Pair<String, String> csd = CdaGeneratorConstants.getCodeSystemFromUrl(parts[0]);

                    // For Connectathon, until we get the right test data finish testing.
                    String vs = CdaGeneratorConstants.RCTC_OID;
                    String vsVersion = ActionRepo.getInstance().getRctcVersion();
                    lrEntry.append(
                        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
                            parts[1], csd.getValue0(), csd.getValue1(), vs, vsVersion, ""));

                    // Adding one is sufficient and only one is possible according to Schema.
                  });
        }

        // End Tag for Entry Relationship
        lrEntry.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
        lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

      } else {

        logger.info(" Not adding matched Trigger codes as they are not present for Lab Result ");
      }
    }

    return lrEntry.toString();
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

  public static List<Observation> getValidLabResults(R4FhirData data) {

    List<Observation> sr = new ArrayList<>();

    if (data.getLabResults() != null && !data.getLabResults().isEmpty()) {

      logger.info(
          " Total num of Service Requests available for Patient {}",
          data.getServiceRequests().size());

      for (Observation s : data.getLabResults()) {

        if (s.getCode() != null
            && s.getCode().getCoding() != null
            && !s.getCode().getCoding().isEmpty()
            && CdaFhirUtilities.isCodingPresentForCodeSystem(
                s.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL)) {

          logger.info(" Found a Service Request with a LOINC code ");
          sr.add(s);
        }
      }
    } else {
      logger.info(" No Valid Lab Results in the bundle to process ");
    }

    return sr;
  }
}
