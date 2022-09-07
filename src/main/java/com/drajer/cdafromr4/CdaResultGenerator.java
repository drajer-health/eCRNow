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
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Reference;
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

    List<Observation> allResults = data.getLabResults();
    List<Observation> results = getValidLabResults(data);
    List<DiagnosticReport> reports = getValidDiagnosticReports(data);

    if ((results != null && !results.isEmpty()) || (reports != null && !reports.isEmpty())) {

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
        logger.debug("Find the Loinc Code as a priority first for Lab Results");
        String codeXml =
            CdaFhirUtilities.getCodingXmlForCodeSystem(
                cds,
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_LOINC_URL,
                false,
                "");

        logger.debug("Code Xml = {}", codeXml);
        if (!codeXml.isEmpty()) {
          lrEntry.append(codeXml);
        } else {
          lrEntry.append(
              CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME, ""));
        }

        lrEntry.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        lrEntry.append(
            getXmlForObservation(
                details, obs, CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT, rowNum));

        // End Tags for Entries
        lrEntry.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
        lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

        resultEntries.append(lrEntry);
        rowNum++;
      }

      if (reports != null && reports.size() > 0) {
        processDiagnosticResults(reports, allResults, details, rowNum, sb, resultEntries);
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

  public static void processDiagnosticResults(
      List<DiagnosticReport> reports,
      List<Observation> allResults,
      LaunchDetails details,
      int rowNum,
      StringBuilder sb,
      StringBuilder resultEntries) {

    for (DiagnosticReport rep : reports) {

      StringBuilder displayAttr = new StringBuilder(200);
      String repDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;
      List<Coding> cds = null;
      if (rep.getCode() != null && rep.getCode().getCodingFirstRep() != null) {

        cds = rep.getCode().getCoding();

        if (!StringUtils.isEmpty(rep.getCode().getCodingFirstRep().getDisplay())) {
          repDisplayName = rep.getCode().getCodingFirstRep().getDisplay();
        } else if (!StringUtils.isEmpty(rep.getCode().getText())) {
          repDisplayName = rep.getCode().getText();
        }
      }

      Map<String, String> bodyvals = new LinkedHashMap<>();
      bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT, repDisplayName);

      String val = CdaGeneratorConstants.UNKNOWN_VALUE;

      if (rep.getResult() != null && rep.getResultFirstRep() != null) {

        val = CdaFhirUtilities.getStringForType(rep.getResultFirstRep());
      }

      bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_2_BODY_CONTENT, val);

      String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
      if (rep.getEffective() != null) {

        dt = CdaFhirUtilities.getStringForType(rep.getEffective());
      }
      bodyvals.put(CdaGeneratorConstants.LABTEST_TABLE_COL_3_BODY_CONTENT, dt);

      displayAttr.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

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
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID));
      lrEntry.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID,
              CdaGeneratorConstants.LAB_RESULTS_ORG_TEMPLATE_ID_EXT));

      lrEntry.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

      // Fix the Code to be the same as the result code..
      logger.debug("Find the Loinc Code as a priority first for Lab Results");
      String codeXml =
          CdaFhirUtilities.getCodingXmlForCodeSystem(
              cds,
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.FHIR_LOINC_URL,
              false,
              "");

      logger.debug("Code Xml = {}", codeXml);
      if (!codeXml.isEmpty()) {
        lrEntry.append(codeXml);
      } else {
        lrEntry.append(CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME, ""));
      }

      lrEntry.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

      String compXml =
          getXmlForComponents(
              rep,
              allResults,
              details,
              CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT,
              rowNum);

      boolean compFound = false;
      if (compXml != null && !compXml.isEmpty()) {
        lrEntry.append(compXml);
        compFound = true;
      }

      // End Tags for Entries
      lrEntry.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
      lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

      if (compFound) {
        sb.append(displayAttr.toString());
        resultEntries.append(lrEntry);
      }
      rowNum++;
    }
  }

  public static Observation findObservation(Reference r, List<Observation> allObs) {

    for (Observation obs : allObs) {

      logger.info(
          " Comparing Observation with id {} with reference id {}",
          obs.getIdElement().getIdPart(),
          r.getReferenceElement().getIdPart());
      if (obs.getIdElement().getIdPart().contentEquals(r.getReferenceElement().getIdPart())) {

        return obs;
      }
    }

    return null;
  }

  public static String getXmlForComponents(
      DiagnosticReport rep,
      List<Observation> allObs,
      LaunchDetails details,
      String contentId,
      int row) {

    logger.info(" Adding References to observations ");
    StringBuilder lrEntry = new StringBuilder(2000);
    String contentRef = contentId + Integer.toString(row);

    Boolean foundComponent = false;

    List<Reference> refs = rep.getResult();

    for (Reference r : refs) {

      // Create an Observation for each entry.
      Observation obs = findObservation(r, allObs);

      if (obs != null && obs.getComponent() != null && !obs.getComponent().isEmpty()) {

        CodeableConcept cc = obs.getCode();
        Type val = obs.getValue();
        List<CodeableConcept> interpretation = obs.getInterpretation();
        StringBuilder id = new StringBuilder(200);
        id.append(obs.getId());
        int rowNum = 1;

        for (ObservationComponentComponent oc : obs.getComponent()) {

          logger.debug("Found Observation Components ");
          if (oc.getCode() != null) {
            cc = oc.getCode();
          } else {
            // use diagnostic report code instead
            cc = rep.getCode();
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
                  details, cc, val, id.toString(), obs.getEffective(), interpretation, contentRef);

          if (!compString.isEmpty() && Boolean.FALSE.equals(foundComponent)) foundComponent = true;

          lrEntry.append(compString);

          rowNum++;
        }
      }

      if (obs != null && Boolean.FALSE.equals(foundComponent)) {

        logger.debug("No component found , so directly adding the observation code ");
        lrEntry.append(
            getXmlForObservationComponent(
                details,
                (obs.getCode() != null ? obs.getCode() : rep.getCode()),
                obs.getValue(),
                obs.getId(),
                obs.getEffective(),
                obs.getInterpretation(),
                contentRef));
      }
    }

    logger.debug("Lr Entry = {}", lrEntry);

    return lrEntry.toString();
  }

  public static String getXmlForObservation(
      LaunchDetails details, Observation obs, String contentId, int row) {

    StringBuilder lrEntry = new StringBuilder(2000);
    String contentRef = contentId + Integer.toString(row);

    Boolean foundComponent = false;

    if (obs.getComponent() != null && !obs.getComponent().isEmpty()) {

      CodeableConcept cc = obs.getCode();
      Type val = obs.getValue();
      List<CodeableConcept> interpretation = obs.getInterpretation();
      StringBuilder id = new StringBuilder(200);
      id.append(obs.getId());
      int rowNum = 1;

      for (ObservationComponentComponent oc : obs.getComponent()) {

        logger.debug("Found Observation Components ");
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
                details, cc, val, id.toString(), obs.getEffective(), interpretation, contentRef);

        if (!compString.isEmpty() && Boolean.FALSE.equals(foundComponent)) foundComponent = true;

        lrEntry.append(compString);

        rowNum++;
      }
    }

    if (Boolean.FALSE.equals(foundComponent)) {

      logger.debug("No component found , so directly adding the observation code ");
      lrEntry.append(
          getXmlForObservationComponent(
              details,
              obs.getCode(),
              obs.getValue(),
              obs.getId(),
              obs.getEffective(),
              obs.getInterpretation(),
              contentRef));
    }

    logger.debug("Lr Entry = {}", lrEntry);

    return lrEntry.toString();
  }

  public static String getXmlForObservationComponent(
      LaunchDetails details,
      CodeableConcept cd,
      Type val,
      String id,
      Type effective,
      List<CodeableConcept> interpretation,
      String contentRef) {

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

    Pair<Boolean, String> obsCodeXml =
        getObservationCodeXml(details, cd, false, contentRef, "Observation.code");

    Pair<Boolean, String> obsValueXml = null;

    if (val instanceof CodeableConcept) {
      CodeableConcept value = (CodeableConcept) val;
      obsValueXml = getObservationCodeXml(details, value, true, contentRef, "Observation.value");
    }
    /*
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
      logger.debug("Found a Matched Code that is for Observation.code ");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromCodeableConceptForCodeSystem(
              matchedTriggerCodes, cd, CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty()) {

        logger.debug("Found exact code that matches the trigger codes");
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
                CdaGeneratorConstants.FHIR_LOINC_URL,
                contentRef);
      } else {
        logger.debug(
            "Did not find the code value in the matched codes, make it a regular Observation");
      }
    } else if (matchedTriggerValues != null && !matchedTriggerValues.isEmpty()) {
      logger.debug("Found a Matched Code that is for Observation.value");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromTypeForCodeSystem(
              matchedTriggerValues, val, CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty() && val instanceof CodeableConcept) {

        logger.debug("Found exact Value Codeable concept that matches the trigger codes");
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
                CdaGeneratorConstants.FHIR_LOINC_URL,
                contentRef);
      } else {
        logger.debug(
            "Did not find the code value in the matched values, make it a regular Observation");
      }
    }
    */
    if ((obsCodeXml != null && obsCodeXml.getValue0())
        || (obsValueXml != null && obsValueXml.getValue0())) {

      lrEntry.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE,
              CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE_EXT));
    }

    lrEntry.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id));

    lrEntry.append(obsCodeXml.getValue1());

    lrEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    lrEntry.append(
        CdaFhirUtilities.getXmlForType(effective, CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    if (obsValueXml == null) {
      lrEntry.append(CdaFhirUtilities.getXmlForType(val, CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      lrEntry.append(obsValueXml.getValue1());
    }

    // Add interpretation code.
    if (interpretation != null) {

      logger.debug("Adding Interpretation Code");

      String interpretXml =
          CdaFhirUtilities.getCodeableConceptXmlForMappedConceptDomain(
              CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
              interpretation,
              CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
              false,
              false);

      if (interpretXml != null && !interpretXml.isEmpty()) lrEntry.append(interpretXml);
    }

    // End Tag for Entry Relationship
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return lrEntry.toString();
  }

  public static Pair<Boolean, String> getObservationCodeXml(
      LaunchDetails details,
      CodeableConcept code,
      Boolean valElement,
      String contentRef,
      String path) {

    Pair<Boolean, String> retVal = null;
    PatientExecutionState state = ApplicationUtils.getDetailStatus(details);

    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {

      // if CodeableConcept present in MTC
      Pair<String, String> matchedCode = mtc.getMatchingCode(code, path);

      if (matchedCode != null) {

        if (!valElement) {
          Pair<String, String> systemName =
              CdaGeneratorConstants.getCodeSystemFromUrl(matchedCode.getValue1());
          String codeXml =
              CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
                  CdaGeneratorConstants.CODE_EL_NAME,
                  matchedCode.getValue0(),
                  systemName.getValue0(),
                  systemName.getValue1(),
                  CdaGeneratorConstants.RCTC_OID,
                  ActionRepo.getInstance().getRctcVersion(),
                  code,
                  CdaGeneratorConstants.FHIR_LOINC_URL,
                  contentRef,
                  valElement);

          retVal = new Pair<Boolean, String>(true, codeXml);
        } else {
          Pair<String, String> systemName =
              CdaGeneratorConstants.getCodeSystemFromUrl(matchedCode.getValue1());
          String valueXml =
              CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
                  CdaGeneratorConstants.VAL_EL_NAME,
                  matchedCode.getValue0(),
                  systemName.getValue0(),
                  systemName.getValue1(),
                  CdaGeneratorConstants.RCTC_OID,
                  ActionRepo.getInstance().getRctcVersion(),
                  code,
                  CdaGeneratorConstants.FHIR_SNOMED_URL,
                  contentRef,
                  valElement);

          retVal = new Pair<Boolean, String>(true, valueXml);
        }
      } else if (code.getCoding() != null) {

        String defCodeXml = "";
        if (!valElement) {
          defCodeXml =
              CdaFhirUtilities.getCodingXml(
                  code.getCoding(), CdaGeneratorConstants.CODE_EL_NAME, contentRef);
        } else {
          defCodeXml =
              CdaFhirUtilities.getCodingXmlForValue(
                  code.getCoding(), CdaGeneratorConstants.VAL_EL_NAME, contentRef);
        }
        retVal = new Pair<Boolean, String>(false, defCodeXml);
      }
    }

    return retVal;
  }

  public static String addTriggerCodes(LaunchDetails details, Observation obs, List<Coding> cds) {

    StringBuilder lrEntry = new StringBuilder();

    logger.debug("Adding Trigger Code Reason for Result Observation");

    PatientExecutionState state = null;

    state = ApplicationUtils.getDetailStatus(details);

    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {

      // Add each code as an entry relationship observation

      if (Boolean.TRUE.equals(mtc.hasMatchedTriggerCodes(OBSERVATION))) {

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
                true,
                "");

        if (!codeXml.isEmpty()) {
          lrEntry.append(codeXml);
        } else {
          lrEntry.append(
              CdaFhirUtilities.getCodingXml(cds, CdaGeneratorConstants.CODE_EL_NAME, ""));
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

        logger.debug("Not adding matched Trigger codes as they are not present for Lab Result");
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

      logger.debug(
          "Total num of Lab Results available for Patient {}", data.getLabResults().size());

      for (Observation s : data.getLabResults()) {

        if (s.getCode() != null
            && s.getCode().getCoding() != null
            && !s.getCode().getCoding().isEmpty()
            && Boolean.TRUE.equals(
                CdaFhirUtilities.isCodingPresentForCodeSystem(
                    s.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL))) {

          logger.debug("Found a Lab Results with a LOINC code");
          sr.add(s);
        }
      }
    } else {
      logger.debug("No Valid Lab Results in the bundle to process");
    }

    return sr;
  }

  public static List<DiagnosticReport> getValidDiagnosticReports(R4FhirData data) {

    List<DiagnosticReport> drs = new ArrayList<>();

    if (data.getDiagReports() != null && !data.getDiagReports().isEmpty()) {

      logger.info(
          "Total num of Diagnostic Reports available for Patient {}", data.getDiagReports().size());

      for (DiagnosticReport dr : data.getDiagReports()) {

        if (dr.getCode() != null
            && dr.getCode().getCoding() != null
            && !dr.getCode().getCoding().isEmpty()
            && dr.getResult() != null
            && dr.getResult().size() > 0
            && Boolean.TRUE.equals(
                CdaFhirUtilities.isCodingPresentForCodeSystem(
                    dr.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL))) {

          logger.debug("Found a DiagnosticReport with a LOINC code");
          drs.add(dr);
        }
      }
    } else {
      logger.debug("No Valid DiagnosticReport in the bundle to process");
    }

    return drs;
  }
}
