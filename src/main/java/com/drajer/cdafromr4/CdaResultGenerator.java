package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.*;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaResultGenerator {

  private static final List<String> SEE_NOTE_PATTERNS =
      Arrays.asList("see note", "see notes", "see below", "see comments", "comment");

  /** participant/@typeCode for the authorizing provider (responsible party). */
  static final String AUTHORIZING_PROVIDER_TYPE_CODE = "RESP";

  /** participantRole/@classCode for a healthcare provider. */
  static final String AUTHORIZING_PROVIDER_CLASS_CODE = "PROV";

  private CdaResultGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaResultGenerator.class);

  public static String generateResultsSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder hsb = new StringBuilder(5000);
    StringBuilder textEntries = new StringBuilder(2000);
    StringBuilder resultEntries = new StringBuilder();

    // Get all Lab Results
    List<Observation> allResults = data.getLabResults();
    Map<String, Observation> uniqueObservations = new HashMap<>();

    // Get Diagnostic Reports with lab results and Observations without Diagnostic Reports
    Map<DiagnosticReport, List<Observation>> reports =
        getDiagnosticReportsWithObservations(data, allResults, uniqueObservations);

    // Sort most recent first for a consistent display order (issue #1152).
    uniqueObservations = CdaSortUtils.sortObservationMap(uniqueObservations);
    reports = CdaSortUtils.sortDiagnosticReports(reports);

    if ((uniqueObservations != null && !uniqueObservations.isEmpty())
        || (reports != null && !reports.isEmpty())) {

      // create section header
      createSectionHeader(data, details, hsb, version);

      // Used for creating the HTML Header in the text section.
      int rowNum = 1;

      // process lab results
      rowNum =
          processLabResults(
              uniqueObservations, details, rowNum, resultEntries, data, textEntries, version);

      // process diagnostic reports
      rowNum =
          processDiagnosticResults(
              reports, details, rowNum, resultEntries, data, textEntries, version);

      // complete section
      createSectionEnd(hsb, textEntries, resultEntries);

    } else {
      logger.info(
          " No Lab Results or Diagnostic Reports to generate data , hence generating a null"
              + " section.");
      hsb.append(generateEmptyLabResults());
    }

    return hsb.toString();
  }

  public static int processLabResults(
      Map<String, Observation> uniqueObservations,
      LaunchDetails details,
      int rowNum,
      StringBuilder resultEntries,
      R4FhirData data,
      StringBuilder textEntries,
      String version) {

    logger.debug(" Starting to process lab observations ");
    for (Map.Entry<String, Observation> entry : uniqueObservations.entrySet()) {

      if (entry.getValue().hasComponent()) {
        Observation obs = entry.getValue();

        logger.info(
            " Using Observation to create Organizer and Result entry for {}",
            obs.getIdElement().getIdPart());
        // create Table Values
        getTableValuesForObservationWithComponents(obs, rowNum, textEntries, data);

        // create one organizer and multiple result observations
        processLabResultsWithComponents(
            obs, obs.getCode(), details, rowNum, resultEntries, data, textEntries, version);

      } else {

        Observation obs = entry.getValue();

        logger.info(
            " Using Observation to create Organizer and Result entry for {}",
            obs.getIdElement().getIdPart());
        // create Table Values
        getTableValues(obs, rowNum, textEntries, data);

        // create Organizer + result observation entry
        getXmlForLabObservation(obs, obs.getCode(), details, rowNum, resultEntries, data, version);
      }

      rowNum++;
    }

    return rowNum;
  }

  public static void processLabResultsWithComponents(
      Observation obsWithComponents,
      CodeableConcept cd,
      LaunchDetails details,
      int rowNum,
      StringBuilder resultEntries,
      R4FhirData data,
      StringBuilder textEntries,
      String version) {

    logger.debug(" Generate XML for Lab Results with Components");
    // Setup the Organizer and Entries
    StringBuilder lrEntry = new StringBuilder(1000);

    // Add static organizer data including start elements for entry and organizer
    lrEntry.append(getOrganizerEntryStaticData());

    // Add dynamic organizer data
    lrEntry.append(
        getOrganizerEntryDynamicData(
            cd,
            details,
            data,
            obsWithComponents.getIssuedElement(),
            obsWithComponents.getPerformer(),
            obsWithComponents.getBasedOn(),
            version));

    int i = 1;
    for (ObservationComponentComponent obs : obsWithComponents.getComponent()) {

      // Add result entry component
      lrEntry.append(
          getXmlForObservationComponent(
              details,
              obs.getCode(),
              obs.getValue(),
              (obs.hasId()
                  ? obs.getIdElement().getId()
                  : new String("Component" + Integer.toString(i))),
              obsWithComponents.getEffective(),
              obs.getInterpretation(),
              rowNum,
              obsWithComponents.getPerformer(),
              data,
              version,
              obsWithComponents.getReferenceRange()));

      i++;
    }

    if (obsWithComponents.hasSpecimen()) {
      Specimen spec =
          data.getSpecimenById(obsWithComponents.getSpecimen().getReferenceElement().getIdPart());
      lrEntry.append(getSpecimenXml(spec, details));
    }

    // End the Organizer and the entry.
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    resultEntries.append(lrEntry);
  }

  public static void getXmlForLabObservation(
      Observation obs,
      CodeableConcept cd,
      LaunchDetails details,
      int rowNum,
      StringBuilder resultEntries,
      R4FhirData data,
      String version) {

    logger.debug(" Generating Lab Result XML for Observation ");
    // Setup the Organizer and Entries
    StringBuilder lrEntry = new StringBuilder(1000);

    // Add static organizer data including start elements for entry and organizer
    lrEntry.append(getOrganizerEntryStaticData());

    // Add dynamic organizer data
    lrEntry.append(
        getOrganizerEntryDynamicData(
            obs.getCode(),
            details,
            data,
            obs.getIssuedElement(),
            obs.getPerformer(),
            obs.getBasedOn(),
            version));

    // Add result entry component
    lrEntry.append(
        getXmlForObservationComponent(
            details,
            obs.getCode(),
            obs.getValue(),
            obs.getIdElement().getIdPart(),
            obs.getEffective(),
            obs.getInterpretation(),
            rowNum,
            obs.getPerformer(),
            data,
            version,
            obs.getReferenceRange()));

    if (obs.hasSpecimen()) {
      Specimen spec = data.getSpecimenById(obs.getSpecimen().getReferenceElement().getIdPart());
      lrEntry.append(getSpecimenXml(spec, details));
    }

    // End the Organizer and the entry.
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    resultEntries.append(lrEntry);
  }

  public static String getOrganizerEntryStaticData() {

    StringBuilder lrEntry = new StringBuilder(200);

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

    return lrEntry.toString();
  }

  public static String getOrganizerEntryDynamicData(
      CodeableConcept cd,
      LaunchDetails details,
      R4FhirData data,
      InstantType issued,
      List<Reference> performerReferences,
      List<Reference> basedOnRefs,
      String version) {

    logger.info("Generating Dynamic Data for Organizer ");
    StringBuilder lrEntry = new StringBuilder(200);
    List<String> paths = new ArrayList<>();
    paths.add("DiagnosticReport.code");
    Pair<Boolean, String> obsCodeXml = null;
    if (version.contentEquals("CDA_R31")) {
      obsCodeXml = getObservationCodeXml(details, cd, false, "", paths, version);
      if (obsCodeXml != null && obsCodeXml.getValue0()) {
        lrEntry.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_TEST_RESULT_ORGANIZER_TRIGGER_TEMPLATE,
                CdaGeneratorConstants.LAB_TEST_RESULT_ORGANIZER_TRIGGER_TEMPLATE_EXT));
      }
    }
    lrEntry.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    if (obsCodeXml != null && obsCodeXml.getValue0()) {

      lrEntry.append(obsCodeXml.getValue1());
    } else {

      lrEntry.append(getCodeXml(cd));
    }
    lrEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    if (issued != null) {
      Pair<Date, TimeZone> issuedDate = CdaFhirUtilities.getActualDate(issued);

      lrEntry.append(
          CdaGeneratorUtils.getXmlForIVLWithTS(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, issuedDate, issuedDate, true));
    }

    if (performerReferences != null && !performerReferences.isEmpty())
      lrEntry.append(CdaFhirUtilities.getXmlForAuthor(performerReferences, data));

    if (basedOnRefs != null && !basedOnRefs.isEmpty()) {
      lrEntry.append(getAuthorizingProviderParticipantXml(basedOnRefs, data));
    }

    return lrEntry.toString();
  }

  public static int processDiagnosticResults(
      Map<DiagnosticReport, List<Observation>> reports,
      LaunchDetails details,
      int rowNum,
      StringBuilder resultEntries,
      R4FhirData data,
      StringBuilder textEntries,
      String version) {

    logger.debug(" Starting to process Diagnostic Results ");

    // Iterate through the diagnostic reports
    for (Map.Entry<DiagnosticReport, List<Observation>> entry : reports.entrySet()) {

      DiagnosticReport rep = entry.getKey();
      List<Observation> observations = entry.getValue();

      getTableValuesForDiagnosticReport(rep, observations, rowNum, textEntries, data);

      logger.info(
          " Using DiagnosticReport to create Organizer and Result entry for {}",
          rep.getIdElement().getIdPart());

      // create one organizer and multiple result observations
      processDiagnosticReportWithObservations(
          rep, observations, details, rowNum, resultEntries, data, textEntries, version);

      rowNum++;
    }

    return rowNum;
  }

  public static void processDiagnosticReportWithObservations(
      DiagnosticReport report,
      List<Observation> observations,
      LaunchDetails details,
      int rowNum,
      StringBuilder resultEntries,
      R4FhirData data,
      StringBuilder textEntries,
      String version) {

    logger.info(" Starting to create organizers and entries ");

    StringBuilder lrEntry = new StringBuilder(1000);

    // Add static organizer data including start elements for entry and organizer
    lrEntry.append(getOrganizerEntryStaticData());

    // Add dynamic organizer data. Authorizing Provider (#1079): DiagnosticReport.basedOn first,
    // then the result Observations' basedOn, per the mapping sheet.
    lrEntry.append(
        getOrganizerEntryDynamicData(
            report.getCode(),
            details,
            data,
            report.getIssuedElement(),
            report.getPerformer(),
            getAuthorizingProviderBasedOnRefs(report, observations),
            version));

    int i = 1;
    for (Observation obs : observations) {

      // Add result entry component
      lrEntry.append(
          getXmlForObservationComponent(
              details,
              obs.getCode(),
              obs.getValue(),
              (obs.hasId()
                  ? obs.getIdElement().getIdPart()
                  : new String("Component" + Integer.toString(i))),
              obs.getEffective(),
              obs.getInterpretation(),
              rowNum,
              obs.getPerformer(),
              data,
              version,
              obs.getReferenceRange(),
              report.getPerformer()));

      i++;
    }

    if (report.hasSpecimen()) {
      lrEntry.append(getSpecimenXml(report.getSpecimen(), data, details));
    }

    // End the Organizer and the entry.
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    resultEntries.append(lrEntry);
  }

  public static Map<DiagnosticReport, List<Observation>> getDiagnosticReportsWithObservations(
      R4FhirData data,
      List<Observation> allObservations,
      Map<String, Observation> uniqueObservations) {

    Map<DiagnosticReport, List<Observation>> reports = new HashMap<>();

    // Build a lookup map using Observation ID.
    // If the same Observation ID occurs more than once, keep the first one.
    Map<String, Observation> observationsById = new HashMap<>();

    for (Observation observation : allObservations) {
      String observationId = observation.getIdElement().getIdPart();

      if (observationId == null || observationId.isEmpty()) {
        logger.warn("Ignoring Observation with missing ID");
        continue;
      }

      if (observationsById.containsKey(observationId)) {
        logger.warn(
            "Duplicate Observation ID {} found in the bundle. Ignoring duplicate occurrence.",
            observationId);
        continue;
      }

      observationsById.put(observationId, observation);
      uniqueObservations.put(observationId, observation);
    }

    if (data.getDiagReports() != null && !data.getDiagReports().isEmpty()) {

      logger.info(
          "Total num of Diagnostic Reports available for Patient {}", data.getDiagReports().size());

      for (DiagnosticReport dr : data.getDiagReports()) {

        if (dr.hasResult()) {

          List<Observation> obs = new ArrayList<>();

          // Prevent the same Observation from being added more than once
          // to the same DiagnosticReport.
          Set<String> processedObservationIds = new HashSet<>();

          List<Reference> obsRefs = dr.getResult();

          for (Reference r : obsRefs) {

            if (r.hasReference()) {

              String observationId = r.getReferenceElement().getIdPart();

              if (observationId == null || observationId.isEmpty()) {
                logger.warn(
                    "Ignoring Observation reference with missing ID in Diagnostic Report {}",
                    dr.getIdElement().getIdPart());
                continue;
              }

              // Guard against duplicate references within the same DiagnosticReport.
              if (!processedObservationIds.add(observationId)) {
                logger.warn(
                    "Ignoring duplicate Observation reference {} in Diagnostic Report {}",
                    observationId,
                    dr.getIdElement().getIdPart());
                continue;
              }

              // Find the Observation by ID.
              Observation observation = observationsById.get(observationId);

              if (observation != null) {
                obs.add(observation);
              }
            }
          }

          if (!obs.isEmpty()) {

            // Remove from uniqueObservations because this Observation
            // has been referenced by at least one DiagnosticReport.
            obs.forEach(
                observation -> uniqueObservations.remove(observation.getIdElement().getIdPart()));

            reports.put(dr, obs);

          } else {
            logger.info(
                "Ignoring Diagnostic Report with id {} as observations referenced cannot be found",
                dr.getIdElement().getIdPart());
          }

        } else {

          logger.info(
              "Ignoring Diagnostic Report with id {} as it has no results",
              dr.getIdElement().getIdPart());
        }
      }

    } else {
      logger.info("No Valid DiagnosticReport in the bundle to process");
    }

    logger.info(
        "Total # of Diagnostic Reports being used for Results section : {}", reports.size());

    return reports;
  }

  public static void createSectionHeader(
      R4FhirData data, LaunchDetails details, StringBuilder hsb, String version) {

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
  }

  public static void createSectionEnd(
      StringBuilder hsb, StringBuilder textEntries, StringBuilder resultEntries) {

    textEntries.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

    hsb.append(textEntries);

    // Add lab results
    hsb.append(resultEntries);

    // Complete the section end tags.
    hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));
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
      HashMap<String, Observation> allObs,
      LaunchDetails details,
      String contentId,
      int row,
      R4FhirData data,
      String version) {

    logger.info(" Adding References to observations ");
    StringBuilder lrEntry = new StringBuilder(2000);
    String contentRef = contentId + Integer.toString(row);

    Boolean foundComponent = false;

    List<Reference> refs = rep.getResult();

    for (Reference r : refs) {

      // Create an Observation for each entry.
      Observation obs = allObs.get(r.getReferenceElement().getIdPart());

      if (obs != null
          && obs.hasComponent()
          && obs.getComponent() != null
          && !obs.getComponent().isEmpty()) {

        CodeableConcept cc = obs.getCode();
        logger.info("CodeableConcept :{}", cc);
        Type val = obs.getValue();
        List<CodeableConcept> interpretation = obs.getInterpretation();
        StringBuilder id = new StringBuilder(200);
        id.append(obs.getIdElement().getIdPart());
        int rowNum = 1;

        for (ObservationComponentComponent oc : obs.getComponent()) {

          logger.debug("Found Observation Components ");
          if (oc.hasCode()
              && oc.getCode().hasCoding()
              && CdaFhirUtilities.isCodingPresentForCodeSystem(
                  oc.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL)) {
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
                  details,
                  cc,
                  val,
                  id.toString(),
                  obs.getEffective(),
                  interpretation,
                  contentRef,
                  null,
                  obs.getPerformer(),
                  data,
                  version,
                  obs.getReferenceRange(),
                  rep.getPerformer());

          if (!compString.isEmpty() && Boolean.FALSE.equals(foundComponent)) foundComponent = true;

          lrEntry.append(compString);

          rowNum++;
        }
      }

      if (obs != null && Boolean.FALSE.equals(foundComponent)) {

        CodeableConcept cc = null;
        logger.info("No component found , so directly adding the observation code ");
        if (obs.hasCode()
            && obs.getCode().hasCoding()
            && CdaFhirUtilities.isCodingPresentForCodeSystem(
                obs.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL)) {
          cc = obs.getCode();
        } else {
          // use diagnostic report code instead
          cc = rep.getCode();
        }

        lrEntry.append(
            getXmlForObservationComponent(
                details,
                cc,
                obs.getValue(),
                obs.getIdElement().getIdPart(),
                obs.getEffective(),
                obs.getInterpretation(),
                contentRef,
                rep.getCode(),
                obs.getPerformer(),
                data,
                version,
                obs.getReferenceRange(),
                rep.getPerformer()));
      }
    }

    logger.debug("Lr Entry = {}", StringEscapeUtils.escapeXml11(lrEntry.toString()));

    return lrEntry.toString();
  }

  public static String getXmlForObservationComponent(
      LaunchDetails details,
      CodeableConcept cd,
      Type val,
      String id,
      Type effective,
      List<CodeableConcept> interpretation,
      int rowNum,
      List<Reference> performerRefs,
      R4FhirData data,
      String version,
      List<Observation.ObservationReferenceRangeComponent> referenceRange) {
    return getXmlForObservationComponent(
        details,
        cd,
        val,
        id,
        effective,
        interpretation,
        rowNum,
        performerRefs,
        data,
        version,
        referenceRange,
        null);
  }

  /**
   * @param reportPerformerRefs DiagnosticReport.performer of the report the observation belongs to;
   *     used for the performing organization when the observation has no Organization performer.
   */
  public static String getXmlForObservationComponent(
      LaunchDetails details,
      CodeableConcept cd,
      Type val,
      String id,
      Type effective,
      List<CodeableConcept> interpretation,
      int rowNum,
      List<Reference> performerRefs,
      R4FhirData data,
      String version,
      List<Observation.ObservationReferenceRangeComponent> referenceRange,
      List<Reference> reportPerformerRefs) {

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

    List<String> paths = new ArrayList<>();
    paths.add("Observation.code");
    paths.add("DiagnosticReport.code");
    paths.add("Observation.value");

    String contentRef =
        CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT + Integer.toString(rowNum);
    Pair<Boolean, String> obsCodeXml =
        getObservationCodeXml(details, cd, false, contentRef, paths, version);

    Pair<Boolean, String> obsValueXml = null;
    contentRef = CdaGeneratorConstants.LABTEST_TABLE_COL_2_BODY_CONTENT + Integer.toString(rowNum);
    if (val instanceof CodeableConcept) {
      paths.clear();
      paths.add("Observation.value");
      CodeableConcept value = (CodeableConcept) val;
      obsValueXml = getObservationCodeXml(details, value, true, contentRef, paths, version);
    }

    boolean isTriggerResult = false;
    if ((obsCodeXml != null && obsCodeXml.getValue0())
        || (obsValueXml != null && obsValueXml.getValue0())) {

      lrEntry.append(getTriggerCodeTemplateXml(version));
      isTriggerResult = true;
    }

    lrEntry.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id));

    if (obsCodeXml != null) {
      lrEntry.append(obsCodeXml.getValue1());
    } else {
      logger.warn("Unable to add code as the xml is null");
    }

    lrEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    lrEntry.append(
        CdaFhirUtilities.getXmlForType(effective, CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    String valueXml =
        (obsValueXml == null)
            ? CdaFhirUtilities.getXmlForType(val, CdaGeneratorConstants.VAL_EL_NAME, true)
            : obsValueXml.getValue1();
    lrEntry.append(valueXml);

    // Add interpretation code (#1151: NI fallback for trigger results with an ST value).
    lrEntry.append(getInterpretationCodeXml(interpretation, isTriggerResult, valueXml));

    lrEntry.append(getPerformingOrganizationXml(performerRefs, reportPerformerRefs, data));

    // Add performer
    lrEntry.append(CdaFhirUtilities.getXmlForAuthor(performerRefs, data));

    // Add reference range if available
    if (referenceRange != null && !referenceRange.isEmpty()) {
      lrEntry.append(getReferenceRangeXml(interpretation, referenceRange));
    }

    // End Tag for Entry Relationship
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    logger.debug("Lr Entry = {}", StringEscapeUtils.escapeXml11(lrEntry.toString()));

    return lrEntry.toString();
  }

  private static final String ST_VALUE_TYPE = "xsi:type=\"" + CdaGeneratorConstants.ST_TYPE + "\"";

  /**
   * interpretationCode for a result observation: from Observation.interpretation when present;
   * otherwise nullFlavor NI when CONF:4411-603 requires one.
   *
   * <p>GitHub #1151, eICR CONF:4411-603: "If value has xsi:type="ST" then there SHALL be at least
   * one [1..*] interpretationCode" (Initial Case Report Trigger Code Result Observation).
   */
  static String getInterpretationCodeXml(
      List<CodeableConcept> interpretation, boolean isTriggerResult, String valueXml) {

    String interpretXml =
        (interpretation == null)
            ? ""
            : CdaFhirUtilities.getCodeableConceptXmlForMappedConceptDomain(
                CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
                interpretation,
                CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
                false,
                false);

    if (StringUtils.isNotEmpty(interpretXml)) {
      return interpretXml;
    }

    if (isTriggerResult && valueXml != null && valueXml.contains(ST_VALUE_TYPE)) {
      logger.info("Trigger result with an ST value has no interpretation; adding nullFlavor NI");
      return CdaGeneratorUtils.getNFXMLForElement(
          CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME, CdaGeneratorConstants.NF_NI);
    }

    return "";
  }

  public static String getTriggerCodeTemplateXml(String version) {

    String tcXml = "";
    if (version.contentEquals("CDA_R31")) {
      tcXml =
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE,
              CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE_EXT_31);
    } else {
      tcXml =
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE,
              CdaGeneratorConstants.LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE_EXT);
    }

    return tcXml;
  }

  public static String getXmlForObservationComponent(
      LaunchDetails details,
      CodeableConcept cd,
      Type val,
      String id,
      Type effective,
      List<CodeableConcept> interpretation,
      String contentRef,
      CodeableConcept altCode,
      List<Reference> performerRefs,
      R4FhirData data,
      String version,
      List<Observation.ObservationReferenceRangeComponent> referenceRange) {
    return getXmlForObservationComponent(
        details,
        cd,
        val,
        id,
        effective,
        interpretation,
        contentRef,
        altCode,
        performerRefs,
        data,
        version,
        referenceRange,
        null);
  }

  /**
   * @param reportPerformerRefs DiagnosticReport.performer of the report the observation belongs to;
   *     used for the performing organization when the observation has no Organization performer.
   */
  public static String getXmlForObservationComponent(
      LaunchDetails details,
      CodeableConcept cd,
      Type val,
      String id,
      Type effective,
      List<CodeableConcept> interpretation,
      String contentRef,
      CodeableConcept altCode,
      List<Reference> performerRefs,
      R4FhirData data,
      String version,
      List<Observation.ObservationReferenceRangeComponent> referenceRange,
      List<Reference> reportPerformerRefs) {

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

    List<String> paths = new ArrayList<>();
    paths.add("Observation.code");
    paths.add("DiagnosticReport.code");
    paths.add("Observation.value");

    Pair<Boolean, String> obsCodeXml =
        getObservationCodeXml(details, cd, false, contentRef, paths, version);

    Pair<Boolean, String> altObsCodeXml = null;
    if (altCode != null) {
      altObsCodeXml = getObservationCodeXml(details, altCode, false, contentRef, paths, version);
    }

    Pair<Boolean, String> obsValueXml = null;

    if (val instanceof CodeableConcept) {

      paths.clear();
      paths.add("Observation.value");
      CodeableConcept value = (CodeableConcept) val;
      obsValueXml = getObservationCodeXml(details, value, true, contentRef, paths, version);
    }

    boolean isTriggerResult = false;
    if ((obsCodeXml != null && obsCodeXml.getValue0())
        || (obsValueXml != null && obsValueXml.getValue0())) {

      lrEntry.append(getTriggerCodeTemplateXml(version));
      isTriggerResult = true;

    } else if (altObsCodeXml != null && altObsCodeXml.getValue0()) {

      // this will catch the case the DiagnosticReport.code is matched and the
      // Observation.code does
      // not exist
      // or is not the same.
      lrEntry.append(getTriggerCodeTemplateXml(version));
      isTriggerResult = true;
    }

    lrEntry.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id));

    if (obsCodeXml != null && obsCodeXml.getValue0()) {
      lrEntry.append(obsCodeXml.getValue1());
    } else if (altObsCodeXml != null && altObsCodeXml.getValue0()) {
      lrEntry.append(altObsCodeXml.getValue1());
    } else if (obsCodeXml != null) {
      lrEntry.append(obsCodeXml.getValue1());
    } else if (altObsCodeXml != null) {
      lrEntry.append(altObsCodeXml.getValue1());
    }

    lrEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    lrEntry.append(
        CdaFhirUtilities.getXmlForType(effective, CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    String valueXml =
        (obsValueXml == null)
            ? CdaFhirUtilities.getXmlForType(val, CdaGeneratorConstants.VAL_EL_NAME, true)
            : obsValueXml.getValue1();
    lrEntry.append(valueXml);

    // Add interpretation code (#1151: NI fallback for trigger results with an ST value).
    lrEntry.append(getInterpretationCodeXml(interpretation, isTriggerResult, valueXml));

    // Performing organization (#1079): performer must precede author in a CDA observation.
    lrEntry.append(getPerformingOrganizationXml(performerRefs, reportPerformerRefs, data));

    // Add performer

    lrEntry.append(CdaFhirUtilities.getXmlForAuthor(performerRefs, data));

    // Add reference range if available
    if (referenceRange != null && !referenceRange.isEmpty()) {
      lrEntry.append(getReferenceRangeXml(interpretation, referenceRange));
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
      List<String> paths,
      String version) {

    String elementType =
        valElement ? CdaGeneratorConstants.VAL_EL_NAME : CdaGeneratorConstants.CODE_EL_NAME;
    PatientExecutionState state = ApplicationUtils.getDetailStatus(details);
    List<MatchedTriggerCodes> mtcs = state.getMatchTriggerStatus().getMatchedCodes();

    for (MatchedTriggerCodes mtc : mtcs) {
      Pair<String, String> matchedCode = findMatchingCode(mtc, code, paths);
      if (matchedCode != null) {
        String systemUrl =
            valElement
                ? CdaGeneratorConstants.FHIR_SNOMED_URL
                : CdaGeneratorConstants.FHIR_LOINC_URL;

        logger.info("Found a matched {} for the observation or diagnostic report", elementType);

        Pair<String, String> systemName =
            CdaGeneratorConstants.getCodeSystemFromUrl(matchedCode.getValue1());
        String xml =
            CdaFhirUtilities.getXmlForCodeableConceptWithCDAndValueSetAndVersion(
                elementType,
                matchedCode.getValue0(),
                systemName.getValue0(),
                systemName.getValue1(),
                details.getRctcOid(),
                details.getRctcVersion(),
                code,
                systemUrl,
                contentRef,
                valElement);
        return new Pair<>(true, xml);
      }
    }

    logger.info("Did not find a matched Code or value for the observation or diagnostic report");

    String xml =
        valElement
            ? CdaFhirUtilities.getCodeableConceptXmlForValue(code, elementType, contentRef)
            : CdaFhirUtilities.getCodeableConceptXml(code, elementType, contentRef);
    return new Pair<>(false, xml);
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

  private static Pair<String, String> findMatchingCode(
      MatchedTriggerCodes mtc, CodeableConcept code, List<String> paths) {

    for (String s : paths) {
      Pair<String, String> matchedCode = mtc.getMatchingCode(code, s);
      if (matchedCode != null) {
        return matchedCode;
      }
    }
    return null; // Indicate no matching code found
  }

  public static Observation.ObservationReferenceRangeComponent getReferenceRange(
      List<Observation.ObservationReferenceRangeComponent> referenceRange) {
    for (Observation.ObservationReferenceRangeComponent refRange : referenceRange) {
      if (refRange.hasLow() && refRange.hasHigh()) {
        return refRange;
      }
    }
    return null;
  }

  public static String getSpecimenXml(Specimen spec, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    if (spec != null) {

      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForAct(
              CdaGeneratorConstants.PROC_ACT_EL_NAME,
              CdaGeneratorConstants.PROC_CLASS_CODE,
              CdaGeneratorConstants.MOOD_CODE_DEF));

      sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.SPECIMEN_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.SPECIMEN_TEMPLATE_ID,
              CdaGeneratorConstants.SPECIMEN_TEMPLATE_ID_EXT));
      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.SPECIMEN_COLLECTION_CODE,
              CdaGeneratorConstants.SNOMED_CODESYSTEM_OID));

      if (spec.hasCollection() && spec.getCollection().hasCollectedDateTimeType()) {
        sb.append(
            CdaFhirUtilities.getXmlForType(
                spec.getCollection().getCollected(),
                CdaGeneratorConstants.EFF_TIME_EL_NAME,
                false));
      }

      if (spec.hasCollection() && spec.getCollection().hasBodySite()) {
        List<CodeableConcept> cds = new ArrayList<>();
        cds.add(spec.getCollection().getBodySite());

        sb.append(
            CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
                cds,
                CdaGeneratorConstants.TARGET_SITE_CODE_EL_NAME,
                false,
                CdaGeneratorConstants.FHIR_SNOMED_URL,
                true,
                ""));
      }

      if (spec.hasType()) {
        // Add participant
        sb.append(
            CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
                CdaGeneratorConstants.PARTICIPANT_EL_NAME, CdaGeneratorConstants.TYPE_CODE_PRD));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PARTICIPANT_TEMPLATE_ID));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.PARTICIPANT_TEMPLATE_ID,
                CdaGeneratorConstants.PARTICIPANT_TEMPLATE_ID_EXT));
        sb.append(
            CdaGeneratorUtils.getXmlForStartElementWithClassCode(
                CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME,
                CdaGeneratorConstants.SPECIMEN_ROLE_CLASS_CODE));
        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), spec.getIdElement().getIdPart()));
        sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.PLAYING_ENTITY));

        sb.append(
            CdaFhirUtilities.getCodeableConceptXml(
                spec.getType(), CdaGeneratorConstants.CODE_EL_NAME, ""));

        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PLAYING_ENTITY));
        sb.append(
            CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_EL_NAME));
      }

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PROC_ACT_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));
    }

    return sb.toString();
  }

  public static String getSpecimenXml(
      List<Reference> refs, R4FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);
    if (refs != null) {

      for (Reference r : refs) {
        Specimen spec = data.getSpecimenById(r.getReferenceElement().getIdPart());
        sb.append(getSpecimenXml(spec, details));
      }
    }

    return sb.toString();
  }

  /**
   * Builds XML representation for a given CodeableConcept. Prioritizes LOINC codes, then falls back
   * to other codings or text.
   */
  private static String getCodeXml(CodeableConcept cd) {
    if (cd == null) {
      return CdaFhirUtilities.getCodingXml(null, CdaGeneratorConstants.CODE_EL_NAME, "");
    }

    if (cd.hasCoding()) {
      logger.debug("Finding LOINC Code (preferred) for Lab Results");

      String codeXml =
          CdaFhirUtilities.getCodingXmlForCodeSystem(
              cd.getCoding(),
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.FHIR_LOINC_URL,
              true,
              "");

      if (codeXml.isEmpty()) {
        codeXml =
            CdaFhirUtilities.getCodingXml(cd.getCoding(), CdaGeneratorConstants.CODE_EL_NAME, "");
      }

      return codeXml;
    }

    if (cd.hasText()) {
      return CdaGeneratorUtils.getXmlForNullCDWithText(
          CdaGeneratorConstants.CODE_EL_NAME, CdaGeneratorConstants.NF_OTH, cd.getText());
    }

    return CdaFhirUtilities.getCodingXml(null, CdaGeneratorConstants.CODE_EL_NAME, "");
  }

  private static String getNarrativeTextObservationValue(Observation obs) {

    String value = obs.hasValue() ? CdaFhirUtilities.getStringForType(obs.getValue()) : "";

    boolean isSeeNote =
        StringUtils.isNotBlank(value)
            && SEE_NOTE_PATTERNS.stream().anyMatch(p -> value.toLowerCase().startsWith(p));

    if (isSeeNote && obs.hasNote()) {

      return obs.getNote().stream()
          .map(Annotation::getText)
          .filter(StringUtils::isNotBlank)
          .collect(Collectors.joining("\n"));
    }

    return value;
  }

  private static String getReferenceRangeXml(
      List<CodeableConcept> interpretations,
      List<Observation.ObservationReferenceRangeComponent> referenceRange) {

    // Early return for null or empty reference ranges
    if (referenceRange == null || referenceRange.isEmpty()) {
      return "";
    }

    StringBuilder sb = new StringBuilder(500);

    // Get the most appropriate reference range component
    Observation.ObservationReferenceRangeComponent refRangeComp = getReferenceRange(referenceRange);
    if (refRangeComp == null) {
      refRangeComp = referenceRange.get(0);
    }

    // Build reference range structure
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REFERENCE_RANGE_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.OBSERVATION_RANGE_EL_NAME));

    // Add text element (always included, defaults to "Unknown")
    appendTextIfPresent(sb, refRangeComp);

    // Add interval value with low/high bounds
    appendIntervalValue(sb, refRangeComp);

    // Add interpretation codes
    appendInterpretationCodes(sb, interpretations);

    // Close elements
    sb.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBSERVATION_RANGE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REFERENCE_RANGE_EL_NAME));

    return sb.toString();
  }

  /**
   * Appends text element - uses reference range text if available, otherwise defaults to "Unknown"
   */
  private static void appendTextIfPresent(
      StringBuilder sb, Observation.ObservationReferenceRangeComponent refRangeComp) {
    String refText;

    if (refRangeComp.hasText() && StringUtils.isNotBlank(refRangeComp.getText())) {
      refText = StringEscapeUtils.escapeXml11(refRangeComp.getText());
    } else {
      refText = CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME, refText));
  }

  /** Appends the interval value element with low and high bounds */
  private static void appendIntervalValue(
      StringBuilder sb, Observation.ObservationReferenceRangeComponent refRangeComp) {
    if (!refRangeComp.hasLow() && !refRangeComp.hasHigh()) {
      sb.append(
          CdaGeneratorUtils.getNFXMLForValue(
              CdaGeneratorConstants.IVL_PQ_TYPE, CdaGeneratorConstants.NF_NI));
      return;
    }

    // Start IVL_PQ value element
    sb.append(CdaGeneratorConstants.START_XMLTAG)
        .append(CdaGeneratorConstants.VAL_EL_NAME)
        .append(CdaGeneratorConstants.SPACE)
        .append(CdaGeneratorConstants.XSI_TYPE)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.IVL_PQ_TYPE)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.RIGHT_ANGLE_BRACKET)
        .append(CdaGeneratorConstants.NEW_LINE);

    // Append low and high bounds
    if (refRangeComp.hasLow()) {
      appendQuantityBound(sb, refRangeComp.getLow(), CdaGeneratorConstants.TIME_LOW_EL_NAME);
    }

    if (refRangeComp.hasHigh()) {
      appendQuantityBound(sb, refRangeComp.getHigh(), CdaGeneratorConstants.TIME_HIGH_EL_NAME);
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.VAL_EL_NAME));
  }

  /** Appends a quantity bound (low or high) with null flavor handling */
  private static void appendQuantityBound(StringBuilder sb, Quantity quantity, String elementName) {
    if (quantity == null) {
      return;
    }

    if (quantity.hasValue()) {
      String unit = quantity.hasUnit() ? quantity.getUnit() : "1";
      sb.append(
          CdaGeneratorUtils.getXmlForQuantityWithUnits(
              elementName, quantity.getValue().toString(), unit));
    } else {
      // Add null flavor for missing value
      logger.warn("Quantity value is missing for reference range element: {}", elementName);
      sb.append(CdaGeneratorConstants.START_XMLTAG)
          .append(elementName)
          .append(CdaGeneratorConstants.SPACE)
          .append(CdaGeneratorConstants.NULLFLAVOR_WITH_EQUAL)
          .append(CdaGeneratorConstants.DOUBLE_QUOTE)
          .append(CdaGeneratorConstants.NF_NI)
          .append(CdaGeneratorConstants.DOUBLE_QUOTE)
          .append(CdaGeneratorConstants.END_XMLTAG_NEWLN);
    }
  }

  /** Appends interpretation codes if present */
  private static void appendInterpretationCodes(
      StringBuilder sb, List<CodeableConcept> interpretations) {
    if (interpretations == null || interpretations.isEmpty()) {
      return;
    }

    String interpretXml =
        CdaFhirUtilities.getCodeableConceptXml(
            interpretations, CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME, false);

    if (StringUtils.isNotEmpty(interpretXml)) {
      sb.append(interpretXml);
    }
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // CdaResultGenerator — replace the three getTableValues* methods
  // and add the private helpers below
  // ─────────────────────────────────────────────────────────────────────────────

  // ── Public methods ────────────────────────────────────────────────────────────

  /**
   * Narrative table for a plain Observation (no components). One outer panel row + one inner detail
   * row.
   */
  public static void getTableValues(
      Observation obs, int rowNum, StringBuilder textEntries, R4FhirData data) {

    String panelId = "id_" + obs.getIdElement().getIdPart() + "_ref";
    String panelName = CdaFhirUtilities.getStringForCodeableConcept(obs.getCode());
    String panelDate = getEffectiveDate(obs.getEffective());

    String outcome =
        obs.hasValue()
            ? CdaFhirUtilities.getStringForType(obs.getValue())
            : CdaGeneratorConstants.UNKNOWN_VALUE;
    String interp = getInterpretation(obs.getInterpretation());
    String refRange = buildReferenceRangeString(obs.getReferenceRange());
    String collection = getCollectionDate(Collections.singletonList(obs.getSpecimen()), data);

    Map<String, String> row =
        buildInnerRow(rowNum, 1, panelName, outcome, interp, panelDate, refRange, collection);

    textEntries.append(
        buildNestedResultTable(panelId, panelName, panelDate, Collections.singletonList(row)));
  }

  /**
   * Narrative table for an Observation that has components (panel). One outer panel row + one inner
   * row per component.
   */
  public static void getTableValuesForObservationWithComponents(
      Observation obs, int rowNum, StringBuilder textEntries, R4FhirData data) {

    String panelId = "id_" + obs.getIdElement().getIdPart() + "_ref";
    String panelName = CdaFhirUtilities.getStringForCodeableConcept(obs.getCode());
    String panelDate = getEffectiveDate(obs.getEffective());

    List<Map<String, String>> rows = new ArrayList<>();

    int compIndex = 1;
    for (ObservationComponentComponent comp : obs.getComponent()) {
      String compId = "id_" + obs.getIdElement().getIdPart() + "_comp" + compIndex + "_ref";
      String name = CdaFhirUtilities.getStringForCodeableConcept(comp.getCode());
      String outcome =
          comp.hasValue()
              ? CdaFhirUtilities.getStringForType(comp.getValue())
              : CdaGeneratorConstants.UNKNOWN_VALUE;
      String interp = getInterpretation(comp.getInterpretation());
      String refRange = buildReferenceRangeString(obs.getReferenceRange());

      rows.add(
          buildInnerRow(
              rowNum,
              compIndex,
              name,
              outcome,
              interp,
              panelDate,
              refRange,
              CdaGeneratorConstants.UNKNOWN_VALUE));
      compIndex++;
    }

    textEntries.append(buildNestedResultTable(panelId, panelName, panelDate, rows));
  }

  /**
   * Narrative table for a DiagnosticReport with its Observations. One outer panel row + one inner
   * row per observation.
   */
  public static void getTableValuesForDiagnosticReport(
      DiagnosticReport report,
      List<Observation> observations,
      int rowNum,
      StringBuilder textEntries,
      R4FhirData data) {

    String panelId = "id_" + report.getIdElement().getIdPart() + "_ref";
    String panelName = CdaFhirUtilities.getStringForCodeableConcept(report.getCode());
    String panelDate =
        report.hasEffective()
            ? CdaFhirUtilities.getStringForType(report.getEffective())
            : CdaGeneratorConstants.UNKNOWN_VALUE;

    List<Map<String, String>> rows = new ArrayList<>();
    if (observations != null) {
      int compIndex = 1;
      for (Observation obs : observations) {

        String compId = "id_" + obs.getIdElement().getIdPart() + "_ref";
        String name = CdaFhirUtilities.getStringForCodeableConcept(obs.getCode());
        String outcome =
            obs.hasValue()
                ? CdaFhirUtilities.getStringForType(obs.getValue())
                : CdaGeneratorConstants.UNKNOWN_VALUE;
        String interp = getInterpretation(obs.getInterpretation());
        String date = getEffectiveDate(obs.getEffective());
        String refRange = buildReferenceRangeString(obs.getReferenceRange());
        String collection =
            obs.hasSpecimen()
                ? getCollectionDate(Collections.singletonList(obs.getSpecimen()), data)
                : CdaGeneratorConstants.UNKNOWN_VALUE;

        rows.add(
            buildInnerRow(rowNum, compIndex, name, outcome, interp, date, refRange, collection));
        compIndex++;
      }
    }

    textEntries.append(buildNestedResultTable(panelId, panelName, panelDate, rows));
  }

  // ── Private helpers ───────────────────────────────────────────────────────────

  /**
   * Builds the complete nested table block for one panel.
   *
   * <p>Produces:
   *
   * <table> outer panel header + summary row
   *     <tr colspan="20"> inner table with one row per component
   *   </table>
   */
  private static String buildNestedResultTable(
      String panelId, String panelName, String panelDate, List<Map<String, String>> innerRows) {

    StringBuilder sb = new StringBuilder(1000);

    // ── outer table ──────────────────────────────────────────────────────────
    sb.append(buildOuterTableOpen());
    sb.append(buildOuterSummaryRow(panelId, panelName, panelDate));
    sb.append(buildInnerTableBlock(innerRows));
    sb.append(buildOuterTableClose());

    return sb.toString();
  }

  /**
   * Opens the outer table with its two-column header.
   *
   * <table border="1" width="100%">
   *   <thead><tr><th>Results Panel</th><th>Date(s)</th></tr></thead>
   *   <tbody>
   */
  private static String buildOuterTableOpen() {

    StringBuilder sb = new StringBuilder(300);

    // <table border="1" width="100%">
    sb.append(CdaGeneratorConstants.START_XMLTAG)
        .append(CdaGeneratorConstants.TABLE_EL_NAME)
        .append(CdaGeneratorConstants.SPACE)
        .append(CdaGeneratorConstants.TABLE_BORDER_ATTR_NAME)
        .append(CdaGeneratorConstants.EQUAL)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.TABLE_BORDER)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.SPACE)
        .append(CdaGeneratorConstants.TABLE_WIDTH_ATTR_NAME)
        .append(CdaGeneratorConstants.EQUAL)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.TABLE_WIDTH)
        .append("%")
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.RIGHT_ANGLE_BRACKET)
        .append(CdaGeneratorConstants.NEW_LINE);

    // <thead>
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.RESULTS_PANEL_COL_TITLE));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.RESULTS_DATE_COL_TITLE));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME));

    // <tbody>
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    return sb.toString();
  }

  /**
   * The outer summary row showing panel name and date.
   *
   * <p><tr ID="panelId"><td>name</td><td>date</td></tr>
   */
  private static String buildOuterSummaryRow(String panelId, String panelName, String panelDate) {

    StringBuilder sb = new StringBuilder(200);

    sb.append(CdaGeneratorConstants.START_XMLTAG)
        .append(CdaGeneratorConstants.TABLE_ROW_EL_NAME)
        .append(CdaGeneratorConstants.SPACE)
        .append(CdaGeneratorConstants.TABLE_BODY_CONTENT_ID_EL_NAME)
        .append(CdaGeneratorConstants.EQUAL)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(panelId)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.RIGHT_ANGLE_BRACKET)
        .append(CdaGeneratorConstants.NEW_LINE);

    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));
    sb.append(StringEscapeUtils.escapeXml11(panelName));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));
    sb.append(StringEscapeUtils.escapeXml11(panelDate));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));

    return sb.toString();
  }

  /**
   * The colspan row containing the inner detail table.
   *
   * <p><tr> <td colspan="20"> <list styleCode="none"><item>
   *
   * <table>
   *         <thead> Test | Outcome | Date(s) | Interpretation | Reference Range | Specimen Collection Date </thead>
   *         <tbody> one <tr> per component </tbody>
   *       </table>
   *
   * </item></list> </td> </tr>
   */
  private static String buildInnerTableBlock(List<Map<String, String>> rows) {

    StringBuilder sb = new StringBuilder(800);

    // <tr>
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));

    // <td colspan="20">
    sb.append(CdaGeneratorConstants.START_XMLTAG)
        .append(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME)
        .append(" colspan=\"20\"")
        .append(CdaGeneratorConstants.RIGHT_ANGLE_BRACKET)
        .append(CdaGeneratorConstants.NEW_LINE);

    // <list styleCode="none"><item>
    sb.append("<list styleCode=\"none\">").append(CdaGeneratorConstants.NEW_LINE);
    sb.append(CdaGeneratorUtils.getXmlForStartElement("item"));

    // ── inner <table> ──────────────────────────────────────────────────────
    sb.append(CdaGeneratorConstants.START_XMLTAG)
        .append(CdaGeneratorConstants.TABLE_EL_NAME)
        .append(CdaGeneratorConstants.RIGHT_ANGLE_BRACKET)
        .append(CdaGeneratorConstants.NEW_LINE);

    // inner <thead>
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.INNER_COL_TEST));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.INNER_COL_OUTCOME));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.INNER_COL_DATE));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.INNER_COL_INTERP));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.INNER_COL_REF_RANGE));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TABLE_HEAD_CONTENT_EL_NAME,
            CdaGeneratorConstants.INNER_COL_COLLECTION));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_HEAD_EL_NAME));

    // inner <tbody>
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
    for (Map<String, String> row : rows) {
      sb.append(buildInnerComponentRow(row));
    }
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    // close inner table
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));

    // </item></list>
    sb.append(CdaGeneratorUtils.getXmlForEndElement("item"));
    sb.append(CdaGeneratorUtils.getXmlForEndElement("list"));

    // </td></tr>
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));

    return sb.toString();
  }

  /** Closes outer tbody and table. */
  private static String buildOuterTableClose() {
    return CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME)
        + CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME);
  }

  private static String buildInnerComponentRow(Map<String, String> row) {

    StringBuilder sb = new StringBuilder(400);
    String trId = row.get(CdaGeneratorConstants.INNER_BODY_ID); // e.g. "labTestName2_1"
    String rowNum = row.get(CdaGeneratorConstants.INNER_ROW_NUM); // e.g. "2"
    String compIdx = row.get(CdaGeneratorConstants.INNER_KEY_COMP_IDX); // e.g. "1"

    String contentSuffix = compIdx.equals("0") ? rowNum : rowNum + "-" + compIdx;

    sb.append(CdaGeneratorConstants.START_XMLTAG)
        .append(CdaGeneratorConstants.TABLE_ROW_EL_NAME)
        .append(CdaGeneratorConstants.SPACE)
        .append(CdaGeneratorConstants.TABLE_BODY_CONTENT_ID_EL_NAME)
        .append(CdaGeneratorConstants.EQUAL)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(trId)
        .append(CdaGeneratorConstants.DOUBLE_QUOTE)
        .append(CdaGeneratorConstants.RIGHT_ANGLE_BRACKET)
        .append(CdaGeneratorConstants.NEW_LINE);

    // content IDs now unique per row
    appendTd(
        sb,
        CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT + contentSuffix,
        row.get(CdaGeneratorConstants.INNER_BODY_TEST));
    appendTd(
        sb,
        CdaGeneratorConstants.LABTEST_TABLE_COL_2_BODY_CONTENT + contentSuffix,
        row.get(CdaGeneratorConstants.INNER_BODY_OUTCOME));
    appendTd(
        sb,
        CdaGeneratorConstants.LABTEST_TABLE_COL_3_BODY_CONTENT + contentSuffix,
        row.get(CdaGeneratorConstants.INNER_BODY_DATE));
    appendTd(
        sb,
        CdaGeneratorConstants.LABTEST_TABLE_COL_4_BODY_CONTENT + contentSuffix,
        row.get(CdaGeneratorConstants.INNER_BODY_INTERP));
    appendTd(
        sb,
        CdaGeneratorConstants.LABTEST_TABLE_COL_5_BODY_CONTENT + contentSuffix,
        row.get(CdaGeneratorConstants.INNER_BODY_REF_RANGE));
    appendTd(
        sb,
        CdaGeneratorConstants.LABTEST_TABLE_COL_6_BODY_CONTENT + contentSuffix,
        row.get(CdaGeneratorConstants.INNER_BODY_COLLECTION));

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_ROW_EL_NAME));
    return sb.toString();
  }

  private static void appendTd(StringBuilder sb, String contentId, String value) {
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForTableBodyContent(
            contentId,
            StringUtils.isNotBlank(value) ? value : CdaGeneratorConstants.UNKNOWN_VALUE));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_ROW_EL_NAME));
  }

  /**
   * Builds the inner row data map. The "_id" key carries the row ID so buildInnerComponentRow() can
   * stamp it on the <tr> and each <content>.
   */
  private static Map<String, String> buildInnerRow(
      int rowNum,
      int compIndex,
      String test,
      String outcome,
      String interpretation,
      String date,
      String refRange,
      String collection) {

    String trId =
        compIndex == 0
            ? CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT + rowNum
            : CdaGeneratorConstants.LABTEST_TABLE_COL_1_BODY_CONTENT + rowNum + "_" + compIndex;

    Map<String, String> row = new LinkedHashMap<>();
    row.put(CdaGeneratorConstants.INNER_BODY_ID, String.valueOf(trId));
    row.put(CdaGeneratorConstants.INNER_ROW_NUM, String.valueOf(rowNum));
    row.put(CdaGeneratorConstants.INNER_KEY_COMP_IDX, String.valueOf(compIndex));
    row.put(CdaGeneratorConstants.INNER_BODY_TEST, StringEscapeUtils.escapeXml11(test));
    row.put(CdaGeneratorConstants.INNER_BODY_OUTCOME, StringEscapeUtils.escapeXml11(outcome));
    row.put(CdaGeneratorConstants.INNER_BODY_INTERP, StringEscapeUtils.escapeXml11(interpretation));
    row.put(CdaGeneratorConstants.INNER_BODY_DATE, date);
    row.put(CdaGeneratorConstants.INNER_BODY_REF_RANGE, refRange);
    row.put(CdaGeneratorConstants.INNER_BODY_COLLECTION, collection);
    return row;
  }

  // ── Small data utilities ──────────────────────────────────────────────────────

  private static String getEffectiveDate(Type effective) {
    return effective != null
        ? CdaFhirUtilities.getStringForType(effective)
        : CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  private static String getInterpretation(List<CodeableConcept> interpretations) {
    if (interpretations == null || interpretations.isEmpty()) {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }
    String val = CdaFhirUtilities.getDisplayStringForCodeableConcept(interpretations);
    return StringUtils.isNotBlank(val) ? val : CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  private static String getCollectionDate(List<Reference> specimenRefs, R4FhirData data) {
    String date = CdaFhirUtilities.getStringForSpecimenCollectionDate(specimenRefs, data);
    return StringUtils.isNotBlank(date) ? date : CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  private static String buildReferenceRangeString(
      List<Observation.ObservationReferenceRangeComponent> ranges) {

    if (ranges == null || ranges.isEmpty()) {
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    Observation.ObservationReferenceRangeComponent best = getReferenceRange(ranges);
    if (best == null) best = ranges.get(0);

    if (!best.hasLow() && !best.hasHigh()) {
      return best.hasText()
          ? StringEscapeUtils.escapeXml11(best.getText())
          : CdaGeneratorConstants.UNKNOWN_VALUE;
    }

    String low = best.hasLow() ? CdaFhirUtilities.getStringForQuantity(best.getLow()) : "";
    String high = best.hasHigh() ? CdaFhirUtilities.getStringForQuantity(best.getHigh()) : "";
    return (low + " | " + high).trim();
  }

  /**
   * Performing organization of a lab result (#1079): the lab that ran the test.
   *
   * <p>Source: Observation.performer (Organization), falling back to DiagnosticReport.performer
   * (Organization). The first Organization found is used.
   *
   * <p>Target: observation/performer/assignedEntity/representedOrganization with name, phone and
   * address. Returns "" when no performing Organization can be resolved.
   */
  public static String getPerformingOrganizationXml(
      List<Reference> observationPerformerRefs,
      List<Reference> reportPerformerRefs,
      R4FhirData data) {

    if (data == null) {
      return "";
    }

    Organization org = findOrganization(observationPerformerRefs, data);
    if (org == null) {
      org = findOrganization(reportPerformerRefs, data);
    }

    if (org == null || (!org.hasName() && !org.hasAddress() && !org.hasTelecom())) {
      return "";
    }

    StringBuilder sb = new StringBuilder(600);

    sb.append(
        CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
            CdaGeneratorConstants.PERF_EL_NAME, CdaGeneratorConstants.DEFAULT_PERF_EL_TYPE_CODE));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));

    // assignedEntity/id is required by the CDA schema; the lab itself is identified below.
    sb.append(CdaGeneratorUtils.getNFXMLForII(CdaGeneratorConstants.NF_NI));

    // representedOrganization: name, telecom, addr (CDA Organization element order)
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.REP_ORG_EL_NAME));

    if (org.hasName()) {
      sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.NAME_EL_NAME, org.getName()));
    }

    ContactPoint phone = pickContactPoint(org.getTelecom(), ContactPoint.ContactPointSystem.PHONE);
    if (phone != null) {
      String use =
          phone.hasUse()
              ? CdaGeneratorConstants.getCodeForTelecomUse(phone.getUse().toCode())
              : "WP";
      sb.append(
          CdaGeneratorUtils.getXmlForTelecom(
              CdaGeneratorConstants.TEL_EL_NAME, phone.getValue(), use, false));
    }

    if (org.hasAddress()) {
      sb.append(CdaFhirUtilities.getAddressXml(org.getAddress(), false));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.REP_ORG_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ASSIGNED_ENTITY_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PERF_EL_NAME));

    return sb.toString();
  }

  /** The first Organization among the references (all fetched Organizations are in the list). */
  static Organization findOrganization(List<Reference> refs, R4FhirData data) {

    if (refs == null) {
      return null;
    }

    for (Reference ref : refs) {

      if (ref == null || !ref.getReferenceElement().hasIdPart()) {
        continue;
      }

      String type = ref.getReferenceElement().getResourceType();
      if (type != null && !ResourceType.Organization.toString().equals(type)) {
        continue;
      }

      Organization org = data.getOrganizationForId(ref.getReferenceElement().getIdPart());
      if (org != null) {
        return org;
      }
    }

    return null;
  }

  /**
   * basedOn references to search for the Authorizing Provider of a DiagnosticReport organizer, in
   * the mapping sheet's priority: DiagnosticReport.basedOn first, then each result Observation's
   * basedOn. The first ServiceRequest whose requester resolves to a Practitioner wins.
   */
  static List<Reference> getAuthorizingProviderBasedOnRefs(
      DiagnosticReport report, List<Observation> observations) {

    List<Reference> refs = new ArrayList<>();
    if (report != null) {
      addServiceRequestReferences(refs, report.getBasedOn());
    }
    if (observations != null) {
      for (Observation obs : observations) {
        addServiceRequestReferences(refs, obs.getBasedOn());
      }
    }
    return refs;
  }

  /** Adds only the basedOn references that point to a ServiceRequest. */
  private static void addServiceRequestReferences(List<Reference> target, List<Reference> basedOn) {
    for (Reference ref : basedOn) {
      if (isServiceRequestReference(ref)) {
        target.add(ref);
      }
    }
  }

  /**
   * True when the reference points to a ServiceRequest: "ServiceRequest/{id}" (relative or
   * absolute), or a reference whose Reference.type is ServiceRequest. basedOn may also point to
   * CarePlan, MedicationRequest, ImmunizationRecommendation and others, which are skipped.
   */
  static boolean isServiceRequestReference(Reference ref) {

    if (ref == null || !ref.getReferenceElement().hasIdPart()) {
      return false;
    }

    String type = ref.getReferenceElement().getResourceType();
    if (type != null) {
      return ResourceType.ServiceRequest.toString().equals(type);
    }

    return ref.hasType() && ResourceType.ServiceRequest.toString().equals(ref.getType());
  }

  /**
   * Authorizing provider of a lab result (#1079): the clinician who ordered the test.
   *
   * <p>Source: DiagnosticReport/Observation.basedOn -> ServiceRequest.requester -> Practitioner, or
   * -> PractitionerRole.practitioner -> Practitioner. The first basedOn ServiceRequest whose
   * requester resolves to a Practitioner is used.
   *
   * <p>Target: organizer/participant[@typeCode='RESP']/participantRole[@classCode='PROV'] with the
   * NPI (id/@root=2.16.840.1.113883.4.6), address, phone, fax and playingEntity/name. Elements
   * without source data are left out; returns "" when no ordering practitioner can be resolved.
   */
  public static String getAuthorizingProviderParticipantXml(
      List<Reference> basedOnRefs, R4FhirData data) {

    if (basedOnRefs == null || basedOnRefs.isEmpty() || data == null) {
      return "";
    }

    Practitioner practitioner = findAuthorizingProvider(basedOnRefs, data);

    if (practitioner == null) {
      logger.info("No authorizing provider found via basedOn ServiceRequest.requester.");
      return "";
    }

    Identifier npi =
        CdaFhirUtilities.getIdentifierForSystem(
            practitioner.getIdentifier(), CdaGeneratorConstants.FHIR_NPI_URL);
    boolean hasNpi = npi != null && npi.hasValue();

    if (!hasNpi && !practitioner.hasName()) {
      logger.info(
          "Authorizing provider {} has neither an NPI nor a name; participant not added.",
          practitioner.getIdElement().getIdPart());
      return "";
    }

    return buildAuthorizingProviderParticipantXml(practitioner, hasNpi ? npi.getValue() : null);
  }

  /** Returns the ordering Practitioner of the first basedOn ServiceRequest that has one. */
  static Practitioner findAuthorizingProvider(List<Reference> basedOnRefs, R4FhirData data) {

    for (Reference ref : basedOnRefs) {

      ServiceRequest serviceRequest = findServiceRequest(data, ref);

      if (serviceRequest == null || !serviceRequest.hasRequester()) {
        continue;
      }

      Practitioner practitioner = resolveRequesterPractitioner(serviceRequest.getRequester(), data);

      if (practitioner != null) {
        return practitioner;
      }
    }

    return null;
  }

  /**
   * Ordering facility (#1079): the organization where the lab order originated, from
   * ServiceRequest.requester -> PractitionerRole.organization, or a requester that is itself an
   * Organization. A plain Practitioner requester has no facility.
   *
   * <p>Uses the first lab result (DiagnosticReports, then lab Observations) whose basedOn
   * ServiceRequest resolves to an ordering facility. Returns null when none does.
   */
  public static Organization findOrderingFacility(R4FhirData data) {

    if (data == null) {
      return null;
    }

    List<List<Reference>> basedOnLists = new ArrayList<>();
    if (data.getDiagReports() != null) {
      for (DiagnosticReport report : data.getDiagReports()) {
        basedOnLists.add(report.getBasedOn());
      }
    }
    if (data.getLabResults() != null) {
      for (Observation obs : data.getLabResults()) {
        basedOnLists.add(obs.getBasedOn());
      }
    }

    for (List<Reference> basedOnRefs : basedOnLists) {
      for (Reference ref : basedOnRefs) {
        ServiceRequest serviceRequest = findServiceRequest(data, ref);
        if (serviceRequest != null && serviceRequest.hasRequester()) {
          Organization facility = resolveRequesterOrganization(serviceRequest.getRequester(), data);
          if (facility != null) {
            return facility;
          }
        }
      }
    }

    return null;
  }

  /** ServiceRequest.requester -> PractitionerRole.organization, or an Organization requester. */
  static Organization resolveRequesterOrganization(Reference requester, R4FhirData data) {

    if (requester == null || !requester.getReferenceElement().hasIdPart()) {
      return null;
    }

    String type = requester.getReferenceElement().getResourceType();
    String id = requester.getReferenceElement().getIdPart();

    if (ResourceType.Organization.toString().equals(type)) {
      return data.getOrganizationForId(id);
    }

    if (type == null || ResourceType.PractitionerRole.toString().equals(type)) {
      PractitionerRole role = data.getPractitionerRoleById(id);
      if (role != null
          && role.hasOrganization()
          && role.getOrganization().getReferenceElement().hasIdPart()) {
        return data.getOrganizationForId(role.getOrganization().getReferenceElement().getIdPart());
      }
    }

    return null;
  }

  /** A basedOn ServiceRequest from the ServiceRequest list, or from the bundle. */
  static ServiceRequest findServiceRequest(R4FhirData data, Reference ref) {

    if (!isServiceRequestReference(ref)) {
      return null;
    }

    String id = ref.getReferenceElement().getIdPart();
    ServiceRequest serviceRequest = data.getServiceRequestById(id);
    return serviceRequest != null
        ? serviceRequest
        : (ServiceRequest) findInBundle(data, ResourceType.ServiceRequest, id);
  }

  /**
   * ServiceRequest.requester -> Practitioner, directly or through PractitionerRole.practitioner.
   */
  static Practitioner resolveRequesterPractitioner(Reference requester, R4FhirData data) {

    if (requester == null || !requester.getReferenceElement().hasIdPart()) {
      return null;
    }

    String type = requester.getReferenceElement().getResourceType();
    String id = requester.getReferenceElement().getIdPart();

    if (type == null || ResourceType.Practitioner.toString().equals(type)) {
      Practitioner practitioner = data.getPractitionerById(id);
      if (practitioner != null) {
        return practitioner;
      }
    }

    if (type == null || ResourceType.PractitionerRole.toString().equals(type)) {
      PractitionerRole role = data.getPractitionerRoleById(id);
      if (role != null
          && role.hasPractitioner()
          && role.getPractitioner().hasReferenceElement()
          && role.getPractitioner().getReferenceElement().hasIdPart()) {
        return data.getPractitionerById(role.getPractitioner().getReferenceElement().getIdPart());
      }
    }

    return null;
  }

  /**
   * Looks up a resource in the R4FhirData bundle. Only needed for ServiceRequests reached through
   * basedOn, which are kept out of the ServiceRequest list so they do not appear in Plan of
   * Treatment.
   */
  static Resource findInBundle(R4FhirData data, ResourceType type, String id) {

    if (id == null || data.getData() == null) {
      return null;
    }

    for (Bundle.BundleEntryComponent entry : data.getData().getEntry()) {
      Resource r = entry.getResource();
      if (r != null && r.getResourceType() == type && id.equals(r.getIdElement().getIdPart())) {
        return r;
      }
    }
    return null;
  }

  /** Produces the participant block; element order follows the CDA ParticipantRole schema. */
  private static String buildAuthorizingProviderParticipantXml(Practitioner pr, String npi) {

    StringBuilder sb = new StringBuilder(800);

    sb.append(
        CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
            CdaGeneratorConstants.PARTICIPANT_EL_NAME, AUTHORIZING_PROVIDER_TYPE_CODE));
    sb.append(CdaGeneratorUtils.getXmlForParticipantRole(AUTHORIZING_PROVIDER_CLASS_CODE));

    // id (NPI)
    if (npi != null) {
      sb.append(CdaGeneratorUtils.getXmlForII(CdaGeneratorConstants.AUTHOR_NPI_AA, npi));
    }

    // addr
    if (pr.hasAddress()) {
      sb.append(CdaFhirUtilities.getAddressXml(pr.getAddress(), false));
    }

    // telecom: phone (use defaults to WP) and fax
    sb.append(getAuthorizingProviderTelecomXml(pr.getTelecom()));

    // playingEntity/name
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.PLAYING_ENTITY));
    if (pr.hasName()) {
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.NAME_EL_NAME));
      sb.append(CdaFhirUtilities.getNameXml(pr.getName(), false));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.NAME_EL_NAME));
    }
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PLAYING_ENTITY));

    sb.append(
        CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_ROLE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PARTICIPANT_EL_NAME));

    return sb.toString();
  }

  /** One phone (work preferred, use defaults to WP) and one fax number, when present. */
  static String getAuthorizingProviderTelecomXml(List<ContactPoint> telecoms) {

    StringBuilder sb = new StringBuilder(200);

    ContactPoint phone = pickContactPoint(telecoms, ContactPoint.ContactPointSystem.PHONE);
    if (phone != null) {
      String use =
          phone.hasUse()
              ? CdaGeneratorConstants.getCodeForTelecomUse(phone.getUse().toCode())
              : "WP";
      sb.append(
          CdaGeneratorUtils.getXmlForTelecom(
              CdaGeneratorConstants.TEL_EL_NAME, phone.getValue(), use, false));
    }

    ContactPoint fax = pickContactPoint(telecoms, ContactPoint.ContactPointSystem.FAX);
    if (fax != null) {
      sb.append(
          CdaGeneratorUtils.getXmlForTelecom(
              CdaGeneratorConstants.TEL_EL_NAME, fax.getValue(), "", true));
    }

    return sb.toString();
  }

  /** The work contact point of the given system if there is one, otherwise the first one. */
  private static ContactPoint pickContactPoint(
      List<ContactPoint> telecoms, ContactPoint.ContactPointSystem system) {

    if (telecoms == null) {
      return null;
    }

    ContactPoint first = null;
    for (ContactPoint cp : telecoms) {
      if (cp.getSystem() == system && StringUtils.isNotBlank(cp.getValue())) {
        if (cp.getUse() == ContactPoint.ContactPointUse.WORK) {
          return cp;
        }
        if (first == null) {
          first = cp;
        }
      }
    }
    return first;
  }
}
