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
import org.hl7.fhir.r4.model.*;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaPlanOfTreatmentGenerator {

  private static final Logger logger = LoggerFactory.getLogger(CdaPlanOfTreatmentGenerator.class);

  public static final String SERVICE_REQUEST_LAB = "108252007";
  public static final String SERVICE_REQUEST_IMAGING = "363679005";
  public static final String SERVICE_REQUEST_COUNSELLING = "409063005";
  public static final String SERVICE_REQUEST_EDUCATION = "409073007";
  public static final String SERVICE_REQUEST_PROCEDURE = " 387713003";
  public static final String COMPLETED = "completed";
  private static final String DID_NOT_FIND_CODE_IN_MATCHED_CODES_MSG =
      "Did not find the code value in the matched codes, make it a regular Planned Observation";
  private static final String CODE_XML_DEBUG_MSG = "Code Xml = {}";
  private static final String USE_AUTHORED_DATE_MSG = "Use Authored Date";

  private CdaPlanOfTreatmentGenerator() {}

  public static String generatePlanOfTreatmentSection(
      R4FhirData data, LaunchDetails details, String version) {

    List<Medication> medList = data.getMedicationList();
    List<MedicationRequest> medReqs = getMedicationRequestsForVersion(version, data, medList);
    List<ServiceRequest> obsReqs = new ArrayList<>();
    List<ServiceRequest> procReqs = new ArrayList<>();
    getServiceRequestsForVersion(version, data, obsReqs, procReqs);
    List<DiagnosticReport> reports = getDiagnosticReportsForVersion(version, data);

    if (isAnyResourceListNonEmpty(obsReqs, reports, procReqs, medReqs)) {
      return generateNonEmptyPlanOfTreatmentSection(
          data, details, version, medList, medReqs, obsReqs, procReqs, reports);
    } else {
      return generateEmptyPlanOfTreatmentSection();
    }
  }

  private static boolean isAnyResourceListNonEmpty(
      List<ServiceRequest> obsReqs,
      List<DiagnosticReport> reports,
      List<ServiceRequest> procReqs,
      List<MedicationRequest> medReqs) {
    return (obsReqs != null && !obsReqs.isEmpty())
        || (reports != null && !reports.isEmpty())
        || (procReqs != null && !procReqs.isEmpty())
        || (medReqs != null && !medReqs.isEmpty());
  }

  private static List<MedicationRequest> getMedicationRequestsForVersion(
      String version, R4FhirData data, List<Medication> medList) {
    if (CdaGeneratorConstants.CDA_EICR_VERSION_R31.equals(version)) {
      return CdaMedicationGenerator.getValidMedicationRequestsForPlanOfTreatment(data, medList);
    } else {
      return CdaMedicationGenerator.getValidMedicationRequests(data, medList);
    }
  }

  private static void getServiceRequestsForVersion(
      String version,
      R4FhirData data,
      List<ServiceRequest> obsReqs,
      List<ServiceRequest> procReqs) {
    if (CdaGeneratorConstants.CDA_EICR_VERSION_R11.equals(version)) {
      obsReqs.addAll(
          data.getServiceRequests() != null ? data.getServiceRequests() : new ArrayList<>());
    } else {
      sortServiceRequestsByType(data, obsReqs, procReqs);
    }
  }

  private static List<DiagnosticReport> getDiagnosticReportsForVersion(
      String version, R4FhirData data) {
    if (CdaGeneratorConstants.CDA_EICR_VERSION_R31.equals(version)) {
      return getValidDiagnosticOrders(data);
    } else {
      return data.getDiagReports() != null ? data.getDiagReports() : new ArrayList<>();
    }
  }

  private static String generateNonEmptyPlanOfTreatmentSection(
      R4FhirData data,
      LaunchDetails details,
      String version,
      List<Medication> medList,
      List<MedicationRequest> medReqs,
      List<ServiceRequest> obsReqs,
      List<ServiceRequest> procReqs,
      List<DiagnosticReport> reports) {

    logger.info(
        "Found {} Observation ServiceRequest, {} Procedure ServiceRequest and {} LabOrder objects to translate to CDA.",
        obsReqs.size(),
        procReqs.size(),
        reports.size());

    StringBuilder sb = new StringBuilder(2000);
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID_EXT));
    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.CAREPLAN_SEC_TITLE));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

    List<String> tableHeaders = new ArrayList<>();
    tableHeaders.add(CdaGeneratorConstants.POT_OBS_TABLE_COL_1_TITLE);
    tableHeaders.add(CdaGeneratorConstants.POT_OBS_TABLE_COL_2_TITLE);
    sb.append(
        CdaGeneratorUtils.getXmlForTableHeader(
            tableHeaders, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    RowCounter rowCounter = new RowCounter();
    StringBuilder potObsXml = new StringBuilder();
    StringBuilder drXml = new StringBuilder();
    StringBuilder procXml = new StringBuilder();
    StringBuilder medReqXml = new StringBuilder();

    processObservationRequests(obsReqs, data, details, version, sb, potObsXml, rowCounter);
    processDiagnosticReports(reports, data, details, version, sb, drXml, rowCounter);
    processProcedureRequests(procReqs, data, details, sb, procXml, rowCounter);
    processMedicationRequests(medReqs, medList, data, details, version, sb, medReqXml, rowCounter);

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));
    sb.append(potObsXml);
    sb.append(drXml);
    sb.append(procXml);
    sb.append(medReqXml);
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  private static void processObservationRequests(
      List<ServiceRequest> obsReqs,
      R4FhirData data,
      LaunchDetails details,
      String version,
      StringBuilder tableRows,
      StringBuilder xmlEntries,
      RowCounter rowCounter) {

    for (ServiceRequest sr : obsReqs) {
      String displayName = CdaFhirUtilities.getStringForCodeableConcept(sr.getCode());
      String serviceDate = extractServiceRequestDate(sr);

      addPlanOfTreatmentTableRow(tableRows, displayName, serviceDate, rowCounter.getAndIncrement());

      String contentRef = buildContentRef(rowCounter.getCurrent());
      xmlEntries.append(getPlannedObservationXml(sr, details, contentRef, data, version));
    }
  }

  private static void processDiagnosticReports(
      List<DiagnosticReport> reports,
      R4FhirData data,
      LaunchDetails details,
      String version,
      StringBuilder tableRows,
      StringBuilder xmlEntries,
      RowCounter rowCounter) {

    for (DiagnosticReport dr : reports) {
      String displayName = CdaFhirUtilities.getStringForCodeableConcept(dr.getCode());
      String orderDate = extractDiagnosticReportDate(dr);

      addPlanOfTreatmentTableRow(tableRows, displayName, orderDate, rowCounter.getAndIncrement());

      String contentRef = buildContentRef(rowCounter.getCurrent());
      xmlEntries.append(getDiagnosticReportXml(dr, details, contentRef, data, version));
    }
  }

  private static void processProcedureRequests(
      List<ServiceRequest> procReqs,
      R4FhirData data,
      LaunchDetails details,
      StringBuilder tableRows,
      StringBuilder xmlEntries,
      RowCounter rowCounter) {

    for (ServiceRequest sr : procReqs) {
      String displayName = CdaFhirUtilities.getStringForCodeableConcept(sr.getCode());
      String serviceDate = extractServiceRequestDate(sr);

      addPlanOfTreatmentTableRow(tableRows, displayName, serviceDate, rowCounter.getAndIncrement());

      String contentRef = buildContentRef(rowCounter.getCurrent());
      xmlEntries.append(getPlannedProcedureXml(sr, details, contentRef, data));
    }
  }

  private static void processMedicationRequests(
      List<MedicationRequest> medReqs,
      List<Medication> medList,
      R4FhirData data,
      LaunchDetails details,
      String version,
      StringBuilder tableRows,
      StringBuilder xmlEntries,
      RowCounter rowCounter) {

    for (MedicationRequest mr : medReqs) {
      logger.info(" Adding medication requests ");
      String medDisplayName = extractMedicationDisplayName(mr, medList);
      MedicationDosageInfo dosageInfo = extractMedicationDosageInfo(mr);
      String startDateStr = extractMedicationStartDateString(dosageInfo.startDate);

      addPlanOfTreatmentTableRow(
          tableRows, medDisplayName, startDateStr, rowCounter.getAndIncrement());

      String contentRef = buildContentRef(rowCounter.getCurrent());
      xmlEntries.append(
          getPlannedMedicationXml(
              mr,
              details,
              contentRef,
              data,
              dosageInfo.startDate,
              dosageInfo.dosage,
              dosageInfo.dose,
              medList,
              details,
              version));
    }
  }

  private static String extractServiceRequestDate(ServiceRequest sr) {
    String serviceDate = CdaFhirUtilities.getStringForType(sr.getOccurrence());
    logger.debug("Service Date for display {} ", serviceDate);

    if (serviceDate.isEmpty() && sr.getAuthoredOnElement() != null) {
      serviceDate = CdaFhirUtilities.getDisplayStringForDateTimeType(sr.getAuthoredOnElement());
    }
    return serviceDate;
  }

  private static String extractDiagnosticReportDate(DiagnosticReport dr) {
    String orderDate = CdaFhirUtilities.getStringForType(dr.getEffective());
    logger.debug("Order Date for display {} ", orderDate);

    if (orderDate.isEmpty() && dr.getIssued() != null) {
      logger.info("Order Date is empty");
    }
    return orderDate;
  }

  private static String extractMedicationDisplayName(
      MedicationRequest mr, List<Medication> medList) {
    if (mr.hasMedication() && mr.getMedication() != null) {
      return CdaFhirUtilities.getStringForMedicationType(mr, medList);
    }
    return CdaGeneratorConstants.UNKNOWN_VALUE;
  }

  private static MedicationDosageInfo extractMedicationDosageInfo(MedicationRequest mr) {
    DateTimeType startDate = null;
    Dosage dosage = null;
    Quantity dose = null;

    if (mr.hasDosageInstruction() && mr.getDosageInstructionFirstRep() != null) {
      dosage = mr.getDosageInstructionFirstRep();

      if (dosage.hasTiming() && dosage.getTiming() != null) {
        Timing t = mr.getDosageInstructionFirstRep().getTiming();
        if (t != null
            && t.hasRepeat()
            && t.getRepeat() != null
            && t.getRepeat().hasBoundsPeriod()
            && t.getRepeat().getBoundsPeriod() != null
            && t.getRepeat().getBoundsPeriod().hasStartElement()) {
          startDate = t.getRepeat().getBoundsPeriod().getStartElement();
        }
      }

      if (dosage.hasDoseAndRate()
          && dosage.getDoseAndRateFirstRep() != null
          && dosage.getDoseAndRateFirstRep().hasDoseQuantity()) {
        dose = dosage.getDoseAndRateFirstRep().getDoseQuantity();
      }
    }

    if (startDate == null && mr.hasAuthoredOn() && mr.getAuthoredOnElement() != null) {
      startDate = mr.getAuthoredOnElement();
    }

    return new MedicationDosageInfo(startDate, dosage, dose);
  }

  private static String extractMedicationStartDateString(DateTimeType startDate) {
    if (startDate != null) {
      return CdaFhirUtilities.getDisplayStringForDateTimeType(startDate);
    } else {
      logger.error(
          " Dosage field does not have a valid period either due to datetime or timezone being null ");
      return CdaGeneratorConstants.UNKNOWN_VALUE;
    }
  }

  private static void addPlanOfTreatmentTableRow(
      StringBuilder sb, String displayName, String date, int rowNum) {
    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(CdaGeneratorConstants.POT_OBS_TABLE_COL_1_BODY_CONTENT, displayName);
    bodyvals.put(CdaGeneratorConstants.POT_OBS_TABLE_COL_2_BODY_CONTENT, date);
    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
  }

  private static String buildContentRef(int rowNum) {
    return CdaGeneratorConstants.POT_OBS_TABLE_COL_1_BODY_CONTENT + Integer.toString(rowNum);
  }

  /** Helper class to track row counter and support pre/post increment patterns */
  private static class RowCounter {
    private int current = 1;

    int getAndIncrement() {
      return current++;
    }

    int getCurrent() {
      return current - 1;
    }
  }

  /** Helper class to encapsulate medication dosage information */
  private static class MedicationDosageInfo {
    final DateTimeType startDate;
    final Dosage dosage;
    final Quantity dose;

    MedicationDosageInfo(DateTimeType startDate, Dosage dosage, Quantity dose) {
      this.startDate = startDate;
      this.dosage = dosage;
      this.dose = dose;
    }
  }

  public static String getPlannedMedicationXml(
      MedicationRequest mr,
      LaunchDetails details,
      String contentRef,
      R4FhirData data,
      DateTimeType startDate,
      Dosage dosage,
      Quantity dose,
      List<Medication> medList,
      LaunchDetails launchDetails,
      String version) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.MED_ACT_EL_NAME,
            CdaGeneratorConstants.MED_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_INT));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLAN_OF_CARE_SUBS_ADM_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLAN_OF_CARE_SUBS_ADM_TEMPLATE_ID,
            CdaGeneratorConstants.PLAN_OF_CARE_SUBS_ADM_TEMPLATE_ID_EXT));

    // add Id
    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), mr.getIdElement().getIdPart()));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    // Setup Effective Start / End times
    if (startDate != null) {
      String val =
          CdaGeneratorUtils.getStringForDateTime(startDate.getValue(), startDate.getTimeZone());
      sb.append(
          CdaGeneratorUtils.getXmlForPartialValueIVLWithTS(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, val, CdaGeneratorConstants.TIME_LOW_EL_NAME));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForValueIVLWithTS(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, "", ""));
    }

    // Set up Effective Time for Frequency.
    String ds = "";
    String freqInHours = CdaGeneratorConstants.UNKNOWN_VALUE;
    if (dosage != null) {

      if (dosage.hasDoseAndRate()
          && dosage.getDoseAndRateFirstRep() != null
          && dosage.getDoseAndRateFirstRep().hasDose()
          && dosage.getDoseAndRateFirstRep().getDose() != null) {
        ds =
            CdaFhirUtilities.getXmlForType(
                dosage.getDoseAndRateFirstRep().getDose(),
                CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME,
                false);
      } else {
        ds =
            CdaFhirUtilities.getQuantityXml(
                dose, CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false);
      }

      if (dosage.hasTiming()
          && dosage.getTiming() != null
          && dosage.getTiming().hasRepeat()
          && dosage.getTiming().getRepeat() != null
          && dosage.getTiming().getRepeat().hasFrequency()) {

        freqInHours = Integer.toString(dosage.getTiming().getRepeat().getFrequency());
      }
    } else {
      ds =
          CdaFhirUtilities.getQuantityXml(dose, CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false);
    }

    // Add PIVL with operator Effective Time
    if (!CdaGeneratorConstants.UNKNOWN_VALUE.contentEquals(freqInHours)) {
      sb.append(
          CdaGeneratorUtils.getXmlForPIVLWithTS(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, freqInHours));
    }

    // add Dose quantity
    sb.append(ds);

    // add the consumable presentation.
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.CONSUMABLE_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForStartElementWithClassCode(
            CdaGeneratorConstants.MAN_PROD_EL_NAME, CdaGeneratorConstants.MANU_CLASS_CODE));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.CONSUMABLE_ENTRY_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.CONSUMABLE_ENTRY_TEMPLATE_ID,
            CdaGeneratorConstants.CONSUMABLE_ENTRY_TEMPLATE_ID_EXT));

    List<String> paths = new ArrayList<>();
    paths.add("MedicationRequest.medication");

    CodeableConcept medicationConcept =
        CdaFhirUtilities.getMedicationCodeableConcept(mr.getMedication(), data.getMedicationList());

    String codeXml = "";
    Pair<Boolean, String> codeXmlPair =
        CdaFhirUtilities.getMedicationCodeXml(
            launchDetails, medicationConcept, false, "", paths, version);

    if (codeXmlPair.getValue0() && !StringUtils.isEmpty(codeXmlPair.getValue1())) {
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.MEDICATION_ACTIVITY_TRIGGER_TEMPLATE_ID,
              CdaGeneratorConstants.MEDICATION_ACTIVITY_TRIGGER_TEMPLATE_ID_EXT_31));
      codeXml = codeXmlPair.getValue1();
    } else {

      codeXml =
          CdaFhirUtilities.getXmlForMedicationTypeForCodeSystem(
              mr.getMedication(),
              CdaGeneratorConstants.CODE_EL_NAME,
              false,
              CdaGeneratorConstants.FHIR_RXNORM_URL,
              false,
              mr,
              medList);

      if (!codeXml.isEmpty()) {
        codeXml = codeXml;
      } else {
        codeXml =
            CdaFhirUtilities.getXmlForTypeForCodeSystem(
                mr.getMedication(),
                CdaGeneratorConstants.CODE_EL_NAME,
                false,
                CdaGeneratorConstants.FHIR_RXNORM_URL,
                true);
      }
    }
    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));
    sb.append(codeXml);
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MAN_PROD_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CONSUMABLE_EL_NAME));

    // End Tags for Entries
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MED_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String getPlannedObservationXml(
      ServiceRequest sr,
      LaunchDetails details,
      String contentRef,
      R4FhirData data,
      String version) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_RQO));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID_EXT));

    List<String> matchedTriggerCodes =
        CdaFhirUtilities.getMatchedCodesForResourceAndUrl(
            details, "ServiceRequest", CdaGeneratorConstants.FHIR_LOINC_URL);

    String codeXml = "";
    // Add Trigger code template if the code matched the Url in the Service Request.
    if (matchedTriggerCodes != null && !matchedTriggerCodes.isEmpty()) {
      logger.info("Found a Matched Code that is for Service Request");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromCodeableConceptForCodeSystem(
              matchedTriggerCodes, sr.getCode(), CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty()) {

        if (version.contentEquals("CDA_R31")) {
          sb.append(
              CdaGeneratorUtils.getXmlForTemplateId(
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE,
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT_31));
        } else {
          sb.append(
              CdaGeneratorUtils.getXmlForTemplateId(
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE,
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT));
        }

        codeXml =
            CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                mCd,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                details.getRctcOid(),
                details.getRctcVersion(),
                "",
                contentRef);
      } else {
        logger.info(DID_NOT_FIND_CODE_IN_MATCHED_CODES_MSG);
      }
    }

    // add Id
    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), sr.getIdElement().getIdPart()));

    if (codeXml.isEmpty()) {

      if (sr.hasCode() && sr.getCode().hasCoding()) {

        logger.debug("Find the Loinc Code as a priority first for Lab Results");
        codeXml =
            CdaFhirUtilities.getCodingXmlForCodeSystem(
                sr.getCode().getCoding(),
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_LOINC_URL,
                false,
                contentRef);

        logger.debug(CODE_XML_DEBUG_MSG, codeXml);

        if (!codeXml.isEmpty()) {
          sb.append(codeXml);
        } else {
          sb.append(
              CdaFhirUtilities.getCodingXml(
                  sr.getCode().getCoding(), CdaGeneratorConstants.CODE_EL_NAME, ""));
        }
      } else if (sr.hasCode() && sr.getCode().hasText()) {

        // Add text value for code in dynamic data
        sb.append(
            CdaGeneratorUtils.getXmlForNullCDWithText(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.NF_OTH,
                sr.getCode().getText()));
      } else {
        // Add null flavor for code in dynamic data
        sb.append(CdaFhirUtilities.getCodingXml(null, CdaGeneratorConstants.CODE_EL_NAME, ""));
      }
    } else {
      sb.append(codeXml);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    Pair<Date, TimeZone> effDate = CdaFhirUtilities.getActualDate(sr.getOccurrence());
    if (effDate.getValue0() == null) {
      logger.debug(USE_AUTHORED_DATE_MSG);

      effDate.setAt0(sr.getAuthoredOn());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate.getValue0(), effDate.getValue1()));

    if (sr.hasRequester()) {
      List<Reference> reqPerfs = new ArrayList<>();
      reqPerfs.add(sr.getRequester());
      sb.append(CdaFhirUtilities.getXmlForAuthor(reqPerfs, data));
    }

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String getDiagnosticReportXml(
      DiagnosticReport dr,
      LaunchDetails details,
      String contentRef,
      R4FhirData data,
      String version) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_RQO));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID_EXT));

    List<String> matchedTriggerCodes =
        CdaFhirUtilities.getMatchedCodesForResourceAndUrl(
            details, "DiagnosticReport", CdaGeneratorConstants.FHIR_LOINC_URL);

    String codeXml = "";
    // Add Trigger code template if the code matched the Url in the Service Request.
    if (matchedTriggerCodes != null && !matchedTriggerCodes.isEmpty()) {
      logger.info("Found a Matched Code that is for DiagnosticReport");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromCodeableConceptForCodeSystem(
              matchedTriggerCodes, dr.getCode(), CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty()) {

        if (version.contentEquals("CDA_R31")) {
          sb.append(
              CdaGeneratorUtils.getXmlForTemplateId(
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE,
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT_31));
        } else {
          sb.append(
              CdaGeneratorUtils.getXmlForTemplateId(
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE,
                  CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT));
        }

        codeXml =
            CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                mCd,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                details.getRctcOid(),
                details.getRctcVersion(),
                "",
                contentRef);
      } else {
        logger.debug(DID_NOT_FIND_CODE_IN_MATCHED_CODES_MSG);
      }
    }

    // add Id
    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), dr.getIdElement().getIdPart()));

    if (codeXml.isEmpty()) {
      if (dr.hasCode() && dr.getCode().hasCoding()) {

        logger.debug("Find the Loinc Code as a priority first for Lab Results");
        codeXml =
            CdaFhirUtilities.getCodingXmlForCodeSystem(
                dr.getCode().getCoding(),
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_LOINC_URL,
                false,
                contentRef);

        logger.debug(CODE_XML_DEBUG_MSG, codeXml);

        if (!codeXml.isEmpty()) {
          sb.append(codeXml);
        } else {
          sb.append(
              CdaFhirUtilities.getCodingXml(
                  dr.getCode().getCoding(), CdaGeneratorConstants.CODE_EL_NAME, ""));
        }
      } else if (dr.hasCode() && dr.getCode().hasText()) {

        // Add text value for code in dynamic data
        sb.append(
            CdaGeneratorUtils.getXmlForNullCDWithText(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.NF_OTH,
                dr.getCode().getText()));
      } else {
        // Add null flavor for code in dynamic data
        sb.append(CdaFhirUtilities.getCodingXml(null, CdaGeneratorConstants.CODE_EL_NAME, ""));
      }
    } else {
      sb.append(codeXml);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    Pair<Date, TimeZone> effDate = CdaFhirUtilities.getActualDate(dr.getEffective());
    if (effDate.getValue0() == null) {
      logger.debug(USE_AUTHORED_DATE_MSG);

      effDate.setAt0(dr.getIssued());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate.getValue0(), effDate.getValue1()));

    sb.append(getPerformerXml(dr.getPerformer(), data));

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static List<ServiceRequest> getValidServiceRequests(R4FhirData data) {

    List<ServiceRequest> sr = new ArrayList<>();

    if (data.getServiceRequests() != null && !data.getServiceRequests().isEmpty()) {

      logger.debug(
          "Total num of Service Requests available for Patient {}",
          data.getServiceRequests().size());

      for (ServiceRequest s : data.getServiceRequests()) {

        if (s.getCode() != null
            && s.getCode().getCoding() != null
            && !s.getCode().getCoding().isEmpty()
            && Boolean.TRUE.equals(
                CdaFhirUtilities.isCodingPresentForCodeSystem(
                    s.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL))) {

          logger.debug("Found a Service Request with a LOINC code");
          sr.add(s);
        } else {
          logger.info(
              " Ignoring Service Request with id {} as it is not having a loinc code ",
              s.getIdElement().getIdPart());
        }
      }
    } else {
      logger.info("No Valid Service Requests in the bundle to process");
    }

    return sr;
  }

  public static String generateEmptyPlanOfTreatmentSection() {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    // Add Plan of Treatment Template Id
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.CAREPLAN_SEC_TITLE));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Plan Of Treatment Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static List<DiagnosticReport> getValidDiagnosticOrders(R4FhirData data) {

    List<DiagnosticReport> drs = new ArrayList<>();

    if (data.getDiagReports() != null && !data.getDiagReports().isEmpty()) {

      logger.info(
          "Total num of Diagnostic Reports available for Patient {}", data.getDiagReports().size());

      for (DiagnosticReport dr : data.getDiagReports()) {

        if (!dr.hasResult()) {
          logger.debug("Found a DiagnosticReport to be added");
          drs.add(dr);
        } else {
          logger.info(" Ignoring Diagnostic Report with id {} ", dr.getIdElement().getIdPart());
        }
      }
    } else {
      logger.debug("No Valid DiagnosticReport in the bundle to process");
    }

    return drs;
  }

  public static void sortServiceRequestsByType(
      R4FhirData data, List<ServiceRequest> obsRequests, List<ServiceRequest> procRequests) {

    List<ServiceRequest> prs = new ArrayList<>();

    if (data.getServiceRequests() != null && !data.getServiceRequests().isEmpty()) {

      logger.info(
          "Total num of ServiceRequests available for Patient {}", data.getDiagReports().size());

      for (ServiceRequest sr : data.getServiceRequests()) {

        if ((sr.hasCode()
            && sr.getCode().hasCoding()
            && Boolean.TRUE.equals(
                CdaFhirUtilities.isCodingPresentForCodeSystem(
                    sr.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL)))) {

          logger.debug("Found a ServiceRequest to be added as an Observation");
          obsRequests.add(sr);
        } else if ((sr.hasCode()
            && sr.getCode().hasCoding()
            && Boolean.TRUE.equals(
                CdaFhirUtilities.isCodingPresentForCodeSystem(
                    sr.getCode().getCoding(), CdaGeneratorConstants.FHIR_CPT_URL)))) {

          logger.debug("Found a ServiceRequest to be added as a Procedure");
          procRequests.add(sr);
        } else {
          logger.info(" Ignoring ServiceRequest with id {} ", sr.getIdElement().getIdPart());
        }
      }
    } else {
      logger.debug("No Valid ServiceRequest in the bundle to process");
    }
  }

  public static String getPlannedProcedureXml(
      ServiceRequest sr, LaunchDetails details, String contentRef, R4FhirData data) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.PROC_ACT_EL_NAME,
            CdaGeneratorConstants.PROC_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_RQO));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLAN_OF_CARE_ACTIVITY_PROC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLAN_OF_CARE_ACTIVITY_PROC_TEMPLATE_ID,
            CdaGeneratorConstants.PLAN_OF_CARE_ACTIVITY_PROC_TEMPLATE_ID_EXT));

    List<String> matchedTriggerCodes =
        CdaFhirUtilities.getMatchedCodesForResourceAndUrl(
            details, "ServiceRequest", CdaGeneratorConstants.FHIR_CPT_URL);

    String codeXml = "";
    // Add Trigger code template if the code matched the Url in the Service Request.
    if (matchedTriggerCodes != null && !matchedTriggerCodes.isEmpty()) {
      logger.info("Found a Matched Code that is for Service Request");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromCodeableConceptForCodeSystem(
              matchedTriggerCodes, sr.getCode(), CdaGeneratorConstants.FHIR_CPT_URL);

      if (!mCd.isEmpty()) {

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.PLANNED_PROCEDURE_TRIGGER_CODE_TEMPLATE,
                CdaGeneratorConstants.PLANNED_PROCEDURE_TRIGGER_CODE_TEMPLATE_EXT));

        codeXml =
            CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                mCd,
                CdaGeneratorConstants.CPT_CODESYSTEM_OID,
                CdaGeneratorConstants.CPT_CODESYSTEM_NAME,
                details.getRctcOid(),
                details.getRctcVersion(),
                "",
                contentRef);
      } else {
        logger.info(DID_NOT_FIND_CODE_IN_MATCHED_CODES_MSG);
      }
    }

    // add Id
    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), sr.getIdElement().getIdPart()));

    if (codeXml.isEmpty()) {

      if (sr.hasCode() && sr.getCode().hasCoding()) {

        logger.debug("Find the CPT Code as a priority first for Procedure");
        codeXml =
            CdaFhirUtilities.getCodingXmlForCodeSystem(
                sr.getCode().getCoding(),
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.FHIR_CPT_URL,
                false,
                contentRef);

        logger.debug(CODE_XML_DEBUG_MSG, codeXml);

        if (!codeXml.isEmpty()) {
          sb.append(codeXml);
        } else {
          sb.append(
              CdaFhirUtilities.getCodingXml(
                  sr.getCode().getCoding(), CdaGeneratorConstants.CODE_EL_NAME, ""));
        }
      } else if (sr.hasCode() && sr.getCode().hasText()) {

        // Add text value for code in dynamic data
        sb.append(
            CdaGeneratorUtils.getXmlForNullCDWithText(
                CdaGeneratorConstants.CODE_EL_NAME,
                CdaGeneratorConstants.NF_OTH,
                sr.getCode().getText()));
      } else {
        // Add null flavor for code in dynamic data
        sb.append(CdaFhirUtilities.getCodingXml(null, CdaGeneratorConstants.CODE_EL_NAME, ""));
      }
    } else {
      sb.append(codeXml);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    Pair<Date, TimeZone> effDate = CdaFhirUtilities.getActualDate(sr.getOccurrence());
    if (effDate.getValue0() == null) {
      logger.debug(USE_AUTHORED_DATE_MSG);

      effDate.setAt0(sr.getAuthoredOn());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate.getValue0(), effDate.getValue1()));

    if (sr.hasRequester()) {
      List<Reference> reqPerfs = new ArrayList<>();
      reqPerfs.add(sr.getRequester());
      sb.append(CdaFhirUtilities.getXmlForAuthor(reqPerfs, data));
    }

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.PROC_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static Object getPlannedActXml(
      ServiceRequest sa, LaunchDetails details, String contentRef, String version) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.ACT_EL_NAME,
            CdaGeneratorConstants.PROC_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_RQO));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PLAN_OF_CARE_ACT_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLAN_OF_CARE_ACT_TEMPLATE_ID,
            CdaGeneratorConstants.PLAN_OF_CARE_ACT_TEMPLATE_ID_EXT));

    // add Id
    sb.append(
        CdaGeneratorUtils.getXmlForII(
            details.getAssigningAuthorityId(), sa.getIdElement().getIdPart()));

    List<CodeableConcept> ccs = new ArrayList<>();
    ccs.add(sa.getCode());
    sb.append(
        CdaFhirUtilities.getCodeableConceptXml(ccs, CdaGeneratorConstants.CODE_EL_NAME, false));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    Pair<Date, TimeZone> effDate = CdaFhirUtilities.getActualDate(sa.getOccurrence());
    if (effDate.getValue0() == null) {
      logger.debug(USE_AUTHORED_DATE_MSG);

      effDate.setAt0(sa.getAuthoredOn());
    }

    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(
            CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate.getValue0(), effDate.getValue1()));

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static Boolean isPlannedObservation(ServiceRequest sr) {

    Boolean retVal = false;

    if (sr.hasCategory()) {

      List<CodeableConcept> cds = sr.getCategory();

      if (CdaFhirUtilities.isCodePresent(
          cds, SERVICE_REQUEST_LAB, CdaGeneratorConstants.FHIR_SNOMED_URL)) retVal = true;

      if (CdaFhirUtilities.isCodePresent(
          cds, SERVICE_REQUEST_IMAGING, CdaGeneratorConstants.FHIR_SNOMED_URL)) retVal = true;
    }

    return retVal;
  }

  public static Boolean isPlannedAct(ServiceRequest sr) {

    Boolean retVal = false;

    if (sr.hasCategory()) {

      List<CodeableConcept> cds = sr.getCategory();

      if (CdaFhirUtilities.isCodePresent(
          cds, SERVICE_REQUEST_EDUCATION, CdaGeneratorConstants.FHIR_SNOMED_URL)) retVal = true;

      if (CdaFhirUtilities.isCodePresent(
          cds, SERVICE_REQUEST_COUNSELLING, CdaGeneratorConstants.FHIR_SNOMED_URL)) retVal = true;
    }

    return retVal;
  }

  public static Boolean isPlannedProcedure(ServiceRequest sr) {

    Boolean retVal = false;

    if (sr.hasCategory()) {

      List<CodeableConcept> cds = sr.getCategory();

      if (CdaFhirUtilities.isCodePresent(
          cds, SERVICE_REQUEST_PROCEDURE, CdaGeneratorConstants.FHIR_SNOMED_URL)) retVal = true;
    }

    return retVal;
  }

  public static String getPerformerXml(List<Reference> performerRefs, R4FhirData data) {
    if (performerRefs == null || performerRefs.isEmpty()) {
      return "";
    }

    StringBuilder sb = new StringBuilder();
    String functionCode = "";

    for (Reference reference : performerRefs) {
      if (reference.hasReferenceElement() && reference.getReferenceElement().hasIdPart()) {
        String idPart = reference.getReferenceElement().getIdPart();

        if (CdaFhirUtilities.isResourceOfType(reference, ResourceType.Practitioner)) {
          Practitioner practitioner = data.getPractitionerById(idPart);
          if (practitioner != null) {
            sb.append(CdaFhirUtilities.getPerformerXml(practitioner, functionCode, null));
          }
        } else if (CdaFhirUtilities.isResourceOfType(reference, ResourceType.Organization)) {
          Organization organization = data.getOrganizationForId(idPart);
          if (organization != null) {
            sb.append(CdaFhirUtilities.getPerformerXml(null, functionCode, organization));
          }
        }
      }
    }

    return sb.toString();
  }
}
