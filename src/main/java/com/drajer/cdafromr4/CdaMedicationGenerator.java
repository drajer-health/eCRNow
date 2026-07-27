package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.*;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaMedicationGenerator {

  public static final String COMPLETED = "completed";
  private static final String UNKNOWN_VALUE = CdaGeneratorConstants.UNKNOWN_VALUE;
  private static final String MEDICATIONS_FOUND_LOG_MSG = "Medications found for processing ";
  private static final String FOUND_CONTAINED_REFERENCE_MSG = " Found a Contained Reference ";
  private static final String EXAMINE_CONTAINED_MEDS_MSG = "starting to examine contained meds ";
  private static final String ADDING_MED_REQ_DUE_TO_CODE_MSG = "Adding Med Req - due to code ";
  private static final String EXAMINE_CONTAINED_INGREDIENTS_MSG =
      "starting to examine contained ingredients ";
  private static final String ADDING_MED_REQ_DUE_TO_INGREDIENT_MSG =
      "Adding Med Req due to ingredient ";
  private static final String FOUND_EXTERNAL_MED_REF_MSG =
      " Found an External Medication Reference ";
  private static final String ADDING_MED_AND_MED_REQ_DUE_TO_CODE_MSG =
      "Adding Medication and MedicationRequest - due to code ";
  private static final String NO_CMEDS_TO_COMPARE_MSG =
      " No cmeds to compare and extract medications ";
  private static final String FOUND_MED_CONCEPT_MSG = " Found a medication codeable concept ";
  private static final String FOUND_MED_REQ_WITH_RXNORM_CODE_MSG =
      "Found a Medication Request with a RxNorm code";
  private static final String NO_VALID_MED_REQUESTS_MSG =
      "No Valid Medication Requests in the bundle to process";

  private CdaMedicationGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaMedicationGenerator.class);

  /** Represents extracted dosage information from a Dosage object. */
  private static class DosageInfo {
    final String periodText;
    final Quantity dose;

    DosageInfo(String periodText, Quantity dose) {
      this.periodText = periodText;
      this.dose = dose;
    }
  }

  /** Represents extracted medication request information. */
  private static class MedicationRequestInfo {
    final Dosage dosage;
    final Quantity dose;
    final DosageInfo dosageInfo;
    final DateTimeType startDate;

    MedicationRequestInfo(
        Dosage dosage, Quantity dose, DosageInfo dosageInfo, DateTimeType startDate) {
      this.dosage = dosage;
      this.dose = dose;
      this.dosageInfo = dosageInfo;
      this.startDate = startDate;
    }
  }

  /** Represents medication status and mood code. */
  private static class StatusAndMoodCode {
    final String status;
    final String moodCode;

    StatusAndMoodCode(String status, String moodCode) {
      this.status = status;
      this.moodCode = moodCode;
    }
  }

  /**
   * Extracts dosage period text and dose quantity from a Dosage object.
   *
   * @param dosage the dosage to extract from
   * @return DosageInfo with period text and dose, or both as UNKNOWN_VALUE/null if not present
   */
  private static DosageInfo extractDosageInfo(Dosage dosage) {
    String periodText = UNKNOWN_VALUE;
    Quantity dose = null;

    if (dosage == null) {
      return new DosageInfo(periodText, dose);
    }

    // Extract timing period
    if (dosage.hasTiming() && dosage.getTiming() != null) {
      Timing t = dosage.getTiming();
      if (t.hasRepeat()) {
        Timing.TimingRepeatComponent repeat = t.getRepeat();
        String period = repeat.hasPeriod() ? repeat.getPeriod().toString() : null;
        String periodUnit = repeat.hasPeriodUnit() ? repeat.getPeriodUnit().toString() : null;
        String frequency = repeat.hasFrequency() ? String.valueOf(repeat.getFrequency()) : null;
        periodText = CdaFhirUtilities.getNarrative(frequency, period, periodUnit);
      }
    }

    // Extract dose quantity
    if (dosage.hasDoseAndRate()
        && dosage.getDoseAndRateFirstRep() != null
        && dosage.getDoseAndRateFirstRep().hasDoseQuantity()) {
      dose = dosage.getDoseAndRateFirstRep().getDoseQuantity();
    }

    return new DosageInfo(periodText, dose);
  }

  /**
   * Adds a medication table row to the StringBuilder.
   *
   * @param sb the StringBuilder to append to
   * @param medDisplayName the medication name
   * @param dateTime the effective date/time
   * @param dosageText the dosage text
   * @param periodText the period text
   * @param rowNum the row number
   */
  private static void addMedicationTableRowToSection(
      StringBuilder sb,
      String medDisplayName,
      String dateTime,
      String dosageText,
      String periodText,
      int rowNum) {
    String medicationDosagePeriodText = dosageText + CdaGeneratorConstants.PIPE + periodText;

    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
    bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dateTime);
    bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_3_BODY_CONTENT, medicationDosagePeriodText);

    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
  }

  /**
   * Adds a medication table row with extracted dosage info.
   *
   * @param sb the StringBuilder to append to
   * @param medDisplayName the medication name
   * @param dateTime the effective date/time
   * @param dosageInfo the extracted dosage info
   * @param rowNum the row number
   */
  private static void addMedicationTableRowToSection(
      StringBuilder sb, String medDisplayName, String dateTime, DosageInfo dosageInfo, int rowNum) {
    String dosageText = CdaFhirUtilities.getStringForQuantity(dosageInfo.dose);
    addMedicationTableRowToSection(
        sb, medDisplayName, dateTime, dosageText, dosageInfo.periodText, rowNum);
  }

  /**
   * Extracts medication display name from a medication request.
   *
   * @param medReq the medication request
   * @param medList the medication list
   * @return the display name or UNKNOWN_VALUE
   */
  private static String extractMedicationDisplayName(
      MedicationRequest medReq, List<Medication> medList) {
    if (medReq.hasMedication() && medReq.getMedication() != null) {
      return CdaFhirUtilities.getStringForMedicationType(medReq, medList);
    }
    return UNKNOWN_VALUE;
  }

  /**
   * Extracts comprehensive dosage and timing information from a medication request.
   *
   * @param medReq the medication request
   * @return MedicationRequestInfo with dosage, dose, dosageInfo, and startDate
   */
  private static MedicationRequestInfo extractMedicationRequestInfo(MedicationRequest medReq) {
    Dosage dosage = null;
    Quantity dose = null;
    DosageInfo dosageInfo = new DosageInfo(UNKNOWN_VALUE, null);
    DateTimeType startDate = null;

    if (medReq.hasDosageInstruction() && medReq.getDosageInstructionFirstRep() != null) {
      dosage = medReq.getDosageInstructionFirstRep();
      startDate = extractStartDateFromDosage(dosage);
      dosageInfo = extractDosageInfo(dosage);
      dose = extractDoseFromDosage(dosage);
    }

    if (startDate == null && medReq.hasAuthoredOn() && medReq.getAuthoredOnElement() != null) {
      startDate = medReq.getAuthoredOnElement();
    }

    return new MedicationRequestInfo(dosage, dose, dosageInfo, startDate);
  }

  /**
   * Extracts start date from dosage timing bounds period.
   *
   * @param dosage the dosage
   * @return the start date or null if not present
   */
  private static DateTimeType extractStartDateFromDosage(Dosage dosage) {
    if (dosage.hasTiming()) {
      Timing t = dosage.getTiming();
      if (t != null && t.hasRepeat() && t.getRepeat().hasBoundsPeriod()) {
        Period boundsPeriod = t.getRepeat().getBoundsPeriod();
        if (boundsPeriod.hasStartElement()) {
          return boundsPeriod.getStartElement();
        }
      }
    }
    return null;
  }

  /**
   * Extracts dose quantity from dosage dose and rate.
   *
   * @param dosage the dosage
   * @return the dose quantity or null if not present
   */
  private static Quantity extractDoseFromDosage(Dosage dosage) {
    if (dosage.hasDoseAndRate()
        && dosage.getDoseAndRateFirstRep() != null
        && dosage.getDoseAndRateFirstRep().hasDoseQuantity()) {
      return dosage.getDoseAndRateFirstRep().getDoseQuantity();
    }
    return null;
  }

  /**
   * Extracts and formats date/time for display.
   *
   * @param startDate the start date
   * @return formatted datetime or UNKNOWN_VALUE
   */
  private static String extractDateTimeDisplay(DateTimeType startDate) {
    if (startDate != null) {
      return CdaFhirUtilities.getDisplayStringForDateTimeType(startDate);
    }
    logger.error(
        " Dosage field does not have a valid period either due to datetime or timezone being null ");
    return UNKNOWN_VALUE;
  }

  /**
   * Determines medication status and mood code from a medication request.
   *
   * @param medReq the medication request
   * @return StatusAndMoodCode with status and mood code
   */
  private static StatusAndMoodCode determineStatusAndMoodCode(MedicationRequest medReq) {
    String status = "active";
    String moodCode = CdaGeneratorConstants.MOOD_CODE_INT;

    if (medReq.hasStatus() && medReq.getStatus() != null) {
      status = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medReq.getStatus().toString());
      if (status.equalsIgnoreCase(COMPLETED)) {
        moodCode = CdaGeneratorConstants.MOOD_CODE_DEF;
      }
    }

    return new StatusAndMoodCode(status, moodCode);
  }

  /** Processes medication statements for the main medication section. */
  private static void processMedicationStatementsForSection(
      List<MedicationStatement> meds,
      List<Medication> medList,
      LaunchDetails details,
      String version,
      StringBuilder sb,
      StringBuilder medEntries,
      int[] rowNum) {
    if (meds == null || meds.isEmpty()) {
      return;
    }

    for (MedicationStatement med : meds) {
      String medDisplayName = UNKNOWN_VALUE;
      if (med.hasMedication() && med.getMedication() != null) {
        medDisplayName = CdaFhirUtilities.getStringForMedicationType(med, medList);
      }

      String dt = null;
      if (med.hasEffective() && med.getEffective() != null) {
        dt = CdaFhirUtilities.getStringForType(med.getEffective());
      }

      DosageInfo dosageInfo =
          UNKNOWN_VALUE.equals(dt)
              ? new DosageInfo(UNKNOWN_VALUE, null)
              : extractDosageInfo(med.hasDosage() ? med.getDosageFirstRep() : null);

      addMedicationTableRowToSection(sb, medDisplayName, dt, dosageInfo, rowNum[0]++);

      String medstatus = COMPLETED;
      if (med.hasStatus() && med.getStatus() != null) {
        medstatus = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(med.getStatus().toString());
      }

      Dosage dosage =
          med.hasDosage() && med.getDosageFirstRep() != null ? med.getDosageFirstRep() : null;

      medEntries.append(
          getEntryForMedication(
              med.getIdElement().getIdPart(),
              med.getMedication(),
              med.getEffective(),
              medstatus,
              dosage,
              details,
              null,
              null,
              CdaGeneratorConstants.MOOD_CODE_DEF,
              med,
              medList,
              version));
    }
  }

  /** Processes medication administrations for the main medication section. */
  private static void processMedicationAdministrationsForSection(
      List<MedicationAdministration> medAdms,
      List<Medication> medList,
      LaunchDetails details,
      String version,
      StringBuilder sb,
      StringBuilder medEntries,
      int[] rowNum) {
    if (medAdms == null || medAdms.isEmpty()) {
      return;
    }

    for (MedicationAdministration medAdm : medAdms) {
      String medDisplayName = UNKNOWN_VALUE;
      if (medAdm.hasMedication() && medAdm.getMedication() != null) {
        medDisplayName = CdaFhirUtilities.getStringForMedicationType(medAdm, medList);
      }

      String dt = null;
      if (medAdm.hasEffective() && medAdm.getEffective() != null) {
        dt = CdaFhirUtilities.getStringForType(medAdm.getEffective());
      }

      Quantity dose = null;
      if (medAdm.hasDosage() && medAdm.getDosage().hasDose()) {
        dose = medAdm.getDosage().getDose();
      }
      String dosageText = CdaFhirUtilities.getStringForQuantity(dose);

      addMedicationTableRowToSection(
          sb, medDisplayName, dt, dosageText, UNKNOWN_VALUE, rowNum[0]++);

      String medstatus = COMPLETED;
      if (medAdm.hasStatus() && medAdm.getStatus() != null) {
        medstatus =
            CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medAdm.getStatus().toCode());
      }

      medEntries.append(
          getEntryForMedication(
              medAdm.getIdElement().getIdPart(),
              medAdm.getMedication(),
              medAdm.getEffective(),
              medstatus,
              null,
              details,
              dose,
              null,
              CdaGeneratorConstants.MOOD_CODE_DEF,
              medAdm,
              medList,
              version));
    }
  }

  /** Processes medication requests for the main medication section. */
  private static void processMedicationRequestsForSection(
      List<MedicationRequest> medReqs,
      List<Medication> medList,
      LaunchDetails details,
      String version,
      StringBuilder sb,
      StringBuilder medEntries,
      int[] rowNum) {
    if (medReqs == null || medReqs.isEmpty()) {
      return;
    }

    for (MedicationRequest medReq : medReqs) {
      logger.info(" Adding medication requests ");
      String medDisplayName = extractMedicationDisplayName(medReq, medList);

      MedicationRequestInfo reqInfo = extractMedicationRequestInfo(medReq);
      String dt = extractDateTimeDisplay(reqInfo.startDate);
      addMedicationTableRowToSection(sb, medDisplayName, dt, reqInfo.dosageInfo, rowNum[0]++);

      StatusAndMoodCode statusInfo = determineStatusAndMoodCode(medReq);
      medEntries.append(
          getEntryForMedication(
              medReq.getIdElement().getIdPart(),
              medReq.getMedication(),
              null,
              statusInfo.status,
              reqInfo.dosage,
              details,
              reqInfo.dose,
              reqInfo.startDate,
              statusInfo.moodCode,
              medReq,
              medList,
              version));
    }
  }

  public static String generateMedicationSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder(2000);
    List<Medication> medList = data.getMedicationList();
    List<MedicationStatement> meds = data.getMedicationStatements();
    List<MedicationAdministration> medAdms = data.getMedicationAdministrations();
    List<MedicationRequest> medReqs = getValidMedicationRequests(data, medList);

    if ((meds != null && !meds.isEmpty())
        || (medAdms != null && !medAdms.isEmpty())
        || (medReqs != null && !medReqs.isEmpty())) {

      logger.info(MEDICATIONS_FOUND_LOG_MSG);
      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID_EXT));

      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.MED_ADM_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.MED_ADM_SEC_NAME));

      // add Title
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.MED_ADM_SEC_TITLE));

      // add Narrative Text
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.MED_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.MED_TABLE_COL_2_TITLE);
      list.add(CdaGeneratorConstants.MED_TABLE_COL_3_TITLE);

      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int[] rowNum = {1};
      StringBuilder medEntries = new StringBuilder();

      processMedicationStatementsForSection(
          meds, medList, details, version, sb, medEntries, rowNum);
      processMedicationAdministrationsForSection(
          medAdms, medList, details, version, sb, medEntries, rowNum);
      processMedicationRequestsForSection(
          medReqs, medList, details, version, sb, medEntries, rowNum);

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add Medication Entries
      sb.append(medEntries);

      // Complete the section end tags.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {
      sb.append(generateEmptyMedicationsAdministeredSection());
    }

    return sb.toString();
  }

  public static String getEntryForMedication(
      String id,
      Type medication,
      Type effectiveTime,
      String medStatus,
      Dosage dosage,
      LaunchDetails details,
      Quantity dose,
      DateTimeType startDate,
      String moodCode,
      DomainResource res,
      List<Medication> medList,
      String version) {

    logger.info(" Adding medication entry ");
    StringBuilder sb = new StringBuilder();

    // add the Entries.
    sb.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));

    // add the medication Act
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.MED_ACT_EL_NAME, CdaGeneratorConstants.MED_CLASS_CODE, moodCode));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.MED_ENTRY_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.MED_ENTRY_TEMPLATE_ID,
            CdaGeneratorConstants.MED_ENTRY_TEMPLATE_ID_EXT));

    sb.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id));

    // set status code
    sb.append(CdaGeneratorUtils.getXmlForCD(CdaGeneratorConstants.STATUS_CODE_EL_NAME, medStatus));

    // Set up Effective Time for start and End time.
    if (effectiveTime != null) {

      if (effectiveTime instanceof DateTimeType) {
        DateTimeType d = (DateTimeType) effectiveTime;
        String val = CdaGeneratorUtils.getStringForDateTime(d.getValue(), d.getTimeZone());
        sb.append(
            CdaGeneratorUtils.getXmlForPartialValueIVLWithTS(
                CdaGeneratorConstants.EFF_TIME_EL_NAME,
                val,
                CdaGeneratorConstants.TIME_LOW_EL_NAME));

      } else {
        sb.append(
            CdaFhirUtilities.getXmlForType(
                effectiveTime, CdaGeneratorConstants.EFF_TIME_EL_NAME, true));
      }
    } else if (startDate != null) {
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

    if (!CdaGeneratorConstants.UNKNOWN_VALUE.contentEquals(freqInHours)) {
      sb.append(
          CdaGeneratorUtils.getXmlForPIVLWithTS(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, freqInHours));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForPIVLWithTS(CdaGeneratorConstants.EFF_TIME_EL_NAME, null));
    }

    // Add Route Code
    if (dosage != null) {
      if (dosage.hasRoute() && dosage.getRoute().hasCoding()) {
        sb.append(
            CdaFhirUtilities.getCodeableConceptXml(
                dosage.getRoute(), CdaGeneratorConstants.ROUTE_CODE_EL_NAME, ""));
      }
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
    paths.add("MedicationAdministration.medication");
    paths.add("MedicationStatement.medication");
    CodeableConcept medicationConcept =
        CdaFhirUtilities.getMedicationCodeableConcept(medication, medList);
    String codeXml = "";
    Pair<Boolean, String> codeXmlPair =
        CdaFhirUtilities.getMedicationCodeXml(
            details, medicationConcept, false, "", paths, version);

    if (codeXmlPair.getValue0() && !StringUtils.isEmpty(codeXmlPair.getValue1())) {
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.MEDICATION_ACTIVITY_TRIGGER_TEMPLATE_ID,
              CdaGeneratorConstants.MEDICATION_ACTIVITY_TRIGGER_TEMPLATE_ID_EXT_31));
      codeXml = codeXmlPair.getValue1();
    } else {

      codeXml =
          CdaFhirUtilities.getXmlForMedicationTypeForCodeSystem(
              medication,
              CdaGeneratorConstants.CODE_EL_NAME,
              false,
              CdaGeneratorConstants.FHIR_RXNORM_URL,
              false,
              res,
              medList);

      if (!codeXml.isEmpty()) {
        codeXml = codeXml;
      } else {
        codeXml =
            CdaFhirUtilities.getXmlForTypeForCodeSystem(
                medication,
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

  public static String generateEmptyMedicationsAdministeredSection() {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.MED_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.MED_ADM_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.MED_ADM_SEC_NAME));

    // add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.MED_ADM_SEC_TITLE));

    // add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Medication Administered Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static List<MedicationRequest> getValidMedicationRequests(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationRequest> result = new ArrayList<>();

    if (data.getMedicationRequests() == null || data.getMedicationRequests().isEmpty()) {
      logger.debug(NO_VALID_MED_REQUESTS_MSG);
      return result;
    }

    logger.info(
        "Total num of Medication Requests available for Patient {}",
        data.getMedicationRequests().size());

    for (MedicationRequest m : data.getMedicationRequests()) {
      if (hasValidRxNormCode(m, cmeds)) {
        result.add(m);
      }
    }

    return result;
  }

  public static List<MedicationAdministration> getValidMedicationAdministrations(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationAdministration> result = new ArrayList<>();

    if (data.getMedicationAdministrations() == null
        || data.getMedicationAdministrations().isEmpty()) {
      logger.info(NO_VALID_MED_REQUESTS_MSG);
      return result;
    }

    logger.info(
        "Total num of Medication Administrations available for Patient {}",
        data.getMedicationAdministrations().size());

    for (MedicationAdministration m : data.getMedicationAdministrations()) {
      if (hasValidRxNormCode(m, cmeds)) {
        result.add(m);
      }
    }

    return result;
  }

  public static List<MedicationStatement> getValidMedicationStatements(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationStatement> result = new ArrayList<>();

    if (data.getMedicationStatements() == null || data.getMedicationStatements().isEmpty()) {
      logger.info(NO_VALID_MED_REQUESTS_MSG);
      return result;
    }

    logger.info(
        "Total num of Medication Statements available for Patient {}",
        data.getMedicationStatements().size());

    for (MedicationStatement m : data.getMedicationStatements()) {
      if (hasValidRxNormCode(m, cmeds)) {
        result.add(m);
      }
    }

    return result;
  }

  public static String generateR31MedicationsAdministeredSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder(2000);
    List<Medication> medList = data.getMedicationList();
    List<MedicationAdministration> medAdms = getValidMedicationAdministrations(data, medList);

    if (medAdms != null && !medAdms.isEmpty()) {

      logger.info(MEDICATIONS_FOUND_LOG_MSG);
      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID_EXT));

      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.MED_ADM_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.MED_ADM_SEC_NAME));

      // add Title
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.MED_ADM_SEC_TITLE));

      // add Narrative Text
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.MED_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.MED_TABLE_COL_2_TITLE);
      list.add(CdaGeneratorConstants.MED_TABLE_COL_3_TITLE);

      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // add Body Rows
      int rowNum = 1;
      StringBuilder medEntries = new StringBuilder();

      // Add Medication Administration
      for (MedicationAdministration medAdm : medAdms) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (medAdm.hasMedication() && medAdm.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForMedicationType(medAdm, medList);
        }

        String dt = null;
        if (medAdm.hasEffective() && medAdm.getEffective() != null) {
          dt = CdaFhirUtilities.getStringForType(medAdm.getEffective());
        }

        Quantity doseQuanity = null;
        String periodText = CdaGeneratorConstants.UNKNOWN_VALUE;
        String dosageText = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (medAdm.hasDosage() && medAdm.getDosage().hasDose()) {
          doseQuanity = medAdm.getDosage().getDose();
          dosageText = CdaFhirUtilities.getStringForQuantity(doseQuanity);
        }

        String medicationDosagePeriodText = dosageText + CdaGeneratorConstants.PIPE + periodText;

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);
        bodyvals.put(
            CdaGeneratorConstants.MED_TABLE_COL_3_BODY_CONTENT, medicationDosagePeriodText);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum;

        // Create the Med Entry for the Medication Statement.
        String medstatus = "";

        if (medAdm.hasStatus()) {
          medstatus =
              CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medAdm.getStatus().toCode());
        } else {
          medstatus = COMPLETED;
        }

        Quantity dose = null;

        if (medAdm.hasDosage()
            && medAdm.getDosage() != null
            && medAdm.getDosage().hasDose()
            && medAdm.getDosage().getDose() != null) {
          dose = medAdm.getDosage().getDose();
        }

        medEntries.append(
            getEntryForMedication(
                medAdm.getIdElement().getIdPart(),
                medAdm.getMedication(),
                medAdm.getEffective(),
                medstatus,
                convertToDosage(medAdm.getDosage()),
                details,
                dose,
                null,
                CdaGeneratorConstants.MOOD_CODE_DEF,
                medAdm,
                medList,
                version));
      }

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add Medication Entries
      sb.append(medEntries);

      // Complete the section end tags.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {
      sb.append(generateEmptyMedicationsAdministeredSection());
    }

    return sb.toString();
  }

  /** Processes medication statements for the R31 medications section. */
  private static void processMedicationStatementsForR31Section(
      List<MedicationStatement> meds,
      List<Medication> medList,
      LaunchDetails details,
      String version,
      StringBuilder sb,
      StringBuilder medEntries,
      int[] rowNum) {
    if (meds == null || meds.isEmpty()) {
      return;
    }

    for (MedicationStatement med : meds) {
      String medDisplayName = UNKNOWN_VALUE;
      if (med.hasMedication() && med.getMedication() != null) {
        medDisplayName = CdaFhirUtilities.getStringForMedicationType(med, medList);
      }

      String dt = null;
      if (med.hasEffective() && med.getEffective() != null) {
        dt = CdaFhirUtilities.getStringForType(med.getEffective());
      }

      DosageInfo dosageInfo =
          UNKNOWN_VALUE.equals(dt)
              ? new DosageInfo(UNKNOWN_VALUE, null)
              : extractDosageInfo(med.hasDosage() ? med.getDosageFirstRep() : null);

      addMedicationTableRowToR31Section(sb, medDisplayName, dt, dosageInfo, rowNum[0]++);

      String medstatus = COMPLETED;
      if (med.hasStatus()) {
        medstatus = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(med.getStatus().toCode());
      }

      Quantity dose = null;
      if (med.hasDosage()
          && med.getDosageFirstRep().hasDoseAndRate()
          && med.getDosageFirstRep().getDoseAndRateFirstRep().hasDoseQuantity()) {
        dose = med.getDosageFirstRep().getDoseAndRateFirstRep().getDoseQuantity();
      }

      medEntries.append(
          getEntryForMedication(
              med.getIdElement().getIdPart(),
              med.getMedication(),
              med.getEffective(),
              medstatus,
              null,
              details,
              dose,
              null,
              CdaGeneratorConstants.MOOD_CODE_DEF,
              med,
              medList,
              version));
    }
  }

  /** Processes medication requests for the R31 medications section. */
  private static void processMedicationRequestsForR31Section(
      List<MedicationRequest> medReqs,
      List<Medication> medList,
      LaunchDetails details,
      String version,
      StringBuilder sb,
      StringBuilder medEntries,
      int[] rowNum) {
    if (medReqs == null || medReqs.isEmpty()) {
      return;
    }

    for (MedicationRequest medReq : medReqs) {
      String medDisplayName = extractMedicationDisplayName(medReq, medList);
      MedicationRequestInfo medInfo = extractMedicationRequestInfo(medReq);
      String dt = extractDateTimeDisplay(medInfo.startDate);

      addMedicationTableRowToR31Section(sb, medDisplayName, dt, medInfo.dosageInfo, rowNum[0]++);

      String medstatus = extractMedicationStatus(medReq);

      medEntries.append(
          getEntryForMedication(
              medReq.getIdElement().getIdPart(),
              medReq.getMedication(),
              null,
              medstatus,
              null,
              details,
              medInfo.dose,
              medInfo.startDate,
              CdaGeneratorConstants.MOOD_CODE_DEF,
              medReq,
              medList,
              version));
    }
  }

  private static String extractMedicationStatus(MedicationRequest medReq) {
    if (medReq.hasStatus()) {
      return CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medReq.getStatus().toCode());
    }
    return COMPLETED;
  }

  /** Adds a medication table row for R31 section with extracted dosage info. */
  private static void addMedicationTableRowToR31Section(
      StringBuilder sb, String medDisplayName, String dateTime, DosageInfo dosageInfo, int rowNum) {
    String dosageText = CdaFhirUtilities.getStringForQuantity(dosageInfo.dose);
    String medicationDosagePeriodText =
        dosageText + CdaGeneratorConstants.PIPE + dosageInfo.periodText;

    Map<String, String> bodyvals = new LinkedHashMap<>();
    bodyvals.put(CdaGeneratorConstants.MED_COL_1_BODY_CONTENT, medDisplayName);
    bodyvals.put(CdaGeneratorConstants.MED_COL_2_BODY_CONTENT, dateTime);
    bodyvals.put(CdaGeneratorConstants.MED_COL_3_BODY_CONTENT, medicationDosagePeriodText);

    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
  }

  public static String generateR31MedicationsSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder(2000);
    List<Medication> medList = data.getMedicationList();
    List<MedicationStatement> meds = getValidMedicationStatements(data, medList);
    List<MedicationRequest> medReqs = getValidMedicationRequestsForMedSection(data, medList);

    if (meds != null && !meds.isEmpty() || medReqs != null && !medReqs.isEmpty()) {

      logger.info(MEDICATIONS_FOUND_LOG_MSG);
      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.MED_SEC_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.MED_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.MED_SEC_TEMPLATE_ID_EXT));

      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.MED_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.MED_SEC_NAME));

      // add Title
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.MED_SEC_TITLE));

      // add Narrative Text
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.MED_COL_1_TITLE);
      list.add(CdaGeneratorConstants.MED_COL_2_TITLE);
      list.add(CdaGeneratorConstants.MED_COL_3_TITLE);

      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int[] rowNum = {1};
      StringBuilder medEntries = new StringBuilder();

      processMedicationStatementsForR31Section(
          meds, medList, details, version, sb, medEntries, rowNum);
      processMedicationRequestsForR31Section(
          medReqs, medList, details, version, sb, medEntries, rowNum);

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add Medication Entries
      sb.append(medEntries);

      // Complete the section end tags.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {
      sb.append(generateEmptyMedicationsSection());
    }

    return sb.toString();
  }

  public static String generateEmptyMedicationsSection() {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.MED_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.MED_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.MED_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.MED_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.MED_SEC_NAME));

    // add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.MED_SEC_TITLE));

    // add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Medication Statement Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static Dosage convertToDosage(
      MedicationAdministration.MedicationAdministrationDosageComponent adminDosage) {
    if (adminDosage == null) {
      return null;
    }

    Dosage dosage = new Dosage();

    if (adminDosage.hasText()) {
      dosage.setText(adminDosage.getText());
    }

    if (adminDosage.hasRoute()) {
      dosage.setRoute(adminDosage.getRoute().copy());
    }

    return dosage;
  }

  /**
   * Checks whether a MedicationRequest has a valid RxNorm code by examining: 1. Contained
   * Medication resources (by code or ingredient) 2. External Medication references (by code or
   * ingredient) 3. Inline CodeableConcept on the MedicationRequest itself
   *
   * <p>When a qualifying contained Medication is found it is added to {@code cmeds} so it is
   * available for downstream CDA rendering — consistent with getValidMedicationRequests behaviour.
   */
  /**
   * Checks if a CodeableConcept has valid RxNorm coding.
   *
   * @param cc the codeable concept
   * @return true if has RxNorm coding
   */
  private static boolean hasRxNormCoding(CodeableConcept cc) {
    return cc != null
        && cc.getCoding() != null
        && !cc.getCoding().isEmpty()
        && Boolean.TRUE.equals(
            CdaFhirUtilities.isCodingPresentForCodeSystem(
                cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL));
  }

  /**
   * Checks if a medication has RxNorm code or ingredient.
   *
   * @param med the medication
   * @param cmeds list to add to if valid
   * @return true if valid RxNorm found
   */
  private static boolean checkMedicationRxNorm(Medication med, List<Medication> cmeds) {
    // Check code
    if (med.getCode() != null && hasRxNormCoding(med.getCode())) {
      if (cmeds != null) cmeds.add(med);
      return true;
    }

    // Check ingredients
    if (med.hasIngredient()) {
      for (Medication.MedicationIngredientComponent ing : med.getIngredient()) {
        if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {
          if (hasRxNormCoding((CodeableConcept) ing.getItem())) {
            if (cmeds != null) cmeds.add(med);
            return true;
          }
        }
      }
    }
    return false;
  }

  /**
   * Handles contained medication reference.
   *
   * @param med the reference
   * @param contained the contained resources
   * @param cmeds list to add to if valid
   * @return true if valid medication found
   */
  private static boolean handleContainedReference(
      Reference med, List<Resource> contained, List<Medication> cmeds) {
    String refId = med.getReference().substring(1);
    if (contained == null) return false;

    for (Resource r : contained) {
      if (r.getId().contains(refId) && r instanceof Medication) {
        return checkMedicationRxNorm((Medication) r, cmeds);
      }
    }
    return false;
  }

  /**
   * Handles external medication reference.
   *
   * @param med the reference
   * @param cmeds the medication list
   * @return true if valid medication found
   */
  private static boolean handleExternalReference(Reference med, List<Medication> cmeds) {
    if (cmeds == null || !med.hasReferenceElement()) return false;

    String refId = med.getReferenceElement().getIdPart();
    if (refId == null) return false;

    for (Medication emed : cmeds) {
      if (emed.getIdElement().getIdPart().contentEquals(refId)) {
        return checkMedicationRxNorm(emed, null);
      }
    }
    return false;
  }

  private static boolean hasValidRxNormCode(MedicationRequest m, List<Medication> cmeds) {
    if (!m.hasMedication()) {
      return false;
    }

    if (m.getMedication() instanceof Reference) {
      Reference med = (Reference) m.getMedication();

      if (med.hasReference()
          && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
        return handleContainedReference(med, m.getContained(), cmeds);
      } else {
        return handleExternalReference(med, cmeds);
      }

    } else if (m.getMedication() instanceof CodeableConcept) {
      return hasRxNormCoding((CodeableConcept) m.getMedication());
    }

    return false;
  }

  /**
   * Checks if a MedicationAdministration has a valid RxNorm code.
   *
   * @param m the medication administration
   * @param cmeds list to add to if valid
   * @return true if valid RxNorm found
   */
  private static boolean hasValidRxNormCode(MedicationAdministration m, List<Medication> cmeds) {
    if (!m.hasMedication()) {
      return false;
    }

    if (m.getMedication() instanceof Reference) {
      Reference med = (Reference) m.getMedication();

      if (med.hasReference()
          && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
        return handleContainedReference(med, m.getContained(), cmeds);
      } else {
        return handleExternalReference(med, cmeds);
      }

    } else if (m.getMedication() instanceof CodeableConcept) {
      return hasRxNormCoding((CodeableConcept) m.getMedication());
    }

    return false;
  }

  /**
   * Checks if a MedicationStatement has a valid RxNorm code.
   *
   * @param m the medication statement
   * @param cmeds list to add to if valid
   * @return true if valid RxNorm found
   */
  private static boolean hasValidRxNormCode(MedicationStatement m, List<Medication> cmeds) {
    if (!m.hasMedication()) {
      return false;
    }

    if (m.getMedication() instanceof Reference) {
      Reference med = (Reference) m.getMedication();

      if (med.hasReference()
          && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
        return handleContainedReference(med, m.getContained(), cmeds);
      } else {
        return handleExternalReference(med, cmeds);
      }

    } else if (m.getMedication() instanceof CodeableConcept) {
      return hasRxNormCoding((CodeableConcept) m.getMedication());
    }

    return false;
  }

  /**
   * Returns MedicationRequests eligible for the <b>Medications Section</b> per eCR guidance.
   *
   * <p>Criteria checked in a single pass:
   *
   * <ul>
   *   <li>RxNorm code system present (via contained Medication, external Medication, or inline
   *       CodeableConcept)
   *   <li>{@code status} = {@code active} or {@code completed}
   *   <li>{@code intent} = {@code order} or {@code plan}
   * </ul>
   *
   * on-hold → Plan of Treatment; cancelled / stopped / entered-in-error / unknown / draft →
   * ignored.
   */
  public static List<MedicationRequest> getValidMedicationRequestsForMedSection(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationRequest> result = new ArrayList<>();

    if (data.getMedicationRequests() == null || data.getMedicationRequests().isEmpty()) {
      logger.debug("getValidMedicationRequestsForMedSection: no MedicationRequests in bundle");
      return result;
    }

    logger.info(
        "getValidMedicationRequestsForMedSection: evaluating {} MedicationRequest(s)",
        data.getMedicationRequests().size());

    for (MedicationRequest m : data.getMedicationRequests()) {

      // --- Status check (active | completed only) ---
      if (!m.hasStatus()) {
        logger.debug(
            "getValidMedicationRequestsForMedSection: skipping id={} — no status",
            m.getIdElement().getIdPart());
        continue;
      }
      String status = m.getStatus().toCode();
      if (!"active".equalsIgnoreCase(status) && !COMPLETED.equalsIgnoreCase(status)) {
        logger.debug(
            "getValidMedicationRequestsForMedSection: skipping id={}, status={}",
            m.getIdElement().getIdPart(),
            status);
        continue;
      }

      // --- Intent check (order | plan only) ---
      if (m.hasIntent()) {
        String intent = m.getIntent().toCode();
        if (!"order".equalsIgnoreCase(intent) && !"plan".equalsIgnoreCase(intent)) {
          logger.debug(
              "getValidMedicationRequestsForMedSection: skipping id={}, intent={}",
              m.getIdElement().getIdPart(),
              intent);
          continue;
        }
      }

      // --- RxNorm code system check ---
      if (hasValidRxNormCode(m, cmeds)) {
        logger.debug(
            "getValidMedicationRequestsForMedSection: adding id={}, status={}",
            m.getIdElement().getIdPart(),
            status);
        result.add(m);
      } else {
        logger.debug(
            "getValidMedicationRequestsForMedSection: skipping id={} — no valid RxNorm code",
            m.getIdElement().getIdPart());
      }
    }

    logger.info(
        "getValidMedicationRequestsForMedSection: {} MedicationRequest(s) qualify for Medications Section",
        result.size());
    return result;
  }

  /**
   * Returns MedicationRequests eligible for the <b>Plan of Treatment Section</b> per eCR guidance.
   *
   * <p>Criteria checked in a single pass:
   *
   * <ul>
   *   <li>RxNorm code system present (via contained Medication, external Medication, or inline
   *       CodeableConcept)
   *   <li>{@code status} = {@code on-hold}
   *   <li>{@code intent} = {@code order} or {@code plan}
   * </ul>
   *
   * active/completed → Medications Section; cancelled / stopped / entered-in-error / unknown /
   * draft → ignored.
   */
  public static List<MedicationRequest> getValidMedicationRequestsForPlanOfTreatment(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationRequest> result = new ArrayList<>();

    if (data.getMedicationRequests() == null || data.getMedicationRequests().isEmpty()) {
      logger.debug("getValidMedicationRequestsForPlanOfTreatment: no MedicationRequests in bundle");
      return result;
    }

    logger.info(
        "getValidMedicationRequestsForPlanOfTreatment: evaluating {} MedicationRequest(s)",
        data.getMedicationRequests().size());

    for (MedicationRequest m : data.getMedicationRequests()) {

      // --- Status check (on-hold only) ---
      if (!m.hasStatus()) {
        logger.debug(
            "getValidMedicationRequestsForPlanOfTreatment: skipping id={} — no status",
            m.getIdElement().getIdPart());
        continue;
      }
      String status = m.getStatus().toCode();
      if (!"on-hold".equalsIgnoreCase(status)) {
        logger.debug(
            "getValidMedicationRequestsForPlanOfTreatment: skipping id={}, status={}",
            m.getIdElement().getIdPart(),
            status);
        continue;
      }

      // --- Intent check (order | plan only) ---
      if (m.hasIntent()) {
        String intent = m.getIntent().toCode();
        if (!"order".equalsIgnoreCase(intent) && !"plan".equalsIgnoreCase(intent)) {
          logger.debug(
              "getValidMedicationRequestsForPlanOfTreatment: skipping id={}, intent={}",
              m.getIdElement().getIdPart(),
              intent);
          continue;
        }
      }

      // --- RxNorm code system check ---
      if (hasValidRxNormCode(m, cmeds)) {
        logger.debug(
            "getValidMedicationRequestsForPlanOfTreatment: adding id={}, status={}",
            m.getIdElement().getIdPart(),
            status);
        result.add(m);
      } else {
        logger.debug(
            "getValidMedicationRequestsForPlanOfTreatment: skipping id={} — no valid RxNorm code",
            m.getIdElement().getIdPart());
      }
    }

    logger.info(
        "getValidMedicationRequestsForPlanOfTreatment: {} MedicationRequest(s) qualify for Plan of Treatment Section",
        result.size());
    return result;
  }
}
