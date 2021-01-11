package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.Dosage;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Timing;
import org.hl7.fhir.r4.model.Type;

public class CdaMedicationGenerator {

  private CdaMedicationGenerator() {}

  public static String generateMedicationSection(R4FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);
    List<MedicationStatement> meds = data.getMedications();
    List<MedicationAdministration> medAdms = data.getMedicationAdministrations();
    List<MedicationRequest> medReqs = data.getMedicationRequests();

    if ((meds != null && !meds.isEmpty())
        || (medAdms != null && !medAdms.isEmpty())
        || (medReqs != null && !medReqs.isEmpty())) {

      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(
          CdaGeneratorUtils.getXmlForNFSection(
              CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.MED_ADM_SEC_TEMPLATE_ID));
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
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.MED_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.MED_TABLE_COL_2_TITLE);

      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // add Body Rows
      int rowNum = 1;
      StringBuilder medEntries = new StringBuilder();

      for (MedicationStatement med : meds) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (med.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForType(med.getMedication());
        }

        String dt = null;
        if (med.getEffective() != null) {
          dt = CdaFhirUtilities.getStringForType(med.getEffective());
        }

        Map<String, String> bodyvals = new HashMap<>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum; // TODO: ++rowNum or rowNum++

        // Create the Med Entry for the Medication Statement.
        String medstatus = "";
        if (med.getStatus() != null) {
          medstatus =
              CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(med.getStatus().toString());
        } else {
          medstatus = "completed";
        }

        Dosage dosage = null;
        if (med.getDosageFirstRep() != null) dosage = med.getDosageFirstRep();

        medEntries.append(
            getEntryForMedication(
                med.getId(),
                med.getMedication(),
                med.getEffective(),
                medstatus,
                dosage,
                details,
                null,
                null,
                CdaGeneratorConstants.MOOD_CODE_DEF));
      }

      // Add Medication Administration
      for (MedicationAdministration medAdm : medAdms) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (medAdm.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForType(medAdm.getMedication());
        }

        String dt = null;
        if (medAdm.getEffective() != null) {
          dt = CdaFhirUtilities.getStringForType(medAdm.getEffective());
        }

        Map<String, String> bodyvals = new HashMap<>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum; // TODO: ++rowNum or rowNum++

        // Create the Med Entry for the Medication Statement.
        String medstatus = "";
        if (medAdm.getStatus() != null) {
          medstatus = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medAdm.getStatus());
        } else {
          medstatus = "completed";
        }

        Quantity dose = null;
        if (medAdm.getDosage() != null && medAdm.getDosage().getDose() != null)
          dose = medAdm.getDosage().getDose();

        medEntries.append(
            getEntryForMedication(
                medAdm.getId(),
                medAdm.getMedication(),
                medAdm.getEffective(),
                medstatus,
                null,
                details,
                dose,
                null,
                CdaGeneratorConstants.MOOD_CODE_DEF));
      }

      // Add Medication Requests
      for (MedicationRequest medReq : medReqs) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (medReq.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForType(medReq.getMedication());
        }

        Date startDate = null;
        Dosage dosage = null;
        if (medReq.getDosageInstructionFirstRep() != null
            && medReq.getDosageInstructionFirstRep().getTiming() != null) {

          dosage = medReq.getDosageInstructionFirstRep();
          Timing t = medReq.getDosageInstructionFirstRep().getTiming();
          if (t != null && t.getRepeat() != null && t.getRepeat().getBoundsPeriod() != null) {
            startDate = t.getRepeat().getBoundsPeriod().getStart();
          }
        }

        String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (startDate != null) {
          dt = CdaGeneratorUtils.getStringForDate(startDate);
        }

        Map<String, String> bodyvals = new HashMap<>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum; // TODO: ++rowNum or rowNum++

        // Create the Med Entry for the Medication Request.
        String medstatus = "";
        if (medReq.getStatus() != null) {
          medstatus =
              CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medReq.getStatus().toString());
        } else {
          medstatus = "active";
        }

        medEntries.append(
            getEntryForMedication(
                medReq.getId(),
                medReq.getMedication(),
                null,
                medstatus,
                dosage,
                details,
                null,
                startDate,
                CdaGeneratorConstants.MOOD_CODE_INT));
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
      sb.append(generateEmptyMedications());
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
      Date startDate,
      String moodCode) {

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
      sb.append(
          CdaFhirUtilities.getXmlForType(
              effectiveTime, CdaGeneratorConstants.EFF_TIME_EL_NAME, false));
    } else if (startDate != null) {
      sb.append(
          CdaGeneratorUtils.getXmlForEffectiveTime(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, startDate));
    } else {
      sb.append(
          CdaGeneratorUtils.getXmlForNullEffectiveTime(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, CdaGeneratorConstants.NF_NI));
    }

    // Set up Effective Time for Frequency.
    String ds = "";
    String freqInHours = CdaGeneratorConstants.UNKNOWN_VALUE;
    if (dosage != null) {

      if (dosage.getDoseAndRateFirstRep() != null
          && dosage.getDoseAndRateFirstRep().getDose() != null)
        ds =
            CdaFhirUtilities.getXmlForType(
                dosage.getDoseAndRateFirstRep().getDose(),
                CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME,
                false);

      if (dosage.getTiming() != null && dosage.getTiming().getRepeat() != null) {

        freqInHours = Integer.toString(dosage.getTiming().getRepeat().getFrequency());
      }
    } else if (dose != null) {
      ds =
          CdaFhirUtilities.getQuantityXml(dose, CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false);
    } else {
      ds =
          CdaFhirUtilities.getQuantityXml(dose, CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForPIVLWithTS(CdaGeneratorConstants.EFF_TIME_EL_NAME, freqInHours));

    // add Dose quantity
    sb.append(ds);
    // sb.append(CdaGeneratorUtils.getXmlForQuantity(CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME,
    // ds));

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

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));

    String codeXml =
        CdaFhirUtilities.getXmlForTypeForCodeSystem(
            medication,
            CdaGeneratorConstants.CODE_EL_NAME,
            false,
            CdaGeneratorConstants.FHIR_RXNORM_URL,
            false);

    if (!codeXml.isEmpty()) {
      sb.append(codeXml);
    } else {
      sb.append(
          CdaFhirUtilities.getXmlForTypeForCodeSystem(
              medication,
              CdaGeneratorConstants.CODE_EL_NAME,
              false,
              CdaGeneratorConstants.FHIR_RXNORM_URL,
              false));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MAN_PROD_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CONSUMABLE_EL_NAME));

    // End Tags for Entries
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MED_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String generateEmptyMedications() {

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
}
