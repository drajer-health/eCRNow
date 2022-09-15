package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.api.IDatatype;
import ca.uhn.fhir.model.api.IResource;
import ca.uhn.fhir.model.dstu2.composite.ContainedDt;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.composite.QuantityDt;
import ca.uhn.fhir.model.dstu2.composite.ResourceReferenceDt;
import ca.uhn.fhir.model.dstu2.resource.BaseResource;
import ca.uhn.fhir.model.dstu2.resource.Medication;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration;
import ca.uhn.fhir.model.dstu2.resource.MedicationAdministration.Dosage;
import ca.uhn.fhir.model.dstu2.resource.MedicationStatement;
import ca.uhn.fhir.model.primitive.DateTimeDt;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.cdafromr4.CdaFhirUtilities;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaMedicationGenerator {

  private static final Logger logger = LoggerFactory.getLogger(Dstu2CdaMedicationGenerator.class);

  public static String generateMedicationSection(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    // List<MedicationStatement> meds = data.getMedications();
    List<MedicationAdministration> meds = data.getMedicationAdministrations();
    List<MedicationStatement> medications = data.getMedications();

    if ((meds != null && meds.size() > 0) || (medications != null && medications.size() > 0)) {

      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

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
      List<String> list = new ArrayList<String>();
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

      for (MedicationAdministration med : meds) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (med.getMedication() != null) {
          medDisplayName = Dstu2CdaFhirUtilities.getStringForIDataType(med.getMedication());
        }

        String dt = null;
        if (med.getEffectiveTime() != null) {
          dt = Dstu2CdaFhirUtilities.getStringForIDataType(med.getEffectiveTime());
        }

        Map<String, String> bodyvals = new LinkedHashMap<String, String>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum;

        // create the med entry

        String medstatus = "";
        if (med.getStatus() != null) {
          medstatus = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(med.getStatus());
        } else {
          medstatus = "completed";
        }

        QuantityDt dose = null;
        if (med.getDosage() != null && med.getDosage().getQuantity() != null)
          dose = med.getDosage().getQuantity();

        medEntries.append(
            getEntryForMedication(
                med.getId().getIdPart(),
                med.getMedication(),
                med.getEffectiveTime(),
                medstatus,
                null,
                details,
                dose,
                null,
                CdaGeneratorConstants.MOOD_CODE_DEF,
                med));
      }

      for (MedicationStatement med : medications) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (med.getMedication() != null) {
          medDisplayName = Dstu2CdaFhirUtilities.getStringForIDataType(med.getMedication());
        }

        String dt = null;
        if (med.getEffective() != null) {
          dt = Dstu2CdaFhirUtilities.getStringForIDataType(med.getEffective());
        }

        Map<String, String> bodyvals = new LinkedHashMap<String, String>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum;

        // create the med entry

        String medstatus = "";
        if (med.getStatus() != null) {
          medstatus = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(med.getStatus());
        } else {
          medstatus = "completed";
        }

        QuantityDt dose = null;
        if (med.getDosageFirstRep() != null
            && med.getDosageFirstRep().getQuantity() != null
            && med.getDosageFirstRep().getQuantity() instanceof QuantityDt)
          dose = (QuantityDt) med.getDosageFirstRep().getQuantity();

        medEntries.append(
            getEntryForMedication(
                med.getId().getIdPart(),
                med.getMedication(),
                med.getEffective(),
                medstatus,
                null,
                details,
                dose,
                null,
                CdaGeneratorConstants.MOOD_CODE_DEF,
                med));
      }

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

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
      IDatatype medication,
      IDatatype effectiveTime,
      String medStatus,
      Dosage dosage,
      LaunchDetails details,
      QuantityDt dose,
      DateTimeDt startDate,
      String moodCode,
      BaseResource res) {

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
    if (effectiveTime != null && effectiveTime instanceof PeriodDt) {
      PeriodDt p = (PeriodDt) effectiveTime;
      sb.append(
          Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(
              p, CdaGeneratorConstants.EFF_TIME_EL_NAME));
    } else if (startDate != null) {
      PeriodDt p = new PeriodDt();
      p.setStart(startDate);
      sb.append(
          Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(
              p, CdaGeneratorConstants.EFF_TIME_EL_NAME));
    } else if (effectiveTime != null && effectiveTime instanceof DateTimeDt) {
      PeriodDt p = new PeriodDt();
      p.setStart((DateTimeDt) effectiveTime);
      sb.append(
          Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(
              p, CdaGeneratorConstants.EFF_TIME_EL_NAME));
    } else {
      sb.append(
          Dstu2CdaFhirUtilities.getPeriodXmlForValueElement(
              null, CdaGeneratorConstants.EFF_TIME_EL_NAME));
    }

    // Set up Effective Time for Frequency.
    String ds = "";
    String freqInHours = CdaGeneratorConstants.UNKNOWN_VALUE;
    if (dosage != null) {

      if (dosage.getQuantity() != null)
        ds =
            Dstu2CdaFhirUtilities.getXmlForType(
                dosage.getQuantity(), CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false);

      // Cannot setup timing as it is not present in HAPI API.

    } else {
      ds =
          Dstu2CdaFhirUtilities.getQuantityXml(
              dose, CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false);
    }

    // Cannot set frequency as it is not in HAPI APIs
    /* sb.append(
    CdaGeneratorUtils.getXmlForPIVLWithTS(CdaGeneratorConstants.EFF_TIME_EL_NAME, freqInHours)); */

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

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));

    String codeXml =
        getXmlForMedicationTypeForCodeSystem(
            medication,
            CdaGeneratorConstants.CODE_EL_NAME,
            false,
            CdaGeneratorConstants.FHIR_RXNORM_URL,
            false,
            res);

    if (!codeXml.isEmpty()) {
      sb.append(codeXml);
    } else {
      sb.append(
          Dstu2CdaFhirUtilities.getXmlForTypeForCodeSystem(
              medication,
              CdaGeneratorConstants.CODE_EL_NAME,
              false,
              CdaGeneratorConstants.FHIR_RXNORM_URL,
              true));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MAN_PROD_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CONSUMABLE_EL_NAME));

    // End Tags for Entries
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MED_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static String getXmlForMedicationTypeForCodeSystem(
      IDatatype dt,
      String elName,
      Boolean valFlag,
      String codeSystemUrl,
      Boolean csOptional,
      BaseResource res) {

    if (dt instanceof ResourceReferenceDt) {

      logger.debug("Found Medication of Type Reference within Domain Resource");
      ResourceReferenceDt med = (ResourceReferenceDt) dt;
      String codeXml = "";
      if (med.getReference() != null
          && med.getReference().getValue() != null
          && med.getReference()
              .getValue()
              .startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {
        // Check contained.
        String refId = med.getReference().getValue().substring(1);

        logger.debug("Found Medication of Type Reference with Id {}", refId);

        if (res.getContained() != null) {

          logger.debug("Contained Elements Not null");
          ContainedDt medication = res.getContained();

          List<IResource> meds = medication.getContainedResources();

          for (IResource r : meds) {

            if (r.getId().getValue().contains(refId) && r instanceof Medication) {

              logger.debug("Found Medication in contained resource");

              Medication cmed = (Medication) r;

              // Found the reference, check the code and ingredients.

              if (cmed.getCode() != null
                  && cmed.getCode().getCoding() != null
                  && !cmed.getCode().getCoding().isEmpty()
                  && Dstu2CdaFhirUtilities.isCodingPresentForCodeSystem(
                      cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL)) {

                logger.debug("Found Medication for code system in code element");
                // Found the Medication that matters.
                codeXml =
                    Dstu2CdaFhirUtilities.getXmlForTypeForCodeSystem(
                        cmed.getCode(), elName, valFlag, codeSystemUrl, csOptional);

              } // if code present
              else {
                // Check the ingredients

                // Not handled for now.
              }
            } // contained med
          } // for all contained resources
        } // contained present

      } // Contained reference
      else {

        // Check the actual medication

      }

      return codeXml;

    } else {
      return Dstu2CdaFhirUtilities.getXmlForTypeForCodeSystem(
          dt, elName, valFlag, codeSystemUrl, csOptional);
    }
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
