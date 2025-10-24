package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Medication.MedicationIngredientComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaMedicationGenerator {

  public static final String COMPLETED = "completed";

  private CdaMedicationGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaMedicationGenerator.class);

  public static String generateMedicationSection(R4FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);
    List<Medication> medList = data.getMedicationList();
    List<MedicationStatement> meds = data.getMedicationStatements();
    List<MedicationAdministration> medAdms = data.getMedicationAdministrations();
    List<MedicationRequest> medReqs = getValidMedicationRequests(data, medList);

    if ((meds != null && !meds.isEmpty())
        || (medAdms != null && !medAdms.isEmpty())
        || (medReqs != null && !medReqs.isEmpty())) {

      logger.info("Medications found for processing ");
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

      for (MedicationStatement med : meds) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (med.hasMedication() && med.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForMedicationType(med, medList);
        }

        String dt = null;
        if (med.hasEffective() && med.getEffective() != null) {
          dt = CdaFhirUtilities.getStringForType(med.getEffective());
        }
        Dosage dosageValue = null;
        Quantity dose = null;

        String periodText = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (med.hasDosage()) {
          dosageValue = med.getDosageFirstRep();

          if (dosageValue.hasTiming() && dosageValue.getTiming() != null) {
            Timing t = dosageValue.getTiming();
            if (t != null && t.hasRepeat()) {

              Timing.TimingRepeatComponent repeat = t.getRepeat();
              String period = repeat.hasPeriod() ? repeat.getPeriod().toString() : null;
              String periodUnit = repeat.hasPeriodUnit() ? repeat.getPeriodUnit().toString() : null;
              String frequency =
                  repeat.hasFrequency() ? String.valueOf(repeat.getFrequency()) : null;
              periodText = CdaFhirUtilities.getNarrative(frequency, period, periodUnit);
            }
          }

          if (dosageValue.hasDoseAndRate()
              && dosageValue.getDoseAndRateFirstRep() != null
              && dosageValue.getDoseAndRateFirstRep().hasDoseQuantity()) {
            dose = dosageValue.getDoseAndRateFirstRep().getDoseQuantity();
          }
        }
        String medicationDosagePeriodText =
            CdaFhirUtilities.getStringForQuantity(dose) + CdaGeneratorConstants.PIPE + periodText;

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);
        bodyvals.put(
            CdaGeneratorConstants.MED_TABLE_COL_3_BODY_CONTENT, medicationDosagePeriodText);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum;

        // Create the Med Entry for the Medication Statement.
        String medstatus = "";
        if (med.hasStatus() && med.getStatus() != null) {
          medstatus =
              CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(med.getStatus().toString());
        } else {
          medstatus = COMPLETED;
        }

        Dosage dosage = null;
        if (med.hasDosage() && med.getDosageFirstRep() != null) dosage = med.getDosageFirstRep();

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
                medList));
      }

      // Add Medication Administration
      for (MedicationAdministration medAdm : medAdms) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        Quantity dose = null;
        String periodText = CdaGeneratorConstants.UNKNOWN_VALUE;
        String dosageText = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (medAdm.hasMedication() && medAdm.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForMedicationType(medAdm, medList);
        }
        if (medAdm.hasDosage() && medAdm.getDosage().hasDose()) {
          dose = medAdm.getDosage().getDose();
          dosageText = CdaFhirUtilities.getStringForQuantity(dose);
        }

        String dt = null;
        if (medAdm.hasEffective() && medAdm.getEffective() != null) {
          dt = CdaFhirUtilities.getStringForType(medAdm.getEffective());
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

        if (medAdm.hasStatus() && medAdm.getStatus() != null) {
          medstatus =
              CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medAdm.getStatus().toCode());
        } else {
          medstatus = COMPLETED;
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
                medList));
      }

      // Add Medication Requests
      for (MedicationRequest medReq : medReqs) {

        logger.info(" Adding medication requests ");
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (medReq.hasMedication() && medReq.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForMedicationType(medReq, medList);
        }

        DateTimeType startDate = null;
        Dosage dosage = null;
        Quantity dose = null;

        String periodText = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (medReq.hasDosageInstruction() && medReq.getDosageInstructionFirstRep() != null) {

          dosage = medReq.getDosageInstructionFirstRep();

          if (dosage.hasTiming()) {
            Timing t = medReq.getDosageInstructionFirstRep().getTiming();
            if (t != null && t.hasRepeat()) {

              if (t.getRepeat().hasBoundsPeriod()) {
                Period boundsPeriod = t.getRepeat().getBoundsPeriod();
                if (boundsPeriod.hasStartElement()) {
                  startDate = t.getRepeat().getBoundsPeriod().getStartElement();
                }
              }

              Timing.TimingRepeatComponent repeat = t.getRepeat();
              String period = repeat.hasPeriod() ? repeat.getPeriod().toString() : null;
              String periodUnit = repeat.hasPeriodUnit() ? repeat.getPeriodUnit().toString() : null;
              String frequency =
                  repeat.hasFrequency() ? String.valueOf(repeat.getFrequency()) : null;

              periodText = CdaFhirUtilities.getNarrative(frequency, period, periodUnit);
            }
          }

          if (dosage.hasDoseAndRate()
              && dosage.getDoseAndRateFirstRep() != null
              && dosage.getDoseAndRateFirstRep().hasDoseQuantity()) {
            dose = dosage.getDoseAndRateFirstRep().getDoseQuantity();
          }
        }

        if (startDate == null && medReq.hasAuthoredOn() && medReq.getAuthoredOnElement() != null) {
          startDate = medReq.getAuthoredOnElement();
        }

        String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (startDate != null) {
          dt = CdaFhirUtilities.getDisplayStringForDateTimeType(startDate);
        } else {
          logger.error(
              " Dosage field does not have a valid period either due to datetime or timezone being null ");
        }
        String medicationDosagePeriodText =
            CdaFhirUtilities.getStringForQuantity(dose) + CdaGeneratorConstants.PIPE + periodText;

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_TABLE_COL_2_BODY_CONTENT, dt);

        bodyvals.put(
            CdaGeneratorConstants.MED_TABLE_COL_3_BODY_CONTENT, medicationDosagePeriodText);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum;

        // Create the Med Entry for the Medication Request.
        String medstatus = "";
        String moodCode = CdaGeneratorConstants.MOOD_CODE_INT;
        if (medReq.hasStatus() && medReq.getStatus() != null) {
          medstatus =
              CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(medReq.getStatus().toString());
          if (medstatus.equalsIgnoreCase(COMPLETED)) {
            moodCode = CdaGeneratorConstants.MOOD_CODE_DEF;
          }
        } else {
          medstatus = "active";
        }

        medEntries.append(
            getEntryForMedication(
                medReq.getIdElement().getIdPart(),
                medReq.getMedication(),
                null,
                medstatus,
                dosage,
                details,
                dose,
                startDate,
                moodCode,
                medReq,
                medList));
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
      List<Medication> medList) {

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

    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));

    String codeXml =
        CdaFhirUtilities.getXmlForMedicationTypeForCodeSystem(
            medication,
            CdaGeneratorConstants.CODE_EL_NAME,
            false,
            CdaGeneratorConstants.FHIR_RXNORM_URL,
            false,
            res,
            medList);

    if (!codeXml.isEmpty()) {
      sb.append(codeXml);
    } else {
      sb.append(
          CdaFhirUtilities.getXmlForTypeForCodeSystem(
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

    List<MedicationRequest> mr = new ArrayList<>();

    if (data.getMedicationRequests() != null && !data.getMedicationRequests().isEmpty()) {

      logger.info(
          "Total num of Medication Requests available for Patient {}",
          data.getMedicationRequests().size());

      for (MedicationRequest m : data.getMedicationRequests()) {

        if (m.hasMedication() && m.getMedication() instanceof Reference) {

          Reference med = (Reference) m.getMedication();

          if (med.hasReference()
              && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {

            logger.info(" Found a Contained Reference ");
            // Check contained.
            String refId = med.getReference().substring(1);

            if (m.hasContained() && m.getContained() != null) {
              List<Resource> meds = m.getContained();

              for (Resource r : meds) {

                logger.debug("starting to examine contained meds ");
                if (r.getId().contains(refId) && r instanceof Medication) {

                  Medication cmed = (Medication) r;
                  // Found the reference, check the code and ingredients.

                  if (cmed.getCode() != null
                      && cmed.getCode().getCoding() != null
                      && !cmed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Med Req - due to code ");
                    cmeds.add(cmed);
                    mr.add(m);
                    break;
                  } // if code present

                  Boolean found = false;
                  // Check Ingredients also.
                  if (cmed.hasIngredient() && cmed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = cmed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          cmeds.add(cmed);
                          mr.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  if (Boolean.TRUE.equals(found)) break;
                } // Found id
              } // For all resources
            } // contained present

          } // Contained reference
          else {

            // Check the actual medication if desired in the future.
            logger.info(" Found an External Medication Reference ");

            if (cmeds != null && med.hasReferenceElement()) {

              for (Medication emed : cmeds) {

                if (med.getReferenceElement().hasIdPart()
                    && emed.getIdElement()
                        .getIdPart()
                        .contentEquals(med.getReferenceElement().getIdPart())) {

                  if (emed.getCode() != null
                      && emed.getCode().getCoding() != null
                      && !emed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              emed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Medication and MedicationRequest - due to code ");
                    mr.add(m);
                    break;
                  } // if code present

                  // If code is absent check ingredient
                  Boolean found = false;
                  // Check Ingredients also.
                  if (emed.hasIngredient() && emed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = emed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          mr.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  // break the outer loop.
                  if (Boolean.TRUE.equals(found)) break;
                } // if same reference
              } // for all medications

            } // if cmeds
            else {
              logger.info(" No cmeds to compare and extract medications ");
            }
          } // Else if it is an external reference

        } else if (m.hasMedication() && m.getMedication() instanceof CodeableConcept) {

          logger.info(" Found a medication codeable concept ");

          CodeableConcept cc = (CodeableConcept) m.getMedication();

          if (cc.getCoding() != null
              && !cc.getCoding().isEmpty()
              && Boolean.TRUE.equals(
                  CdaFhirUtilities.isCodingPresentForCodeSystem(
                      cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

            logger.debug("Found a Medication Request with a RxNorm code");
            mr.add(m);
          }
        }
      }
    } else {
      logger.debug("No Valid Medication Requests in the bundle to process");
    }

    return mr;
  }

  public static List<MedicationAdministration> getValidMedicationAdministrations(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationAdministration> madms = new ArrayList<>();

    if (data.getMedicationAdministrations() != null
        && !data.getMedicationAdministrations().isEmpty()) {

      logger.info(
          "Total num of Medication Administrations available for Patient {}",
          data.getMedicationAdministrations().size());

      for (MedicationAdministration m : data.getMedicationAdministrations()) {

        if (m.hasMedication() && m.getMedication() instanceof Reference) {

          Reference med = (Reference) m.getMedication();

          if (med.hasReference()
              && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {

            logger.info(" Found a Contained Reference ");
            // Check contained.
            String refId = med.getReference().substring(1);

            if (m.hasContained()) {
              List<Resource> meds = m.getContained();

              for (Resource r : meds) {

                logger.debug("starting to examine contained meds ");
                if (r.getId().contains(refId) && r instanceof Medication) {

                  Medication cmed = (Medication) r;
                  // Found the reference, check the code and ingredients.

                  if (cmed.getCode() != null
                      && cmed.getCode().getCoding() != null
                      && !cmed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Med Req - due to code ");
                    cmeds.add(cmed);
                    madms.add(m);
                    break;
                  } // if code present

                  Boolean found = false;
                  // Check Ingredients also.
                  if (cmed.hasIngredient() && cmed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = cmed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          cmeds.add(cmed);
                          madms.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  if (Boolean.TRUE.equals(found)) break;
                } // Found id
              } // For all resources
            } // contained present

          } // Contained reference
          else {

            // Check the actual medication if desired in the future.
            logger.info(" Found an External Medication Reference ");

            if (cmeds != null && med.hasReferenceElement()) {

              for (Medication emed : cmeds) {

                if (med.getReferenceElement().hasIdPart()
                    && emed.getIdElement()
                        .getIdPart()
                        .contentEquals(med.getReferenceElement().getIdPart())) {

                  if (emed.getCode() != null
                      && emed.getCode().getCoding() != null
                      && !emed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              emed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Medication and MedicationRequest - due to code ");
                    madms.add(m);
                    break;
                  } // if code present

                  // If code is absent check ingredient
                  Boolean found = false;
                  // Check Ingredients also.
                  if (emed.hasIngredient() && emed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = emed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          madms.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  // break the outer loop.
                  if (Boolean.TRUE.equals(found)) break;
                } // if same reference
              } // for all medications

            } // if cmeds
            else {
              logger.info(" No cmeds to compare and extract medications ");
            }
          } // Else if it is an external reference

        } else if (m.hasMedication() && m.getMedication() instanceof CodeableConcept) {

          logger.info(" Found a medication codeable concept ");

          CodeableConcept cc = (CodeableConcept) m.getMedication();

          if (cc.getCoding() != null
              && !cc.getCoding().isEmpty()
              && Boolean.TRUE.equals(
                  CdaFhirUtilities.isCodingPresentForCodeSystem(
                      cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

            logger.debug("Found a Medication Request with a RxNorm code");
            madms.add(m);
          }
        }
      }
    } else {
      logger.info("No Valid Medication Requests in the bundle to process");
    }

    return madms;
  }

  public static List<MedicationStatement> getValidMedicationStatements(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationStatement> mstmts = new ArrayList<>();

    if (data.getMedicationStatements() != null && !data.getMedicationStatements().isEmpty()) {

      logger.info(
          "Total num of Medication Statements available for Patient {}",
          data.getMedicationStatements().size());

      for (MedicationStatement m : data.getMedicationStatements()) {

        if (m.hasMedication() && m.getMedication() instanceof Reference) {

          Reference med = (Reference) m.getMedication();

          if (med.hasReference()
              && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {

            logger.info(" Found a Contained Reference ");
            // Check contained.
            String refId = med.getReference().substring(1);

            if (m.hasContained()) {
              List<Resource> meds = m.getContained();

              for (Resource r : meds) {

                logger.debug("starting to examine contained meds ");
                if (r.getId().contains(refId) && r instanceof Medication) {

                  Medication cmed = (Medication) r;
                  // Found the reference, check the code and ingredients.

                  if (cmed.getCode() != null
                      && cmed.getCode().getCoding() != null
                      && !cmed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Med Req - due to code ");
                    cmeds.add(cmed);
                    mstmts.add(m);
                    break;
                  } // if code present

                  Boolean found = false;
                  // Check Ingredients also.
                  if (cmed.hasIngredient() && cmed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = cmed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          cmeds.add(cmed);
                          mstmts.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  if (Boolean.TRUE.equals(found)) break;
                } // Found id
              } // For all resources
            } // contained present

          } // Contained reference
          else {

            // Check the actual medication if desired in the future.
            logger.info(" Found an External Medication Reference ");

            if (cmeds != null && med.hasReferenceElement()) {

              for (Medication emed : cmeds) {

                if (med.getReferenceElement().hasIdPart()
                    && emed.getIdElement()
                        .getIdPart()
                        .contentEquals(med.getReferenceElement().getIdPart())) {

                  if (emed.getCode() != null
                      && emed.getCode().getCoding() != null
                      && !emed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              emed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Medication and MedicationRequest - due to code ");
                    mstmts.add(m);
                    break;
                  } // if code present

                  // If code is absent check ingredient
                  Boolean found = false;
                  // Check Ingredients also.
                  if (emed.hasIngredient() && emed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = emed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          mstmts.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  // break the outer loop.
                  if (Boolean.TRUE.equals(found)) break;
                } // if same reference
              } // for all medications

            } // if cmeds
            else {
              logger.info(" No cmeds to compare and extract medications ");
            }
          } // Else if it is an external reference

        } else if (m.hasMedication() && m.getMedication() instanceof CodeableConcept) {

          logger.info(" Found a medication codeable concept ");

          CodeableConcept cc = (CodeableConcept) m.getMedication();

          if (cc.getCoding() != null
              && !cc.getCoding().isEmpty()
              && Boolean.TRUE.equals(
                  CdaFhirUtilities.isCodingPresentForCodeSystem(
                      cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

            logger.debug("Found a Medication Request with a RxNorm code");
            mstmts.add(m);
          }
        }
      }
    } else {
      logger.info("No Valid Medication Requests in the bundle to process");
    }

    return mstmts;
  }

  public static List<MedicationDispense> getValidMedicationDispenses(
      R4FhirData data, List<Medication> cmeds) {

    List<MedicationDispense> mdisps = new ArrayList<>();

    if (data.getMedicationDispenses() != null && !data.getMedicationDispenses().isEmpty()) {

      logger.info(
          "Total num of Medication Dispenses available for Patient {}",
          data.getMedicationDispenses().size());

      for (MedicationDispense m : data.getMedicationDispenses()) {

        if (m.hasMedication() && m.getMedication() instanceof Reference) {

          Reference med = (Reference) m.getMedication();

          if (med.hasReference()
              && med.getReference().startsWith(CdaGeneratorConstants.FHIR_CONTAINED_REFERENCE)) {

            logger.info(" Found a Contained Reference ");
            // Check contained.
            String refId = med.getReference().substring(1);

            if (m.hasContained()) {
              List<Resource> meds = m.getContained();

              for (Resource r : meds) {

                logger.debug("starting to examine contained meds ");
                if (r.getId().contains(refId) && r instanceof Medication) {

                  Medication cmed = (Medication) r;
                  // Found the reference, check the code and ingredients.

                  if (cmed.getCode() != null
                      && cmed.getCode().getCoding() != null
                      && !cmed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Med Req - due to code ");
                    cmeds.add(cmed);
                    mdisps.add(m);
                    break;
                  } // if code present

                  Boolean found = false;
                  // Check Ingredients also.
                  if (cmed.hasIngredient() && cmed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = cmed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          cmeds.add(cmed);
                          mdisps.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  if (Boolean.TRUE.equals(found)) break;
                } // Found id
              } // For all resources
            } // contained present

          } // Contained reference
          else {

            // Check the actual medication if desired in the future.
            logger.info(" Found an External Medication Reference ");

            if (cmeds != null && med.hasReferenceElement()) {

              for (Medication emed : cmeds) {

                if (med.getReferenceElement().hasIdPart()
                    && emed.getIdElement()
                        .getIdPart()
                        .contentEquals(med.getReferenceElement().getIdPart())) {

                  if (emed.getCode() != null
                      && emed.getCode().getCoding() != null
                      && !emed.getCode().getCoding().isEmpty()
                      && Boolean.TRUE.equals(
                          CdaFhirUtilities.isCodingPresentForCodeSystem(
                              emed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                    // Found the Medication that matters.
                    logger.info("Adding Medication and MedicationRequest - due to code ");
                    mdisps.add(m);
                    break;
                  } // if code present

                  // If code is absent check ingredient
                  Boolean found = false;
                  // Check Ingredients also.
                  if (emed.hasIngredient() && emed.getIngredient() != null) {

                    List<MedicationIngredientComponent> ings = emed.getIngredient();

                    for (MedicationIngredientComponent ing : ings) {

                      logger.info("starting to examine contained ingredients ");
                      if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                        CodeableConcept cc = (CodeableConcept) ing.getItem();

                        if (cc.getCoding() != null
                            && !cc.getCoding().isEmpty()
                            && Boolean.TRUE.equals(
                                CdaFhirUtilities.isCodingPresentForCodeSystem(
                                    cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                          logger.info("Adding Med Req due to ingredient ");
                          mdisps.add(m);
                          found = true;
                          break;
                        } // Code check.
                      } // Ingredient is a Codeable Concept
                    } // Ingredients present
                  } // Ingredient present

                  // break the outer loop.
                  if (Boolean.TRUE.equals(found)) break;
                } // if same reference
              } // for all medications

            } // if cmeds
            else {
              logger.info(" No cmeds to compare and extract medications ");
            }
          } // Else if it is an external reference

        } else if (m.hasMedication() && m.getMedication() instanceof CodeableConcept) {

          logger.info(" Found a medication codeable concept ");

          CodeableConcept cc = (CodeableConcept) m.getMedication();

          if (cc.getCoding() != null
              && !cc.getCoding().isEmpty()
              && Boolean.TRUE.equals(
                  CdaFhirUtilities.isCodingPresentForCodeSystem(
                      cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

            logger.debug("Found a Medication Request with a RxNorm code");
            mdisps.add(m);
          }
        }
      }
    } else {
      logger.info("No Valid Medication Requests in the bundle to process");
    }

    return mdisps;
  }

  public static String generateR31MedicationsAdministeredSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder(2000);
    List<Medication> medList = data.getMedicationList();
    List<MedicationAdministration> medAdms = getValidMedicationAdministrations(data, medList);

    if (medAdms != null && !medAdms.isEmpty()) {

      logger.info("Medications found for processing ");
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

        Map<String, String> bodyvals = new HashMap<>();
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
            && medAdm.getDosage().getDose() != null) dose = medAdm.getDosage().getDose();

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
                medList));
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

  public static String generateR31MedicationsSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder(2000);
    List<Medication> medList = data.getMedicationList();
    List<MedicationStatement> meds = getValidMedicationStatements(data, medList);

    if (meds != null && !meds.isEmpty()) {

      logger.info("Medications found for processing ");
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

      // add Body Rows
      int rowNum = 1;
      StringBuilder medEntries = new StringBuilder();

      // Add Medication Administration
      for (MedicationStatement med : meds) {
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (med.hasMedication() && med.getMedication() != null) {
          medDisplayName = CdaFhirUtilities.getStringForMedicationType(med, medList);
        }

        String dt = null;
        if (med.hasEffective() && med.getEffective() != null) {
          dt = CdaFhirUtilities.getStringForType(med.getEffective());
        }
        Dosage dosageValue = null;
        Quantity doseQuanity = null;
        String periodText = CdaGeneratorConstants.UNKNOWN_VALUE;
        String dosageText = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (med.hasDosage()) {
          dosageValue = med.getDosageFirstRep();

          if (dosageValue.hasTiming() && dosageValue.getTiming() != null) {
            Timing t = dosageValue.getTiming();
            if (t != null && t.hasRepeat()) {

              Timing.TimingRepeatComponent repeat = t.getRepeat();
              String period = repeat.hasPeriod() ? repeat.getPeriod().toString() : null;
              String periodUnit = repeat.hasPeriodUnit() ? repeat.getPeriodUnit().toString() : null;
              String frequency =
                  repeat.hasFrequency() ? String.valueOf(repeat.getFrequency()) : null;
              periodText = CdaFhirUtilities.getNarrative(frequency, period, periodUnit);
            }
          }

          if (dosageValue.hasDoseAndRate()
              && dosageValue.getDoseAndRateFirstRep() != null
              && dosageValue.getDoseAndRateFirstRep().hasDoseQuantity()) {
            doseQuanity = dosageValue.getDoseAndRateFirstRep().getDoseQuantity();
          }
        }
        String medicationDosagePeriodText =
            CdaFhirUtilities.getStringForQuantity(doseQuanity)
                + CdaGeneratorConstants.PIPE
                + periodText;

        Map<String, String> bodyvals = new HashMap<>();
        bodyvals.put(CdaGeneratorConstants.MED_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.MED_COL_2_BODY_CONTENT, dt);
        bodyvals.put(CdaGeneratorConstants.MED_COL_3_BODY_CONTENT, medicationDosagePeriodText);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum;

        // Create the Med Entry for the Medication Statement.
        String medstatus = "";

        if (med.hasStatus()) {
          medstatus = CdaFhirUtilities.getStatusCodeForFhirMedStatusCodes(med.getStatus().toCode());
        } else {
          medstatus = COMPLETED;
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
                medList));
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

  public static Boolean getContainedMedication(
      List<Resource> res, String refId, Map<String, Medication> medMap) {

    Boolean retVal = false;
    for (Resource r : res) {

      logger.debug("starting to examine contained meds ");
      if (r.getId().contains(refId) && r instanceof Medication) {

        Medication cmed = (Medication) r;
        // Found the reference, check the code and ingredients.

        if (cmed.hasCode()
            && cmed.getCode().hasCoding()
            && !cmed.getCode().getCoding().isEmpty()
            && Boolean.TRUE.equals(
                CdaFhirUtilities.isCodingPresentForCodeSystem(
                    cmed.getCode().getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

          // Found the Medication that matters.
          logger.info("Adding Med Req - due to code in contained med ");
          medMap.put(r.getIdElement().getIdPart(), cmed);
          retVal = true;
          break;
        } // if code present

        Boolean found = false;
        // Check Ingredients also.
        if (cmed.hasIngredient() && cmed.getIngredient() != null) {

          List<MedicationIngredientComponent> ings = cmed.getIngredient();

          for (MedicationIngredientComponent ing : ings) {

            logger.info("starting to examine contained ingredients ");
            if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

              CodeableConcept cc = (CodeableConcept) ing.getItem();

              if (cc.hasCoding()
                  && !cc.getCoding().isEmpty()
                  && Boolean.TRUE.equals(
                      CdaFhirUtilities.isCodingPresentForCodeSystem(
                          cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                logger.info("Adding Med Req due to ingredient code in contained med ");
                medMap.put(r.getIdElement().getIdPart(), cmed);
                retVal = true;
                found = true;

                // Break the Medication Ingredient loop.
                break;
              } // Code check.
            } // Ingredient is a Codeable Concept
          } // Ingredients present
        } // Ingredient present

        // If we found a medication ingredient then break
        if (Boolean.TRUE.equals(found)) break;
      } // Found id
    } // For all resources

    return retVal;
  }

  public static Boolean isMedicationPresent(Map<String, Medication> cmeds, Reference med) {

    Boolean retVal = false;

    if (cmeds != null) {

      for (Map.Entry<String, Medication> entry : cmeds.entrySet()) {

        if (entry.getKey().contentEquals(med.getReferenceElement().getIdPart())) {

          if (entry.getValue().hasCode()
              && entry.getValue().getCode().hasCoding()
              && !entry.getValue().getCode().getCoding().isEmpty()
              && Boolean.TRUE.equals(
                  CdaFhirUtilities.isCodingPresentForCodeSystem(
                      entry.getValue().getCode().getCoding(),
                      CdaGeneratorConstants.FHIR_RXNORM_URL))) {

            // Found the Medication that matters.
            logger.info(
                "Adding Medication and MedicationRequest - Found code in External Med Reference ");
            retVal = true;
            break;
          } // if code present

          // If code is absent check ingredient
          Boolean found = false;
          // Check Ingredients also.
          if (entry.getValue().hasIngredient() && entry.getValue().getIngredient() != null) {

            List<MedicationIngredientComponent> ings = entry.getValue().getIngredient();

            for (MedicationIngredientComponent ing : ings) {

              logger.info("starting to examine contained ingredients ");
              if (ing.hasItem() && ing.getItem() instanceof CodeableConcept) {

                CodeableConcept cc = (CodeableConcept) ing.getItem();
                if (cc.getCoding() != null
                    && !cc.getCoding().isEmpty()
                    && Boolean.TRUE.equals(
                        CdaFhirUtilities.isCodingPresentForCodeSystem(
                            cc.getCoding(), CdaGeneratorConstants.FHIR_RXNORM_URL))) {

                  logger.info("Adding Med Req due to ingredient ");
                  retVal = true;
                  found = true;
                  break;
                } // Code check.
              } // Ingredient is a Codeable Concept
            } // Ingredients present
          } // Ingredient present

          // break the outer loop if the ingredient is present
          if (Boolean.TRUE.equals(found)) break;
        } // if same reference
      } // for all medications

    } // if cmeds
    else {
      logger.info(" No cmeds to compare and extract medications ");
    }

    return retVal;
  }
}
