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
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.Immunization.ImmunizationPerformerComponent;
import org.hl7.fhir.r4.model.Immunization.ImmunizationStatus;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaImmunizationGenerator {

  private CdaImmunizationGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaImmunizationGenerator.class);

  public static String generateImmunizationSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder sb = new StringBuilder(2000);

    List<Immunization> imms = data.getImmunizations();

    if (imms != null && !imms.isEmpty()) {

      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.IMMUNIZATION_SEC_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.IMMUNIZATION_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.IMMUNIZATION_SEC_TEMPLATE_ID_EXT));

      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.IMMUNIZATION_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.IMMUNIZATION_SEC_NAME));

      // add Title
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.IMMUNIZATION_SEC_TITLE));

      // add Narrative Text
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.IMM_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.IMM_TABLE_COL_2_TITLE);

      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // add Body Rows
      int rowNum = 1;
      for (Immunization imm : imms) {

        String medDisplayName = CdaFhirUtilities.getStringForCodeableConcept(imm.getVaccineCode());
        if (StringUtils.isEmpty(medDisplayName))
          medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (imm.hasOccurrenceDateTimeType() && imm.getOccurrenceDateTimeType() != null) {
          dt = imm.getOccurrenceDateTimeType().getValue().toString();
        }

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(CdaGeneratorConstants.IMM_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.IMM_TABLE_COL_2_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum;
      }

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      for (Immunization imm : imms) {
        // add the Entries.
        sb.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));

        // add the immunization Act
        if (imm.getStatus() != ImmunizationStatus.COMPLETED) {
          sb.append(
              CdaGeneratorUtils.getXmlForActWithNegationInd(
                  CdaGeneratorConstants.MED_ACT_EL_NAME,
                  CdaGeneratorConstants.MED_CLASS_CODE,
                  CdaGeneratorConstants.MOOD_CODE_DEF,
                  "true",
                  true));
        } else {
          sb.append(
              CdaGeneratorUtils.getXmlForActWithNegationInd(
                  CdaGeneratorConstants.MED_ACT_EL_NAME,
                  CdaGeneratorConstants.MED_CLASS_CODE,
                  CdaGeneratorConstants.MOOD_CODE_DEF,
                  "false",
                  true));
        }

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.IMMUNIZATION_ACTIVITY_TEMPLATE_ID));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.IMMUNIZATION_ACTIVITY_TEMPLATE_ID,
                CdaGeneratorConstants.IMMUNIZATION_ACTIVITY_TEMPLATE_ID_EXT));

        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), imm.getIdElement().getIdPart()));

        // set status code
        sb.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        // Set up Effective Time for start and End time.
        if (imm.getOccurrenceDateTimeType() != null) {
          logger.debug("Date Value = {}", imm.getOccurrenceDateTimeType().getValue());
          sb.append(
              CdaFhirUtilities.getDateTimeTypeXml(
                  imm.getOccurrenceDateTimeType(), CdaGeneratorConstants.EFF_TIME_EL_NAME));
        } else {
          sb.append(
              CdaGeneratorUtils.getXmlForNullEffectiveTime(
                  CdaGeneratorConstants.EFF_TIME_EL_NAME, CdaGeneratorConstants.NF_NI));
        }

        if (imm.hasRoute() && imm.getRoute().hasCoding()) {
          sb.append(
              CdaFhirUtilities.getCodeableConceptXml(
                  imm.getRoute(), CdaGeneratorConstants.ROUTE_CODE_EL_NAME, ""));
        }

        if (imm.hasDoseQuantity()) {
          sb.append(
              CdaFhirUtilities.getQuantityXml(
                  imm.getDoseQuantity(), CdaGeneratorConstants.DOSE_QUANTITY_EL_NAME, false));
        }

        // add the consumable presentation.
        sb.append(
            CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.CONSUMABLE_EL_NAME));
        sb.append(
            CdaGeneratorUtils.getXmlForStartElementWithClassCode(
                CdaGeneratorConstants.MAN_PROD_EL_NAME, CdaGeneratorConstants.MANU_CLASS_CODE));

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.IMMUNIZATION_MEDICATION_INFORMATION));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.IMMUNIZATION_MEDICATION_INFORMATION,
                CdaGeneratorConstants.IMMUNIZATION_MEDICATION_INFORMATION_EXT));

        sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());
        sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));

        List<CodeableConcept> cds = new ArrayList<>();
        cds.add(imm.getVaccineCode());

        String codeXml =
            CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
                cds,
                CdaGeneratorConstants.CODE_EL_NAME,
                false,
                CdaGeneratorConstants.FHIR_CVX_URL,
                false,
                "");

        if (!codeXml.isEmpty()) {
          sb.append(codeXml);
        } else {
          sb.append(
              CdaFhirUtilities.getCodeableConceptXml(
                  cds, CdaGeneratorConstants.CODE_EL_NAME, false));
        }

        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));

        if (imm.hasManufacturer()) {
          sb.append(getManufacturerXml(imm, data));
        }

        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MAN_PROD_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CONSUMABLE_EL_NAME));

        if (imm.hasPerformer()) {
          sb.append(getXmlForPerformer(imm.getPerformer(), data));
        }

        // End Tags for Entries
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MED_ACT_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));
      }

      // Complete the section end tags.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {
      sb.append(generateEmptyImmunizations());
    }

    return sb.toString();
  }

  public static String getManufacturerXml(Immunization imm, R4FhirData data) {

    StringBuilder retVal = new StringBuilder();

    Organization manufacturer = null;

    if (imm.getManufacturer().hasReference())
      manufacturer =
          data.getOrganizationForId(imm.getManufacturer().getReferenceElement().getIdPart());

    if (manufacturer != null && manufacturer.hasName()) {

      retVal.append(
          CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.MANUFACTURER_ORGANIZATION));
      retVal.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.NAME_EL_NAME, manufacturer.getName()));
      retVal.append(
          CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MANUFACTURER_ORGANIZATION));
    }

    return retVal.toString();
  }

  public static String getXmlForPerformer(
      List<ImmunizationPerformerComponent> izcs, R4FhirData data) {

    StringBuilder sb = new StringBuilder(400);
    String functionCode = "";

    if (izcs != null && data != null) {
      for (ImmunizationPerformerComponent perf : izcs) {
        Reference actor = perf.getActor();

        if (actor != null
            && actor.hasReferenceElement()
            && actor.getReferenceElement().hasResourceType()
            && ResourceType.fromCode(actor.getReferenceElement().getResourceType())
                == ResourceType.Practitioner) {

          Practitioner pract = data.getPractitionerById(actor.getReferenceElement().getIdPart());

          if (pract != null) {
            sb.append(CdaFhirUtilities.getPerformerXml(pract, functionCode));
            return sb.toString();
          }
        }
      }
    }
    return sb.toString();
  }

  public static String generateEmptyImmunizations() {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.IMMUNIZATION_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.IMMUNIZATION_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.IMMUNIZATION_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.IMMUNIZATION_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.IMMUNIZATION_SEC_NAME));

    // add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.IMMUNIZATION_SEC_TITLE));

    // add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No ImmunizationInformation"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static Object generateR31ImmunizationSection(
      R4FhirData data, LaunchDetails details, String version) {
    // TODO Auto-generated method stub
    return null;
  }
}
