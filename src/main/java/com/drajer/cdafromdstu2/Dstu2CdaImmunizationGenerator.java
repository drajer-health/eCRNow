package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.resource.Immunization;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Dstu2CdaImmunizationGenerator {

  private static final Logger logger = LoggerFactory.getLogger(Dstu2CdaImmunizationGenerator.class);

  public static String generateImmunizationSection(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    List<Immunization> imms = data.getImmunizations();

    if (imms != null && imms.size() > 0) {

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
      List<String> list = new ArrayList<String>();
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
        String medDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (imm.getVaccineCode() != null
            && imm.getVaccineCode().getCodingFirstRep() != null
            && !StringUtils.isEmpty(imm.getVaccineCode().getCodingFirstRep().getDisplay())) {

          medDisplayName = imm.getVaccineCode().getCodingFirstRep().getDisplay();
        }

        String dt = null;
        if (imm.getDate() != null) {
          dt = imm.getDate().toString();
        }

        Map<String, String> bodyvals = new HashMap<String, String>();
        bodyvals.put(CdaGeneratorConstants.IMM_TABLE_COL_1_BODY_CONTENT, medDisplayName);
        bodyvals.put(CdaGeneratorConstants.IMM_TABLE_COL_2_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        ++rowNum; // TODO: ++rowNum or rowNum++
      }

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));

      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      for (Immunization imm : imms) {
        // add the Entries.
        sb.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));

        // add the medication Act
        sb.append(
            CdaGeneratorUtils.getXmlForAct(
                CdaGeneratorConstants.MED_ACT_EL_NAME,
                CdaGeneratorConstants.MED_CLASS_CODE,
                CdaGeneratorConstants.MOOD_CODE_DEF));

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.IMMUNIZATION_ACTIVITY_TEMPLATE_ID));
        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.IMMUNIZATION_ACTIVITY_TEMPLATE_ID,
                CdaGeneratorConstants.IMMUNIZATION_ACTIVITY_TEMPLATE_ID_EXT));

        sb.append(
            CdaGeneratorUtils.getXmlForII(
                details.getAssigningAuthorityId(), imm.getId().getIdPart()));

        // set status code
        sb.append(
            CdaGeneratorUtils.getXmlForCD(
                CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

        // Set up Effective Time for start and End time.
        sb.append(
            CdaGeneratorUtils.getXmlForEffectiveTime(
                CdaGeneratorConstants.EFF_TIME_EL_NAME, imm.getDate()));

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

        List<CodeableConceptDt> cds = new ArrayList<CodeableConceptDt>();
        cds.add(imm.getVaccineCode());
        sb.append(
            Dstu2CdaFhirUtilities.getCodeableConceptXml(
                cds, CdaGeneratorConstants.CODE_EL_NAME, false));

        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MANU_MAT_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.MAN_PROD_EL_NAME));
        sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CONSUMABLE_EL_NAME));

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
}
