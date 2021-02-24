package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.Condition;
import org.springframework.util.StringUtils;

public class CdaHistoryOfPresentIllnessGenerator {

  private CdaHistoryOfPresentIllnessGenerator() {}

  public static String generateHistoryOfPresentIllnessSection(R4FhirData data) {

    StringBuilder sb = new StringBuilder(2000);

    // Will have to wait to discuss with vendors on History of Present Illness and how to obtain
    // that information reliably..
    // Then we can generate better text.

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SEC_TEMPLATE_ID));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SEC_CODE_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME,
            CdaGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SEC_TITLE));

    // Add Narrative Text
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

    List<Condition> conds = data.getEncounterDiagnosisConditions();

    // Create Table Header.
    List<String> list = new ArrayList<>();
    list.add(CdaGeneratorConstants.NARRATIVE_TEXT_EL_NAME);
    sb.append(
        CdaGeneratorUtils.getXmlForTableHeader(
            list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

    // Add Table Body
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    String text = CdaGeneratorConstants.UNKNOWN_HISTORY_OF_PRESENT_ILLNESS;
    int rowNum = 1;

    if (conds != null && !conds.isEmpty()) {

      // Add Body Rows
      for (Condition prob : conds) {
        String probDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (prob.getCode() != null && !StringUtils.isEmpty(prob.getCode().getText())) {

          probDisplayName = prob.getCode().getText();
        } else if (prob.getCode().getCodingFirstRep() != null
            && !StringUtils.isEmpty(prob.getCode().getCodingFirstRep().getDisplay())) {

          probDisplayName = prob.getCode().getCodingFirstRep().getDisplay();
        }

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(
            CdaGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_BODY_CONTENT, probDisplayName);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
        ++rowNum;
      }
    } else {

      Map<String, String> bodyvals = new HashMap<>();
      bodyvals.put(CdaGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_BODY_CONTENT, text);

      sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
    }

    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    // End Table.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }
}
