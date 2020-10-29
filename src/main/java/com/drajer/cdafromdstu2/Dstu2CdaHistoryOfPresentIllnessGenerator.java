package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.resource.Condition;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

public class Dstu2CdaHistoryOfPresentIllnessGenerator {

  private static final Logger logger =
      LoggerFactory.getLogger(Dstu2CdaHistoryOfPresentIllnessGenerator.class);

  public static String generateHistoryOfPresentIllnessSection(
      Dstu2FhirData data, LaunchDetails details) {

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
    // Need to Discuss with vendors on how to best get this information.
    // sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME,
    //    "History of Present Illness Not Known"));

    // Add Narrative Text
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

    List<Condition> conds = data.getConditions();

    // Create Table Header.
    List<String> list = new ArrayList<String>();
    list.add(CdaGeneratorConstants.NARRATIVE_TEXT_EL_NAME);
    sb.append(
        CdaGeneratorUtils.getXmlForTableHeader(
            list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

    // Add Table Body
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    String text = CdaGeneratorConstants.UNKNOWN_HISTORY_OF_PRESENT_ILLNESS;
    int rowNum = 1;

    if (conds != null && conds.size() > 0) {

      // Add Body Rows
      for (Condition prob : conds) {
        String probDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (prob.getCode() != null && !StringUtils.isEmpty(prob.getCode().getText())) {

          probDisplayName = prob.getCode().getText();
        } else if (prob.getCode().getCodingFirstRep() != null
            && !StringUtils.isEmpty(prob.getCode().getCodingFirstRep().getDisplay())) {

          probDisplayName = prob.getCode().getCodingFirstRep().getDisplay();
        }

        Map<String, String> bodyvals = new HashMap<String, String>();
        bodyvals.put(
            CdaGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_BODY_CONTENT, probDisplayName);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));
        ++rowNum;
      }
    } else {

      Map<String, String> bodyvals = new HashMap<String, String>();
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
