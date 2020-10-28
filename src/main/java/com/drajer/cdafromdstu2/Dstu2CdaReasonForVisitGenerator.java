package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.resource.Encounter;
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

public class Dstu2CdaReasonForVisitGenerator {

  private static final Logger logger =
      LoggerFactory.getLogger(Dstu2CdaReasonForVisitGenerator.class);

  public static String generateReasonForVisitSection(Dstu2FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    Encounter encounter = data.getEncounter();

    // Will have to wait to discuss with vendors on Reason For Visit and how to obtain that
    // information reliably..
    // Then we can generate better text.

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.REASON_FOR_VISIT_SEC_TEMPLATE_ID));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.REASON_FOR_VISIT_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.REASON_FOR_VISIT_SEC_CODE_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.REASON_FOR_VISIT_SEC_TITLE));

    // Add Narrative Text
    // Need to Discuss with vendors on how to best get this information.
    // sb.append(CdaGeneratorUtils.getXmlForText(CdaGeneratorConstants.TEXT_EL_NAME,
    //     "Reason Not Known"));

    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

    // Create Table Header.
    List<String> list = new ArrayList<String>();
    list.add(CdaGeneratorConstants.TEXT_EL_NAME);
    sb.append(
        CdaGeneratorUtils.getXmlForTableHeader(
            list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

    // Add Table Body
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

    // Add Body Rows
    int rowNum = 1;
    String text = CdaGeneratorConstants.UNKNOWN_REASON_FOR_VISIT;

    if (encounter != null && encounter.getReasonFirstRep() != null) {

      if (!StringUtils.isEmpty(encounter.getReasonFirstRep().getText())) {
        text = encounter.getReasonFirstRep().getText();
      } else if (encounter.getReasonFirstRep().getCodingFirstRep() != null
          && !StringUtils.isEmpty(encounter.getReasonFirstRep().getCodingFirstRep().getDisplay())) {
        text = encounter.getReasonFirstRep().getCodingFirstRep().getDisplay();
      }
    }

    Map<String, String> bodyvals = new HashMap<String, String>();
    bodyvals.put(CdaGeneratorConstants.REASON_FOR_VISIT_BODY_CONTENT, text);

    sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

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
