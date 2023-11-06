package com.drajer.cdafromr4;

import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.junit.Test;

public class CdaHistoryOfPresentIllnessGeneratorTest extends BaseGeneratorTest {

  static final String CONDITION_JSON_FILE_PATH =
      "R4/Condition/Condition-Problem-WithCovidTriggerCode.json";
  private static final String HISTORY_OF_PATIENT_ILLNESS_CDA_FILE =
      "CdaTestData/Cda/HistoryOfPresentIllness/HistoryOfPresentIllness.xml";

  @Test
  public void testGenerateHistoryOfPresentIllnessSection() {
    R4FhirData r4FhirData = createResourceData(CONDITION_JSON_FILE_PATH);

    r4FhirData.setEncounterDiagnosisConditions(r4FhirData.getConditions());
    String expectedXml = TestUtils.getFileContentAsString(HISTORY_OF_PATIENT_ILLNESS_CDA_FILE);
    String actualXml =
        CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(r4FhirData);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateHistoryOfPresentIllnessSectionWithEmptyConditions() {
    String expectedXml =
        "<component>\r\n"
            + "<section>\r\n"
            + "<templateId root=\"1.3.6.1.4.1.19376.1.5.3.1.3.4\"/>\r\n"
            + "<code code=\"10164-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"History of Present Illness\"/>\r\n"
            + "<title>History of Present Illness</title>\r\n"
            + "<text>\r\n"
            + "<table border=\"1\" width=\"100%\">\r\n"
            + "<thead>\r\n"
            + "<tr>\r\n"
            + "<th>Narrative Text</th>\r\n"
            + "</tr>\r\n"
            + "</thead>\r\n"
            + "<tbody>\r\n"
            + "<tr>\r\n"
            + "<td>\r\n"
            + "<content ID=\"historyOfPresentIllness1\">Unknown History of Present Illness</content>\r\n"
            + "</td>\r\n"
            + "</tr>\r\n"
            + "</tbody>\r\n"
            + "</table>\r\n"
            + "</text>\r\n"
            + "</section>\r\n"
            + "</component>\r\n"
            + "";
    String actualXml =
        CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
            new R4FhirData());

    assertXmlEquals(expectedXml, actualXml);
  }
}
