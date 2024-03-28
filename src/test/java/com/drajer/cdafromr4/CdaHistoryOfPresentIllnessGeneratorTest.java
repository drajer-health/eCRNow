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
            + "<section nullFlavor=\"NI\">\r\n"
            + "<templateId root=\"1.3.6.1.4.1.19376.1.5.3.1.3.4\"/>\r\n"
            + "<code code=\"10164-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"History of Present Illness\"/>\r\n"
            + "<title>History of Present Illness</title>\r\n"
            + "<text>No History of Present Illness Information</text>\r\n"
            + "</section>\r\n"
            + "</component>";

    String actualXml =
        CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
            new R4FhirData());

    assertXmlEquals(expectedXml, actualXml);
  }
}
