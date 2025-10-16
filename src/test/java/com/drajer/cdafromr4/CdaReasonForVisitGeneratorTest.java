package com.drajer.cdafromr4;

import static org.junit.Assert.*;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Test;

public class CdaReasonForVisitGeneratorTest extends BaseGeneratorTest {

  static final String REASON_FOR_VISIT_FILENAME =
      "CdaTestData/Encounter/ReasonForVisitWithCode.json";
  static final String ENCOUNTER_WITH_DISPLAY_FILE =
      "CdaTestData/Encounter/Encounter_WithDisplay.json";
  static final String ENCOUNTER_WITH_TEXT_FILE = "CdaTestData/Encounter/Encounter_text.json";

  private static final String REASON_FOR_VISIT_CDA_FILE =
      "CdaTestData/Cda/Encounter/ReasonForVisit.xml";

  private static final String REASON_FOR_VISIT_WITH_DISPLAY_CDA_FILE_ =
      "CdaTestData/Cda/Encounter/ReasonForVisitWithDisplay.xml";

  private static final String REASON_FOR_VISIT_WITH_TEXT_CDA_FILE =
      "CdaTestData/Cda/Encounter/ReasonForVisitWithText.xml";

  @Test
  public void testGenerateReasonForVisitSection() {

    Encounter encounter =
        (Encounter) loadResourceDataFromFile(Encounter.class, REASON_FOR_VISIT_FILENAME);
    r4FhirData.setEncounter(encounter);
    String expectedXml = TestUtils.getFileContentAsString(REASON_FOR_VISIT_CDA_FILE);
    String actualXml =
        CdaReasonForVisitGenerator.generateReasonForVisitSection(
            r4FhirData, CdaGeneratorConstants.CDA_EICR_VERSION_R11);
    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateReasonForVisitSection_withDisplay() {

    Encounter encounter =
        (Encounter) loadResourceDataFromFile(Encounter.class, ENCOUNTER_WITH_DISPLAY_FILE);
    r4FhirData.setEncounter(encounter);
    String expectedXml = TestUtils.getFileContentAsString(REASON_FOR_VISIT_WITH_DISPLAY_CDA_FILE_);
    String actualXml =
        CdaReasonForVisitGenerator.generateReasonForVisitSection(
            r4FhirData, CdaGeneratorConstants.CDA_EICR_VERSION_R11);
    assertNotNull(actualXml);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateReasonForVisitSection_withText() {

    Encounter encounter =
        (Encounter) loadResourceDataFromFile(Encounter.class, ENCOUNTER_WITH_TEXT_FILE);
    r4FhirData.setEncounter(encounter);
    String expectedXml = TestUtils.getFileContentAsString(REASON_FOR_VISIT_WITH_TEXT_CDA_FILE);
    String actualXml =
        CdaReasonForVisitGenerator.generateReasonForVisitSection(
            r4FhirData, CdaGeneratorConstants.CDA_EICR_VERSION_R11);
    assertNotNull(actualXml);
    assertXmlEquals(expectedXml, actualXml);
  }
}
