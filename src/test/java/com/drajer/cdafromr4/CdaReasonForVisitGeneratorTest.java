package com.drajer.cdafromr4;

import static org.junit.Assert.*;
import static org.junit.Assert.assertNotNull;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Test;
import org.powermock.core.classloader.annotations.PowerMockIgnore;

@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaReasonForVisitGeneratorTest extends BaseGeneratorTest {

  private static final String VERSION = CdaGeneratorConstants.CDA_EICR_VERSION_R11;

  static final String REASON_FOR_VISIT_FILENAME =
      "CdaTestData/Encounter/ReasonForVisitWithCode.json";
  static final String ENCOUNTER_WITH_DISPLAY_FILE =
      "CdaTestData/Encounter/Encounter_WithDisplay.json";
  static final String ENCOUNTER_WITH_TEXT_FILE = "CdaTestData/Encounter/Encounter_text.json";

  private static final String REASON_FOR_VISIT_CDA_FILE =
      "CdaTestData/Cda/Encounter/ReasonForVisit.xml";

  private static final String REASON_FOR_VISIT_WITH_TEXT_CDA_FILE =
      "CdaTestData/Cda/Encounter/ReasonForVisitWithText.xml";

  @Test
  public void testGenerateReasonForVisitSection() {

    Encounter encounter =
        (Encounter) loadResourceDataFromFile(Encounter.class, REASON_FOR_VISIT_FILENAME);
    r4FhirData.setEncounter(encounter);
    String expectedXml = TestUtils.getFileContentAsString(REASON_FOR_VISIT_CDA_FILE);
    String actualXml =
        CdaReasonForVisitGenerator.generateReasonForVisitSection(r4FhirData, VERSION);
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
        CdaReasonForVisitGenerator.generateReasonForVisitSection(r4FhirData, VERSION);
    assertNotNull(actualXml);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateReasonForVisitSection_variants() {
    Encounter withReason = new Encounter();
    withReason.setId("rfv-1");
    withReason.addReasonCode(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL)
                    .setCode("386661006")
                    .setDisplay("Fever"))
            .setText("Fever"));
    R4FhirData d1 = new R4FhirData();
    d1.setEncounter(withReason);
    assertNotNull(
        "A section must be produced for an encounter with reason codes",
        CdaReasonForVisitGenerator.generateReasonForVisitSection(d1, VERSION));

    R4FhirData d2 = new R4FhirData();
    d2.setEncounter(new Encounter());
    assertNotNull(
        "A section must be produced for an encounter with no reason codes",
        CdaReasonForVisitGenerator.generateReasonForVisitSection(d2, VERSION));

    assertNotNull(
        "A section must be produced when there is no encounter",
        CdaReasonForVisitGenerator.generateReasonForVisitSection(new R4FhirData(), VERSION));
  }
}
