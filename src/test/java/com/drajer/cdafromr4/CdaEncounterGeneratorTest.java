package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Period;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaEncounterGeneratorTest extends BaseGeneratorTest {

  private static final String VERSION = CdaGeneratorConstants.CDA_EICR_VERSION_R11;

  private static final String ENCOUNTER_FILE = "R4/Encounter/EncounterBundle_97953900.json";
  private static final String ENCOUNTER_DIAGNOSIS_CONDITION_FILE =
      "R4/Condition/Condition-Problem-WithCovidTriggerCode.json";
  private static final String ENCOUNTER_CDA_FILE = "CdaTestData/Cda/Encounter/encounter.xml";
  private static final String EXPECTED_EMPTY_ENCOUNTER_SECTION =
      """
            <component>\r
            <section nullFlavor="NI">\r
            <templateId root="2.16.840.1.113883.10.20.22.2.22.1"/>\r
            <templateId root="2.16.840.1.113883.10.20.22.2.22.1" extension="2015-08-01"/>\r
            <code code="46240-8" codeSystem="2.16.840.1.113883.6.1" codeSystemName="LOINC" displayName="History of Encounters"/>\r
            <title>ENCOUNTERS</title>\r
            <text>No Encounter Information</text>\r
            </section>\r
            </component>\r
            """;

  @Test
  public void testGenerateEncounterSection() {

    R4FhirData r4FhirData = createResourceData(ENCOUNTER_FILE);
    r4FhirData.setEncounterDiagnosisConditions(
        getEncounterDiagnosisConditions(ENCOUNTER_DIAGNOSIS_CONDITION_FILE));
    String expectedXml = TestUtils.getFileContentAsString(ENCOUNTER_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getGuid())
        .thenReturn("b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9");

    String actualXml =
        CdaEncounterGenerator.generateEncounterSection(r4FhirData, launchDetails, VERSION);

    assertNotNull(actualXml);
    assertEquals(
        StringUtils.normalizeSpace(expectedXml).trim(),
        StringUtils.normalizeSpace(actualXml).trim());
  }

  @Test
  public void testGenerateEmptyEncounterSection() {

    String actualXml = CdaEncounterGenerator.generateEmptyEncounterSection();

    assertEquals(
        StringUtils.normalizeSpace(EXPECTED_EMPTY_ENCOUNTER_SECTION).trim(),
        StringUtils.normalizeSpace(actualXml).trim());
  }

  @Test
  public void testGetEncounterCodeXmlWithvalidEncounter() {
    Encounter encounter = new Encounter();
    Coding encClass = new Coding();
    encClass.setCode("123");
    encounter.setClass_(encClass);
    List<CodeableConcept> cds = new ArrayList<>();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();
    coding.setCode("456");
    codeableConcept.addCoding(coding);
    cds.add(codeableConcept);
    String contentRef = "test";
    String expectedCodeXml = "<code nullFlavor=\"NI\"/>";
    String actualCodeXml = CdaEncounterGenerator.getEncounterCodeXml(encounter, contentRef);
    assertEquals(expectedCodeXml.trim(), actualCodeXml.trim());

    Encounter en1 = new Encounter();
    Coding enClass1 = new Coding();
    enClass1.setCode("AMB");
    enClass1.setSystem(CdaGeneratorConstants.FHIR_ENCOUNTER_CLASS_URL);
    en1.setClass_(enClass1);

    actualCodeXml = CdaEncounterGenerator.getEncounterCodeXml(en1, contentRef);
    assertEquals(actualCodeXml.trim(), actualCodeXml.trim());

    Encounter en2 = new Encounter();
    List<CodeableConcept> cds2 = new ArrayList<>();
    CodeableConcept codeableConcept2 = new CodeableConcept();
    Coding coding2 = new Coding();
    coding2.setCode("456");
    coding2.setSystem("http://www.ama-assn.org/go/cpt");
    codeableConcept2.addCoding(coding2);
    cds2.add(codeableConcept2);
    en2.setType(cds2);

    actualCodeXml = CdaEncounterGenerator.getEncounterCodeXml(en2, contentRef);
    assertEquals(actualCodeXml.trim(), actualCodeXml.trim());
  }

  private void mockGuid() {
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
  }

  private R4FhirData dataWithEncounter(Encounter enc) {
    R4FhirData data = new R4FhirData();
    data.setEncounter(enc);
    return data;
  }

  @Test
  public void testGenerateEncounterSection_displayNameFromEncounterClass() {
    Encounter enc = new Encounter();
    enc.setId("enc-class");
    enc.setClass_(new Coding().setCode("AMB").setDisplay("ambulatory"));
    enc.setPeriod(new Period().setStart(new Date()));

    mockGuid();
    String xml =
        CdaEncounterGenerator.generateEncounterSection(
            dataWithEncounter(enc), launchDetails, VERSION);

    assertNotNull(xml);
    assertTrue("the class display must be used in the narrative", xml.contains("ambulatory"));
  }

  @Test
  public void testGenerateEncounterSection_displayNameFromEncounterType() {
    Encounter enc = new Encounter();
    enc.setId("enc-type");
    enc.addType(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL)
                    .setCode("162673000")
                    .setDisplay("General examination")));
    enc.setPeriod(new Period().setStart(new Date()));

    mockGuid();
    String xml =
        CdaEncounterGenerator.generateEncounterSection(
            dataWithEncounter(enc), launchDetails, VERSION);

    assertNotNull(xml);
    assertTrue(
        "the type display must be used in the narrative", xml.contains("General examination"));
  }

  @Test
  public void testGenerateEncounterSection_noDisplayNameAndNoPeriod() {
    Encounter enc = new Encounter();
    enc.setId("enc-bare");

    mockGuid();
    String xml =
        CdaEncounterGenerator.generateEncounterSection(
            dataWithEncounter(enc), launchDetails, VERSION);

    assertNotNull("A section must still be produced", xml);
    assertTrue(
        "the unknown placeholder must be used", xml.contains(CdaGeneratorConstants.UNKNOWN_VALUE));
  }

  @Test
  public void testGenerateEncounterSection_withNoEncounter() {
    mockGuid();
    String xml =
        CdaEncounterGenerator.generateEncounterSection(new R4FhirData(), launchDetails, VERSION);

    assertNotNull(xml);
    assertTrue("an empty encounter section must be produced", xml.contains("<section"));
  }

  @Test
  public void testGetEncounterCodeXml_allArms() {
    Encounter withClass = new Encounter();
    withClass.setClass_(
        new Coding()
            .setSystem(CdaGeneratorConstants.FHIR_ENCOUNTER_CLASS_URL)
            .setCode("AMB")
            .setDisplay("ambulatory"));
    assertNotNull(
        "class_ must yield code xml", CdaEncounterGenerator.getEncounterCodeXml(withClass, "ref1"));

    Encounter withType = new Encounter();
    withType.addType(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL)
                    .setCode("162673000")));
    assertNotNull(
        "type must be used when class_ yields nothing",
        CdaEncounterGenerator.getEncounterCodeXml(withType, "ref2"));

    assertNotNull(
        "a null encounter must not throw", CdaEncounterGenerator.getEncounterCodeXml(null, "ref3"));

    assertNotNull(
        "a bare encounter must not throw",
        CdaEncounterGenerator.getEncounterCodeXml(new Encounter(), "ref4"));
  }

  @Test
  public void testGenerateEncounterReasonXml_withAndWithoutReasonCode() {
    Encounter withReason = new Encounter();
    withReason.addReasonCode(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL)
                    .setCode("386661006")
                    .setDisplay("Fever")));

    String xml =
        CdaEncounterGenerator.generateEncounterReasonXml(
            new R4FhirData(), launchDetails, withReason);
    assertNotNull("A reason xml must be produced for a SNOMED reason code", xml);

    String none =
        CdaEncounterGenerator.generateEncounterReasonXml(
            new R4FhirData(), launchDetails, new Encounter());
    assertNotNull("An encounter with no reasonCode must not throw", none);
    assertTrue("no reason code must yield an empty string", none.isEmpty());
  }

  @Test
  public void testGenerateEncounterDiagnosisXml_withNoDiagnoses() {
    String xml =
        CdaEncounterGenerator.generateEncounterDiagnosisXml(new R4FhirData(), launchDetails);

    assertNotNull("Must return a value rather than null", xml);
  }

  @Test
  public void testGenerate31EncounterSection_isStillAStub() {
    assertNull(
        "generate31EncounterSection is a TODO stub and returns null",
        CdaEncounterGenerator.generate31EncounterSection(new R4FhirData(), launchDetails, VERSION));
  }
}
