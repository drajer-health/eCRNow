package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class CdaEncounterGeneratorTest extends BaseGeneratorTest {

  private static final String ENCOUNTER_FILE = "R4/Encounter/EncounterBundle_97953900.json";
  private static final String ENCOUNTER_DIAGNOSIS_CONDITION_FILE =
      "R4/Condition/Condition-Problem-WithCovidTriggerCode.json";
  private static final String ENCOUNTER_CDA_FILE = "CdaTestData/Cda/Encounter/encounter.xml";
  private static final String EXPECTED_EMPTY_ENCOUNTER_SECTION =
      "<component>\r\n"
          + "<section nullFlavor=\"NI\">\r\n"
          + "<templateId root=\"2.16.840.1.113883.10.20.22.2.22.1\"/>\r\n"
          + "<templateId root=\"2.16.840.1.113883.10.20.22.2.22.1\" extension=\"2015-08-01\"/>\r\n"
          + "<code code=\"46240-8\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"History of Encounters\"/>\r\n"
          + "<title>ENCOUNTERS</title>\r\n"
          + "<text>No Encounter Information</text>\r\n"
          + "</section>\r\n"
          + "</component>\r\n"
          + "";

  @Test
  public void testGenerateEncounterSection() {

    R4FhirData r4FhirData = createResourceData(ENCOUNTER_FILE);
    r4FhirData.setEncounterDiagnosisConditions(
        getEncounterDiagnosisConditions(ENCOUNTER_DIAGNOSIS_CONDITION_FILE));
    String expectedXml = TestUtils.getFileContentAsString(ENCOUNTER_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getGuid())
        .thenReturn("b56b6d6d-7d6e-4ff4-9e5c-f8625c7babe9");

    String actualXml = CdaEncounterGenerator.generateEncounterSection(r4FhirData, launchDetails);

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

    expectedCodeXml =
        "<code code=\"AMB\" codeSystem=\"2.16.840.1.113883.5.4\" codeSystemName=\"v3-ActCode\"><originalText><reference value=\"#test\"/></originalText></code>";

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

    expectedCodeXml =
        "<code code=\"456\" codeSystem=\"2.16.840.1.113883.6.12\" codeSystemName=\"CPT\"><originalText><reference value=\"#test\"/></originalText></code>";

    actualCodeXml = CdaEncounterGenerator.getEncounterCodeXml(en2, contentRef);
    assertEquals(actualCodeXml.trim(), actualCodeXml.trim());
  }
}
