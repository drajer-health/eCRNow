package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TimeZone;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.javatuples.Pair;
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
public class CdaProblemGeneratorTest extends BaseGeneratorTest {
  static final String CONDITION_PROBLEM_WITH_COVID_TRIGGER_CODE_FILE =
      "R4/Condition/Condition-Problem-WithCovidTriggerCode.json";

  private static final String PROBLEM_CDA_FILE = "CdaTestData/Cda/Problem/Problem.xml";

  private static final String VERSION = CdaGeneratorConstants.CDA_EICR_VERSION_R11;

  @Test
  public void testGenerateProblemSection() {
    R4FhirData r4Data = createResourceData(CONDITION_PROBLEM_WITH_COVID_TRIGGER_CODE_FILE);
    String expectedXml = TestUtils.getFileContentAsString(PROBLEM_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaProblemGenerator.generateProblemSection(r4Data, launchDetails, VERSION);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testAddTriggerCodes() {
    LaunchDetails details = new LaunchDetails();
    Condition cond = new Condition();
    cond.setCode(
        new CodeableConcept()
            .addCoding(new Coding().setCode("68518-0").setSystem("http://loinc.org")));
    Pair<Date, TimeZone> onset = new Pair<>(new Date(), TimeZone.getTimeZone("UTC"));
    Pair<Date, TimeZone> abatement = new Pair<>(new Date(), TimeZone.getTimeZone("UTC"));
    PatientExecutionState state =
        createPatientExecutionState("Condition", "http://loinc.org|68518-0");

    details.setStatus(TestUtils.toJsonString(state));

    String result =
        CdaProblemGenerator.addTriggerCodes(
            details, cond.getCode(), onset, abatement, cond.getIdElement().getId());
    assertNotNull(result);
  }

  @Test
  public void testGenerateEmptyProblemSection() {
    String expectedXml =
        """
                <component>\r
                <section nullFlavor="NI">\r
                <templateId root="2.16.840.1.113883.10.20.22.2.5.1"/>\r
                <templateId root="2.16.840.1.113883.10.20.22.2.5.1" extension="2015-08-01"/>\r
                <code code="11450-4" codeSystem="2.16.840.1.113883.6.1" codeSystemName="LOINC" displayName="PROBLEM LIST"/>\r
                <title>PROBLEMS - DIAGNOSES</title>\r
                <text>No Problem Information</text>\r
                </section>\r
                </component>""";
    String actualXml = CdaProblemGenerator.generateEmptyProblemSection();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetValueCodeXmlFromMatchedCode_emitsValueCdForFirstMatch() {
    Set<String> matchedCodes = new LinkedHashSet<>();
    matchedCodes.add("http://snomed.info/sct|840539006");

    LaunchDetails details = new LaunchDetails();
    details.setRctcOid("2.16.840.1.114222.4.11.7508");
    details.setRctcVersion("20210526");

    String xml = CdaProblemGenerator.getValueCodeXmlFromMatchedCode(matchedCodes, details, 1, null);

    assertNotNull("A value CD must be produced for a matched code", xml);
    assertTrue("the code part must be emitted", xml.contains("840539006"));
    assertTrue("the RCTC OID must be emitted", xml.contains("2.16.840.1.114222.4.11.7508"));
    assertTrue("the RCTC version must be emitted", xml.contains("20210526"));
    assertTrue(
        "the narrative content reference must use the row number",
        xml.contains(CdaGeneratorConstants.PROB_TABLE_COL_1_BODY_CONTENT + "1"));
  }

  @Test
  public void testGetValueCodeXmlFromMatchedCode_usesOnlyFirstNonNullCode() {
    Set<String> matchedCodes = new LinkedHashSet<>();
    matchedCodes.add(null);
    matchedCodes.add("http://snomed.info/sct|111111");
    matchedCodes.add("http://snomed.info/sct|222222");

    LaunchDetails details = new LaunchDetails();
    details.setRctcOid("oid");
    details.setRctcVersion("v1");

    String xml = CdaProblemGenerator.getValueCodeXmlFromMatchedCode(matchedCodes, details, 2, null);

    assertNotNull(xml);
    assertTrue("first non-null code must be used", xml.contains("111111"));
    assertTrue("later codes must be ignored", !xml.contains("222222"));
  }

  @Test
  public void testGetValueCodeXmlFromMatchedCode_nullOrEmptyReturnsEmptyString() {
    LaunchDetails details = new LaunchDetails();

    assertEquals(
        "null set must yield an empty string",
        "",
        CdaProblemGenerator.getValueCodeXmlFromMatchedCode(null, details, 0, null));
    assertEquals(
        "empty set must yield an empty string",
        "",
        CdaProblemGenerator.getValueCodeXmlFromMatchedCode(new HashSet<>(), details, 0, null));
  }

  @Test
  public void testGetValueCodeXmlFromMatchedCode_unknownCodeSystemUrl() {
    Set<String> matchedCodes = new LinkedHashSet<>();
    matchedCodes.add("http://example.org/not-a-known-system|XYZ");

    LaunchDetails details = new LaunchDetails();
    details.setRctcOid("oid");
    details.setRctcVersion("v1");

    String xml = CdaProblemGenerator.getValueCodeXmlFromMatchedCode(matchedCodes, details, 3, null);

    assertNotNull("Must still produce output for an unknown code system", xml);
    assertTrue("the code part must still be emitted", xml.contains("XYZ"));
  }
}
