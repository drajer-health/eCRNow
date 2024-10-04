package com.drajer.cdafromr4;

import static org.junit.Assert.assertNotNull;

import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.Date;
import java.util.TimeZone;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Condition;
import org.javatuples.Pair;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class CdaProblemGeneratorTest extends BaseGeneratorTest {
  static final String CONDITION_PROBLEM_WITH_COVID_TRIGGER_CODE_FILE =
      "R4/Condition/Condition-Problem-WithCovidTriggerCode.json";

  private static final String PROBLEM_CDA_FILE = "CdaTestData/Cda/Problem/Problem.xml";

  @Test
  public void testGenerateProblemSection() {
    R4FhirData r4Data = createResourceData(CONDITION_PROBLEM_WITH_COVID_TRIGGER_CODE_FILE);
    String expectedXml = TestUtils.getFileContentAsString(PROBLEM_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaProblemGenerator.generateProblemSection(r4Data, launchDetails);

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
        "<component>\r\n"
            + "<section nullFlavor=\"NI\">\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.5.1\"/>\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.5.1\" extension=\"2015-08-01\"/>\r\n"
            + "<code code=\"11450-4\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"PROBLEM LIST\"/>\r\n"
            + "<title>PROBLEMS - DIAGNOSES</title>\r\n"
            + "<text>No Problem Information</text>\r\n"
            + "</section>\r\n"
            + "</component>";
    String actualXml = CdaProblemGenerator.generateEmptyProblemSection();
    assertXmlEquals(expectedXml, actualXml);
  }
}
