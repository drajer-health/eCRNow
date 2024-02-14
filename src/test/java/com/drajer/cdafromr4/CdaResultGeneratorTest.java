package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.StringType;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class CdaResultGeneratorTest extends BaseGeneratorTest {

  private static final String RESULT_CDA_FILE = "CdaTestData/Cda/Result/Result.xml";
  private static final String OBSERVATION_START_JSON =
      "CdaTestData/Observation/ObservationStart.json";
  private static final String DIAGNOSTIC_REPORT_JSON =
      "CdaTestData/DiagnosticReport/DiagnosticReport.json";

  @Test
  public void testGenerateResultsSection() {
    R4FhirData fhirData = new R4FhirData();
    List<Observation> labResults = getObs(OBSERVATION_START_JSON);
    List<DiagnosticReport> diagnosticReports = getDiagnosticReport(DIAGNOSTIC_REPORT_JSON);

    fhirData.addLabResults(labResults);
    fhirData.setDiagReports(diagnosticReports);

    String expectedXml = TestUtils.getFileContentAsString(RESULT_CDA_FILE);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml = CdaResultGenerator.generateResultsSection(fhirData, launchDetails);

    logger.info(" Actual XML ", actualXml);

    assertNotNull("CDA result section should not be null", actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testFindObservation() {

    Observation obs1 = new Observation();
    obs1.setId("Observation/123");
    Observation obs2 = new Observation();
    obs2.setId("Observation/456");
    List<Observation> obsList = Arrays.asList(obs1, obs2);
    Reference r = new Reference("Observation/123");
    Observation result = CdaResultGenerator.findObservation(r, obsList);
    assertEquals(result, obs1);
  }

  @Test
  public void testGetXmlForComponents() {
    DiagnosticReport report = new DiagnosticReport();
    report.setId("1234");
    CodeableConcept reportCode = new CodeableConcept();
    Coding reportCoding = new Coding();
    reportCoding.setSystem("http://loinc.org");
    reportCoding.setCode("12345-6");
    reportCode.addCoding(reportCoding);
    report.setCode(reportCode);
    Reference obsRef = new Reference();
    obsRef.setReference("Observation/5678");
    report.addResult(obsRef);
    Observation obs = new Observation();
    obs.setId("5678");
    obs.setCode(reportCode);
    obs.setValue(new StringType("test value"));
    List<ObservationComponentComponent> components = new ArrayList<>();
    ObservationComponentComponent component = new ObservationComponentComponent();
    CodeableConcept componentCode = new CodeableConcept();
    Coding componentCoding = new Coding();
    componentCoding.setSystem("http://loinc.org");
    componentCoding.setCode("12345-7");
    componentCode.addCoding(componentCoding);
    component.setCode(componentCode);
    component.setValue(new StringType("test component value"));
    components.add(component);
    obs.setComponent(components);
    HashMap<String, Observation> allObs = new HashMap<>();
    allObs.put("5678", obs);

    String contentId = "content";
    int row = 1;

    String actualXml =
        CdaResultGenerator.getXmlForComponents(report, allObs, launchDetails, contentId, row);

    assertNotNull(actualXml);
    assertFalse(actualXml.isEmpty());
  }

  @Test
  public void testAddTriggerCodes() {
    DateTimeType dateTimeType = new DateTimeType("2022-04-01T00:00:00Z");
    Observation observation = new Observation();
    observation.setId("5474974");
    observation.setEffective(dateTimeType);

    String actual = CdaResultGenerator.addTriggerCodes(launchDetails, observation, null);

    assertNotNull(actual);
  }

  @Test
  public void testGenerateEmptyLabResults() {
    StringBuilder expectedXml = new StringBuilder();
    expectedXml.append("<component>").append(System.lineSeparator());
    expectedXml.append("<section nullFlavor=\"NI\">").append(System.lineSeparator());
    expectedXml
        .append("<templateId root=\"2.16.840.1.113883.10.20.22.2.3.1\"/>")
        .append(System.lineSeparator());
    expectedXml
        .append("<templateId root=\"2.16.840.1.113883.10.20.22.2.3.1\" extension=\"2015-08-01\"/>")
        .append(System.lineSeparator());
    expectedXml
        .append(
            "<code code=\"30954-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"RESULTS\"/>")
        .append(System.lineSeparator());
    expectedXml.append("<title>RESULTS</title>").append(System.lineSeparator());
    expectedXml.append("<text>No Lab Results Information</text>").append(System.lineSeparator());
    expectedXml.append("</section>").append(System.lineSeparator());
    expectedXml.append("</component>").append(System.lineSeparator());

    String actualXml = CdaResultGenerator.generateEmptyLabResults();
    assertNotNull(actualXml);

    assertXmlEquals(expectedXml.toString(), actualXml);
  }
}
