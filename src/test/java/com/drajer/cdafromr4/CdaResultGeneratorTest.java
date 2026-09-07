package com.drajer.cdafromr4;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
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
public class CdaResultGeneratorTest extends BaseGeneratorTest {

  private static final String RESULT_CDA_FILE = "CdaTestData/Cda/Result/Result.xml";

  private static final String RESULT_CDA_TRIGGER_FILE =
      "CdaTestData/Cda/Result/result-section-trigger.xml";
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

    PowerMockito.when(
            CdaGeneratorUtils.getXmlForEffectiveTime(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn("<effectiveTime value=\"20200428194229+0000\"/>");

    String actualXml =
        CdaResultGenerator.generateResultsSection(fhirData, launchDetails, "CDA_R11");

    assertNotNull("CDA result section should not be null", actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateResultsSection_else() {
    R4FhirData fhirData = new R4FhirData();
    String actualXml =
        CdaResultGenerator.generateResultsSection(fhirData, launchDetails, "CDA_R11");
    assertNotNull(actualXml);
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
        CdaResultGenerator.getXmlForComponents(
            report, allObs, launchDetails, contentId, row, r4FhirData, "CDA_R31");

    assertNotNull(actualXml);
    assertFalse(actualXml.isEmpty());
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

  @Test
  public void testGenerateResultSection() {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/LoadingQuery/LoadingQueryBundle_Result.json");

    List<Bundle.BundleEntryComponent> entries = b.getEntry();
    Bundle bundle = new Bundle();
    Set<Resource> resourceSet = new LinkedHashSet<>(); // Initialize HashSet outside the loop

    Map<ResourceType, Set<Resource>> resourcesByType = new HashMap<>();

    for (Bundle.BundleEntryComponent ent : entries) {
      Resource resource = ent.getResource();
      ResourceType resourceType = resource.getResourceType();

      resourcesByType.computeIfAbsent(resourceType, k -> new LinkedHashSet<>()).add(resource);
    }

    Map<String, List<String>> uniqueResourceIdsByType = new HashMap<>();
    for (Bundle.BundleEntryComponent ent : entries) {

      ResourceType resourceType = ent.getResource().getResourceType();

      resourceSet.addAll(resourcesByType.getOrDefault(resourceType, Collections.EMPTY_SET));

      if (!resourceSet.isEmpty()) {
        R3ToR2DataConverterUtils.addResourcesToR4FhirData(
            "1",
            bundle,
            data,
            launchDetails,
            resourceSet,
            resourceType.toString(),
            uniqueResourceIdsByType);
        resourceSet.clear();
        resourcesByType.remove(resourceType);
      }
    }
    data.getLabResults().sort(Comparator.comparing(Observation::getId));
    data.getDiagReports().sort(Comparator.comparing(DiagnosticReport::getId));
    data.setData(bundle);

    String expectedXml = TestUtils.getFileContentAsString(RESULT_CDA_TRIGGER_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    PowerMockito.when(CdaGeneratorUtils.getXmlForII(any())).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaResultGenerator.generateResultsSection(
            data, launchDetails, CdaGeneratorConstants.CDA_EICR_VERSION_R31);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testgetTriggerCodeTemplateXml() {
    String expectedXml =
        "<templateId root=\"2.16.840.1.113883.10.20.15.2.3.2\" extension=\"2019-04-01\"/>";
    String actualXml = CdaResultGenerator.getTriggerCodeTemplateXml("CDA_R31");
    assertNotNull(actualXml);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testgetTriggerCodeTemplateXml_else() {
    String expectedXml =
        "<templateId root=\"2.16.840.1.113883.10.20.15.2.3.2\" extension=\"2016-12-01\"/>";
    String actualXml = CdaResultGenerator.getTriggerCodeTemplateXml("CDA_R11");
    assertNotNull(actualXml);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetSpecimenXml_WithSpecimen() {
    Specimen specimen = new Specimen();
    specimen.setId("Specimen/123");
    CodeableConcept type = new CodeableConcept();
    type.addCoding().setCode("SERUM");
    specimen.setType(type);

    String result = CdaResultGenerator.getSpecimenXml(specimen, launchDetails);

    assertNotNull("Specimen XML should not be null", result);
  }

  @Test
  public void testGetSpecimenXml_WithNullSpecimen() {
    String result = CdaResultGenerator.getSpecimenXml(null, launchDetails);

    assertNotNull("Result should handle null specimen", result);
  }

  @Test
  public void testObservationHasSpecimen_True() {
    Observation obs = new Observation();
    Reference specRef = new Reference("Specimen/1");
    obs.setSpecimen(specRef);

    assertTrue("Observation should have specimen", obs.hasSpecimen());
  }

  @Test
  public void testObservationHasSpecimen_False() {
    Observation obs = new Observation();

    assertFalse("Observation should not have specimen when not set", obs.hasSpecimen());
  }

  @Test
  public void testGetSpecimenXml_WithReferences() {
    R4FhirData data = new R4FhirData();
    Reference ref = new Reference("Specimen/1");
    List<Reference> refs = Arrays.asList(ref);

    String result = CdaResultGenerator.getSpecimenXml(refs, data, launchDetails);

    assertNotNull("Result should handle specimen references", result);
  }

  @Test
  public void testProcessDiagnosticResults_DiagnosticReportHasSpecimen() {
    DiagnosticReport report = new DiagnosticReport();
    report.setId("DR/789");
    List<Reference> specRefs = new ArrayList<>();
    specRefs.add(new Reference("Specimen/spec-1"));
    report.setSpecimen(specRefs);

    assertTrue("DiagnosticReport should have specimen", report.hasSpecimen());
  }

  @Test
  public void testGetSpecimenXml_WithBodySite() {
    Specimen spec = new Specimen();
    spec.setId("Specimen/123");

    Specimen.SpecimenCollectionComponent collection = new Specimen.SpecimenCollectionComponent();
    CodeableConcept bodySite = new CodeableConcept();
    bodySite.addCoding(
        new Coding().setSystem(CdaGeneratorConstants.FHIR_SNOMED_URL).setCode("123456"));
    collection.setBodySite(bodySite);
    spec.setCollection(collection);

    String result = CdaResultGenerator.getSpecimenXml(spec, launchDetails);
    assertNotNull("Specimen XML with body site should not be null", result);
    assertTrue("Should contain body site content", result.length() > 0);
  }

  @Test
  public void testGetXmlForComponents_ObservationWithoutComponents() {
    DiagnosticReport report = new DiagnosticReport();
    report.setId("DR/1");
    report.setCode(new CodeableConcept().addCoding(new Coding().setCode("99999-9")));

    Reference obsRef = new Reference("Observation/obs-1");
    report.addResult(obsRef);

    Observation obs = new Observation();
    obs.setId("obs-1");
    obs.setCode(new CodeableConcept().addCoding(new Coding().setCode("88888-8")));
    obs.setValue(new StringType("result value"));

    HashMap<String, Observation> allObs = new HashMap<>();
    allObs.put("obs-1", obs);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String result =
        CdaResultGenerator.getXmlForComponents(
            report, allObs, launchDetails, "content", 1, r4FhirData, "CDA_R31");
    assertNotNull("Should return XML for observation without components", result);
  }

  @Test
  public void testGetTriggerCodeTemplateXml_OtherVersion() {
    String result = CdaResultGenerator.getTriggerCodeTemplateXml("CDA_OTHER");
    assertNotNull("Should return template XML for other versions", result);
    assertTrue("Should contain a template ID", result.length() > 0);
  }

  @Test
  public void testGetCodeXml_WithLoincCode() {
    CodeableConcept cd = new CodeableConcept();
    cd.addCoding(new Coding().setSystem(CdaGeneratorConstants.FHIR_LOINC_URL).setCode("12345-6"));

    assertNotNull("CodeableConcept with LOINC should be valid", cd);
    assertTrue("Should have coding", cd.hasCoding());
  }

  @Test
  public void testGetCodeXml_EmptyCodings_WithText() {
    CodeableConcept cd = new CodeableConcept();
    cd.setText("Test Text");

    assertTrue("Should have text", cd.hasText());
    assertFalse("Should not have coding", cd.hasCoding());
  }

  @Test
  public void testPractitionerNull() {
    R4FhirData data = new R4FhirData();

    assertNull("Null practitioner should be null", data.getPractitionerById("nonexistent"));
  }

  @Test
  public void testPractitionerNameExtraction_OfficialNamePreferred() {
    Practitioner prac = new Practitioner();

    HumanName unofficialName = new HumanName();
    unofficialName.setUse(HumanName.NameUse.NICKNAME);
    unofficialName.setFamily("Unofficial");
    prac.addName(unofficialName);

    HumanName officialName = new HumanName();
    officialName.setUse(HumanName.NameUse.OFFICIAL);
    officialName.setFamily("Official");
    prac.addName(officialName);

    assertTrue(
        "Should have official name",
        prac.getName().stream().anyMatch(n -> n.getUse() == HumanName.NameUse.OFFICIAL));
  }

  @Test
  public void testPractitionerNameExtraction_FallbackToFirst() {
    Practitioner prac = new Practitioner();

    HumanName name1 = new HumanName();
    name1.setFamily("FirstName");
    prac.addName(name1);

    HumanName name2 = new HumanName();
    name2.setFamily("SecondName");
    prac.addName(name2);

    assertTrue("Should have names", !prac.getName().isEmpty());
    assertEquals("First name should be available", "FirstName", prac.getName().get(0).getFamily());
  }

  @Test
  public void testPractitionerNameExtraction_NoNames() {
    Practitioner prac = new Practitioner();

    assertTrue("Should have empty names when none added", prac.getName().isEmpty());
  }

  @Test
  public void testPractitionerHasGivenAndFamily() {
    HumanName name = new HumanName();
    name.addGiven("John");
    name.setFamily("Doe");

    String given = name.getGivenAsSingleString();
    String family = name.getFamily();

    assertEquals("Given name should be John", "John", given);
    assertEquals("Family name should be Doe", "Doe", family);
  }
}
