package com.drajer.cdafromr4;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.PatientExecutionState;
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
@PowerMockIgnore({
  "com.sun.org.apache.*",
  "javax.xml.*",
  "org.xml.*",
  "org.w3c.*",
  "javax.management.*",
  "javax.crypto.*"
})
public class CdaResultGeneratorTest extends BaseGeneratorTest {

  private static final String RESULT_CDA_FILE = "CdaTestData/Cda/Result/Result.xml";

  private static final String RESULT_CDA_TRIGGER_FILE =
      "CdaTestData/Cda/Result/result-section-trigger.xml";
  private static final String OBSERVATION_START_JSON =
      "CdaTestData/Observation/ObservationStart.json";
  private static final String DIAGNOSTIC_REPORT_JSON =
      "CdaTestData/DiagnosticReport/DiagnosticReport.json";
  private static final String LOADING_QUERY_BUNDLE_WITH_DIAGNOSTIC_REPORT =
      "CdaTestData/LoadingQuery/LoadingQueryBundle_a-432.E-1563935_a-432.encounter-156141 (1).json";
  private static final String LOADING_QUERY_BUNDLE_WITHOUT_DIAGNOSTIC_REPORT =
      "CdaTestData/bundle/LoadingQueryBundle_2099780595662060_2099780857662530.json";

  private R4FhirData buildR4FhirDataFromBundle(String filename) {

    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile(filename);

    List<Bundle.BundleEntryComponent> entries = b.getEntry();
    Bundle bundle = new Bundle();
    Map<ResourceType, Set<Resource>> resourcesByType = new HashMap<>();

    for (Bundle.BundleEntryComponent ent : entries) {
      Resource resource = ent.getResource();
      ResourceType resourceType = resource.getResourceType();

      resourcesByType.computeIfAbsent(resourceType, k -> new LinkedHashSet<>()).add(resource);
    }

    Map<String, List<String>> uniqueResourceIdsByType = new HashMap<>();
    for (ResourceType resourceType : resourcesByType.keySet()) {

      Set<Resource> resourceSet = resourcesByType.get(resourceType);

      if (resourceSet != null && !resourceSet.isEmpty()) {
        R3ToR2DataConverterUtils.addResourcesToR4FhirData(
            "1",
            bundle,
            data,
            launchDetails,
            resourceSet,
            resourceType.toString(),
            uniqueResourceIdsByType);
      }
    }

    data.setData(bundle);
    return data;
  }

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
            "<code code=\"30954-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\""
                + " displayName=\"RESULTS\"/>")
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
    Set<Resource> resourceSet = new LinkedHashSet<>();

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

  @Test
  public void testGetDiagnosticReportsWithObservations_emptyAndPopulated() {
    Map<DiagnosticReport, List<Observation>> empty =
        CdaResultGenerator.getDiagnosticReportsWithObservations(
            new R4FhirData(), new ArrayList<>(), new HashMap<>());
    assertNotNull("must return a map rather than null", empty);
    assertTrue("no diagnostic reports must yield an empty map", empty.isEmpty());

    Observation obs = new Observation();
    obs.setId("obs-1");
    obs.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding().setSystem(CdaGeneratorConstants.FHIR_LOINC_URL).setCode("2339-0")));

    DiagnosticReport dr = new DiagnosticReport();
    dr.setId("dr-1");
    dr.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding().setSystem(CdaGeneratorConstants.FHIR_LOINC_URL).setCode("24357-6")));
    dr.addResult(new Reference("Observation/obs-1"));

    R4FhirData data = new R4FhirData();
    data.setDiagReports(new ArrayList<>(Collections.singletonList(dr)));
    data.setLabResults(new ArrayList<>(Collections.singletonList(obs)));

    Map<String, Observation> unique = new HashMap<>();
    unique.put("obs-1", obs);

    assertNotNull(
        "a report with a resolvable result must not throw",
        CdaResultGenerator.getDiagnosticReportsWithObservations(
            data, new ArrayList<>(Collections.singletonList(obs)), unique));
  }

  @Test
  public void testFindObservation_allArms() {
    Observation obs = new Observation();
    obs.setId("obs-find");
    List<Observation> all = new ArrayList<>(Collections.singletonList(obs));

    assertNotNull(
        "a resolvable reference must return the observation",
        CdaResultGenerator.findObservation(new Reference("Observation/obs-find"), all));

    assertNull(
        "an unresolvable reference must return null",
        CdaResultGenerator.findObservation(new Reference("Observation/missing"), all));

    assertNull(
        "an empty observation list must return null",
        CdaResultGenerator.findObservation(
            new Reference("Observation/obs-find"), new ArrayList<>()));
  }

  @Test
  public void testGetAuthorizingProviderParticipantXml_guardArms() {
    assertEquals(
        "null basedOn refs must yield an empty string",
        "",
        CdaResultGenerator.getAuthorizingProviderParticipantXml(null, new R4FhirData()));

    assertEquals(
        "empty basedOn refs must yield an empty string",
        "",
        CdaResultGenerator.getAuthorizingProviderParticipantXml(
            new ArrayList<>(), new R4FhirData()));

    assertEquals(
        "null data must yield an empty string",
        "",
        CdaResultGenerator.getAuthorizingProviderParticipantXml(
            new ArrayList<>(Collections.singletonList(new Reference("ServiceRequest/sr-1"))),
            null));

    assertNotNull(
        "an unresolvable ServiceRequest reference must not throw",
        CdaResultGenerator.getAuthorizingProviderParticipantXml(
            new ArrayList<>(Collections.singletonList(new Reference("ServiceRequest/missing"))),
            new R4FhirData()));
  }

  @Test
  public void testGetXmlForObservationComponent_withReferenceRanges() {
    CodeableConcept code =
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem(CdaGeneratorConstants.FHIR_LOINC_URL)
                    .setCode("2339-0")
                    .setDisplay("Glucose"));

    Quantity value = new Quantity();
    value.setValue(new java.math.BigDecimal("95"));
    value.setUnit("mg/dL");

    Observation.ObservationReferenceRangeComponent bothBounds =
        new Observation.ObservationReferenceRangeComponent();
    SimpleQuantity low = new SimpleQuantity();
    low.setValue(new java.math.BigDecimal("70"));
    low.setUnit("mg/dL");
    SimpleQuantity high = new SimpleQuantity();
    high.setValue(new java.math.BigDecimal("110"));
    high.setUnit("mg/dL");
    bothBounds.setLow(low);
    bothBounds.setHigh(high);

    assertNotNull(
        "a reference range with both bounds must produce xml",
        CdaResultGenerator.getXmlForObservationComponent(
            launchDetails,
            code,
            value,
            "obs-rr-1",
            new DateTimeType(new Date()),
            new ArrayList<>(),
            1,
            new ArrayList<>(),
            new R4FhirData(),
            "CDA_R31",
            new ArrayList<>(Collections.singletonList(bothBounds))));

    assertNotNull(
        "an empty reference range list must not throw",
        CdaResultGenerator.getXmlForObservationComponent(
            launchDetails,
            code,
            value,
            "obs-rr-2",
            new DateTimeType(new Date()),
            new ArrayList<>(),
            2,
            new ArrayList<>(),
            new R4FhirData(),
            "CDA_R31",
            new ArrayList<>()));
  }

  // ---------------------------------------------------------------------------------------
  // GitHub #1079: Authorizing Provider (sheet rows 2-9), Performing Organization (rows 16-18)
  // and the Ordering Facility lookup used by the header (rows 10-15).
  //
  // LoadingQueryBundle_1079.json has three lab orders:
  //   94500-6  ServiceRequest.requester = PractitionerRole (Practitioner + Organization);
  //            Observation.performer = lab org, DiagnosticReport.performer = another lab org
  //   94309-2  requester = Practitioner; Observation.performer = Practitioner only,
  //            DiagnosticReport.performer = lab org
  //   80383-3  requester = Organization; standalone Observation, performer = lab org
  // ---------------------------------------------------------------------------------------

  private static final String LOADING_QUERY_BUNDLE_1079 =
      "CdaTestData/LoadingQuery/LoadingQueryBundle_1079.json";

  private static final String RESULT_ORGANIZER =
      "//organizer[templateId[@root='2.16.840.1.113883.10.20.22.4.1']]";

  private static final String RESP_PROV =
      "/participant[@typeCode='RESP']/participantRole[@classCode='PROV']";

  private static final String PERFORMING_ORG =
      "/component/observation[templateId[@root='2.16.840.1.113883.10.20.22.4.2']]"
          + "/performer/assignedEntity/representedOrganization";

  private static String organizer(String loinc) {
    return RESULT_ORGANIZER + "[code/@code='" + loinc + "']";
  }

  /**
   * The generated Results section, wrapped in a root that binds the xsi/sdtc prefixes. No default
   * namespace, so the sheet's unprefixed XPaths match.
   */
  private String resultsSection1079(R4FhirData data) {
    String xml =
        CdaResultGenerator.generateResultsSection(
            data, launchDetails, CdaGeneratorConstants.CDA_EICR_VERSION_R31);
    assertNotNull(xml);
    return "<root xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
        + " xmlns:sdtc=\"urn:hl7-org:sdtc\">"
        + xml
        + "</root>";
  }

  // Evaluated on the XML text (no DOM types) so it works under the PowerMock class loader.
  private static String xpath(String xml, String expr) throws Exception {
    return javax.xml.xpath.XPathFactory.newInstance()
        .newXPath()
        .evaluate(expr, new org.xml.sax.InputSource(new java.io.StringReader(xml)));
  }

  @Test
  public void test1079_authorizingProvider_viaPractitionerRole_populatesSheetRows2to9()
      throws Exception {
    String doc = resultsSection1079(buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079));
    String p = organizer("94500-6") + RESP_PROV;

    // Row 2: name
    assertEquals("Alice", xpath(doc, p + "/playingEntity/name/given[1]"));
    assertEquals("M", xpath(doc, p + "/playingEntity/name/given[2]"));
    assertEquals("Ordering", xpath(doc, p + "/playingEntity/name/family"));
    // Row 3: NPI
    assertEquals("1234567893", xpath(doc, p + "/id[@root='2.16.840.1.113883.4.6']/@extension"));
    // Rows 4-7: address
    assertEquals("100 Order Ave", xpath(doc, p + "/addr/streetAddressLine"));
    assertEquals("Seattle", xpath(doc, p + "/addr/city"));
    assertEquals("WA", xpath(doc, p + "/addr/state"));
    assertEquals("98101", xpath(doc, p + "/addr/postalCode"));
    // Row 8: work phone, Row 9: fax
    assertEquals("tel:(206)555-0101", xpath(doc, p + "/telecom[@use='WP'][1]/@value"));
    assertEquals("fax:(206)555-0102", xpath(doc, p + "/telecom[contains(@value,'fax')]/@value"));
  }

  @Test
  public void test1079_authorizingProvider_directPractitionerRequester() throws Exception {
    String doc = resultsSection1079(buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079));
    String p = organizer("94309-2") + RESP_PROV;

    assertEquals("Brian", xpath(doc, p + "/playingEntity/name/given"));
    assertEquals("Direct", xpath(doc, p + "/playingEntity/name/family"));
    assertEquals("1245319599", xpath(doc, p + "/id[@root='2.16.840.1.113883.4.6']/@extension"));
    assertEquals("Tacoma", xpath(doc, p + "/addr/city"));
    assertEquals("tel:(253)555-0201", xpath(doc, p + "/telecom[@use='WP'][1]/@value"));
    assertEquals("fax:(253)555-0202", xpath(doc, p + "/telecom[contains(@value,'fax')]/@value"));
  }

  @Test
  public void test1079_organizationRequester_hasNoAuthorizingProvider() throws Exception {
    String doc = resultsSection1079(buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079));

    assertEquals("1", xpath(doc, "count(" + organizer("80383-3") + ")"));
    assertEquals(
        "0", xpath(doc, "count(" + organizer("80383-3") + "/participant[@typeCode='RESP'])"));
  }

  @Test
  public void test1079_oneAuthorizingProviderPerOrganizer_beforeComponents() throws Exception {
    String doc = resultsSection1079(buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079));

    for (String loinc : new String[] {"94500-6", "94309-2"}) {
      String org = organizer(loinc);
      assertEquals(loinc, "1", xpath(doc, "count(" + org + "/participant[@typeCode='RESP'])"));
      // CDA organizer schema: participant comes before component
      assertEquals(
          loinc, "0", xpath(doc, "count(" + org + "/component[1]/following-sibling::participant)"));
    }
  }

  @Test
  public void test1079_performingOrganization_observationPerformerWins() throws Exception {
    String doc = resultsSection1079(buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079));
    String o = organizer("94500-6") + PERFORMING_ORG;

    // Row 16: name (Observation.performer, not DiagnosticReport.performer)
    assertEquals("Test1079 Observation Lab", xpath(doc, o + "/name"));
    // Row 17: address
    assertEquals("1610 Lab Pkwy", xpath(doc, o + "/addr/streetAddressLine"));
    assertEquals("Shoreline", xpath(doc, o + "/addr/city"));
    // Row 18: phone
    assertEquals("tel:(206)555-0150", xpath(doc, o + "/telecom[contains(@value,'tel')]/@value"));
    // assignedEntity/id is required by the schema
    assertEquals(
        "NI",
        xpath(
            doc,
            organizer("94500-6")
                + "/component/observation/performer/assignedEntity/id/@nullFlavor"));
  }

  @Test
  public void test1079_performingOrganization_fallsBackToDiagnosticReportPerformer()
      throws Exception {
    String doc = resultsSection1079(buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079));
    String o = organizer("94309-2") + PERFORMING_ORG;

    assertEquals("Test1079 Report Lab", xpath(doc, o + "/name"));
    assertEquals("500 Report Blvd", xpath(doc, o + "/addr/streetAddressLine"));
    assertEquals("Spokane", xpath(doc, o + "/addr/city"));
    assertEquals("tel:(509)555-0160", xpath(doc, o + "/telecom[contains(@value,'tel')]/@value"));
  }

  @Test
  public void test1079_performingOrganization_standaloneLabObservation() throws Exception {
    String doc = resultsSection1079(buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079));

    assertEquals(
        "Test1079 Observation Lab", xpath(doc, organizer("80383-3") + PERFORMING_ORG + "/name"));
  }

  @Test
  public void test1079_basedOnServiceRequestOnlyInBundle_isStillResolved() throws Exception {
    // In production basedOn ServiceRequests are secondary resources: they are in the bundle but
    // not in the ServiceRequest list (so they stay out of Plan of Treatment).
    R4FhirData data = buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079);
    data.setServiceRequests(new ArrayList<>());

    String doc = resultsSection1079(data);

    assertEquals(
        "1234567893",
        xpath(
            doc,
            organizer("94500-6") + RESP_PROV + "/id[@root='2.16.840.1.113883.4.6']/@extension"));
    assertEquals(
        "Test1079 Ordering Clinic", CdaResultGenerator.findOrderingFacility(data).getName());
  }

  @Test
  public void test1079_withoutBasedOnOrPerformers_nothingIsAdded() throws Exception {
    R4FhirData data = buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079);
    for (DiagnosticReport dr : data.getDiagReports()) {
      dr.setBasedOn(new ArrayList<>());
      dr.setPerformer(new ArrayList<>());
    }
    for (Observation obs : data.getLabResults()) {
      obs.setBasedOn(new ArrayList<>());
      obs.setPerformer(new ArrayList<>());
    }

    String doc = resultsSection1079(data);

    assertEquals("3", xpath(doc, "count(" + RESULT_ORGANIZER + ")"));
    assertEquals("0", xpath(doc, "count(//participant[@typeCode='RESP'])"));
    assertEquals("0", xpath(doc, "count(//observation/performer/assignedEntity)"));
    assertNull(CdaResultGenerator.findOrderingFacility(data));
  }

  @Test
  public void test1079_authorizingProvider_fromObservationBasedOnWhenReportHasNone()
      throws Exception {
    R4FhirData data = buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079);
    // dr-2 loses its basedOn; its result obs-2 still points at sr-2 (requester pr-2)
    for (DiagnosticReport dr : data.getDiagReports()) {
      if ("t1079-dr-2".equals(dr.getIdElement().getIdPart())) {
        dr.setBasedOn(new ArrayList<>());
      }
    }

    String doc = resultsSection1079(data);

    assertEquals(
        "1245319599",
        xpath(
            doc,
            organizer("94309-2") + RESP_PROV + "/id[@root='2.16.840.1.113883.4.6']/@extension"));
  }

  @Test
  public void test1079_authorizingProvider_reportBasedOnHasPriorityOverObservation()
      throws Exception {
    R4FhirData data = buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079);
    // dr-1 -> sr-1 (Alice, via PractitionerRole); point its result obs-1 at sr-2 (Brian) instead
    for (Observation obs : data.getLabResults()) {
      if ("t1079-obs-1".equals(obs.getIdElement().getIdPart())) {
        obs.setBasedOn(new ArrayList<>(List.of(new Reference("ServiceRequest/t1079-sr-2"))));
      }
    }

    String doc = resultsSection1079(data);
    String p = organizer("94500-6") + RESP_PROV;

    assertEquals(
        "1", xpath(doc, "count(" + organizer("94500-6") + "/participant[@typeCode='RESP'])"));
    assertEquals("1234567893", xpath(doc, p + "/id[@root='2.16.840.1.113883.4.6']/@extension"));
  }

  @Test
  public void test1079_getAuthorizingProviderBasedOnRefs_reportFirstThenObservations() {
    DiagnosticReport dr = new DiagnosticReport();
    dr.addBasedOn(new Reference("ServiceRequest/a"));
    Observation o1 = new Observation();
    o1.addBasedOn(new Reference("ServiceRequest/b"));
    Observation o2 = new Observation();
    o2.addBasedOn(new Reference("ServiceRequest/c"));

    List<Reference> refs =
        CdaResultGenerator.getAuthorizingProviderBasedOnRefs(dr, List.of(o1, o2));

    assertEquals(3, refs.size());
    assertEquals("ServiceRequest/a", refs.get(0).getReference());
    assertEquals("ServiceRequest/b", refs.get(1).getReference());
    assertEquals("ServiceRequest/c", refs.get(2).getReference());
    assertTrue(CdaResultGenerator.getAuthorizingProviderBasedOnRefs(null, null).isEmpty());
  }

  @Test
  public void test1079_findOrderingFacility_firstResultWithAFacilityWins() {
    R4FhirData data = buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079);

    // dr-1's order is placed through a PractitionerRole -> its organization
    assertEquals(
        "t1079-org-ordering",
        CdaResultGenerator.findOrderingFacility(data).getIdElement().getIdPart());

    // With only the direct-Practitioner order (no facility) and the Organization requester left,
    // the Organization requester is the facility.
    data.getDiagReports().removeIf(dr -> "t1079-dr-1".equals(dr.getIdElement().getIdPart()));
    data.getLabResults().removeIf(o -> "t1079-obs-1".equals(o.getIdElement().getIdPart()));
    assertEquals(
        "t1079-org-ordering-2",
        CdaResultGenerator.findOrderingFacility(data).getIdElement().getIdPart());
  }

  // ---------------------------------------------------------------------------------------
  // GitHub #1151 / CONF:4411-603: a trigger-code result whose value is xsi:type="ST" must
  // carry at least one interpretationCode.
  // ---------------------------------------------------------------------------------------

  private static final String ST_VALUE = "<value xsi:type=\"ST\">Detected</value>";
  private static final String CD_VALUE =
      "<value xsi:type=\"CD\" code=\"260373001\" codeSystem=\"2.16.840.1.113883.6.96\"/>";
  private static final String NI_INTERPRETATION = "<interpretationCode nullFlavor=\"NI\"/>";

  private static List<CodeableConcept> fhirInterpretation(String code) {
    return new ArrayList<>(
        Collections.singletonList(
            new CodeableConcept()
                .addCoding(
                    new Coding(
                        "http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation",
                        code,
                        null))));
  }

  @Test
  public void test1151_triggerStValueWithoutInterpretation_addsNullFlavorNI() {
    assertEquals(
        NI_INTERPRETATION,
        CdaResultGenerator.getInterpretationCodeXml(null, true, ST_VALUE).trim());
    assertEquals(
        NI_INTERPRETATION,
        CdaResultGenerator.getInterpretationCodeXml(new ArrayList<>(), true, ST_VALUE).trim());
  }

  @Test
  public void test1151_fhirInterpretationIsUsedAndNotDuplicated() {
    String xml =
        CdaResultGenerator.getInterpretationCodeXml(fhirInterpretation("A"), true, ST_VALUE);

    assertTrue(xml, xml.contains("code=\"A\""));
    assertFalse(xml, xml.contains("nullFlavor=\"NI\""));
    assertEquals(xml, 1, xml.split("<interpretationCode", -1).length - 1);
  }

  @Test
  public void test1151_nothingAddedWhenNotRequired() {
    // not a trigger-code result
    assertEquals("", CdaResultGenerator.getInterpretationCodeXml(null, false, ST_VALUE));
    // value is not ST
    assertEquals("", CdaResultGenerator.getInterpretationCodeXml(null, true, CD_VALUE));
    assertEquals("", CdaResultGenerator.getInterpretationCodeXml(null, true, null));
  }

  @Test
  public void test1151_endToEnd_triggerResultsWithStValuesGetInterpretationCode() throws Exception {
    R4FhirData data = buildR4FhirDataFromBundle(LOADING_QUERY_BUNDLE_1079);
    for (Observation obs : data.getLabResults()) {
      obs.setInterpretation(new ArrayList<>());
      String id = obs.getIdElement().getIdPart();
      if ("t1079-obs-1".equals(id)) {
        obs.setValue(new StringType("Detected"));
      } else if ("t1079-obs-2".equals(id)) {
        obs.setValue(new StringType("RNA found, see lab note"));
      }
    }
    String savedStatus = launchDetails.getStatus();
    try {
      PatientExecutionState state =
          createPatientExecutionState("Observation.code", "http://loinc.org|94500-6");
      state
          .getMatchTriggerStatus()
          .getMatchedCodes()
          .get(0)
          .getMatchedCodes()
          .add("http://loinc.org|94309-2");
      launchDetails.setStatus(TestUtils.toJsonString(state));

      String doc = resultsSection1079(data);
      String trigger =
          "//observation[templateId[@root='2.16.840.1.113883.10.20.15.2.3.2']][value/@*[local-name()='type']='ST']";

      // Both results are trigger results with an ST value ...
      assertEquals("2", xpath(doc, "count(" + trigger + ")"));
      // ... and each now has an interpretationCode (CONF:4411-603)
      assertEquals("0", xpath(doc, "count(" + trigger + "[not(interpretationCode)])"));
      // ... added as nullFlavor NI (no interpretation is derived from the value text)
      assertEquals("2", xpath(doc, "count(" + trigger + "/interpretationCode[@nullFlavor='NI'])"));
    } finally {
      launchDetails.setStatus(savedStatus);
    }
  }
}
