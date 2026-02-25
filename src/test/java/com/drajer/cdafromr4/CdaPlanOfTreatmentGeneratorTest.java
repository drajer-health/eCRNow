package com.drajer.cdafromr4;

import static com.drajer.cda.utils.CdaGeneratorConstants.FHIR_SNOMED_URL;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.powermock.api.mockito.PowerMockito.*;
import static org.powermock.api.mockito.PowerMockito.when;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.javatuples.Pair;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class, CdaFhirUtilities.class})
@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaPlanOfTreatmentGeneratorTest extends BaseGeneratorTest {

  static final String PLAN_OF_TREATMENT_BUNDLE_RESOURCE_FILE =
      "CdaTestData/PlanOfTreatment/plan_of_treatment_bundle_resource.json";

  static final String PLAN_OF_TREATMENT_CDA_FILE =
      "CdaTestData//Cda//PlanOfTreatment//PlanOfTreatment.xml";

  static final String PLAN_OF_TREATMENT_V31_CDA_FILE =
      "CdaTestData//Cda//PlanOfTreatment//PlanOfTreatment_v3.1.xml";

  static final String PLAN_OF_TREATMENT_V31_CDA_V2_FILE =
      "CdaTestData//Cda//PlanOfTreatment//PlanOfTreatment_3.1_v2.xml";

  static final String PLANNED_PROCEDURE_ENTRY_CDA_FILE =
      "CdaTestData//Cda//PlanOfTreatment//plannedProcedureEntry.xml";

  static final String PLANNED_ACT_CDA_FILE = "CdaTestData//Cda//PlanOfTreatment//plannedAct.xml";
  public static final String SERVICE_REQUEST_LAB = "108252007";
  public static final String SERVICE_REQUEST_IMAGING = "363679005";
  public static final String SERVICE_REQUEST_COUNSELLING = "409063005";
  public static final String SERVICE_REQUEST_EDUCATION = "409073007";
  public static final String SERVICE_REQUEST_PROCEDURE = " 387713003";
  public static final String COMPLETED = "completed";

  @Test
  public void testGeneratePlanOfTreatmentSection() {
    R4FhirData r4Data = createResourceData(PLAN_OF_TREATMENT_BUNDLE_RESOURCE_FILE);

    String expectedXml = TestUtils.getFileContentAsString(PLAN_OF_TREATMENT_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(
            r4Data, launchDetails, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGeneratePlanOfTreatmentSection_withVersionR31() {
    R4FhirData r4Data = createResourceData(PLAN_OF_TREATMENT_BUNDLE_RESOURCE_FILE);

    String expectedXml = TestUtils.getFileContentAsString(PLAN_OF_TREATMENT_V31_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(
            r4Data, launchDetails, CdaGeneratorConstants.CDA_EICR_VERSION_R31);

    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetPlannedProcedureXml() {
    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/serviceRequest.json");

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
    //    data.getLabResults().sort(Comparator.comparing(Observation::getId));
    //    data.getDiagReports().sort(Comparator.comparing(DiagnosticReport::getId));
    data.setData(bundle);
    String expectedXml = TestUtils.getFileContentAsString(PLANNED_PROCEDURE_ENTRY_CDA_FILE);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.mockStatic(CdaFhirUtilities.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    PowerMockito.when(CdaFhirUtilities.getXmlForAuthor(any(), any())).thenReturn(getAuthor());

    ServiceRequest serviceRequest = data.getServiceRequests().stream().findFirst().get();

    String actualXml =
        CdaPlanOfTreatmentGenerator.getPlannedProcedureXml(
            serviceRequest, launchDetails, "conf123", data);

    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetPlannedActXml() {
    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/serviceRequest.json");

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
    //    data.getLabResults().sort(Comparator.comparing(Observation::getId));
    //    data.getDiagReports().sort(Comparator.comparing(DiagnosticReport::getId));
    data.setData(bundle);
    String expectedXml = TestUtils.getFileContentAsString(PLANNED_ACT_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    ServiceRequest serviceRequest = data.getServiceRequests().stream().findFirst().get();
    String actualXml =
        (String)
            CdaPlanOfTreatmentGenerator.getPlannedActXml(serviceRequest, launchDetails, "", "");

    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetPlannedObservationXml() {
    R4FhirData data = new R4FhirData();
    LaunchDetails details = new LaunchDetails();
    ServiceRequest sr = new ServiceRequest();
    Coding coding = new Coding();
    coding.setCode("68518-0");
    coding.setSystem(CdaGeneratorConstants.FHIR_LOINC_URL);
    sr.setCode(new CodeableConcept().addCoding(coding));

    PatientExecutionState state =
        createPatientExecutionState("ServiceRequest", "http://loinc.org|68518-0");

    details.setStatus(TestUtils.toJsonString(state));

    String contentRef = "contentRef";

    String actual =
        CdaPlanOfTreatmentGenerator.getPlannedObservationXml(
            sr, details, contentRef, data, "CDA_R31");
    assertNotNull(actual);
  }

  @Test
  public void testGetDiagnosticReportXml() {
    R4FhirData data = new R4FhirData();
    LaunchDetails details = new LaunchDetails();
    DiagnosticReport dr = new DiagnosticReport();
    Coding coding = new Coding();
    coding.setCode("68518-0");
    coding.setSystem(CdaGeneratorConstants.FHIR_LOINC_URL);
    dr.setCode(new CodeableConcept().addCoding(coding));
    PatientExecutionState state =
        createPatientExecutionState("DiagnosticReport", "http://loinc.org|68518-0");

    details.setStatus(TestUtils.toJsonString(state));

    String contentRef = "contentRef";
    String actual =
        CdaPlanOfTreatmentGenerator.getDiagnosticReportXml(
            dr, details, contentRef, data, "CDA_R31");
    assertNotNull(actual);
  }

  @Test
  public void testGenerateEmptyPlanOfTreatmentSection() {

    String expectedXml =
        "<component>\r\n"
            + "<section nullFlavor=\"NI\">\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.10\"/>\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.10\" extension=\"2014-06-09\"/>\r\n"
            + "<code code=\"18776-5\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Treatment Plan\"/>\r\n"
            + "<title>Plan of Treatment</title>\r\n"
            + "<text>No Plan Of Treatment Information</text>\r\n"
            + "</section>\r\n"
            + "</component>\r\n"
            + "";
    String actualXml = CdaPlanOfTreatmentGenerator.generateEmptyPlanOfTreatmentSection();

    assertNotNull(actualXml);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testIsPlannedProcedure_WithMatchingCode() {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept category = new CodeableConcept();
    category.addCoding(
        new Coding(FHIR_SNOMED_URL, CdaPlanOfTreatmentGenerator.SERVICE_REQUEST_PROCEDURE, null));
    sr.setCategory(Collections.singletonList(category));

    assertTrue(
        "Expected true for matching SNOMED code",
        CdaPlanOfTreatmentGenerator.isPlannedProcedure(sr));
  }

  @Test
  public void testIsPlannedProcedure_WithNonMatchingCode() {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept category = new CodeableConcept();
    category.addCoding(new Coding(FHIR_SNOMED_URL, "123456", null)); // Different SNOMED code
    sr.setCategory(Collections.singletonList(category));

    assertFalse(
        "Expected false for non-matching SNOMED code",
        CdaPlanOfTreatmentGenerator.isPlannedProcedure(sr));
  }

  @Test
  public void testIsPlannedProcedure_WithEmptyCategory() {
    ServiceRequest sr = new ServiceRequest();
    sr.setCategory(Collections.emptyList()); // Empty category list

    assertFalse(
        "Expected false for empty category list",
        CdaPlanOfTreatmentGenerator.isPlannedProcedure(sr));
  }

  @Test
  public void testIsPlannedObservation_WithLabCode() {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept category = new CodeableConcept();
    category.addCoding(new Coding(FHIR_SNOMED_URL, SERVICE_REQUEST_LAB, null));
    sr.setCategory(Collections.singletonList(category));

    assertTrue(
        "Expected true for matching LAB SNOMED code",
        CdaPlanOfTreatmentGenerator.isPlannedObservation(sr));
  }

  @Test
  public void testIsPlannedObservation_WithImagingCode() {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept category = new CodeableConcept();
    category.addCoding(new Coding(FHIR_SNOMED_URL, SERVICE_REQUEST_IMAGING, null));
    sr.setCategory(Collections.singletonList(category));

    assertTrue(
        "Expected true for matching IMAGING SNOMED code",
        CdaPlanOfTreatmentGenerator.isPlannedObservation(sr));
  }

  @Test
  public void testIsPlannedObservation_WithBothMatchingCodes() {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept category1 = new CodeableConcept();
    category1.addCoding(new Coding(FHIR_SNOMED_URL, SERVICE_REQUEST_LAB, null));

    CodeableConcept category2 = new CodeableConcept();
    category2.addCoding(new Coding(FHIR_SNOMED_URL, SERVICE_REQUEST_IMAGING, null));

    sr.setCategory(Arrays.asList(category1, category2));

    assertTrue(
        "Expected true for both LAB and IMAGING SNOMED codes",
        CdaPlanOfTreatmentGenerator.isPlannedObservation(sr));
  }

  @Test
  public void testIsPlannedObservation_WithNonMatchingCode() {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept category = new CodeableConcept();
    category.addCoding(new Coding(FHIR_SNOMED_URL, "999999999", null)); // Some random code
    sr.setCategory(Collections.singletonList(category));

    assertFalse(
        "Expected false for non-matching SNOMED code",
        CdaPlanOfTreatmentGenerator.isPlannedObservation(sr));
  }

  @Test
  public void testIsPlannedObservation_WithEmptyCategory() {
    ServiceRequest sr = new ServiceRequest();
    sr.setCategory(Collections.emptyList()); // Empty category list

    assertFalse(
        "Expected false for empty category list",
        CdaPlanOfTreatmentGenerator.isPlannedObservation(sr));
  }

  @Test
  public void testIsPlannedObservation_WithNoCategory() {
    ServiceRequest sr = new ServiceRequest();
    // Not setting any category

    assertFalse(
        "Expected false when no category is present",
        CdaPlanOfTreatmentGenerator.isPlannedObservation(sr));
  }

  @Test(expected = NullPointerException.class)
  public void testIsPlannedObservation_WithNullServiceRequest() {
    // This test checks behavior if method is called with null (should ideally be handled in the
    // method)
    CdaPlanOfTreatmentGenerator.isPlannedObservation(null);
  }

  @Test
  public void testIsPlannedAct_withEducationCategory_shouldReturnTrue() {
    ServiceRequest sr =
        createServiceRequestWithCategory(CdaPlanOfTreatmentGenerator.SERVICE_REQUEST_EDUCATION);
    assertTrue(CdaPlanOfTreatmentGenerator.isPlannedAct(sr));
  }

  @Test
  public void testIsPlannedAct_withCounsellingCategory_shouldReturnTrue() {
    ServiceRequest sr =
        createServiceRequestWithCategory(CdaPlanOfTreatmentGenerator.SERVICE_REQUEST_COUNSELLING);
    assertTrue(CdaPlanOfTreatmentGenerator.isPlannedAct(sr));
  }

  @Test
  public void testIsPlannedAct_withBothEducationAndCounselling_shouldReturnTrue() {
    ServiceRequest sr =
        createServiceRequestWithCategories(
            CdaPlanOfTreatmentGenerator.SERVICE_REQUEST_EDUCATION,
            CdaPlanOfTreatmentGenerator.SERVICE_REQUEST_COUNSELLING);
    assertTrue(CdaPlanOfTreatmentGenerator.isPlannedAct(sr));
  }

  @Test
  public void testIsPlannedAct_withDifferentCategory_shouldReturnFalse() {
    ServiceRequest sr = createServiceRequestWithCategory("123456"); // Random SNOMED code
    assertFalse(CdaPlanOfTreatmentGenerator.isPlannedAct(sr));
  }

  @Test
  public void testIsPlannedAct_withEmptyCategoryList_shouldReturnFalse() {
    ServiceRequest sr = new ServiceRequest();
    sr.setCategory(Collections.emptyList()); // Empty category
    assertFalse(CdaPlanOfTreatmentGenerator.isPlannedAct(sr));
  }

  @Test
  public void testIsPlannedAct_withNoCategory_shouldReturnFalse() {
    ServiceRequest sr = new ServiceRequest(); // No category set
    assertFalse(CdaPlanOfTreatmentGenerator.isPlannedAct(sr));
  }

  // Helper method to create a ServiceRequest with a single category
  private ServiceRequest createServiceRequestWithCategory(String snomedCode) {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept category =
        new CodeableConcept().addCoding(new Coding(FHIR_SNOMED_URL, snomedCode, null));
    sr.setCategory(Collections.singletonList(category));
    return sr;
  }

  // Helper method to create a ServiceRequest with multiple categories
  private ServiceRequest createServiceRequestWithCategories(String... snomedCodes) {
    ServiceRequest sr = new ServiceRequest();
    for (String code : snomedCodes) {
      CodeableConcept category =
          new CodeableConcept().addCoding(new Coding(FHIR_SNOMED_URL, code, null));
      sr.getCategory().add(category);
    }
    return sr;
  }

  public String getAuthor() {
    String authorXml =
        "<author>\n"
            + "            <time value=\"20250217125323+0000\"/>\n"
            + "            <assignedAuthor>\n"
            + "                <id root=\"2.16.840.1.113883.4.6\" extension=\"9999993519\"/>\n"
            + "                <addr>\n"
            + "                    <streetAddressLine>5400 N Oak Trfy</streetAddressLine>\n"
            + "                    <city>New York City</city>\n"
            + "                    <county>Manhattan</county>\n"
            + "                    <state>NY</state>\n"
            + "                    <postalCode>10001</postalCode>\n"
            + "                    <country>US</country>\n"
            + "                </addr>\n"
            + "                <telecom value=\"tel:(816)673-2878\" use=\"MC\"/>\n"
            + "                <telecom value=\"mailto:arthur.james73@gmail.com\"/>\n"
            + "                <assignedPerson>\n"
            + "                    <name>\n"
            + "                        <given>Arthur</given>\n"
            + "                        <given>James</given>\n"
            + "                        <family>Smith</family>\n"
            + "                    </name>\n"
            + "                </assignedPerson>\n"
            + "            </assignedAuthor>\n"
            + "        </author>";
    return authorXml;
  }

  @Test
  public void testGeneratePlanOfTreatmentSection_T2() {

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
    data.getMedicationRequests().sort(Comparator.comparing(MedicationRequest::getId));
    data.getServiceRequests().sort(Comparator.comparing(ServiceRequest::getId));

    data.setData(bundle);

    String expectedXml = TestUtils.getFileContentAsString(PLAN_OF_TREATMENT_V31_CDA_V2_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    PowerMockito.when(CdaGeneratorUtils.getXmlForII(any())).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(
            data, launchDetails, CdaGeneratorConstants.CDA_EICR_VERSION_R31);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testServiceDateFallbackToAuthoredOn() {

    ServiceRequest sr = new ServiceRequest();
    sr.setOccurrence((Type) null);
    sr.setAuthoredOnElement(new DateTimeType("2025-02-17"));
    mockStatic(CdaFhirUtilities.class, Mockito.CALLS_REAL_METHODS);
    String serviceDate = CdaFhirUtilities.getStringForType(sr.getOccurrence());
    if (serviceDate.isEmpty() && sr.getAuthoredOnElement() != null) {
      serviceDate = CdaFhirUtilities.getDisplayStringForDateTimeType(sr.getAuthoredOnElement());
    }
    assertNotNull(serviceDate);
    assertFalse(serviceDate.isEmpty());
  }

  @Test
  public void testGetPlannedActXml_UsesAuthoredOnWhenOccurrenceMissing() {
    ServiceRequest sr = new ServiceRequest();
    sr.setId("sr-authored-only");
    Coding coding = new Coding();
    coding.setSystem(FHIR_SNOMED_URL);
    coding.setCode(CdaPlanOfTreatmentGenerator.SERVICE_REQUEST_EDUCATION);
    sr.setCode(new CodeableConcept().addCoding(coding));
    Date authoredDate = new Date();
    sr.setAuthoredOn(authoredDate);
    LaunchDetails details = new LaunchDetails();
    details.setAssigningAuthorityId("1.2.3.4.5");
    mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    mockStatic(CdaFhirUtilities.class, Mockito.CALLS_REAL_METHODS);
    when(CdaGeneratorUtils.getXmlForII(any(), any()))
        .thenReturn("<id root=\"1.2.3.4.5\" extension=\"sr-authored-only\"/>");
    String xml =
        (String) CdaPlanOfTreatmentGenerator.getPlannedActXml(sr, details, "contentRef", "CDA_R31");
    assertNotNull(xml);
    assertTrue(xml.contains("effectiveTime"));
  }

  @Test
  public void testGetPlannedProcedureXml_useAuthoredDate_branch() {
    ServiceRequest sr = new ServiceRequest();
    sr.setId("sr-123");
    sr.setAuthoredOn(new Date());
    sr.setCode(new CodeableConcept().setText("Test Procedure"));
    LaunchDetails details = new LaunchDetails();
    details.setAssigningAuthorityId("AUTH-123");
    details.setRctcOid("1.2.3");
    details.setRctcVersion("v1");
    R4FhirData data = new R4FhirData();
    mockStatic(CdaGeneratorUtils.class);
    mockStatic(CdaFhirUtilities.class);
    when(CdaGeneratorUtils.getXmlForStartElement(any())).thenReturn("<start/>");
    when(CdaGeneratorUtils.getXmlForEndElement(any())).thenReturn("<end/>");
    when(CdaGeneratorUtils.getXmlForAct(any(), any(), any())).thenReturn("<act/>");
    when(CdaGeneratorUtils.getXmlForTemplateId(anyString())).thenReturn("<template/>");
    when(CdaGeneratorUtils.getXmlForTemplateId(anyString(), anyString()))
        .thenReturn("<template-ext/>");
    when(CdaGeneratorUtils.getXmlForII(any(), any())).thenReturn("<II/>");
    when(CdaGeneratorUtils.getXmlForNullCDWithText(anyString(), anyString(), anyString()))
        .thenReturn("<nullCD/>");
    when(CdaGeneratorUtils.getXmlForCD(anyString(), anyString())).thenReturn("<CD/>");
    Pair<Date, TimeZone> mockedEffDate = new Pair<>(null, TimeZone.getDefault());
    when(CdaFhirUtilities.getActualDate(any())).thenReturn(mockedEffDate);
    when(CdaFhirUtilities.getCodingXmlForCodeSystem(
            any(), anyString(), anyString(), anyBoolean(), anyString()))
        .thenReturn("<codingXml/>");

    when(CdaFhirUtilities.getCodingXml(any(), anyString(), anyString())).thenReturn("<codingXml/>");

    String xml =
        CdaPlanOfTreatmentGenerator.getPlannedProcedureXml(sr, details, "contentRef", data);
    assertNotNull(xml);
    assertTrue(xml.contains("<CD/>") || xml.contains("<nullCD/>") || xml.contains("<codingXml/>"));
  }

  @Test
  public void testGetPlannedProcedureXml_codeXmlBranches() {
    ServiceRequest sr = new ServiceRequest();
    sr.setId("sr-456");
    sr.setAuthoredOn(new Date());

    sr.setCode(new CodeableConcept().setText("Procedure Text"));
    LaunchDetails details = new LaunchDetails();
    details.setAssigningAuthorityId("AUTH-123");
    details.setRctcOid("1.2.3");
    details.setRctcVersion("v1");
    R4FhirData data = new R4FhirData();
    mockStatic(CdaGeneratorUtils.class);
    mockStatic(CdaFhirUtilities.class);
    when(CdaGeneratorUtils.getXmlForStartElement(any())).thenReturn("<start/>");
    when(CdaGeneratorUtils.getXmlForEndElement(any())).thenReturn("<end/>");
    when(CdaGeneratorUtils.getXmlForAct(any(), any(), any())).thenReturn("<act/>");
    when(CdaGeneratorUtils.getXmlForTemplateId(anyString())).thenReturn("<template/>");
    when(CdaGeneratorUtils.getXmlForTemplateId(anyString(), anyString()))
        .thenReturn("<template-ext/>");
    when(CdaGeneratorUtils.getXmlForII(any(), any())).thenReturn("<II/>");
    when(CdaGeneratorUtils.getXmlForNullCDWithText(anyString(), anyString(), anyString()))
        .thenReturn("<nullCD/>");
    when(CdaGeneratorUtils.getXmlForCD(anyString(), anyString())).thenReturn("<CD/>");
    Pair<Date, TimeZone> mockedEffDate = new Pair<>(null, TimeZone.getDefault());
    when(CdaFhirUtilities.getActualDate(any())).thenReturn(mockedEffDate);
    when(CdaFhirUtilities.getCodingXmlForCodeSystem(
            any(), anyString(), anyString(), anyBoolean(), anyString()))
        .thenReturn("");
    when(CdaFhirUtilities.getCodingXml(any(), anyString(), anyString())).thenReturn("<codingXml/>");
    when(CdaFhirUtilities.getMatchedCodesForResourceAndUrl(any(), anyString(), anyString()))
        .thenReturn(Collections.emptyList());
    String xml =
        CdaPlanOfTreatmentGenerator.getPlannedProcedureXml(sr, details, "contentRef", data);
    assertNotNull(xml);
    assertTrue(xml.contains("<nullCD/>"));
  }

  @Test
  public void testSortServiceRequests_addsToProcedure() {
    R4FhirData data = mock(R4FhirData.class);
    ServiceRequest sr = mock(ServiceRequest.class);
    CodeableConcept code = mock(CodeableConcept.class);
    Coding coding = mock(Coding.class);
    when(sr.hasCode()).thenReturn(true);
    when(sr.getCode()).thenReturn(code);
    when(code.hasCoding()).thenReturn(true);
    when(code.getCoding()).thenReturn(List.of(coding));
    mockStatic(CdaFhirUtilities.class);
    when(CdaFhirUtilities.isCodingPresentForCodeSystem(
            anyList(), eq(CdaGeneratorConstants.FHIR_CPT_URL)))
        .thenReturn(true);
    when(data.getServiceRequests()).thenReturn(List.of(sr));
    List<ServiceRequest> obsRequests = new ArrayList<>();
    List<ServiceRequest> procRequests = new ArrayList<>();
    CdaPlanOfTreatmentGenerator.sortServiceRequestsByType(data, obsRequests, procRequests);
    assertEquals(1, procRequests.size());
    assertEquals(sr, procRequests.get(0));
  }

  @Test
  public void testGetValidDiagnosticOrders_noDiagnosticReports() {
    R4FhirData data = mock(R4FhirData.class);
    when(data.getDiagReports()).thenReturn(null);
    List<DiagnosticReport> result = CdaPlanOfTreatmentGenerator.getValidDiagnosticOrders(data);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetValidServiceRequests_noServiceRequests() {
    R4FhirData data = mock(R4FhirData.class);
    when(data.getServiceRequests()).thenReturn(null);
    List<ServiceRequest> result = CdaPlanOfTreatmentGenerator.getValidServiceRequests(data);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetValidServiceRequests_withLoincCode() {
    ServiceRequest sr = mock(ServiceRequest.class);
    CodeableConcept cc = mock(CodeableConcept.class);
    List<Coding> codings = new ArrayList<>();
    codings.add(mock(Coding.class));
    when(cc.getCoding()).thenReturn(codings);
    when(sr.getCode()).thenReturn(cc);
    when(sr.getIdElement()).thenReturn(new IdType("sr-1"));

    mockStatic(CdaFhirUtilities.class);
    when(CdaFhirUtilities.isCodingPresentForCodeSystem(
            codings, CdaGeneratorConstants.FHIR_LOINC_URL))
        .thenReturn(true);

    R4FhirData data = mock(R4FhirData.class);
    when(data.getServiceRequests()).thenReturn(Collections.singletonList(sr));

    List<ServiceRequest> result = CdaPlanOfTreatmentGenerator.getValidServiceRequests(data);

    assertEquals(1, result.size());
    assertEquals(sr, result.get(0));
  }

  @Test
  public void testGetValidServiceRequests_withoutLoincCode() {
    ServiceRequest sr = mock(ServiceRequest.class);
    CodeableConcept cc = mock(CodeableConcept.class);
    List<Coding> codings = new ArrayList<>();
    codings.add(mock(Coding.class));
    when(cc.getCoding()).thenReturn(codings);
    when(sr.getCode()).thenReturn(cc);
    when(sr.getIdElement()).thenReturn(new IdType("sr-2"));

    mockStatic(CdaFhirUtilities.class);
    when(CdaFhirUtilities.isCodingPresentForCodeSystem(
            codings, CdaGeneratorConstants.FHIR_LOINC_URL))
        .thenReturn(false);

    R4FhirData data = mock(R4FhirData.class);
    when(data.getServiceRequests()).thenReturn(Collections.singletonList(sr));

    List<ServiceRequest> result = CdaPlanOfTreatmentGenerator.getValidServiceRequests(data);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testTimingBoundsPeriodStartDate() {
    MedicationRequest mr = new MedicationRequest();
    Dosage dosage = new Dosage();
    Timing t = new Timing();
    Timing.TimingRepeatComponent repeat = new Timing.TimingRepeatComponent();
    Period bounds = new Period();
    bounds.setStartElement(new DateTimeType("2025-02-17"));
    repeat.setBounds(bounds);
    t.setRepeat(repeat);
    dosage.setTiming(t);
    mr.addDosageInstruction(dosage);

    DateTimeType startDate = null;
    Timing timing = mr.getDosageInstructionFirstRep().getTiming();
    if (timing != null
        && timing.hasRepeat()
        && timing.getRepeat() != null
        && timing.getRepeat().hasBoundsPeriod()
        && timing.getRepeat().getBoundsPeriod() != null
        && timing.getRepeat().getBoundsPeriod().hasStartElement()) {
      startDate = timing.getRepeat().getBoundsPeriod().getStartElement();
    }
    assertNotNull(startDate);
    assertEquals("2025-02-17", startDate.getValueAsString());
  }

  @Test
  public void testGetXmlForValueIVLWithTSFallback() {
    String xml =
        CdaGeneratorUtils.getXmlForValueIVLWithTS(CdaGeneratorConstants.EFF_TIME_EL_NAME, "", "");
    assertNotNull(xml);
    assertTrue(xml.contains(CdaGeneratorConstants.EFF_TIME_EL_NAME));
  }

  @Test
  public void testPIVLEffectiveTimeAdded() {
    String freqInHours = "8"; // not UNKNOWN
    StringBuilder sb = new StringBuilder();
    if (!CdaGeneratorConstants.UNKNOWN_VALUE.contentEquals(freqInHours)) {
      sb.append(
          CdaGeneratorUtils.getXmlForPIVLWithTS(
              CdaGeneratorConstants.EFF_TIME_EL_NAME, freqInHours));
    }
    String xml = sb.toString();
    assertNotNull(xml);
    assertTrue(xml.contains("8"));
  }

  @Test
  public void testLabTestOrderTriggerTemplateXml() {
    String xml =
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE,
            CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT);
    assertNotNull(xml);
    assertTrue(xml.contains(CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE));
  }

  @Test
  public void testPlannedMedicationXml_CodeXmlPairTrue() {
    MedicationRequest mr = new MedicationRequest();
    mr.setMedication(new CodeableConcept().setText("MedA"));
    mr.setId("med-006");

    LaunchDetails details = new LaunchDetails();
    details.setAssigningAuthorityId("AUTH123");

    R4FhirData data = mock(R4FhirData.class);
    when(data.getMedicationList()).thenReturn(new ArrayList<>());

    List<Medication> medList = new ArrayList<>();
    Pair<Boolean, String> pair = new Pair<>(true, "<codeXml>test</codeXml>");
    mockStatic(CdaFhirUtilities.class);
    when(CdaFhirUtilities.getMedicationCodeXml(any(), any(), anyBoolean(), any(), anyList(), any()))
        .thenReturn(pair);
    when(CdaFhirUtilities.getMedicationCodeableConcept(any(), any())).thenCallRealMethod();

    String xml =
        CdaPlanOfTreatmentGenerator.getPlannedMedicationXml(
            mr, details, "ref6", data, null, null, null, medList, details, "v1");
    assertTrue(xml.contains("codeXml"));
  }

  @Test
  public void testPlannedMedicationXml_CodeXmlPairFalseNonEmptyFallback() {
    MedicationRequest mr = new MedicationRequest();
    mr.setMedication(new CodeableConcept().setText("MedB"));
    mr.setId("med-007");

    LaunchDetails details = new LaunchDetails();
    details.setAssigningAuthorityId("AUTH123");

    R4FhirData data = mock(R4FhirData.class);
    when(data.getMedicationList()).thenReturn(new ArrayList<>());

    List<Medication> medList = new ArrayList<>();
    mockStatic(CdaFhirUtilities.class);
    when(CdaFhirUtilities.getMedicationCodeXml(any(), any(), anyBoolean(), any(), anyList(), any()))
        .thenReturn(new Pair<>(false, ""));
    when(CdaFhirUtilities.getXmlForMedicationTypeForCodeSystem(
            any(), any(), anyBoolean(), anyString(), anyBoolean(), any(), any()))
        .thenReturn("<codeXml>fallback</codeXml>");

    String xml =
        CdaPlanOfTreatmentGenerator.getPlannedMedicationXml(
            mr, details, "ref7", data, null, null, null, medList, details, "v1");
    assertTrue(xml.contains("fallback"));
  }

  @Test
  public void testPlannedMedicationXml_CodeXmlEmptyFinalFallback() {
    MedicationRequest mr = new MedicationRequest();
    mr.setMedication(new CodeableConcept().setText("MedC"));
    mr.setId("med-008");

    LaunchDetails details = new LaunchDetails();
    details.setAssigningAuthorityId("AUTH123");

    R4FhirData data = mock(R4FhirData.class);
    when(data.getMedicationList()).thenReturn(new ArrayList<>());

    List<Medication> medList = new ArrayList<>();
    mockStatic(CdaFhirUtilities.class);
    when(CdaFhirUtilities.getMedicationCodeXml(any(), any(), anyBoolean(), any(), anyList(), any()))
        .thenReturn(new Pair<>(false, ""));
    when(CdaFhirUtilities.getXmlForMedicationTypeForCodeSystem(
            any(), any(), anyBoolean(), anyString(), anyBoolean(), any(), any()))
        .thenReturn("");
    when(CdaFhirUtilities.getXmlForTypeForCodeSystem(
            any(), any(), anyBoolean(), anyString(), anyBoolean()))
        .thenReturn("<codeXml>final</codeXml>");

    String xml =
        CdaPlanOfTreatmentGenerator.getPlannedMedicationXml(
            mr, details, "ref8", data, null, null, null, medList, details, "v1");
    assertTrue(xml.contains("final"));
  }

  @Test
  public void testServiceRequest_WithCoding() {
    ServiceRequest sr = new ServiceRequest();
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding().setSystem("http://loinc.org").setCode("12345"));
    sr.setCode(code);

    StringBuilder sb = new StringBuilder();

    if (sr.hasCode() && sr.getCode().getCoding() != null) {
      sb.append(
          CdaFhirUtilities.getCodingXml(
              sr.getCode().getCoding(), CdaGeneratorConstants.CODE_EL_NAME, ""));
    }

    String xml = sb.toString();
    assertTrue(xml.contains("12345"));
  }
}
