package com.drajer.cdafromr4;

import static com.drajer.cda.utils.CdaGeneratorConstants.FHIR_SNOMED_URL;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class, CdaFhirUtilities.class})
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
    //    data.getLabResults().sort(Comparator.comparing(Observation::getId));
    //    data.getDiagReports().sort(Comparator.comparing(DiagnosticReport::getId));
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
}
