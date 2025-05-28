package com.drajer.cdafromr4;

import static org.junit.Assert.*;

import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Medication.MedicationIngredientComponent;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@ActiveProfiles("test")
@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaMedicationGeneratorTest extends BaseGeneratorTest {

  static final String MEDICATION_BUNDLE_SAMPLE = "R4/Medication/Medication_bundle_sample.json";
  private static final String MEDICATION_CDA_FILE = "CdaTestData/Cda/Medication/Medication.xml";

  private static final String MEDICATION_R31_CDA_FILE =
      "CdaTestData/Cda/Medication/MedicationR31.xml";
  private static final String MEDICATION_ADMINSTRACTION_R31_CDA_FILE =
      "CdaTestData/Cda/Medication/MedicationAdminR31.xml";

  @Test
  public void testGenerateMedicationSectionWithSampleData() {
    R4FhirData medicationResourceData = createResourceData(MEDICATION_BUNDLE_SAMPLE);
    String expectedXml = TestUtils.getFileContentAsString(MEDICATION_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaMedicationGenerator.generateMedicationSection(medicationResourceData, launchDetails);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateMedicationR31SectionWithSampleData() {
    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/MedicationStatement.json");

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
    data.getMedicationStatements().sort(Comparator.comparing(MedicationStatement::getId));
    data.setData(bundle);
    String expectedXml = TestUtils.getFileContentAsString(MEDICATION_R31_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaMedicationGenerator.generateR31MedicationsSection(data, launchDetails, "");

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateR31MedicationsAdministeredSection() {
    R4FhirData data = new R4FhirData();
    Bundle b = loadBundleFromFile("CdaTestData/bundle/medicationAdministration.json");

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
    data.getMedicationAdministrations().sort(Comparator.comparing(MedicationAdministration::getId));
    data.setData(bundle);
    String expectedXml = TestUtils.getFileContentAsString(MEDICATION_ADMINSTRACTION_R31_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaMedicationGenerator.generateR31MedicationsAdministeredSection(data, launchDetails, "");

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateMedicationSectionWithEmptyData() {

    String actual =
        CdaMedicationGenerator.generateMedicationSection(new R4FhirData(), launchDetails);

    assertNotNull(actual);
  }

  @Test
  public void TestGenerateEmptyMedicationsAdministeredSection() {
    String expectedXml =
        "<component>\r\n"
            + "<section nullFlavor=\"NI\">\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.38\"/>\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.38\" extension=\"2014-06-09\"/>\r\n"
            + "<code code=\"29549-3\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Medications Administered\"/>\r\n"
            + "<title>Medications Administered</title>\r\n"
            + "<text>No Medication Administered Information</text>\r\n"
            + "</section>\r\n"
            + "</component>";
    String actualXml = CdaMedicationGenerator.generateEmptyMedicationsAdministeredSection();

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateEmptyMedicationsSection() {
    String expectedXml =
        "<component>\n"
            + "<section nullFlavor=\"NI\">\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.1.1\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.1.1\" extension=\"2014-06-09\"/>\n"
            + "<code code=\"10160-0\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"History of Medication Use\"/>\n"
            + "<title>MEDICATIONS</title>\n"
            + "<text>No Medication Statement Information</text>\n"
            + "</section>\n"
            + "</component>\n";
    String actualXml = CdaMedicationGenerator.generateEmptyMedicationsSection();

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetValidMedicationRequestsWithExternalReference() {
    R4FhirData r4FhirData = new R4FhirData();
    MedicationRequest request = new MedicationRequest();
    request.setId("request-1");
    Reference medRef = new Reference();
    medRef.setReference("Medication/medication-1");
    request.setMedication(medRef);

    r4FhirData.getMedicationRequests().add(request);

    List<Medication> cmeds = new ArrayList<>();
    Medication medication1 = new Medication();
    medication1.setId("medication-1");
    medication1.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem("http://www.nlm.nih.gov/research/umls/rxnorm")
                    .setCode("1234")));

    MedicationIngredientComponent medicationIngredientComponent =
        new MedicationIngredientComponent();

    medicationIngredientComponent.setItem(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem("http://www.nlm.nih.gov/research/umls/rxnorm")
                    .setCode("5678")));
    Medication medication2 = new Medication();
    medication2.setId("medication2");
    medication2.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem("http://www.nlm.nih.gov/research/umls/rxnorm")
                    .setCode("5678")));
    cmeds.add(medication1);
    cmeds.add(medication2);

    List<MedicationRequest> result =
        CdaMedicationGenerator.getValidMedicationRequests(r4FhirData, cmeds);

    assertEquals(1, result.size());
  }

  @Test
  public void testGetValidMedicationRequestsWithCodeableConcept() {
    R4FhirData r4FhirData = new R4FhirData();
    MedicationRequest request = new MedicationRequest();
    request.setId("request-1");

    CodeableConcept code = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL);
    coding.setCode("code-1");
    code.setCoding(Collections.singletonList(coding));
    request.setMedication(code);

    r4FhirData.getMedicationRequests().add(request);

    List<Medication> cmeds = new ArrayList<>();
    Medication medication2 = new Medication();
    medication2.setId("medication2");
    medication2.setCode(
        new CodeableConcept()
            .addCoding(
                new Coding()
                    .setSystem("http://www.nlm.nih.gov/research/umls/rxnorm")
                    .setCode("5678")));
    cmeds.add(medication2);

    List<MedicationRequest> result =
        CdaMedicationGenerator.getValidMedicationRequests(r4FhirData, cmeds);

    assertEquals(1, result.size());
    assertEquals(request.getId(), result.get(0).getId());
  }
}
