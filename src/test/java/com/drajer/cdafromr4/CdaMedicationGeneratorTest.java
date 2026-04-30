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
  private static final String VERSION = "R1.1";
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
        CdaMedicationGenerator.generateMedicationSection(
            medicationResourceData, launchDetails, VERSION);

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
        CdaMedicationGenerator.generateMedicationSection(new R4FhirData(), launchDetails, VERSION);

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

  @Test
  public void testConvertToDosage_whenAdminDosageIsNull() {
    MedicationAdministration.MedicationAdministrationDosageComponent adminDosage = null;
    Dosage result = CdaMedicationGenerator.convertToDosage(adminDosage);
    assertEquals(null, result);
  }

  @Test
  public void testGenerateMedicationSection_withDosageTimingAndDose() {
    R4FhirData data = new R4FhirData();
    MedicationStatement medStatement = new MedicationStatement();
    medStatement.setId("med-1");
    medStatement.setStatus(MedicationStatement.MedicationStatementStatus.ACTIVE);
    Dosage dosage = new Dosage();
    Timing timing = new Timing();
    Timing.TimingRepeatComponent repeat = new Timing.TimingRepeatComponent();
    repeat.setFrequency(2);
    repeat.setPeriod(1);
    repeat.setPeriodUnit(Timing.UnitsOfTime.D);
    timing.setRepeat(repeat);
    dosage.setTiming(timing);
    Dosage.DosageDoseAndRateComponent doseAndRate = new Dosage.DosageDoseAndRateComponent();
    Quantity doseQuantity = new Quantity();
    doseQuantity.setValue(500);
    doseQuantity.setUnit("mg");
    doseAndRate.setDose(doseQuantity);
    dosage.addDoseAndRate(doseAndRate);
    medStatement.addDosage(dosage);
    data.getMedicationStatements().add(medStatement);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String actualXml =
        CdaMedicationGenerator.generateMedicationSection(data, launchDetails, VERSION);
    assertNotNull(actualXml);
    assertEquals(true, actualXml.contains("500"));
    assertEquals(true, actualXml.contains("mg"));
  }

  @Test
  public void testGenerateMedicationSection_withoutStatus_setsActiveStatus() {
    R4FhirData data = new R4FhirData();
    MedicationStatement medStatement = new MedicationStatement();
    medStatement.setId("med-active-1");
    data.getMedicationStatements().add(medStatement);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String actualXml =
        CdaMedicationGenerator.generateMedicationSection(data, launchDetails, VERSION);
    assertNotNull(actualXml);
    assertEquals(true, actualXml.contains("active"));
  }

  @Test
  public void testGetValidMedicationRequests_withIngredient() {
    R4FhirData data = new R4FhirData();
    MedicationRequest req = new MedicationRequest();
    req.setId("req-2");
    Reference medRef = new Reference("med-2");
    req.setMedication(medRef);
    data.getMedicationRequests().add(req);
    Medication med = new Medication();
    med.setId("med-2");
    MedicationIngredientComponent ingredient = new MedicationIngredientComponent();
    ingredient.setItem(
        new CodeableConcept()
            .addCoding(
                new Coding().setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL).setCode("6789")));
    med.setIngredient(Collections.singletonList(ingredient));
    List<Medication> meds = Collections.singletonList(med);
    List<MedicationRequest> result = CdaMedicationGenerator.getValidMedicationRequests(data, meds);
    assertEquals(1, result.size());
    assertEquals("req-2", result.get(0).getId());
  }

  @Test
  public void testGetValidMedicationAdministrations_realObjects() {
    R4FhirData data = new R4FhirData();
    Medication containedMed = new Medication();
    containedMed.setId("med1");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding(CdaGeneratorConstants.FHIR_RXNORM_URL, "123", null));
    containedMed.setCode(code);
    MedicationAdministration medAdm = new MedicationAdministration();
    medAdm.setId("adm1");
    Reference medRef = new Reference("#med1");
    medAdm.setMedication(medRef);
    medAdm.getContained().add(containedMed);
    MedicationIngredientComponent ingComp = new MedicationIngredientComponent();
    CodeableConcept ingCode = new CodeableConcept();
    ingCode.addCoding(new Coding(CdaGeneratorConstants.FHIR_RXNORM_URL, "567", null));
    ingComp.setItem(ingCode);
    Medication extMed = new Medication();
    extMed.setId("med2");
    extMed.addIngredient(ingComp);
    MedicationAdministration medAdmExt = new MedicationAdministration();
    medAdmExt.setId("adm2");
    Reference extRef = new Reference("Medication/med2");
    medAdmExt.setMedication(extRef);
    MedicationAdministration medAdmCC = new MedicationAdministration();
    CodeableConcept ccMed = new CodeableConcept();
    ccMed.addCoding(new Coding(CdaGeneratorConstants.FHIR_RXNORM_URL, "789", null));
    medAdmCC.setMedication(ccMed);
    data.getMedicationAdministrations().add(medAdm);
    data.getMedicationAdministrations().add(medAdmExt);
    data.getMedicationAdministrations().add(medAdmCC);
    List<Medication> cmeds = new ArrayList<>();
    cmeds.add(extMed);
    List<MedicationAdministration> result =
        CdaMedicationGenerator.getValidMedicationAdministrations(data, cmeds);
    assertEquals(3, result.size());
    assertTrue(result.contains(medAdm));
    assertTrue(result.contains(medAdmExt));
    assertTrue(result.contains(medAdmCC));
  }

  @Test
  public void testGetValidMedicationAdministrations_withContainedMedicationIngredient() {
    R4FhirData data = new R4FhirData();
    List<Medication> cmeds = new ArrayList<>();
    MedicationAdministration medAdm = new MedicationAdministration();
    Medication containedMed = new Medication();
    containedMed.setId("med1");
    Medication.MedicationIngredientComponent ingredient =
        new Medication.MedicationIngredientComponent();
    CodeableConcept cc = new CodeableConcept();
    cc.addCoding(new Coding().setSystem(CdaGeneratorConstants.FHIR_RXNORM_URL).setCode("12345"));
    ingredient.setItem(cc);
    containedMed.addIngredient(ingredient);
    medAdm.getContained().add(containedMed);
    medAdm.setMedication(new Reference("#med1"));
    data.getMedicationAdministrations().add(medAdm);
    List<MedicationAdministration> result =
        CdaMedicationGenerator.getValidMedicationAdministrations(data, cmeds);
    assertEquals(1, result.size());
    assertEquals(medAdm, result.get(0));
    assertEquals(1, cmeds.size());
    assertEquals(containedMed, cmeds.get(0));
  }

  @Test
  public void testGetValidMedicationAdministrations_noMedicationAdministrations() {
    R4FhirData data = new R4FhirData();
    data.setMedicationAdministrations(new ArrayList<>());
    List<Medication> cmeds = new ArrayList<>();
    List<MedicationAdministration> result =
        CdaMedicationGenerator.getValidMedicationAdministrations(data, cmeds);
    assertNotNull(result);
    assertTrue(result.isEmpty());
    assertTrue(cmeds.isEmpty());
  }

  @Test
  public void testContainedMedicationWithCodeAndIngredient() {
    R4FhirData data = new R4FhirData();
    List<Medication> cmeds = new ArrayList<>();
    MedicationStatement stmt = new MedicationStatement();
    Medication med = new Medication();
    med.setId("med1");
    CodeableConcept code = new CodeableConcept();
    code.addCoding(new Coding(CdaGeneratorConstants.FHIR_RXNORM_URL, "12345", "Test"));
    med.setCode(code);
    MedicationIngredientComponent ing = new MedicationIngredientComponent();
    CodeableConcept ingCC = new CodeableConcept();
    ingCC.addCoding(new Coding(CdaGeneratorConstants.FHIR_RXNORM_URL, "67890", "Ing"));
    ing.setItem(ingCC);
    List<MedicationIngredientComponent> ings = new ArrayList<>();
    ings.add(ing);
    med.setIngredient(ings);
    stmt.addContained(med);
    Reference ref = new Reference("#med1");
    stmt.setMedication(ref);
    List<MedicationStatement> statements = new ArrayList<>();
    statements.add(stmt);
    data.setMedicationStatements(statements);
    List<MedicationStatement> result =
        com.drajer.cdafromr4.CdaMedicationGenerator.getValidMedicationStatements(data, cmeds);
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(1, cmeds.size());
  }

  @Test
  public void testExternalMedicationIngredient() {
    R4FhirData data = new R4FhirData();
    List<Medication> cmeds = new ArrayList<>();
    MedicationStatement stmt = new MedicationStatement();
    stmt.setId("stmt1");
    Medication extMed = new Medication();
    extMed.setId("medext1");
    MedicationIngredientComponent ing = new MedicationIngredientComponent();
    CodeableConcept ingCC = new CodeableConcept();
    ingCC.addCoding(new Coding(CdaGeneratorConstants.FHIR_RXNORM_URL, "11111", "IngExt"));
    ing.setItem(ingCC);
    List<MedicationIngredientComponent> ings = new ArrayList<>();
    ings.add(ing);
    extMed.setIngredient(ings);
    cmeds.add(extMed);
    Reference ref = new Reference("Medication/medext1");
    stmt.setMedication(ref);
    List<MedicationStatement> statements = new ArrayList<>();
    statements.add(stmt);
    data.setMedicationStatements(statements);
    List<MedicationStatement> result =
        com.drajer.cdafromr4.CdaMedicationGenerator.getValidMedicationStatements(data, cmeds);
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void testMedicationStatementEmptyData() {
    R4FhirData data = new R4FhirData();
    List<Medication> cmeds = new ArrayList<>();
    List<MedicationStatement> result =
        com.drajer.cdafromr4.CdaMedicationGenerator.getValidMedicationStatements(data, cmeds);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testContainedMedicationIngredientBranch() {
    R4FhirData data = new R4FhirData();
    List<Medication> cmeds = new ArrayList<>();
    MedicationStatement stmt = new MedicationStatement();
    Medication containedMed = new Medication();
    containedMed.setId("med1");
    MedicationIngredientComponent ing = new MedicationIngredientComponent();
    CodeableConcept cc = new CodeableConcept();
    cc.addCoding(new Coding(CdaGeneratorConstants.FHIR_RXNORM_URL, "11111", "Ing"));
    ing.setItem(cc);
    List<MedicationIngredientComponent> ingredients = new ArrayList<>();
    ingredients.add(ing);
    containedMed.setIngredient(ingredients);
    stmt.addContained(containedMed);
    Reference ref = new Reference("#med1");
    stmt.setMedication(ref);
    List<MedicationStatement> statements = new ArrayList<>();
    statements.add(stmt);
    data.setMedicationStatements(statements);
    List<MedicationStatement> result =
        CdaMedicationGenerator.getValidMedicationStatements(data, cmeds);
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(1, cmeds.size());
    assertEquals(containedMed, cmeds.get(0));
  }
}
