package com.drajer.cdafromr4;

import static org.junit.Assert.*;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Medication;
import org.hl7.fhir.r4.model.Medication.MedicationIngredientComponent;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Reference;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@ActiveProfiles("test")
@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class CdaMedicationGeneratorTest extends BaseGeneratorTest {

  static final String MEDICATION_BUNDLE_SAMPLE = "R4/Medication/Medication_bundle_sample.json";
  private static final String MEDICATION_CDA_FILE = "CdaTestData/Cda/Medication/Medication.xml";

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
  public void testGenerateMedicationSectionWithEmptyData() {

    String actual =
        CdaMedicationGenerator.generateMedicationSection(new R4FhirData(), launchDetails);

    assertNotNull(actual);
  }

  @Test
  public void testGenerateEmptyMedicationsSection() {
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
    String actualXml = CdaMedicationGenerator.generateEmptyMedications();

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
