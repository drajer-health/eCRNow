package com.drajer.cdafromr4;

import static org.junit.Assert.assertTrue;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.Patient;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
@RunWith(PowerMockRunner.class)
@PrepareForTest({
  CdaBodyGenerator.class,
  CdaProblemGenerator.class,
  CdaEncounterGenerator.class,
  CdaMedicationGenerator.class,
  CdaImmunizationGenerator.class,
  CdaResultGenerator.class,
  CdaChiefComplaintGenerator.class,
  CdaPlanOfTreatmentGenerator.class,
  CdaSocialHistoryGenerator.class,
  CdaPregnancyGenerator.class,
  CdaProcedureGenerator.class,
  CdaVitalSignsGenerator.class,
  CdaHistoryOfPresentIllnessGenerator.class,
  CdaReasonForVisitGenerator.class
})
public class CdaBodyGeneratorTest extends BaseGeneratorTest {

  private static final String EMPTY_SECTION_BODY_CDA_FILE =
      "CdaTestData/Cda/BodySection/emptySectionBody.xml";

  @Test
  public void testGenerateCdaBodyWithEmptyComponent() {
    String expectedXml =
        "<component>\r\n" + "<structuredBody>\r\n" + "</structuredBody>\r\n" + "</component>";
    String actualXml =
        CdaBodyGenerator.generateCdaBody(null, null, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateCdaBodyWithEmptyData() {
    R4FhirData r4FhirData = createEmptyR4FhirData();
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_SECTION_BODY_CDA_FILE);
    String actualXml =
        CdaBodyGenerator.generateCdaBody(
            r4FhirData, null, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  private R4FhirData createEmptyR4FhirData() {
    R4FhirData r4FhirData = new R4FhirData();
    r4FhirData.setPatient(new Patient());
    return r4FhirData;
  }

  @Test
  public void testGenerateCdaBody_R31Version_AllSections() throws Exception {
    R4FhirData data = new R4FhirData();
    data.setPatient(new Patient());
    LaunchDetails details = Mockito.mock(LaunchDetails.class);
    String version = "CDA_R31";

    PowerMockito.mockStatic(CdaProblemGenerator.class);
    PowerMockito.when(CdaProblemGenerator.generateProblemSection(data, details, version))
        .thenReturn("<problem/>");

    PowerMockito.mockStatic(CdaEncounterGenerator.class);
    PowerMockito.when(CdaEncounterGenerator.generateEncounterSection(data, details, version))
        .thenReturn("<encounter/>");

    PowerMockito.mockStatic(CdaMedicationGenerator.class);
    PowerMockito.when(
            CdaMedicationGenerator.generateR31MedicationsAdministeredSection(
                data, details, version))
        .thenReturn("<medAdmin/>");
    PowerMockito.when(CdaMedicationGenerator.generateR31MedicationsSection(data, details, version))
        .thenReturn("<meds/>");

    PowerMockito.mockStatic(CdaImmunizationGenerator.class);
    PowerMockito.when(CdaImmunizationGenerator.generateImmunizationSection(data, details, version))
        .thenReturn("<immunization/>");

    PowerMockito.mockStatic(CdaResultGenerator.class);
    PowerMockito.when(CdaResultGenerator.generateResultsSection(data, details, version))
        .thenReturn("<results/>");

    PowerMockito.mockStatic(CdaChiefComplaintGenerator.class);
    PowerMockito.when(
            CdaChiefComplaintGenerator.generateChiefComplaintSection(data, details, version))
        .thenReturn("<chiefComplaint/>");

    PowerMockito.mockStatic(CdaPlanOfTreatmentGenerator.class);
    PowerMockito.when(
            CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(data, details, version))
        .thenReturn("<planOfTreatment/>");

    PowerMockito.mockStatic(CdaSocialHistoryGenerator.class);
    PowerMockito.when(
            CdaSocialHistoryGenerator.generateR31SocialHistorySection(data, details, version))
        .thenReturn("<socialHistory/>");

    PowerMockito.mockStatic(CdaPregnancyGenerator.class);
    PowerMockito.when(CdaPregnancyGenerator.generatePregnancySection(data, details, version))
        .thenReturn("<pregnancy/>");

    PowerMockito.mockStatic(CdaProcedureGenerator.class);
    PowerMockito.when(CdaProcedureGenerator.generateProcedureSection(data, details, version))
        .thenReturn("<procedure/>");

    PowerMockito.mockStatic(CdaVitalSignsGenerator.class);
    PowerMockito.when(CdaVitalSignsGenerator.generateVitalsSection(data, details, version))
        .thenReturn("<vitals/>");
    PowerMockito.mockStatic(CdaHistoryOfPresentIllnessGenerator.class);
    PowerMockito.when(
            CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
                data, version))
        .thenReturn("<history/>");
    PowerMockito.mockStatic(CdaReasonForVisitGenerator.class);
    PowerMockito.when(CdaReasonForVisitGenerator.generateReasonForVisitSection(data, version))
        .thenReturn("<reason/>");
    String xml = CdaBodyGenerator.generateCdaBody(data, details, version);
    assertTrue(xml.contains("<problem/>"));
    assertTrue(xml.contains("<encounter/>"));
    assertTrue(xml.contains("<medAdmin/>"));
    assertTrue(xml.contains("<meds/>"));
    assertTrue(xml.contains("<immunization/>"));
    assertTrue(xml.contains("<results/>"));
    assertTrue(xml.contains("<chiefComplaint/>"));
    assertTrue(xml.contains("<planOfTreatment/>"));
    assertTrue(xml.contains("<socialHistory/>"));
    assertTrue(xml.contains("<pregnancy/>"));
    assertTrue(xml.contains("<procedure/>"));
    assertTrue(xml.contains("<vitals/>"));
    assertTrue(xml.contains("<history/>"));
    assertTrue(xml.contains("<reason/>"));
    assertTrue(xml.contains("<structuredBody>"));
    assertTrue(xml.contains("<component>"));
  }
}
