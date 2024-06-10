package com.drajer.cdafromr4;

import static org.junit.Assert.*;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class CdaPlanOfTreatmentGeneratorTest extends BaseGeneratorTest {

  static final String PLAN_OF_TREATMENT_BUNDLE_RESOURCE_FILE =
      "CdaTestData/PlanOfTreatment/plan_of_treatment_bundle_resource.json";
  static final String PLAN_OF_TREATMENT_CDA_FILE =
      "CdaTestData//Cda//PlanOfTreatment//PlanOfTreatment.xml";

  @Test
  public void testGeneratePlanOfTreatmentSection() {
    R4FhirData r4Data = createResourceData(PLAN_OF_TREATMENT_BUNDLE_RESOURCE_FILE);

    String expectedXml = TestUtils.getFileContentAsString(PLAN_OF_TREATMENT_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(r4Data, launchDetails);

    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetPlannedObservationXml() {
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

    String actual = CdaPlanOfTreatmentGenerator.getPlannedObservationXml(sr, details, contentRef);
    assertNotNull(actual);
  }

  @Test
  public void testGetDiagnosticReportXml() {
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
    String actual = CdaPlanOfTreatmentGenerator.getDiagnosticReportXml(dr, details, contentRef);
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
}
