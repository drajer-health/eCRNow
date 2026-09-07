package com.drajer.cdafromr4;

import static org.junit.Assert.*;

import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Immunization.ImmunizationStatus;
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
public class CdaImmunizationGeneratorTest extends BaseGeneratorTest {

  private static final String IMMUNIZATION_FILE = "CdaTestData/Immunization/Immunization.json";
  private static final String IMMUNIZATION_WITH_PERFORMER_BUNDLE_FILE =
      "CdaTestData/Immunization/Immunization_with_performer.json";
  private static final String IMMUNIZATION_CDA_FILE =
      "CdaTestData/Cda/Immunization/Immunization.xml";
  private static final String IMMUNIZATION_WITH_PERFORMER_CDA_FILE =
      "CdaTestData/Cda/Immunization/immunization_with_performer.xml";

  @Test
  public void testGenerateImmunizationSection() {
    R4FhirData immunizationResourceData = createResourceData(IMMUNIZATION_FILE);
    String expectedXml = TestUtils.getFileContentAsString(IMMUNIZATION_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String actualXml =
        CdaImmunizationGenerator.generateImmunizationSection(
            immunizationResourceData, launchDetails, "CDA_R11");
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateEmptyImmunizations() {

    String expectedXml =
        "<component>\r\n"
            + "<section nullFlavor=\"NI\">\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.2.1\"/>\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.2.2.1\" extension=\"2015-08-01\"/>\r\n"
            + "<code code=\"11369-6\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"History of immunizations\"/>\r\n"
            + "<title>IMMUNIZATIONS</title>\r\n"
            + "<text>No ImmunizationInformation</text>\r\n"
            + "</section>\r\n"
            + "</component>";

    String actualXml = CdaImmunizationGenerator.generateEmptyImmunizations();
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateImmunizationSectionWithPerformer() {
    Bundle b = loadBundleFromFile(IMMUNIZATION_WITH_PERFORMER_BUNDLE_FILE);

    R4FhirData immunizationResourceData = createR4Resource(new R4FhirData(), b);

    String expectedXml = TestUtils.getFileContentAsString(IMMUNIZATION_WITH_PERFORMER_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String actualXml =
        CdaImmunizationGenerator.generateImmunizationSection(
            immunizationResourceData, launchDetails, "CDA_R11");
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetManufacturerXml_NoManufacturerReference() {
    Immunization imm = new Immunization();
    imm.setId("Immunization/1");

    R4FhirData data = new R4FhirData();

    String result = CdaImmunizationGenerator.getManufacturerXml(imm, data);

    assertNotNull("Should return empty string when no manufacturer reference", result);
    assertEquals("Should be empty", "", result);
  }

  @Test
  public void testGetManufacturerXml_ManufacturerNotFound() {
    Immunization imm = new Immunization();
    imm.setId("Immunization/2");
    Reference mfgRef = new Reference("Organization/mfg-1");
    imm.setManufacturer(mfgRef);

    R4FhirData data = new R4FhirData();

    String result = CdaImmunizationGenerator.getManufacturerXml(imm, data);

    assertNotNull("Should handle manufacturer not found", result);
    assertEquals("Should be empty when manufacturer not in data", "", result);
  }

  @Test
  public void testGetManufacturerXml_ManufacturerNoName() {
    Immunization imm = new Immunization();
    imm.setId("Immunization/3");
    Reference mfgRef = new Reference("Organization/mfg-2");
    imm.setManufacturer(mfgRef);

    Organization org = new Organization();
    org.setId("mfg-2");

    R4FhirData data = new R4FhirData();
    java.util.List<Organization> orgs = new java.util.ArrayList<>();
    orgs.add(org);
    data.addOrganization(orgs);

    String result = CdaImmunizationGenerator.getManufacturerXml(imm, data);

    assertNotNull("Should handle organization without name", result);
    assertEquals("Should be empty when manufacturer has no name", "", result);
  }

  @Test
  public void testGetManufacturerXml_WithManufacturerAndName() {
    Immunization imm = new Immunization();
    imm.setId("Immunization/4");
    Reference mfgRef = new Reference("Organization/mfg-3");
    imm.setManufacturer(mfgRef);

    Organization org = new Organization();
    org.setId("mfg-3");
    org.setName("Vaccine Manufacturer Inc");

    R4FhirData data = new R4FhirData();
    java.util.List<Organization> orgs = new java.util.ArrayList<>();
    orgs.add(org);
    data.addOrganization(orgs);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String result = CdaImmunizationGenerator.getManufacturerXml(imm, data);

    assertNotNull("Should generate manufacturer XML", result);
    assertTrue("Should contain manufacturer organization", result.length() > 0);
  }

  @Test
  public void testAddImmunizationStatus_NotCompleted() {
    Immunization imm = new Immunization();
    imm.setId("Immunization/status-2");
    imm.setStatus(ImmunizationStatus.ENTEREDINERROR);

    StringBuilder sb = new StringBuilder();
    R4FhirData data = new R4FhirData();
    java.util.List<Immunization> imms = new java.util.ArrayList<>();
    imms.add(imm);
    data.setImmunizations(imms);

    String result =
        CdaImmunizationGenerator.generateImmunizationSection(data, launchDetails, "CDA_R31");
    assertNotNull("Should generate section with non-completed status", result);
  }

  @Test
  public void testAddRouteAndDoseInformation_WithRoute() {
    Immunization imm = new Immunization();
    imm.setId("Immunization/route-1");
    CodeableConcept route = new CodeableConcept();
    route.addCoding(new Coding().setSystem("http://snomed.info/sct").setCode("34206005"));
    imm.setRoute(route);

    R4FhirData data = new R4FhirData();
    java.util.List<Immunization> imms = new java.util.ArrayList<>();
    imms.add(imm);
    data.setImmunizations(imms);

    String result =
        CdaImmunizationGenerator.generateImmunizationSection(data, launchDetails, "CDA_R31");
    assertNotNull("Should handle immunization with route", result);
  }
}
