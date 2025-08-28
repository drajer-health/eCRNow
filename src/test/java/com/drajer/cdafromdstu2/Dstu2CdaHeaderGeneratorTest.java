package com.drajer.cdafromdstu2;

import static org.junit.Assert.*;

import ca.uhn.fhir.model.dstu2.composite.*;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.model.dstu2.resource.Patient;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.test.util.TestUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class, Dstu2CdaFhirUtilities.class, CdaGeneratorConstants.class})
public class Dstu2CdaHeaderGeneratorTest extends BaseGenerator {

  private static final String PATIENT_DETAILS_CDA_FILE =
      "CdaDstuTestData/cda/Patient/patient_details.xml";
  private static final String MINIMAL_PATIENT_DETAILS_CDA_FILE =
      "CdaDstuTestData/cda/Patient/minimal_patient_details.xml";

  @Test
  public void testCreateCdaHeader_FromPatientBundle() {
    // Arrange
    String bundleFile = "CdaDstuTestData/Bundle/PatientBundle.json";
    String expectedCdaHeaderFile = "CdaDstuTestData/cda/Header/cda_header.xml";
    Bundle bundle = loadBundleFromFile(bundleFile);

    Dstu2FhirData data = new Dstu2FhirData();
    data.setData(bundle);
    addResourceToFhirData(bundle, data);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getGuid())
        .thenReturn("123e4567-e89b-12d3-a456-426614174000");
    PowerMockito.when(CdaGeneratorUtils.getCurrentDateTime()).thenReturn("2023-10-01T12:00:00Z");

    Eicr eicr = new Eicr();
    String actualCdaHeader =
        Dstu2CdaHeaderGenerator.createCdaHeader(data, launchDetails, eicr, "2.1");

    // Assert
    String expectedCdaHeader = TestUtils.getFileContentAsString(expectedCdaHeaderFile);
    assertXmlEquals(expectedCdaHeader, actualCdaHeader);
  }

  @Test
  public void testGetParticipantXml_NoAddressOrTelecom() {
    Patient.Contact contact = new Patient.Contact();
    HumanNameDt name = new HumanNameDt().addGiven("John");
    contact.setName(name);
    CodingDt coding = new CodingDt();
    coding.setCode("parent");
    coding.setSystem("system");
    coding.setDisplay("Parent");
    PowerMockito.mockStatic(
        CdaGeneratorUtils.class, Dstu2CdaFhirUtilities.class, CdaGeneratorConstants.class);
    Mockito.when(
            CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
                Mockito.anyString(), Mockito.anyString()))
        .thenReturn("<participant>");
    Mockito.when(CdaGeneratorConstants.getCodeForContactRelationship(Mockito.anyString()))
        .thenReturn("FRND");
    Mockito.when(
            CdaGeneratorUtils.getXmlForStartElementWithClassCode(
                Mockito.anyString(), Mockito.anyString()))
        .thenReturn("<associatedEntity>");
    Mockito.when(Dstu2CdaFhirUtilities.getAddressXml(Mockito.anyList())).thenReturn("");
    Mockito.when(Dstu2CdaFhirUtilities.getTelecomXml(Mockito.anyList())).thenReturn("");
    Mockito.when(CdaGeneratorUtils.getXmlForStartElement(Mockito.anyString()))
        .thenReturn("<start>");
    Mockito.when(CdaGeneratorUtils.getXmlForEndElement(Mockito.anyString())).thenReturn("</end>");
    Mockito.when(Dstu2CdaFhirUtilities.getNameXml(Mockito.anyList())).thenReturn("<name/>");

    String xml = Dstu2CdaHeaderGenerator.getParticipantXml(contact, coding);

    assertNotNull(xml);
    assertTrue(xml.contains("<participant>"));
    assertTrue(xml.contains("<associatedEntity>"));
    assertTrue(xml.contains("<name/>"));
  }

  @Test
  public void testGetParticipantXml_NullName() {
    Patient.Contact contact = new Patient.Contact();
    contact.setAddress(new AddressDt().addLine("456 Elm St"));
    CodingDt coding = new CodingDt();
    coding.setSystem("system").setCode("guardian").setDisplay("Guardian");

    PowerMockito.mockStatic(
        CdaGeneratorUtils.class, Dstu2CdaFhirUtilities.class, CdaGeneratorConstants.class);
    Mockito.when(
            CdaGeneratorUtils.getXmlForStartElementWithTypeCode(
                Mockito.anyString(), Mockito.anyString()))
        .thenReturn("<participant>");
    Mockito.when(CdaGeneratorConstants.getCodeForContactRelationship(Mockito.anyString()))
        .thenReturn("GDN");
    Mockito.when(
            CdaGeneratorUtils.getXmlForStartElementWithClassCode(
                Mockito.anyString(), Mockito.anyString()))
        .thenReturn("<associatedEntity>");
    Mockito.when(Dstu2CdaFhirUtilities.getAddressXml(Mockito.anyList())).thenReturn("<address/>");
    Mockito.when(Dstu2CdaFhirUtilities.getTelecomXml(Mockito.anyList())).thenReturn("");
    Mockito.when(CdaGeneratorUtils.getXmlForStartElement(Mockito.anyString()))
        .thenReturn("<start>");
    Mockito.when(CdaGeneratorUtils.getXmlForEndElement(Mockito.anyString())).thenReturn("</end>");
    Mockito.when(Dstu2CdaFhirUtilities.getNameXml(Mockito.anyList())).thenReturn("");

    String xml = Dstu2CdaHeaderGenerator.getParticipantXml(contact, coding);

    assertNotNull(xml);
    assertTrue(xml.contains("<participant>"));
    assertTrue(xml.contains("<associatedEntity>"));
    assertTrue(xml.contains("<address/>"));
  }

  @Test
  public void testGetOrganizationXml() {
    Bundle bundle = loadBundleFromFile("CdaDstuTestData/Bundle/locationBundle.json");

    Dstu2FhirData data = new Dstu2FhirData();
    data.setData(bundle);
    addResourceToFhirData(bundle, data);
    String expectedXml =
        "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"LOC-123\"/>\n"
            + "<name>General Hospital - Ward 1</name>\n"
            + "<telecom value=\"tel:(555)123-4567\" use=\"WP\"/>\n"
            + "<addr>\n"
            + "<streetAddressLine>123 Main St</streetAddressLine>\n"
            + "<city>Metropolis</city>\n"
            + "<state>NY</state>\n"
            + "<postalCode>10001</postalCode>\n"
            + "<country>USA</country>\n"
            + "</addr>\n";

    String actualXml =
        Dstu2CdaHeaderGenerator.getOrganizationXml(
            data.getOrganization(), data.getLocation(), launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetOrganizationXml_withEmpty() {
    Dstu2FhirData data = new Dstu2FhirData();

    String expectedXml =
        "<id nullFlavor=\"NI\"/>\n"
            + "<name>Unknown</name>\n"
            + "<telecom nullFlavor=\"NI\"/>\n"
            + "<addr>\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\n"
            + "<city nullFlavor=\"NI\"/>\n"
            + "<state nullFlavor=\"NI\"/>\n"
            + "<postalCode nullFlavor=\"NI\"/>\n"
            + "<country nullFlavor=\"NI\"/>\n"
            + "</addr>\n";

    String actualXml =
        Dstu2CdaHeaderGenerator.getOrganizationXml(
            data.getOrganization(), data.getLocation(), launchDetails);
    assertEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetPractitionerXml_withEmpty() {
    Dstu2FhirData data = new Dstu2FhirData();

    String expectedXml =
        "<id root=\"2.16.840.1.113883.4.6\"/>\n"
            + "<addr>\n"
            + "<streetAddressLine nullFlavor=\"NI\"/>\n"
            + "<city nullFlavor=\"NI\"/>\n"
            + "<state nullFlavor=\"NI\"/>\n"
            + "<postalCode nullFlavor=\"NI\"/>\n"
            + "<country nullFlavor=\"NI\"/>\n"
            + "</addr>\n"
            + "<telecom nullFlavor=\"NI\"/>\n"
            + "<assignedPerson>\n"
            + "<name>\n"
            + "<given nullFlavor=\"NI\"/>\n"
            + "<family nullFlavor=\"NI\"/>\n"
            + "</name>\n"
            + "</assignedPerson>\n";

    String actualXml = Dstu2CdaHeaderGenerator.getPractitionerXml(null);
    assertEquals(expectedXml, actualXml);
  }
}
