package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
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
            immunizationResourceData, launchDetails);
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
            immunizationResourceData, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }
}
