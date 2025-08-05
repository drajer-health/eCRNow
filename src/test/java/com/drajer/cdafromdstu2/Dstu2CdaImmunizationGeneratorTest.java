package com.drajer.cdafromdstu2;

import static org.junit.Assert.*;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.test.util.TestUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class Dstu2CdaImmunizationGeneratorTest extends BaseGenerator {
  private static final String IMMUNIZATION_CDA_FILE =
      "CdaDstuTestData/cda/Immunization/immunization.xml";
  private static final String EMPTY_IMMUNIZATION_CDA_FILE =
      "CdaDstuTestData/cda/Immunization/empty_immunization.xml";
  private static final String IMMUNIZATION_BUNDLE_FILE =
      "CdaDstuTestData/Bundle/ImmunizationBundle.json";

  @Test
  public void testGenerateImmunizationSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(IMMUNIZATION_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String expectedXml = TestUtils.getFileContentAsString(IMMUNIZATION_CDA_FILE);
    String actualXml =
        Dstu2CdaImmunizationGenerator.generateImmunizationSection(data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateImmunizationSection_Empty() {
    Dstu2FhirData data = new Dstu2FhirData();
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_IMMUNIZATION_CDA_FILE);
    String actualXml =
        Dstu2CdaImmunizationGenerator.generateImmunizationSection(data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }
}
