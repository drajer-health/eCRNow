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
public class Dstu2CdaHistoryOfPresentIllnessGeneratorTest extends BaseGenerator {

  private static final String HPI_CDA_FILE =
      "CdaDstuTestData/cda/HistoryOfPresentIllness/historyofpresentillness.xml";
  private static final String EMPTY_HPI_CDA_FILE =
      "CdaDstuTestData/cda/HistoryOfPresentIllness/empty_historyofpresentillness.xml";
  private static final String HPI_BUNDLE_FILE =
      "CdaDstuTestData/Bundle/HistoryOfPresentIllnessBundle.json";

  @Test
  public void testGenerateHistoryOfPresentIllnessSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(HPI_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String expectedXml = TestUtils.getFileContentAsString(HPI_CDA_FILE);
    String actualXml =
        Dstu2CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
            data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateHistoryOfPresentIllnessSection_Empty() {
    Dstu2FhirData data = new Dstu2FhirData();
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_HPI_CDA_FILE);
    String actualXml =
        Dstu2CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
            data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }
}
