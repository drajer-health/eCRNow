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
public class Dstu2CdaReasonForVisitGeneratorTest extends BaseGenerator {

  private static final String REASON_FOR_VISIT_CDA_FILE =
      "CdaDstuTestData/cda/ReasonForVisit/reasonforvisit.xml";
  private static final String EMPTY_REASON_FOR_VISIT_CDA_FILE =
      "CdaDstuTestData/cda/ReasonForVisit/empty_reasonforvisit.xml";
  private static final String REASON_FOR_VISIT_BUNDLE_FILE =
      "CdaDstuTestData/Bundle/ReasonForVisitBundle.json";

  @Test
  public void testGenerateReasonForVisitSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(REASON_FOR_VISIT_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String expectedXml = TestUtils.getFileContentAsString(REASON_FOR_VISIT_CDA_FILE);
    String actualXml =
        Dstu2CdaReasonForVisitGenerator.generateReasonForVisitSection(data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateReasonForVisitSection_Empty() {
    Dstu2FhirData data = new Dstu2FhirData();
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_REASON_FOR_VISIT_CDA_FILE);
    String actualXml =
        Dstu2CdaReasonForVisitGenerator.generateReasonForVisitSection(data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }
}
