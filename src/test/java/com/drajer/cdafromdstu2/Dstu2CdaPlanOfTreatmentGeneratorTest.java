package com.drajer.cdafromdstu2;

import static com.drajer.test.util.TestUtils.loadBundleFromFile;

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
public class Dstu2CdaPlanOfTreatmentGeneratorTest extends BaseGenerator {

  public static final String XML_FOR_ENC_TEMPLATE_ID =
      "<templateId root=\"2.16.840.1.113883.10.20.22.2.22.1\" extension=\"2015-08-01\"/>";

  private static final String POT_HISTORY_CDA_FILE =
      "CdaDstuTestData/cda/PlanOfTreatment/PlanOfTreatment.xml";

  private static final String EMPTY_POT_CDA_FILE =
      "CdaDstuTestData/cda/PlanOfTreatment/EmptyPlanOfTreatment.xml";

  private static final String POT_BUNDLE_FILE = "CdaDstuTestData/Bundle/PlanOfTreatmentBundle.json";

  @Test
  public void TestGeneratePlanOfTreatmentSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(POT_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    String expectedXml = TestUtils.getFileContentAsString(POT_HISTORY_CDA_FILE);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        Dstu2CdaPlanOfTreatmentGenerator.generatePlanOfTreatmentSection(data, launchDetails);
    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void TestGenerateEmptyPlanOfTreatmentSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(POT_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_POT_CDA_FILE);
    String actualXml = Dstu2CdaPlanOfTreatmentGenerator.generateEmptyPlanOfTreatmentSection();
    assertXmlEquals(expectedXml, actualXml);
  }
}
