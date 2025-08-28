package com.drajer.cdafromdstu2;

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
public class Dstu2CdaSocialHistoryTest extends BaseGenerator {

  public static final String XML_FOR_ENC_TEMPLATE_ID =
      "<templateId root=\"2.16.840.1.113883.10.20.22.2.22.1\" extension=\"2015-08-01\"/>";

  private static final String SOCIAL_HISTORY_CDA_FILE =
      "CdaDstuTestData/cda/SocialHistory/socialHistory.xml";

  private static final String EMPTY_SOCIAL_HISTORY_CDA_FILE =
      "CdaDstuTestData/cda/SocialHistory/empty_socialHistory.xml";

  private static final String SOCIAL_HISTORY_BUNDLE_FILE =
      "CdaDstuTestData/Bundle/socialHistoryBundle.json";

  @Test
  public void TestGenerateSocialHistorySection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(SOCIAL_HISTORY_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String exceptedXml = TestUtils.getFileContentAsString(SOCIAL_HISTORY_CDA_FILE);
    String actualXml =
        Dstu2CdaSocialHistoryGenerator.generateSocialHistorySection(data, launchDetails);
    assertXmlEquals(exceptedXml, actualXml);
  }

  @Test
  public void TestGenerateEmptySocialHistorySection() {
    Dstu2FhirData data = new Dstu2FhirData();

    String exceptedXml = TestUtils.getFileContentAsString(EMPTY_SOCIAL_HISTORY_CDA_FILE);
    String actualXml = Dstu2CdaSocialHistoryGenerator.generateEmptySocialHistorySection();
    assertXmlEquals(exceptedXml, actualXml);
  }
}
