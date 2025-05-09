package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import com.drajer.cda.utils.CdaGeneratorConstants;
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
public class Dstu2CdaEncounterGeneratorTest extends BaseGenerator {

  public static final String XML_FOR_ENC_TEMPLATE_ID =
      "<templateId root=\"2.16.840.1.113883.10.20.22.2.22.1\" extension=\"2015-08-01\"/>";

  private static final String ENCOUNTER_CDA_FILE = "CdaDstuTestData/Cda/Encounter/encounter.xml";

  private static final String EMPTY_ENCOUNTER_CDA_FILE =
      "CdaDstuTestData/Cda/Encounter/empty_encounter.xml";

  private static final String ENCOUNTER_BUNDLE_FILE =
      "CdaDstuTestData/Bundle/EncounterBundle_97953900.json";

  @Test
  public void TestGenerateEncounterSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(ENCOUNTER_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    String exceptedXml = TestUtils.getFileContentAsString(ENCOUNTER_CDA_FILE);
    String actualXml = Dstu2CdaEncounterGenerator.generateEncounterSection(data, launchDetails);
    assertXmlEquals(exceptedXml, actualXml);
  }

  @Test
  public void TestGenerateEmptyEncounterSection() {
    Dstu2FhirData data = new Dstu2FhirData();
    String exceptedXml = TestUtils.getFileContentAsString(EMPTY_ENCOUNTER_CDA_FILE);
    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID,
                CdaGeneratorConstants.ENC_SEC_TEMPLATE_ID_EXT))
        .thenReturn(XML_FOR_ENC_TEMPLATE_ID);

    String actualXml = Dstu2CdaEncounterGenerator.generateEmptyEncounterSection();
    assertXmlEquals(exceptedXml, actualXml);
  }
}
