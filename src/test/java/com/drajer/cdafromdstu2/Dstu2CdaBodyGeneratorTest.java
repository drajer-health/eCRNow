package com.drajer.cdafromdstu2;

import static org.junit.Assert.*;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.test.util.TestUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
public class Dstu2CdaBodyGeneratorTest extends BaseGenerator {

  private static final String BODY_CDA_FILE = "CdaDstuTestData/cda/Body/body.xml";
  private static final String EMPTY_BODY_CDA_FILE = "CdaDstuTestData/cda/Bundle/empty_body.xml";
  private static final String BODY_BUNDLE_FILE = "CdaDstuTestData/Bundle/empty_bundle.json";

  @Test
  public void testGenerateCdaBody_Empty() {
    Dstu2FhirData data = new Dstu2FhirData();
    Bundle bundle = loadBundleFromFile(BODY_BUNDLE_FILE);
    addResourceToFhirData(bundle, data);
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_BODY_CDA_FILE);
    String actualXml = Dstu2CdaBodyGenerator.generateCdaBody(data, launchDetails, "2.1");
    assertXmlEquals(expectedXml, actualXml);
  }
}
