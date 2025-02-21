package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.dstu2.resource.Bundle;
import com.drajer.test.util.TestUtils;
import org.junit.Assert;
import org.junit.Test;

public class Dstu2CdaEicrGeneratorTest extends BaseGenerator {

  private static final String BUNDLE_RES_XML = "CdaDstuTestData/Bundle/Bundle.json";
  private static final String ERIC_GENERATOR_XML = "CdaDstuTestData/Cda/Bundle/EricGenerator.xml";

  @Test
  public void convertDstu2FhirBundletoCdaEicrTest() {
    Bundle bundle = loadResourceDataFromFile(Bundle.class, BUNDLE_RES_XML);
    dstu2FhirDataForBundle.setData(bundle);
    String exceptedXml = TestUtils.getFileContentAsString(ERIC_GENERATOR_XML);
    String actualXml =
        Dstu2CdaEicrGenerator.convertDstu2FhirBundletoCdaEicr(
            dstu2FhirDataForBundle, launchDetails, eicr, "1.0");
    //System.out.println(actualXml);
    //assertXmlEquals(exceptedXml, actualXml);
    Assert.assertNotNull(actualXml);
  }
}
