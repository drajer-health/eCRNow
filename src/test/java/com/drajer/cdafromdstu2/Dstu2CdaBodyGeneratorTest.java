package com.drajer.cdafromdstu2;

import com.drajer.test.util.TestUtils;
import org.junit.Test;

public class Dstu2CdaBodyGeneratorTest extends BaseGenerator {

  @Test
  public void generateCdaBodyTest() {
    String exceptedXml = TestUtils.getFileContentAsString(PATIENT_CDA_FILE);
    String actualXml =
        Dstu2CdaBodyGenerator.generateCdaBody(dstu2FhirDataForPatient, launchDetails, "Dummy");
    assertXmlEquals(exceptedXml, actualXml);
  }
}
