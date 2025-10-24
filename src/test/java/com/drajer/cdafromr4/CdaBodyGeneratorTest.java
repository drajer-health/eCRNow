package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.Patient;
import org.junit.Test;
import org.powermock.core.classloader.annotations.PowerMockIgnore;

@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaBodyGeneratorTest extends BaseGeneratorTest {

  private static final String EMPTY_SECTION_BODY_CDA_FILE =
      "CdaTestData/Cda/BodySection/emptySectionBody.xml";

  @Test
  public void testGenerateCdaBodyWithEmptyComponent() {
    String expectedXml =
        "<component>\r\n" + "<structuredBody>\r\n" + "</structuredBody>\r\n" + "</component>";
    String actualXml =
        CdaBodyGenerator.generateCdaBody(null, null, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateCdaBodyWithEmptyData() {
    R4FhirData r4FhirData = createEmptyR4FhirData();
    String expectedXml = TestUtils.getFileContentAsString(EMPTY_SECTION_BODY_CDA_FILE);
    String actualXml =
        CdaBodyGenerator.generateCdaBody(
            r4FhirData, null, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  private R4FhirData createEmptyR4FhirData() {
    R4FhirData r4FhirData = new R4FhirData();
    r4FhirData.setPatient(new Patient());
    return r4FhirData;
  }
}
