package com.drajer.cdafromr4;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.test.util.TestUtils;
import org.junit.Test;

public class CdaChiefComplaintGeneratorTest extends BaseGeneratorTest {

  static final String PLAN_OF_TREATMENT_BUNDLE_RESOURCE_FILE =
      "CdaTestData/PlanOfTreatment/plan_of_treatment_bundle_resource.json";
  static final String CHEIF_COMPLAINT_CDA_FILE =
      "CdaTestData//Cda//cheif_Complaint//cheifComplaint.xml";

  @Test
  public void testGenerateChiefComplaintSection() {
    String expectedXml = TestUtils.getFileContentAsString(CHEIF_COMPLAINT_CDA_FILE);
    String actualXml =
        CdaChiefComplaintGenerator.generateChiefComplaintSection(
            null, null, CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testgenerateChiefComplaintHeader_withNullNf() {

    String expectedXml =
        """
        <component>
        <section>
        <templateId root="1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1"/>
        <code code="10154-3" codeSystem="2.16.840.1.113883.6.1" codeSystemName="LOINC" displayName="CHIEF COMPLAINT"/>
        <title>CHIEF COMPLAINT</title>
        """;
    String actualXml = CdaChiefComplaintGenerator.generateChiefComplaintHeader(null);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateChiefComplaintHeader_nullFlavorArm() {
    String nf =
        CdaChiefComplaintGenerator.generateChiefComplaintHeader(CdaGeneratorConstants.NF_NI);
    assertNotNull(nf);
    assertTrue("nullFlavor section must be emitted", nf.contains(CdaGeneratorConstants.NF_NI));

    String normal = CdaChiefComplaintGenerator.generateChiefComplaintHeader("");
    assertNotNull(normal);
    assertTrue("a plain section must be opened", normal.contains("<section"));

    String nullArg = CdaChiefComplaintGenerator.generateChiefComplaintHeader(null);
    assertNotNull("a null nf must take the plain arm", nullArg);
  }
}
