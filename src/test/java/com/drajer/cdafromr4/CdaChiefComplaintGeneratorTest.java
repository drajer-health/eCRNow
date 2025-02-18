package com.drajer.cdafromr4;

import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import org.junit.Test;

public class CdaChiefComplaintGeneratorTest extends BaseGeneratorTest {

  static final String PLAN_OF_TREATMENT_BUNDLE_RESOURCE_FILE =
      "CdaTestData/PlanOfTreatment/plan_of_treatment_bundle_resource.json";
  static final String CHEIF_COMPLAINT_CDA_FILE =
      "CdaTestData//Cda//cheif_Complaint//cheifComplaint.xml";

  @Test
  public void testGenerateChiefComplaintSection() {
    R4FhirData data = new R4FhirData();
    String expectedXml = TestUtils.getFileContentAsString(CHEIF_COMPLAINT_CDA_FILE);
    String actualXml =
        CdaChiefComplaintGenerator.generateChiefComplaintSection(data, launchDetails, "3.1");

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testgenerateChiefComplaintHeader_withNullNf() {

    String expectedXml =
        "<component>\n"
            + "<section>\n"
            + "<templateId root=\"1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1\"/>\n"
            + "<code code=\"10154-3\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"CHIEF COMPLAINT\"/>\n"
            + "<title>CHIEF COMPLAINT</title>\n";
    String actualXml = CdaChiefComplaintGenerator.generateChiefComplaintHeader(null);

    assertXmlEquals(expectedXml, actualXml);
  }
}
