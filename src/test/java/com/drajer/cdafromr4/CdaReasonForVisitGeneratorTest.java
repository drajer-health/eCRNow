package com.drajer.cdafromr4;

import static org.junit.Assert.*;

import com.drajer.test.util.TestUtils;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Test;

public class CdaReasonForVisitGeneratorTest extends BaseGeneratorTest {

  static final String FILENAME = "CdaTestData/Encounter/ReasonForVisitWithCode.json";

  private static final String REASON_FOR_VISIT_CDA_FILE =
      "CdaTestData/Cda/Encounter/ReasonForVisit.xml";

  @Test
  public void testGenerateReasonForVisitSection() {

    Encounter encounter = (Encounter) loadResourceDataFromFile(Encounter.class, FILENAME);
    r4FhirData.setEncounter(encounter);
    String expectedXml = TestUtils.getFileContentAsString(REASON_FOR_VISIT_CDA_FILE);
    String actualXml = CdaReasonForVisitGenerator.generateReasonForVisitSection(r4FhirData);
    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }
}
