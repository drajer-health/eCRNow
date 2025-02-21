package com.drajer.cdafromdstu2;

import com.drajer.test.util.TestUtils;
import org.junit.Test;

public class Dstu2CdaReasonForVisitGeneratorTest extends BaseGenerator {
    private static String REASONOFVISITSECTION = "CdaDstuTestData/Cda/Visit/ReasonOfVisitSection.xml";

    @Test
    public void generateReasonForVisitSectionTest() {
        String exceptedXml = TestUtils.getFileContentAsString(REASONOFVISITSECTION);
        String actualXml = Dstu2CdaReasonForVisitGenerator.generateReasonForVisitSection(dstu2FhirDataForPatient, launchDetails);
        assertXmlEquals(exceptedXml, actualXml);
    }
}
