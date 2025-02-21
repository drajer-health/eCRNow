package com.drajer.cdafromdstu2;

import com.drajer.test.util.TestUtils;
import org.junit.Test;

public class Dstu2CdaHistoryOfPresentIllnessGeneratorTest extends BaseGenerator {

    private static String PATIENT_ILLNESS_HISTORY = "CdaDstuTestData/Cda/Patient/PatientIllenessHistorySection.xml";

    @Test
    public void generateHistoryOfPresentIllnessSectionTest() {
        String exceptedXml = TestUtils.getFileContentAsString(PATIENT_ILLNESS_HISTORY);
        String actualXml = Dstu2CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
                dstu2FhirDataForPatient, launchDetails);
        assertXmlEquals(exceptedXml, actualXml);
    }

    @Test
    public void getParticipantXmlTest() {
        String exceptedXml = TestUtils.getFileContentAsString(PATIENT_ILLNESS_HISTORY);
        String actualXml = Dstu2CdaHistoryOfPresentIllnessGenerator.generateHistoryOfPresentIllnessSection(
                dstu2FhirDataForPatient, launchDetails);
        assertXmlEquals(exceptedXml, actualXml);
    }
}
