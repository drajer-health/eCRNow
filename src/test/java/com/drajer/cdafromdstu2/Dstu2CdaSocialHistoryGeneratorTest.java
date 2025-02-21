package com.drajer.cdafromdstu2;

import ca.uhn.fhir.model.api.IDatatype;
import ca.uhn.fhir.model.dstu2.composite.CodeableConceptDt;
import ca.uhn.fhir.model.dstu2.resource.Condition;
import ca.uhn.fhir.model.dstu2.resource.Observation;
import ca.uhn.fhir.model.primitive.CodeDt;
import ca.uhn.fhir.model.primitive.StringDt;
import com.drajer.test.util.TestUtils;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class Dstu2CdaSocialHistoryGeneratorTest extends BaseGenerator {
    private static String SOCIALHISTORYSECTION = "CdaDstuTestData/Cda/SocialHistory/SocialHistorySection.xml";
    private static String EMPTY_SOCIAL_HISTORY_SECTION = "CdaDstuTestData/Cda/SocialHistory/EmptySocialHistorySection.xml";
    private static String OBSERVATION_RES_FILE = "CdaDstuTestData/Observation/ObservationBundle_1.json";

    @Test
    public void generateSocialHistorySectionTest() {
        Observation pregOb = new Observation();
        Observation travelHistoryForObservation = new Observation();
        Condition pregCondition = new Condition();
        Observation occHistoryObservation = new Observation();
        List<Observation> pregObs = new ArrayList<>();
        pregObs.add(pregOb);
        List<Observation> travelHistory = new ArrayList<>();
        travelHistory.add(travelHistoryForObservation);
        List<Condition> pregCond = new ArrayList<>();
        pregCond.add(pregCondition);
        List<Observation> occHistory = new ArrayList<>();
        occHistory.add(occHistoryObservation);
        dstu2FhirDataForPatient.setPregnancyObs(pregObs);
        dstu2FhirDataForPatient.setTravelObs(travelHistory);
        dstu2FhirDataForPatient.setTravelObs(travelHistory);
        dstu2FhirDataForPatient.setPregnancyConditions(pregCond);
        String exceptedXml = TestUtils.getFileContentAsString(SOCIALHISTORYSECTION);
        dstu2FhirDataForPatient.getPatient().addUndeclaredExtension(true, "dummy");
        String actualXml = Dstu2CdaSocialHistoryGenerator.generateSocialHistorySection(dstu2FhirDataForPatient, launchDetails);
        assertXmlEquals(exceptedXml, actualXml);
    }

    @Test
    public void generateBirthSexEntryTest() {
        String actualXml = Dstu2CdaSocialHistoryGenerator.generateBirthSexEntry(dstu2FhirDataForPatient, launchDetails, new CodeDt("M"));
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void generateTravelHistoryEntryTest() {
        ca.uhn.fhir.model.dstu2.resource.Observation observation = new ca.uhn.fhir.model.dstu2.resource.Observation();
        IDatatype iDatatype = new StringDt("2/2/25");
        observation.setEffective(iDatatype);
        String actualXml = Dstu2CdaSocialHistoryGenerator.generateTravelHistoryEntry(observation, "Travel History");
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void generateOccHistoryEntryTest() {
        ca.uhn.fhir.model.dstu2.resource.Observation observation = new ca.uhn.fhir.model.dstu2.resource.Observation();
        IDatatype iDatatype = new StringDt("2/2/25");
        observation.setEffective(iDatatype);
        String actualXml = Dstu2CdaSocialHistoryGenerator.generateOccHistoryEntry(observation);
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void generatePregnancyEntryTest() {
        ca.uhn.fhir.model.dstu2.resource.Observation observation = new ca.uhn.fhir.model.dstu2.resource.Observation();
        IDatatype iDatatype = new StringDt("2/2/25");
        observation.setEffective(iDatatype);
        Condition condition = new Condition();
        CodeableConceptDt codeableConceptDt = new CodeableConceptDt();
        condition.setCode(codeableConceptDt);
        String actualXml = Dstu2CdaSocialHistoryGenerator.generatePregnancyEntry(condition);
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void generateSocialHistorySectionHeader() {
        String actualXml = Dstu2CdaSocialHistoryGenerator.generateSocialHistorySectionHeader("Header");
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void generateSocialHistorySectionEndHeader() {
        String actualXml = Dstu2CdaSocialHistoryGenerator.generateSocialHistorySectionEndHeader();
        Assert.assertNotNull(actualXml);
    }

    @Test
    public void generateEmptySocialHistorySectionTes() {
        String exceptedXml = TestUtils.getFileContentAsString(EMPTY_SOCIAL_HISTORY_SECTION);
        String actualXml = Dstu2CdaSocialHistoryGenerator.generateEmptySocialHistorySection();
        assertXmlEquals(exceptedXml, actualXml);
    }
}
