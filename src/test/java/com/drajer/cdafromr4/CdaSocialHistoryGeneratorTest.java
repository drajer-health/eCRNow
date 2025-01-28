package com.drajer.cdafromr4;

import static org.junit.Assert.assertNotNull;

import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.R4FhirData;
import com.drajer.test.util.TestUtils;
import java.util.List;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.DateTimeType;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Period;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({CdaGeneratorUtils.class})
@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaSocialHistoryGeneratorTest extends BaseGeneratorTest {

  private static final String SOCIAL_HISTORY_CDA_FILE =
      "CdaTestData/Cda/SocialHistory/SocialHistory.xml";

  private static final String TREAVEL_OBS_FILE1 = "CdaTestData/Observation/travelHistory.json";
  private static final String PREGNANCY_CONDITION_FILE =
      "CdaTestData/Condition/CondPregnancy_SCT_77386006.json";
  private static final String TRAVEL_OBS_FILE = "R4/Observation/ObsTravel_SCT.json";
  private static final String PREGNANCY_OBS_FILE =
      "CdaTestData/Observation/ObservationPregnancy.json";
  private static final String OCCUPATION_OBS_FILE =
      "CdaTestData/Observation/OccupationHistoryObs.json";

  @Test
  public void shouldGenerateSocialHistorySection() {
    R4FhirData r4FhirData = new R4FhirData();
    List<Condition> conditions = getPregnancyConditions(PREGNANCY_CONDITION_FILE);
    r4FhirData.setPregnancyConditions(conditions);
    r4FhirData.setPatient(getPatientData());
    r4FhirData.setTravelObs(getObs(TRAVEL_OBS_FILE));
    r4FhirData.setPregnancyObs(getObs(PREGNANCY_OBS_FILE));
    r4FhirData.setOccupationObs(getObs(OCCUPATION_OBS_FILE));

    String expectedXml = TestUtils.getFileContentAsString(SOCIAL_HISTORY_CDA_FILE);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    String actualXml =
        CdaSocialHistoryGenerator.generateSocialHistorySection(
            r4FhirData, launchDetails, "CDA_R11");

    assertNotNull(actualXml);

    assertXmlEquals(expectedXml, actualXml);
  }

  @Test
  public void testGenerateBirthSexEntry() {
    CodeType birthSex = new CodeType("M");

    String actual = CdaSocialHistoryGenerator.generateBirthSexEntry(birthSex);

    assertNotNull(actual);
  }

  @Test
  public void testGenerateTravelHistoryEntry() {

    String expectedXml =
        "<entry>\r\n"
            + "<act classCode=\"ACT\" moodCode=\"EVN\">\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.15.2.3.1\" extension=\"2016-12-01\"/>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"null\"/>\r\n"
            + "<code code=\"420008001\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Travel\"/>\r\n"
            + "<text>Travel history</text>\r\n"
            + "<statusCode code=\"completed\"/>\r\n"
            + "<effectiveTime>\r\n"
            + "<low value=\"20220403153000-0400\"/>\r\n"
            + "<high value=\"20230403153000-0400\"/>\r\n"
            + "</effectiveTime>\r\n"
            + "</act>\r\n"
            + "</entry>\r\n"
            + "";

    Observation observation = new Observation();
    Period period = new Period();
    period.setStartElement(new DateTimeType("2022-04-03T15:30:00-04:00"));
    period.setEndElement(new DateTimeType("2023-04-03T15:30:00-04:00"));

    observation.setEffective(period);

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);
    String actualXml =
        CdaSocialHistoryGenerator.generateTravelHistoryEntry(
            observation, "Travel history", launchDetails);
    assertXmlEquals(expectedXml.toString(), actualXml);
  }

  @Test
  public void testGenerateTravelHistoryEntry1() {

    String expectedXml =
        "<entry>\r\n"
            + "<act classCode=\"ACT\" moodCode=\"EVN\">\r\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.15.2.3.1\" extension=\"2016-12-01\"/>\r\n"
            + "<id root=\"2.16.840.1.113883.1.1.1.1\" extension=\"null\"/>\r\n"
            + "<code code=\"420008001\" codeSystem=\"2.16.840.1.113883.6.96\" codeSystemName=\"SNOMED-CT\" displayName=\"Travel\"/>\r\n"
            + "<text>Travel history</text>\r\n"
            + "<statusCode code=\"completed\"/>\r\n"
            + "<effectiveTime value=\"20230403153000-0400\"/>\r\n"
            + "</act>\r\n"
            + "</entry>\r\n"
            + "";

    Observation observation = new Observation();
    DateTimeType dateTimeType = new DateTimeType("2023-04-03T15:30:00-04:00");

    PowerMockito.mockStatic(CdaGeneratorUtils.class, Mockito.CALLS_REAL_METHODS);
    PowerMockito.when(CdaGeneratorUtils.getXmlForIIUsingGuid()).thenReturn(XML_FOR_II_USING_GUID);

    observation.setEffective(dateTimeType);

    String actualXml =
        CdaSocialHistoryGenerator.generateTravelHistoryEntry(
            observation, "Travel history", launchDetails);
    assertXmlEquals(expectedXml.toString(), actualXml);
  }

  @Test
  public void testGenerateEmptySocialHistorySection() {
    StringBuilder expectedXml = new StringBuilder();
    expectedXml.append("<component>").append(System.lineSeparator());
    expectedXml.append("<section nullFlavor=\"NI\">").append(System.lineSeparator());
    expectedXml
        .append("<templateId root=\"2.16.840.1.113883.10.20.22.2.17\"/>")
        .append(System.lineSeparator());
    expectedXml
        .append("<templateId root=\"2.16.840.1.113883.10.20.22.2.17\" extension=\"2015-08-01\"/>")
        .append(System.lineSeparator());
    expectedXml
        .append(
            "<code code=\"29762-2\" codeSystem=\"2.16.840.1.113883.6.1\" codeSystemName=\"LOINC\" displayName=\"Social History\"/>")
        .append(System.lineSeparator());
    expectedXml.append("<title>SOCIAL HISTORY</title>").append(System.lineSeparator());
    expectedXml.append("<text>No Social History Information</text>").append(System.lineSeparator());
    expectedXml.append(System.lineSeparator());
    expectedXml.append("</section>").append(System.lineSeparator());
    expectedXml.append("</component>").append(System.lineSeparator());

    String actualXml = CdaSocialHistoryGenerator.generateEmptySocialHistorySection();
    assertNotNull(actualXml);

    assertXmlEquals(expectedXml.toString(), actualXml);
  }

  @Test
  public void testGenerateParticipantWithAddressExtension() {

    R4FhirData r4FhirData = new R4FhirData();

    r4FhirData.setTravelObs(getObs(TREAVEL_OBS_FILE1));

    String participantXml =
        CdaSocialHistoryGenerator.generateParticipant(r4FhirData.getTravelObs().get(0));
    assertNotNull(participantXml);
  }
}
