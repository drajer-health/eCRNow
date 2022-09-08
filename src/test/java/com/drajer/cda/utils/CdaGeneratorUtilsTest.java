package com.drajer.cda.utils;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.TimeZone;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaGeneratorUtilsTest {

  public static final Logger logger = LoggerFactory.getLogger(CdaGeneratorUtilsTest.class);

  @Before
  public void setUp() {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
  }

  @Test
  public void testGetCurrentDateTime() {

    String formattedTime = CdaGeneratorUtils.getCurrentDateTime();
    logger.info(" Formatted Time {}", formattedTime);
    assertTrue(formattedTime.contains("+0000"));
    assertEquals(19, formattedTime.length());
  }

  @Test
  public void testGetStringForDateTime() {

    LocalDate anotherSummerDay = LocalDate.of(2016, 8, 23);
    LocalTime anotherTime = LocalTime.of(13, 12, 45);
    ZonedDateTime zonedDateTime =
        ZonedDateTime.of(anotherSummerDay, anotherTime, ZoneId.of("America/New_York"));

    Date d = Date.from(zonedDateTime.toInstant());

    String formattedTime =
        CdaGeneratorUtils.getStringForDateTime(d, TimeZone.getTimeZone(zonedDateTime.getZone()));
    logger.info(" Formatted Time {}", formattedTime);
    assertTrue(formattedTime.contains("20160823131245"));
    assertTrue(formattedTime.contains("-0400") || formattedTime.contains("-0500"));

    String nullZoneDate =
        CdaGeneratorUtils.getStringForDateTime(
            Date.from(anotherSummerDay.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant()),
            null);

    assertEquals(8, nullZoneDate.length());
    assertTrue(nullZoneDate.equalsIgnoreCase("20160823"));

    SimpleDateFormat formatter = new SimpleDateFormat("yyyy");
    SimpleDateFormat formatter1 = new SimpleDateFormat("yyyyMM");

    String dateInString = "2013";
    try {
      Date d2 = formatter.parse(dateInString);
      String d2String = CdaGeneratorUtils.getStringForDateTime(d2, null);

      assertEquals(8, d2String.length());
      assertTrue(d2String.equalsIgnoreCase("20130101"));

      dateInString = "201904";
      Date d3 = formatter1.parse(dateInString);
      String d3String = CdaGeneratorUtils.getStringForDateTime(d3, null);

      assertEquals(8, d3String.length());
      assertTrue(d3String.equalsIgnoreCase("20190401"));

    } catch (ParseException e) {
      logger.error("Error executing test testGetStringForDateTime", e);
    }
  }

  @Test
  public void getXmlForTableBodyContentTest() {
    String expectedResult = "<content ID=\"testEl\">Display &amp; Display</content>\n";
    String result = CdaGeneratorUtils.getXmlForTableBodyContent("testEl", "Display & Display");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueEdTest() {
    String expectedResult = "<value xsi:type=\"ED\">TestValue &amp; TestValue</value>\n";
    String result = CdaGeneratorUtils.getXmlForValueEd("TestValue & TestValue");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueStringTest() {
    String expectedResult = "<value xsi:type=\"ST\">TestString &gt; &lt; TestString</value>\n";
    String result = CdaGeneratorUtils.getXmlForValueString("TestString > < TestString");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getNFXmlForValueStringTest() {
    String expectedResult =
        "<value xsi:type=\"ST\" nullFlavor=\"TestString &quot; TestString\"/>\n";
    String result = CdaGeneratorUtils.getNFXmlForValueString("TestString \" TestString");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForQuantityWithUnitsTest_valueFlagTrue() {
    String expectedResult = "<value xsi:type=\"PQ\" value=\"TestValue\" unit=\"TestUnit\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForQuantityWithUnits("TestEL", "TestValue", "TestUnit", true);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForQuantityWithUnitsTest_valueFlagFalse() {
    String expectedResult = "<TestEL value=\"TestValue\" unit=\"TestUnit\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForQuantityWithUnits("TestEL", "TestValue", "TestUnit", false);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForQuantity_valueFlagFalse() {
    String expectedResult = "<TestEL value=\"TestValue\"/>\n";
    String result = CdaGeneratorUtils.getXmlForQuantity("TestEL", "TestValue", "TestUnit", false);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForQuantity_valueFlagTrue() {
    String expectedResult = "<value xsi:type=\"PQ\" value=\"TestValue\" unit=\"TestUnit\"/>\n";
    String result = CdaGeneratorUtils.getXmlForQuantity("TestEL", "TestValue", "TestUnit", true);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getStringForDate_nullDate() {
    String expectedResult = "Unknown";
    String result = CdaGeneratorUtils.getStringForDateTime(null, null);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForStartElementTest() {
    String expectedResult = "<ElementName>\n";
    String result = CdaGeneratorUtils.getXmlForStartElement("ElementName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForStartElementWithClassCodeTest() {
    String expectedResult = "<ElementName classCode=\"ClassName\">\n";
    String result =
        CdaGeneratorUtils.getXmlForStartElementWithClassCode("ElementName", "ClassName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForStartElementWithTypeCodeTest() {
    String expectedResult = "<ElementName typeCode=\"TypeName\">\n";
    String result = CdaGeneratorUtils.getXmlForStartElementWithTypeCode("ElementName", "TypeName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNFSectionTest() {
    String expectedResult = "<ElementName nullFlavor=\"Null Flavor Val\">\n";
    String result = CdaGeneratorUtils.getXmlForNFSection("ElementName", "Null Flavor Val");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEndElementTest() {
    String expectedResult = "</ElementName>\n";
    String result = CdaGeneratorUtils.getXmlForEndElement("ElementName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlHeaderForClinicalDocumentTest() {
    String expectedResult =
        "<?xml version=\"1.0\"?>\n"
            + "<ClinicalDocument xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
            + " xmlns=\"urn:hl7-org:v3\"\n"
            + " xmlns:cda=\"urn:hl7-org:v3\"\n"
            + " xmlns:sdtc=\"urn:hl7-org:sdtc\">\n"
            + "<realmCode code=\"US\"/>\n"
            + "<typeId root=\"2.16.840.1.113883.1.3\" extension=\"POCD_HD000040\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\" extension=\"2015-08-01\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.15.2\" extension=\"2016-12-01\"/>\n";
    String result = CdaGeneratorUtils.getXmlHeaderForClinicalDocument();
    assertEquals(expectedResult, result);
  }

  @Test
  public void getEndXMLHeaderForCdaDocumentTest() {
    String expectedResult = "</ClinicalDocument>";
    String result = CdaGeneratorUtils.getEndXMLHeaderForCdaDocument();
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForTemplateIdTest() {
    String expectedResult = "<templateId root=\"TemplId\"/>\n";
    String result = CdaGeneratorUtils.getXmlForTemplateId("TemplId");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForTemplateIdTest_withExt() {
    String expectedResult = "<templateId root=\"TemplId\" extension=\"Extension\"/>\n";
    String result = CdaGeneratorUtils.getXmlForTemplateId("TemplId", "Extension");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getNFXMLForIITest() {
    String expectedResult = "<id nullFlavor=\"NullFlavourText\"/>\n";
    String result = CdaGeneratorUtils.getNFXMLForII("NullFlavourText");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getNFXMLForElementTest() {
    String expectedResult = "<ElementName nullFlavor=\"NullFlavourText\"/>\n";
    String result = CdaGeneratorUtils.getNFXMLForElement("ElementName", "NullFlavourText");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDTest_NoDisplayName() {
    String expectedResult =
        "<CodeName code=\"Code\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForCD("CodeName", "Code", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNullValueCDTest() {
    String expectedResult = "<CodeName xsi:type=\"CD\" nullFlavor=\"Code\"/>\n";
    String result = CdaGeneratorUtils.getXmlForNullValueCD("CodeName", "Code");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNullValueCDWithoutEndTagTest() {
    String expectedResult = "<CodeName xsi:type=\"CD\" nullFlavor=\"Code\">";
    String result = CdaGeneratorUtils.getXmlForNullValueCDWithoutEndTag("CodeName", "Code");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getCDADocHeaderTemplateXMLTest() {
    String expectedResult =
        "<typeId root=\"2.16.840.1.113883.1.3\" extension=\"POCD_HD000040\"/>\n";
    String result = CdaGeneratorUtils.getCDADocHeaderTemplateXML();
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEntryTemplateTest() {
    String expectedResult = "<TemplateName typeCode=\"TypeCodeName\">\n";
    String result = CdaGeneratorUtils.getXmlForEntryTemplate("TemplateName", "TypeCodeName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForPartialValueIVLWithTSTest_low() {
    String expectedResult =
        "<elName xsi:type=\"IVL_TS\"><low value=\"valueText\"/>\n" + "</elName>\n";
    String result = CdaGeneratorUtils.getXmlForPartialValueIVLWithTS("elName", "valueText", "low");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForPartialValueIVLWithTSTest_high() {
    String expectedResult =
        "<elName xsi:type=\"IVL_TS\"><high value=\"valueText\"/>\n" + "</elName>\n";
    String result = CdaGeneratorUtils.getXmlForPartialValueIVLWithTS("elName", "valueText", "high");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForPIVLWithTSTest() {
    String expectedResult =
        "<elName xsi:type=\"PIVL_TS\" institutionSpecified=\"true\" operator=\"A\">\n"
            + "<period value=\"0\" unit=\"h\"/>\n"
            + "</elName>\n";
    String result = CdaGeneratorUtils.getXmlForPIVLWithTS("elName", 1000);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSet_EmptyDisplay_EmptyValue() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSet(
            "codeName", "CodeSystem", "CodeSystemName", "", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSet_EmptyValue() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSet(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSet() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName &quot; DisplayName\" sdtc:valueSet=\"ValueName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSet(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName \" DisplayName", "ValueName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetTest_EmptyDisplay_EmptyValue() {
    String expectedResult =
        "<elName  code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSet(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetTest_EmptyValue() {
    String expectedResult =
        "<elName  code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName &amp; DisplayName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSet(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "DisplayName & DisplayName", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetTest() {
    String expectedResult =
        "<elName  code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSet(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "DisplayName", "ValueName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWihoutEndTagTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCOTest() {
    String expectedResult =
        "<value xsi:type=\"CO\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCO(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWihoutEndTagEmptyDisplayTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWihoutEndTagDisplayTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWihoutEndTagElseTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    expectedResult += "\n" + "</elName>" + "\n";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "DisplayName",
            "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWithDisplayTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    expectedResult += "\n" + "</elName>" + "\n";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionElseTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    expectedResult += "\n" + "</elName>" + "\n";

    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNullValuePQTest() {
    String expectedResult = "<value xsi:type=\"PQ\" nullFlavor=\"NF\"/>\n";
    String result = CdaGeneratorUtils.getXmlForNullValuePQ("NF");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueINTTest() {
    String expectedResult = "<value xsi:type=\"INT\" value=\"1357\"/>\n";
    String result = CdaGeneratorUtils.getXmlForValueINT("1357");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getNFXMLForValueTest() {
    String expectedResult = "<value xsi:type=\"CD\" nullFlavor=\"NF\"/>\n";
    String result = CdaGeneratorUtils.getNFXMLForValue("NF");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getNFXMLForValueWithTextTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" nullFlavor=\"NF\"><originalText>COVID-19</originalText>\n"
            + "</value>"
            + "\n";
    String result = CdaGeneratorUtils.getNFXMLForValueWithText("NF", "COVID-19");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDTranslationTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" nullFlavor=\"OTH\"><translation code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n"
            + "</value>"
            + "\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDTranslation(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForPerformerTest() {
    String expectedResult = "<performer typeCode=\"PERFTYPE\">\n";
    String result = CdaGeneratorUtils.getXmlForPerformer("PERFTYPE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForParticipantTest() {
    String expectedResult = "<participant typeCode=\"NURSE\">\n";
    String result = CdaGeneratorUtils.getXmlForParticipant("NURSE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForParticipantRoleTest() {
    String expectedResult = "<participantRole classCode=\"NURSE\">\n";
    String result = CdaGeneratorUtils.getXmlForParticipantRole("NURSE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForReferenceTest() {
    String expectedResult = "<reference typeCode=\"TYPECODE\">\n";
    String result = CdaGeneratorUtils.getXmlForReference("TYPECODE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getRootOidTest() {
    String expectedResult = "1357";
    String result = CdaGeneratorUtils.getRootOid("COVID", "1357");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForIIWithElNameTest() {
    String expectedResult = "<elName root=\"ROOT\"/>\n";
    String result = CdaGeneratorUtils.getXmlForIIWithElName("elName", "ROOT");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForSDTCElementTest() {
    String expectedResult = "<elName value=\"value\"/>\n";
    String result = CdaGeneratorUtils.getXmlForSDTCElement("elName", "value");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNfQuantityTest() {
    String expectedResult = "<elName nullFlavor=\"NF\"/>\n";
    String result = CdaGeneratorUtils.getXmlForNfQuantity("elName", "NF");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNullEffectiveTimeTest() {
    String expectedResult = "<elName nullFlavor=\"2021-02-10\"/>\n";
    String result = CdaGeneratorUtils.getXmlForNullEffectiveTime("elName", "2021-02-10");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForTelecomTest() {
    String expectedResult = "<MOBILE value=\"tel:(202)102-1012\" use=\"WORK\"/>\n";
    String result = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "2021021012", "WORK");
    assertEquals(expectedResult, result);

    String trimmedResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "+1(202)1021012", "WORK");
    assertEquals(expectedResult, trimmedResult);
    
    String expectedResult2 = "<MOBILE value=\"tel:(555)555-5006\" use=\"WORK\"/>\n";
    String trimmedResult2 = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "555-555-5006", "WORK");
    assertEquals(expectedResult2, trimmedResult2);

    String noUseAttrResult = "<MOBILE value=\"tel:(202)102-1012\"/>\n";
    String noAttrResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "2021021012", "");
    assertEquals(noUseAttrResult, noAttrResult);

    String nullExpectedResult = "<MOBILE nullFlavor=\"NI\"/>\n";
    String nullResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "1021012", "WORK");
    assertEquals(nullExpectedResult, nullResult);

    String nullResult2 = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "", "WORK");
    assertEquals(nullExpectedResult, nullResult2);
  }

  @Test
  public void getXmlForEmailTest() {
    String expectedResult = "<MOBILE value=\"mailto:20210210123\"/>\n";
    String result = CdaGeneratorUtils.getXmlForEmail("MOBILE", "20210210123", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEntryRelationshipTest() {
    String expectedResult = "<entryRelationship typeCode=\"TYPECODE\" inversionInd=\"IND\">\n";
    String result = CdaGeneratorUtils.getXmlForEntryRelationship("TYPECODE", "IND");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForActWithNegationIndTest() {
    String expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\" negationInd=\"true\" >\n";
    String result =
        CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "T", true);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForActWithNoNegationIndTest() {
    String expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\">\n";
    String result =
        CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "", false);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForIVLWithTSTest() {
    String expectedResult =
        "<EL>\n" + "<low value=\"LO\"/>\n" + "<high value=\"HIGH\"/>\n" + "</EL>\n";
    String result = CdaGeneratorUtils.getXmlForIVLWithTS("EL", "LO", "HIGH");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForIVLWithNullTSTest() {
    Pair<Date, TimeZone> date = null;
    String expectedResult = "<EL nullFlavor=\"NI\"/>\n";
    String result = CdaGeneratorUtils.getXmlForIVLWithTS("EL", date, date, false);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueIVLWithTSTest() {
    String expectedResult =
        "<EL xsi:type=\"IVL_TS\"><low value=\"LO\"/>\n" + "<high value=\"HIGH\"/>\n" + "</EL>\n";
    String result = CdaGeneratorUtils.getXmlForValueIVLWithTS("EL", "LO", "HIGH");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForLowIVLWithTSWithNFHighTest() {
    String expectedResult =
        "<EL xsi:type=\"IVL_TS\">\n"
            + "<low value=\"VALUE\"/>\n"
            + "<high nullFlavor=\"UNK\"/>\n"
            + "</EL>\n";
    String result = CdaGeneratorUtils.getXmlForLowIVLWithTSWithNFHigh("EL", "VALUE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCD("codeName", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithDisplayTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
            "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithNoDisplayTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
            "codeName", "CodeSystem", "CodeSystemName", "", "", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWihoutEndTagTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWihoutEndTagWithDisplayTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWihoutEndTagWithoutDisplayTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "");
    assertEquals(expectedResult, result);
  }
}
