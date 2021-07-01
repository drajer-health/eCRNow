package com.drajer.cda.utils;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
    assertTrue(formattedTime.length() == 19);
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
    assertTrue(nullZoneDate.length() == 8);
    assertTrue(nullZoneDate.equalsIgnoreCase("20160823"));

    SimpleDateFormat formatter = new SimpleDateFormat("yyyy");
    SimpleDateFormat formatter1 = new SimpleDateFormat("yyyyMM");

    String dateInString = "2013";
    try {
      Date d2 = formatter.parse(dateInString);
      String d2String = CdaGeneratorUtils.getStringForDateTime(d2, null);
      assertTrue(d2String.length() == 8);
      assertTrue(d2String.equalsIgnoreCase("20130101"));

      dateInString = "201904";
      Date d3 = formatter1.parse(dateInString);
      String d3String = CdaGeneratorUtils.getStringForDateTime(d3, null);
      assertTrue(d3String.length() == 8);
      assertTrue(d3String.equalsIgnoreCase("20190401"));

    } catch (ParseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  @Test
  public void getXmlForTableBodyContentTest() {
    String expectedResult = "<content ID=\"testEl\">Display &amp; Display</content>\n";
    String result = CdaGeneratorUtils.getXmlForTableBodyContent("testEl", "Display & Display");
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
  public void getXmlForQuantityWithUnitsTest() {
    String expectedResult = "<TestEL value=\"TestValue\" unit=\"TestUnit\"/>\n";
    String result = CdaGeneratorUtils.getXmlForQuantityWithUnits("TestEL", "TestValue", "TestUnit");
    assertEquals(expectedResult, result);

    expectedResult = "<TestEL value=\"TestValue\"/>\n";
    result = CdaGeneratorUtils.getXmlForQuantityWithUnits("TestEL", "TestValue", "");
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
  public void getXmlForIITest() {
    String expectedResult = "<id root=\"TemplId\"/>\n";
    String result = CdaGeneratorUtils.getXmlForII("TemplId");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForIIWithExtTest() {
    String expectedResult = "<id root=\"TemplId\" extension=\"EXT\"/>\n";
    String result = CdaGeneratorUtils.getXmlForII("TemplId", "EXT");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForIIUsingGuidTest() {
    String result = CdaGeneratorUtils.getXmlForIIUsingGuid();
    assertNotNull(result);
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
  public void getXmlForCD_NoDisplayTest() {
    String expectedResult = "<CodeName code=\"Code\" codeSystem=\"CodeSystem\"/>\n";
    String result = CdaGeneratorUtils.getXmlForCD("CodeName", "Code", "CodeSystem");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueTest() {
    String expectedResult = "<ElementName value=\"11\"/>\n";
    String result = CdaGeneratorUtils.getXmlForValue("ElementName", "11");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNullCDTest() {
    String expectedResult = "<CodeName nullFlavor=\"Code\"/>\n";
    String result = CdaGeneratorUtils.getXmlForNullCD("CodeName", "Code");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDTest() {
    String expectedResult =
        "<CodeName code=\"Code\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForCD("CodeName", "Code", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, result);

    expectedResult =
        "<CodeName code=\"Code\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    result =
        CdaGeneratorUtils.getXmlForCD(
            "CodeName", "Code", "CodeSystem", "CodeSystemName", "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForCDWithoutEndTagTest() {
    String expectedResult =
        "<CodeName code=\"Code\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    String result =
        CdaGeneratorUtils.getXmlForCDWithoutEndTag(
            "CodeName", "Code", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, result);

    expectedResult =
        "<CodeName code=\"Code\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    result =
        CdaGeneratorUtils.getXmlForCDWithoutEndTag(
            "CodeName", "Code", "CodeSystem", "CodeSystemName", "DisplayName");
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
  public void getXmlForNullCDWithoutEndTagTest() {
    String expectedResult = "<CodeName nullFlavor=\"Code\">";
    String result = CdaGeneratorUtils.getXmlForNullCDWithoutEndTag("CodeName", "Code");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForPIVLWithTSTest() {
    String expectedResult =
        "<elName xsi:type=\"PIVL_TS\" institutionSpecified=\"true\" operator=\"A\">\n"
            + "<period value=\"1000\" unit=\"h\"/>\n"
            + "</elName>\n";
    String result = CdaGeneratorUtils.getXmlForPIVLWithTS("elName", "1000");
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

    expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">\n"
            + "<originalText>\n"
            + "<reference value=\"#ContentRef\"/>\n"
            + "</originalText>\n"
            + "</elName>\n";
    result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "DisplayName",
            "ContentRef");
    assertEquals(expectedResult, result);

    expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">\n"
            + "</elName>\n";
    result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "",
            "");
    assertEquals(expectedResult, result);

    expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">\n"
            + "<originalText>\n"
            + "<reference value=\"#ContentRef\"/>\n"
            + "</originalText>\n"
            + "</elName>\n";

    result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "",
            "DisplayName",
            "ContentRef");
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
  public void getNFXMLForValueTest() {
    String expectedResult = "<value xsi:type=\"CD\" nullFlavor=\"NF\"/>\n";
    String result = CdaGeneratorUtils.getNFXMLForValue("NF");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getRootOidTest() {
    String expectedResult = "1357";
    String result = CdaGeneratorUtils.getRootOid("COVID", "1357");
    assertEquals(expectedResult, result);

    expectedResult = "12345";
    result = CdaGeneratorUtils.getRootOid(null, "12345");
    assertEquals(expectedResult, result);

    expectedResult = "12345";
    result = CdaGeneratorUtils.getRootOid("urn:oid:12345", "12345");
    assertEquals(expectedResult, result);

    expectedResult = "2.16.840.1.113883.11.11555";
    result = CdaGeneratorUtils.getRootOid("http://terminology.hl7.org/RoleClass", "1357");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForIIWithElNameTest() {
    String expectedResult = "<elName root=\"ROOT\" extension=\"EXT\"/>\n";
    String result = CdaGeneratorUtils.getXmlForIIWithElName("elName", "ROOT", "EXT");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForTextTest() {
    String expectedResult = "<elName>Text</elName>\n";
    String result = CdaGeneratorUtils.getXmlForText("elName", "Text");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForNFTextTest() {
    String expectedResult = "<elName nullFlavor=\"Text\"/>\n";
    String result = CdaGeneratorUtils.getXmlForNFText("elName", "Text");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEffectiveTimeTest() {
    String expectedResult = "<elName value=\"VALUE\"/>\n";
    String result = CdaGeneratorUtils.getXmlForEffectiveTime("elName", "VALUE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEffectiveTimeWithTimeZoneTest() {

    LocalDate date = LocalDate.of(2021, 6, 30);
    LocalTime time = LocalTime.of(6, 6, 6);
    ZonedDateTime zonedDateTime = ZonedDateTime.of(date, time, ZoneId.of("America/New_York"));

    Date d = Date.from(zonedDateTime.toInstant());
    String expectedResult = "<elName value=\"20210630060606-0400\"/>\n";

    String result =
        CdaGeneratorUtils.getXmlForEffectiveTime(
            "elName", d, TimeZone.getTimeZone(zonedDateTime.getZone()));
    assertEquals(expectedResult, result);
    expectedResult = "<elName nullFlavor=\"NI\"/>\n";
    result =
        CdaGeneratorUtils.getXmlForEffectiveTime(
            "elName", null, TimeZone.getTimeZone(zonedDateTime.getZone()));
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
    String expectedResult = "<MOBILE value=\"tel:(202)102-1012\"/>\n";
    String result = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "20210210123", "v");
    assertEquals(expectedResult, result);

    expectedResult = "<MOBILE value=\"tel:(202)102-1012\" use=\"WORK\"/>\n";
    result = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "2021021012", "WORK");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEmailTest() {
    String expectedResult = "<MOBILE value=\"mailto:2021021012\" use=\"WORK\"/>\n";
    String result = CdaGeneratorUtils.getXmlForEmail("MOBILE", "2021021012", "WORK");
    assertEquals(expectedResult, result);

    expectedResult = "<MOBILE value=\"mailto:2021021012\"/>\n";
    result = CdaGeneratorUtils.getXmlForEmail("MOBILE", "2021021012", "");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEntryTest() {
    String expectedResult = "<entry typeCode=\"TYPECODE\">\n";
    String result = CdaGeneratorUtils.getXmlForActEntry("TYPECODE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForEntryRelationshipTest() {
    String expectedResult = "<entryRelationship typeCode=\"TYPECODE\">\n";
    String result = CdaGeneratorUtils.getXmlForEntryRelationship("TYPECODE");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForActWithNegationIndTest() {
    String expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\" negationInd=\"true\" >\n";
    String result =
        CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "T", true);
    assertEquals(expectedResult, result);

    result = CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "true", true);
    assertEquals(expectedResult, result);

    expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\" negationInd=\"false\" >\n";
    result = CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "false", true);
    assertEquals(expectedResult, result);

    result = CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", null, true);
    assertEquals(expectedResult, result);

    expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\">\n";
    result = CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "false", false);
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
  public void getXmlForIVLWithNullTSTest() {
    LocalDate date1 = LocalDate.of(2021, 6, 30);
    LocalTime time = LocalTime.of(6, 6, 6);
    ZonedDateTime zonedDateTime = ZonedDateTime.of(date1, time, ZoneId.of("America/New_York"));

    Date d = Date.from(zonedDateTime.toInstant());
    Pair<Date, TimeZone> lowHigh =
        new Pair<Date, TimeZone>(d, TimeZone.getTimeZone(zonedDateTime.getZone()));
    String expectedResult =
        "<EL>\n"
            + "<low value=\"20210630060606-0400\"/>\n"
            + "<high value=\"20210630060606-0400\"/>\n"
            + "</EL>\n";
    String result = CdaGeneratorUtils.getXmlForIVLWithTS("EL", lowHigh, lowHigh, false);
    assertEquals(expectedResult, result);

    expectedResult = "<EL>\n" + "<low value=\"20210630060606-0400\"/>\n" + "</EL>\n";
    result = CdaGeneratorUtils.getXmlForIVLWithTS("EL", lowHigh, null, false);
    assertEquals(expectedResult, result);

    expectedResult =
        "<EL>\n"
            + "<low nullFlavor=\"NI\"/>\n"
            + "<high value=\"20210630060606-0400\"/>\n"
            + "</EL>\n";
    result = CdaGeneratorUtils.getXmlForIVLWithTS("EL", null, lowHigh, false);
    assertEquals(expectedResult, result);

    Pair<Date, TimeZone> date = null;
    expectedResult = "<EL nullFlavor=\"NI\"/>\n";
    result = CdaGeneratorUtils.getXmlForIVLWithTS("EL", date, date, false);
    assertEquals(expectedResult, result);

    expectedResult =
        "<EL>\n" + "<low nullFlavor=\"NI\"/>\n" + "<high nullFlavor=\"NI\"/>\n" + "</EL>\n";
    result = CdaGeneratorUtils.getXmlForIVLWithTS("EL", date, date, true);
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCD("codeName", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, result);

    expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    result =
        CdaGeneratorUtils.getXmlForValueCD(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueCDWithoutEndTagTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
            "codeName", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, result);

    expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    result =
        CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName");
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

    expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    result =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
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
  public void getXmlForTableHeaderTest() {
    String expectedResult =
        "<table border=\"10\" width=\"12%\">\n"
            + "<thead>\n"
            + "<tr>\n"
            + "<th>HEADER1</th>\n"
            + "<th>HEADER2</th>\n"
            + "</tr>\n"
            + "</thead>\n";
    List<String> headerVals = new ArrayList<String>();
    headerVals.add("HEADER1");
    headerVals.add("HEADER2");

    String result = CdaGeneratorUtils.getXmlForTableHeader(headerVals, 10, 12);
    assertEquals(expectedResult, result);
  }

  @Test
  public void addTableRowTest() {
    String expectedResult =
        "<tr>\n"
            + "<td>\n"
            + "<content ID=\"VALUE110\">value1</content>\n"
            + "</td>\n"
            + "<td>\n"
            + "<content ID=\"VALUE210\">value2</content>\n"
            + "</td>\n"
            + "</tr>\n";
    Map<String, String> vals = new HashMap<String, String>();
    vals.put("VALUE1", "value1");
    vals.put("VALUE2", "value2");

    String result = CdaGeneratorUtils.addTableRow(vals, 10);
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

  @Test
  public void getXmlForOriginalTextWithReferenceTest() {
    String expectedResult =
        "<originalText>\n" + "<reference value=\"#REF-TEXT\"/>\n" + "</originalText>\n";
    String result = CdaGeneratorUtils.getXmlForOriginalTextWithReference("REF-TEXT");
    assertEquals(expectedResult, result);
  }
}
