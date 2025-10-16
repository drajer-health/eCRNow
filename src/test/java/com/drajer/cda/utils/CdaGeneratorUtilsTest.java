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
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RunWith(MockitoJUnitRunner.class)
public class CdaGeneratorUtilsTest {

  public static final Logger logger = LoggerFactory.getLogger(CdaGeneratorUtilsTest.class);

  @Before
  public void setUp() {
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
  }

  @Test
  public void testGetXmlForValueBoolean() {
    String expectedXml = "<value xsi:type=\"BL\" value=\"true\"/>\n";
    String actualXml = CdaGeneratorUtils.getXmlForValueBoolean("true");

    assertNotNull(actualXml);
    assertEquals(expectedXml, actualXml);
  }

  @Test
  public void testGetCurrentDateTime() {

    String formattedTime = CdaGeneratorUtils.getCurrentDateTime();
    logger.info(" Formatted Time {}", formattedTime);
    assertEquals(19, formattedTime.length());
  }

  @Test
  public void testGetCurrentDate() {

    String formattedDate = CdaGeneratorUtils.getCurrentDate();
    logger.info(" Formatted Time {}", formattedDate);
    assertEquals(8, formattedDate.length());
  }

  @Test
  public void getXmlForStartElementWithAttribute() {
    String actualXML =
        CdaGeneratorUtils.getXmlForStartElementWithAttribute(
            "ElementName", "AttributeElement", "765754");
    assertNotNull(actualXML);
  }

  @Test
  public void getXmlForStartElementWithAttributeNegationAttributeName() {
    String actualXML = CdaGeneratorUtils.getXmlForStartElementWithAttribute("ElementName", "", "");
    assertNotNull(actualXML);
  }

  @Test
  public void getXmlForIIUsingGuid() {
    String actualXML = CdaGeneratorUtils.getXmlForIIUsingGuid();
    assertNotNull(actualXML);
  }

  @Test
  public void getXmlForCDWithoutEndTag() {
    String expected =
        "<clinicalStatus code=\"active\" codeSystem=\"http://terminology.hl7.org/CodeSystem/condition-clinical\" codeSystemName=\"clinical\" displayName=\"Active\">";
    String actualXML =
        CdaGeneratorUtils.getXmlForCDWithoutEndTag(
            "clinicalStatus",
            "active",
            "http://terminology.hl7.org/CodeSystem/condition-clinical",
            "clinical",
            "Active");
    assertEquals(expected, actualXML);
  }

  @Test
  public void getXmlForCDWithoutEndTagWithoutDisplayName() {
    String expectedXML =
        "<clinicalStatus code=\"active\" codeSystem=\"http://terminology.hl7.org/CodeSystem/condition-clinical\" codeSystemName=\"clinical\">";
    String actualXML =
        CdaGeneratorUtils.getXmlForCDWithoutEndTag(
            "clinicalStatus",
            "active",
            "http://terminology.hl7.org/CodeSystem/condition-clinical",
            "clinical",
            "");
    assertEquals(expectedXML, actualXML);
  }

  @Test
  public void getXmlForCD() {
    String expectedXMLForCOD =
        "<clinicalStatus code=\"active\" codeSystem=\"http://terminology.hl7.org/CodeSystem/condition-clinical\"/>";
    if (CdaGeneratorUtils.getXmlForCD(
            "clinicalStatus", "active", "http://terminology.hl7.org/CodeSystem/condition-clinical")
        .contains(expectedXMLForCOD)) assertTrue(true);
  }

  @Test
  public void getXmlForValue() {
    String expectedXmlValue = "<ElementName value=\"3585576\"/>";
    if (CdaGeneratorUtils.getXmlForValue("ElementName", "3585576").contains(expectedXmlValue))
      assertTrue(true);
  }

  @Test
  public void getXmlForNullCD() {
    String expectedXmlForNullCD = "<clinicalStatus nullFlavor=\"active\"/>";
    String actualXmlForNullCD = CdaGeneratorUtils.getXmlForNullCD("clinicalStatus", "active");
    assertEquals(expectedXmlForNullCD.trim(), actualXmlForNullCD.trim());
  }

  @Test
  public void getXmlForNullCDWithoutEndTag() {
    String expectedXmlForNullCDWithoutEndTag = "<clinicalStatus nullFlavor=\"active\">";
    String actualXmlForNullCDWithoutEndTag =
        CdaGeneratorUtils.getXmlForNullCDWithoutEndTag("clinicalStatus", "active");
    assertEquals(expectedXmlForNullCDWithoutEndTag.trim(), actualXmlForNullCDWithoutEndTag.trim());
  }

  @Test
  public void getXmlForII() {
    String actualXmlForNullCDWithoutEndTag = CdaGeneratorUtils.getXmlForII("ROOT", "Extension");
    assertEquals(40, actualXmlForNullCDWithoutEndTag.length());
  }

  @Test
  public void getXmlForIIWithElName() {
    String actualXmlForIIWithElementName =
        CdaGeneratorUtils.getXmlForIIWithElName("Patient", "Bundle", ".xml");
    assertEquals(42, actualXmlForIIWithElementName.length());
  }

  @Test
  public void getXmlForTextWithAttribute() {
    String actualXmlForTextWithAttribute =
        CdaGeneratorUtils.getXmlForTextWithAttribute("Patient", "id", "8768768", "generated");
    assertEquals(42, actualXmlForTextWithAttribute.length());
  }

  @Test
  public void getXmlForTextWithAttributeWithNegationAttributeName() {
    String actualXmlForTextWithAttribute =
        CdaGeneratorUtils.getXmlForTextWithAttribute("Patient", "", "865985", "generated");
    assertEquals(29, actualXmlForTextWithAttribute.length());
  }

  @Test
  public void getXmlForNFText() {
    String xmlForNFText = CdaGeneratorUtils.getXmlForNFText("Patient", "NF");
    assertEquals(27, xmlForNFText.length());
  }

  @Test
  public void getHl7StringForDate() {
    String hl7StringForDate = CdaGeneratorUtils.getHl7StringForDate(new Date());
    assertNotNull(hl7StringForDate);
  }

  @Test
  public void getXmlForQuantityWithoutUnits() {
    String xmlForQuantityWithUnits =
        CdaGeneratorUtils.getXmlForQuantityWithUnits("valueQuantity", "92", "");
    assertEquals(28, xmlForQuantityWithUnits.length());
  }

  @Test
  public void getXmlForActEntry() {
    String expectedXmlForActEntry = "<entry typeCode=\"2556421249\">\n";
    String actualXmlForActEntry = CdaGeneratorUtils.getXmlForActEntry("2556421249");
    assertEquals(expectedXmlForActEntry, actualXmlForActEntry);
  }

  @Test
  public void getXmlForEntryRelationship() {
    String expectedXmlForEntryRelationship = "<entryRelationship typeCode=\"2556421249\">\n";
    String actualXmlForEntryRelationship =
        CdaGeneratorUtils.getXmlForEntryRelationship("2556421249");
    assertEquals(expectedXmlForEntryRelationship, actualXmlForEntryRelationship);
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
    String actualResult =
        CdaGeneratorUtils.getXmlForTableBodyContent("testEl", "Display & Display");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueEdTest() {
    String expectedResult = "<value xsi:type=\"ED\">TestValue &amp; TestValue</value>\n";
    String actualResult = CdaGeneratorUtils.getXmlForValueEd("TestValue & TestValue");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueStringTest() {
    String expectedResult = "<value xsi:type=\"ST\">TestString &gt; &lt; TestString</value>\n";
    String actualResult = CdaGeneratorUtils.getXmlForValueString("TestString > < TestString");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getNFXmlForValueStringTest() {
    String expectedResult =
        "<value xsi:type=\"ST\" nullFlavor=\"TestString &quot; TestString\"/>\n";
    String actualResult = CdaGeneratorUtils.getNFXmlForValueString("TestString \" TestString");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForQuantityWithUnitsTest_valueFlagTrue() {
    String expectedResult = "<value xsi:type=\"PQ\" value=\"TestValue\" unit=\"TestUnit\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForQuantityWithUnits("TestEL", "TestValue", "TestUnit", true);
    assertEquals(expectedResult, actualResult);
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
    String actualResult =
        CdaGeneratorUtils.getXmlForQuantity("TestEL", "TestValue", "TestUnit", false);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForQuantity_valueFlagTrue() {
    String expectedXml = "<value xsi:type=\"PQ\" value=\"TestValue\" unit=\"TestUnit\"/>\n";
    String actualXml = CdaGeneratorUtils.getXmlForQuantity("TestEL", "TestValue", "TestUnit", true);
    assertEquals(expectedXml, actualXml);
  }

  @Test
  public void getStringForDate_nullDate() {
    String expectedResult = "Unknown";
    String actualResult = CdaGeneratorUtils.getStringForDateTime(null, null);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForStartElementTest() {
    String expectedXml = "<ElementName>\n";
    String actualXml = CdaGeneratorUtils.getXmlForStartElement("ElementName");
    assertEquals(expectedXml, actualXml);
  }

  @Test
  public void getXmlForStartElementWithClassCodeTest() {
    String expectedXml = "<ElementName classCode=\"ClassName\">\n";
    String actualXml =
        CdaGeneratorUtils.getXmlForStartElementWithClassCode("ElementName", "ClassName");
    assertEquals(expectedXml, actualXml);
  }

  @Test
  public void getXmlForStartElementWithTypeCodeTest() {
    String expectedResult = "<ElementName typeCode=\"TypeName\">\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForStartElementWithTypeCode("ElementName", "TypeName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForNFSectionTest() {
    String expectedResult = "<ElementName nullFlavor=\"Null Flavor Val\">\n";
    String actualResult = CdaGeneratorUtils.getXmlForNFSection("ElementName", "Null Flavor Val");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForEndElementTest() {
    String expectedResult = "</ElementName>\n";
    String actualResult = CdaGeneratorUtils.getXmlForEndElement("ElementName");
    assertEquals(expectedResult, actualResult);
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
    String version = CdaGeneratorConstants.CDA_EICR_VERSION_R11;
    String actualResult = CdaGeneratorUtils.getXmlHeaderForClinicalDocument(version);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getEndXMLHeaderForCdaDocumentTest() {
    String expectedResult = "</ClinicalDocument>";
    String actualResult = CdaGeneratorUtils.getEndXMLHeaderForCdaDocument();
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForTemplateIdTest() {
    String expectedResult = "<templateId root=\"TemplId\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForTemplateId("TemplId");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForTemplateIdTest_withExt() {
    String expectedResult = "<templateId root=\"TemplId\" extension=\"Extension\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForTemplateId("TemplId", "Extension");
    assertEquals(expectedResult, actualResult);
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
    String actualResult = CdaGeneratorUtils.getNFXMLForElement("ElementName", "NullFlavourText");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDTest_NoDisplayName() {
    String expectedResult =
        "<CodeName code=\"Code\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForCD("CodeName", "Code", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForNullValueCDTest() {
    String expectedResult = "<CodeName xsi:type=\"CD\" nullFlavor=\"Code\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForNullValueCD("CodeName", "Code");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForNullValueCDWithoutEndTagTest() {
    String expectedResult = "<CodeName xsi:type=\"CD\" nullFlavor=\"Code\">";
    String actualResult = CdaGeneratorUtils.getXmlForNullValueCDWithoutEndTag("CodeName", "Code");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getCDADocHeaderTemplateXMLTest() {
    String expectedResult =
        "<typeId root=\"2.16.840.1.113883.1.3\" extension=\"POCD_HD000040\"/>\n";
    String actualResult = CdaGeneratorUtils.getCDADocHeaderTemplateXML();
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForEntryTemplateTest() {
    String expectedResult = "<TemplateName typeCode=\"TypeCodeName\">\n";
    String actualResult = CdaGeneratorUtils.getXmlForEntryTemplate("TemplateName", "TypeCodeName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForPartialValueIVLWithTSTest_low() {
    String expectedResult =
        "<elName xsi:type=\"IVL_TS\"><low value=\"valueText\"/>\n" + "</elName>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForPartialValueIVLWithTS("elName", "valueText", "low");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForPartialValueIVLWithTSTest_high() {
    String expectedResult =
        "<elName xsi:type=\"IVL_TS\"><high value=\"valueText\"/>\n" + "</elName>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForPartialValueIVLWithTS("elName", "valueText", "high");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForPIVLWithTSTest() {
    String expectedResult =
        "<elName xsi:type=\"PIVL_TS\" institutionSpecified=\"true\" operator=\"A\">\n"
            + "<period value=\"0\" unit=\"h\"/>\n"
            + "</elName>\n";
    String actualResult = CdaGeneratorUtils.getXmlForPIVLWithTS("elName", 1000);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSet_EmptyDisplay_EmptyValue() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSet(
            "codeName", "CodeSystem", "CodeSystemName", "", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSet_EmptyValue() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSet(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSet() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName &quot; DisplayName\" sdtc:valueSet=\"ValueName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSet(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName \" DisplayName", "ValueName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDWithValueSetTest_EmptyDisplay_EmptyValue() {
    String expectedResult =
        "<elName  code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSet(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "");
    assertEquals(expectedResult, actualResult);
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
    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSet(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "DisplayName", "ValueName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithoutEndTagTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "DisplayName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCOTest() {
    String expectedResult =
        "<value xsi:type=\"CO\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCO(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithoutEndTagEmptyDisplayTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithoutEndTagDisplayTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithoutEndTagElseTest() {
    String expectedResult =
        "<elName xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    expectedResult += "\n" + "</elName>" + "\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "DisplayName",
            "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWithDisplayTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    expectedResult += "\n" + "</elName>" + "\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionElseTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    expectedResult += "\n" + "</elName>" + "\n";

    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForNullValuePQTest() {
    String expectedResult = "<value xsi:type=\"PQ\" nullFlavor=\"NF\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForNullValuePQ("NF");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueINTTest() {
    String expectedResult = "<value xsi:type=\"INT\" value=\"1357\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForValueINT("1357");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getNFXMLForValueTest() {
    String expectedResult = "<value xsi:type=\"CD\" nullFlavor=\"NF\"/>\n";
    String actualResult = CdaGeneratorUtils.getNFXMLForValue("NF");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getNFXMLForValueWithTextTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" nullFlavor=\"NF\"><originalText>COVID-19</originalText>\n"
            + "</value>"
            + "\n";
    String actualResult = CdaGeneratorUtils.getNFXMLForValueWithText("NF", "COVID-19");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDTranslationTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" nullFlavor=\"OTH\"><translation code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n"
            + "</value>"
            + "\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDTranslation(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForPerformerTest() {
    String expectedResult = "<performer typeCode=\"PERFTYPE\">\n";
    String actualResult = CdaGeneratorUtils.getXmlForPerformer("PERFTYPE");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForParticipantTest() {
    String expectedResult = "<participant typeCode=\"NURSE\">\n";
    String actualResult = CdaGeneratorUtils.getXmlForParticipant("NURSE");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForeTest() {
    String expectedResult = "<participantRole classCode=\"NURSE\">\n";
    String actualResult = CdaGeneratorUtils.getXmlForParticipantRole("NURSE");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForReferenceTest() {
    String expectedResult = "<reference typeCode=\"TYPECODE\">\n";
    String actualResult = CdaGeneratorUtils.getXmlForReference("TYPECODE");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getRootOidTest() {
    String expectedRootOidTest = "urn:oid";
    String actualRootOidTest = CdaGeneratorUtils.getRootOid("urn:oid", "1357");
    assertEquals(expectedRootOidTest, actualRootOidTest);
  }

  @Test
  public void getRootOidWithIOD() {
    String expectedRootOid = "2.16.840.1.113883.11.11555";
    String actualRootOid =
        CdaGeneratorUtils.getRootOid("http://terminology.hl7.org/RoleClass", "1357");
    assertEquals(expectedRootOid, actualRootOid);
  }

  @Test
  public void getRootOidWithoutSystemValue() {
    String expectedRootOid = "1357";
    String actualRootOid = CdaGeneratorUtils.getRootOid("", "1357");
    assertEquals(expectedRootOid, actualRootOid);
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
    String actualResult = CdaGeneratorUtils.getXmlForSDTCElement("elName", "value");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForNfQuantityTest() {
    String expectedResult = "<elName nullFlavor=\"NF\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForNfQuantity("elName", "NF", false);
    assertEquals(expectedResult, actualResult);

    String expectedXml = "<elName xsi:type=\"PQ\" nullFlavor=\"NF\"/>\n";
    String actualXml = CdaGeneratorUtils.getXmlForNfQuantity("elName", "NF", true);
    assertEquals(expectedXml, actualXml);
  }

  @Test
  public void getXmlForNullEffectiveTimeTest() {
    String expectedResult = "<elName nullFlavor=\"2021-02-10\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForNullEffectiveTime("elName", "2021-02-10");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForTelecomTest() {
    String expectedResult = "<MOBILE value=\"tel:(202)102-1012\" use=\"WORK\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "2021021012", "WORK", false);
    assertEquals(expectedResult, actualResult);

    actualResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "202-102-1012", "WORK", false);
    assertEquals(expectedResult, actualResult);

    actualResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "(202)102-1012", "WORK", false);
    assertEquals(expectedResult, actualResult);

    actualResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "+12021021012", "WORK", false);
    expectedResult = "<MOBILE value=\"tel:+1(202)102-1012\" use=\"WORK\"/>\n";
    assertEquals(expectedResult, actualResult);

    actualResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "+91-202-102-1012", "WORK", false);
    expectedResult = "<MOBILE value=\"tel:+91(202)102-1012\" use=\"WORK\"/>\n";
    assertEquals(expectedResult, actualResult);

    actualResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "+91(202)102-1012", "WORK", false);
    expectedResult = "<MOBILE value=\"tel:+91(202)102-1012\" use=\"WORK\"/>\n";
    assertEquals(expectedResult, actualResult);

    actualResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "+2021021012", "WORK", false);
    expectedResult = "<MOBILE value=\"tel:(202)102-1012\" use=\"WORK\"/>\n";
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForTelecomTest_TelecomNumberLesserThan10() {
    String expectedResult = "<PHONE nullFlavor=\"NI\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForTelecom("PHONE", "2021022", "HOME", false);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForTelecomTest_WithSpecialCharacters() {
    String expectedResult = "<MOBILE value=\"tel:(202)102-1012\" use=\"WORK\"/>\n";
    String result = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "202-102-1012", "WORK", false);
    assertEquals(expectedResult, result);

    String trimmedResult =
        CdaGeneratorUtils.getXmlForTelecom("MOBILE", "+1(202)1021012", "WORK", false);
    expectedResult = "<MOBILE value=\"tel:+1(202)102-1012\" use=\"WORK\"/>\n";
    assertEquals(expectedResult, trimmedResult);

    String expectedResult2 = "<MOBILE value=\"tel:(555)555-5006\" use=\"WORK\"/>\n";
    String trimmedResult2 =
        CdaGeneratorUtils.getXmlForTelecom("MOBILE", "555-555-5006", "WORK", false);
    assertEquals(expectedResult2, trimmedResult2);

    String noUseAttrResult = "<MOBILE value=\"tel:(202)102-1012\"/>\n";
    String noAttrResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "2021021012", "", false);
    assertEquals(noUseAttrResult, noAttrResult);

    String nullExpectedResult = "<MOBILE nullFlavor=\"NI\"/>\n";
    String nullResult = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "1021012", "WORK", false);
    assertEquals(nullExpectedResult, nullResult);

    String nullResult2 = CdaGeneratorUtils.getXmlForTelecom("MOBILE", "", "WORK", false);
    assertEquals(nullExpectedResult, nullResult2);
  }

  @Test
  public void getXmlForEmailTest() {
    String expectedResult = "<MOBILE value=\"mailto:20210210123\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForEmail("MOBILE", "20210210123", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForEntryRelationshipTest() {
    String expectedResult = "<entryRelationship typeCode=\"TYPECODE\" inversionInd=\"IND\">\n";
    String actualResult = CdaGeneratorUtils.getXmlForEntryRelationship("TYPECODE", "IND");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForActWithNegationIndTest() {
    String expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\" negationInd=\"true\" >\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "T", true);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForActWithNoNegationIndTest() {
    String expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\" negationInd=\"false\" >\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "", true);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForActWithIncludeNegAsFalseTest() {
    String expectedResult = "<ACT classCode=\"CLASS\" moodCode=\"MOOD\">\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForActWithNegationInd("ACT", "CLASS", "MOOD", "", false);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForIVLWithTSTest() {
    String expectedResult =
        "<EL>\n" + "<low value=\"LO\"/>\n" + "<high value=\"HIGH\"/>\n" + "</EL>\n";
    String actualResult = CdaGeneratorUtils.getXmlForIVLWithTS("EL", "LO", "HIGH");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForIVLWithNullTSTest() {
    Pair<Date, TimeZone> date = null;
    String expectedResult = "<EL nullFlavor=\"NI\"/>\n";
    String actualResult = CdaGeneratorUtils.getXmlForIVLWithTS("EL", date, date, false);
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForIVLWithTS() {
    Pair<Date, TimeZone> low = new Pair<Date, TimeZone>(new Date(), TimeZone.getDefault());
    Pair<Date, TimeZone> high = new Pair<Date, TimeZone>(new Date(), TimeZone.getDefault());
    String actualXmlForIVLWithTS =
        CdaGeneratorUtils.getXmlForIVLWithTS("normalRange", low, high, false);
    assertEquals(100, actualXmlForIVLWithTS.length());
  }

  @Test
  public void getXmlForIVLWithTSNegationLow() {
    Pair<Date, TimeZone> high = new Pair<Date, TimeZone>(new Date(), TimeZone.getDefault());
    String actualXmlForIVLWithTS =
        CdaGeneratorUtils.getXmlForIVLWithTS("normalRange", null, high, false);
    assertEquals(88, actualXmlForIVLWithTS.length());
  }

  @Test
  public void getXmlForIVLWithTSNegationHigh() {
    Pair<Date, TimeZone> low = new Pair<Date, TimeZone>(new Date(), TimeZone.getDefault());
    String actualXmlForIVLWithTS =
        CdaGeneratorUtils.getXmlForIVLWithTS("normalRange", low, null, false);
    assertEquals(64, actualXmlForIVLWithTS.length());
  }

  @Test
  public void getXmlForIVLWithTSNegationRequireNullFlavorAsTrue() {
    String actualXmlForIVLWithTS =
        CdaGeneratorUtils.getXmlForIVLWithTS("normalRange", null, null, true);
    assertEquals(76, actualXmlForIVLWithTS.length());
  }

  @Test
  public void getXmlForValueIVLWithNoNegationLowTest() {

    String xmlForValueIVLWithTS = CdaGeneratorUtils.getXmlForValueIVLWithTS("EL", "", "");
    assertEquals(75, xmlForValueIVLWithTS.length());
  }

  @Test
  public void getXmlForValueIVLWithTSTest() {
    String expectedResult =
        "<EL xsi:type=\"IVL_TS\"><low value=\"LO\"/>\n" + "<high value=\"HIGH\"/>\n" + "</EL>\n";
    String actualResult = CdaGeneratorUtils.getXmlForValueIVLWithTS("EL", "LO", "HIGH");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForLowIVLWithTSWithNFHighTest() {
    String expectedResult =
        "<EL xsi:type=\"IVL_TS\">\n"
            + "<low value=\"VALUE\"/>\n"
            + "<high nullFlavor=\"UNK\"/>\n"
            + "</EL>\n";
    String actualResult = CdaGeneratorUtils.getXmlForLowIVLWithTSWithNFHigh("EL", "VALUE");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCD("codeName", "CodeSystem", "CodeSystemName", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "DisplayName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithDisplayTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
            "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionWithNoDisplayTest() {
    String expectedResult =
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\"/>\n";
    String actualResult =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
            "codeName", "CodeSystem", "CodeSystemName", "", "", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWithoutEndTagTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" sdtc:valueSet=\"ValueName\" sdtc:valueSetVersion=\"ValueSetVersion\">";
    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
            "elName",
            "codeName",
            "CodeSystem",
            "CodeSystemName",
            "ValueName",
            "ValueSetVersion",
            "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWithoutEndTagWithDisplayTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\">";
    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "DisplayName");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWithoutEndTagWithoutDisplayTest() {
    String expectedResult =
        "<elName code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\">";
    String actualResult =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "", "", "");
    assertEquals(expectedResult, actualResult);
  }

  @Test
  public void testGetXmlForPIVLWithTS() {
    String expectedXmlForPIVLWithTS =
        "<Period xsi:type=\"PIVL_TS\" institutionSpecified=\"true\" operator=\"A\">\n"
            + "<period value=\"1000\" unit=\"h\"/>\n"
            + "</Period>\n";
    String actualXmlForPIVLWithTS = CdaGeneratorUtils.getXmlForPIVLWithTS("Period", "1000");
    assertEquals(expectedXmlForPIVLWithTS, actualXmlForPIVLWithTS);
  }

  @Test
  public void getXmlForPIVLWithTS() {
    String expectedXmlForPIVLWithTS =
        "<Period xsi:type=\"PIVL_TS\" institutionSpecified=\"true\" operator=\"A\">\n"
            + "<period value=\"0\" unit=\"h\"/>\n"
            + "</Period>\n";
    String actualXmlForPIVLWithTS = CdaGeneratorUtils.getXmlForPIVLWithTS("Period", 1000);
    assertEquals(expectedXmlForPIVLWithTS, actualXmlForPIVLWithTS);
  }

  @Test
  public void getXmlForValueCD() {
    String expectedXmlForValueCD =
        "<value xsi:type=\"CD\" code=\"2556421249\" codeSystem=\"https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/72\" codeSystemName=\"coding\" displayName=\"zoster vaccine, inactivated\"/>\n";
    String actualXmlForValueCD =
        CdaGeneratorUtils.getXmlForValueCD(
            "2556421249",
            "https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/72",
            "coding",
            "zoster vaccine, inactivated");
    assertEquals(expectedXmlForValueCD, actualXmlForValueCD);
  }

  @Test
  public void getXmlForValueCDWithoutEndTag() {
    String xmlForValueCDWithoutEndTag =
        CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
            "2556421249",
            "https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/72",
            "coding",
            "zoster vaccine, inactivated");
    assertEquals(190, xmlForValueCDWithoutEndTag.length());
  }

  @Test
  public void getXmlForValueCDWithoutEndTagWithNegationDisplayName() {
    String xmlForValueCDWithoutEndTag =
        CdaGeneratorUtils.getXmlForValueCDWithoutEndTag(
            "2556421249",
            "https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/72",
            "coding",
            "");
    assertEquals(148, xmlForValueCDWithoutEndTag.length());
  }

  @Test
  public void getXmlForValueCDWithValueSetAndVersionNegationValueSet() {
    String expectedXmlForValueCDWithValueSetAndVersion =
        "<value xsi:type=\"CD\" code=\"2556421249\" codeSystem=\"https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/72\" codeSystemName=\"coding\" sdtc:valueSet=\"valueSet\" sdtc:valueSetVersion=\"1.0.0\"/>\n";
    String actualXmlForValueCDWithValueSetAndVersion =
        CdaGeneratorUtils.getXmlForValueCDWithValueSetAndVersion(
            "2556421249",
            "https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/72",
            "coding",
            "valueSet",
            "1.0.0",
            "");
    assertEquals(
        expectedXmlForValueCDWithValueSetAndVersion, actualXmlForValueCDWithValueSetAndVersion);
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionWithoutEndTag() {
    String xmlForCDWithValueSetAndVersionWithoutEndTag =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersionWihoutEndTag(
            "Coding",
            "2556421249",
            "https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/72",
            "coding",
            "valueSet",
            "1.0.0",
            "zoster vaccine, inactivated");
    assertEquals(231, xmlForCDWithValueSetAndVersionWithoutEndTag.length());
  }

  @Test
  public void getXmlForCDWithValueSetAndVersionNegationDisplayName() {
    String expectedXmlForCDWithValueSetAndVersion =
        "<Coding code=\"19162977\" codeSystem=\"https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/30200\" codeSystemName=\"coding\" sdtc:valueSet=\"valueSet\" sdtc:valueSetVersion=\"1.0.0\">\n"
            + "<originalText>\n"
            + "<reference value=\"#Location/29598629\"/>\n"
            + "</originalText>\n"
            + "</Coding>\n";
    String actualXmlForCDWithValueSetAndVersion =
        CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
            "Coding",
            "19162977",
            "https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/30200",
            "coding",
            "valueSet",
            "1.0.0",
            "",
            "Location/29598629");
    assertEquals(expectedXmlForCDWithValueSetAndVersion, actualXmlForCDWithValueSetAndVersion);
  }

  @Test
  public void getXmlForTableHeader() {
    String expectedXmlForTableHeader =
        "<table border=\"1\" width=\"200%\">\n"
            + "<thead>\n"
            + "<tr>\n"
            + "<th>Name</th>\n"
            + "<th>Date of Birth</th>\n"
            + "<th>Gender</th>\n"
            + "</tr>\n"
            + "</thead>\n";
    List<String> headerValues = Arrays.asList("Name", "Date of Birth", "Gender");
    String actualXmlForTableHeader = CdaGeneratorUtils.getXmlForTableHeader(headerValues, 1, 200);
    assertEquals(expectedXmlForTableHeader, actualXmlForTableHeader);
  }

  @Test
  public void addTableRow() {
    Map<String, String> values = new HashMap<String, String>();
    values.put("code", "19162977");
    values.put("display", "Spouse");
    values.put(
        "system", "https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/30200");
    String expectedTableRow =
        "<tr>\n"
            + "<td>\n"
            + "<content ID=\"code3\">19162977</content>\n"
            + "</td>\n"
            + "<td>\n"
            + "<content ID=\"system3\">https://fhir.cerner.com/ec2458f2-1e24-41c8-b71b-0e701af7583d/codeSet/30200</content>\n"
            + "</td>\n"
            + "<td>\n"
            + "<content ID=\"display3\">Spouse</content>\n"
            + "</td>\n"
            + "</tr>\n";
    String actualTableRow = CdaGeneratorUtils.addTableRow(values, 3);
    assertEquals(expectedTableRow, actualTableRow);
  }

  @Test
  public void getXmlForIITest() {
    String expectedXmlForII = "<id root=\"875845\"/>\n";
    String actualXmlForII = CdaGeneratorUtils.getXmlForII("875845");
    assertEquals(expectedXmlForII, actualXmlForII);
  }

  @Test
  public void getXmlForElementWithAttribute() {
    String expectedXmlForElementWithAttribute = "<coding code\"19162977\"/>\n";
    String actualXmlForElementWithAttribute =
        CdaGeneratorUtils.getXmlForElementWithAttribute("coding", "code", "19162977");
    assertEquals(expectedXmlForElementWithAttribute, actualXmlForElementWithAttribute);
  }

  @Test
  public void getXmlForEffectiveTime() {
    String expectedXmlForEffectiveTime = "<Patient nullFlavor=\"NI\"/>\n";
    TimeZone timeZone = TimeZone.getTimeZone("America/New_York");
    String actualXmlForEffectiveTime =
        CdaGeneratorUtils.getXmlForEffectiveTime("Patient", null, timeZone);
    assertEquals(expectedXmlForEffectiveTime, actualXmlForEffectiveTime);
  }

  @Test
  public void getXmlForEmail() {
    String expectedXmlForEmail = "<telecom value=\"mailto:2125555554\" use=\"mobile\"/>\n";
    String actualXmlForEmail = CdaGeneratorUtils.getXmlForEmail("telecom", "2125555554", "mobile");
    assertEquals(expectedXmlForEmail, actualXmlForEmail);
  }
}
