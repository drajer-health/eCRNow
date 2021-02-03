package com.drajer.cda.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.Test;

public class CdaGeneratorUtilsTest {

  @Test
  public void getXmlForValueEdTest() {
    String expectedResult = "<value xsi:type=\"ED\">TestValue</value>\n";
    String result = CdaGeneratorUtils.getXmlForValueEd("TestValue");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getXmlForValueStringTest() {
    String expectedResult = "<value xsi:type=\"ST\">TestString</value>\n";
    String result = CdaGeneratorUtils.getXmlForValueString("TestString");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getNFXmlForValueStringTest() {
    String expectedResult = "<value xsi:type=\"ST\" nullFlavor=\"TestString\"/>\n";
    String result = CdaGeneratorUtils.getNFXmlForValueString("TestString");
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
    String result = CdaGeneratorUtils.getStringForDate(null);
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
  public void getHeaderTemplatesXMLTest() {
    String expectedResult =
        "<typeId root=\"2.16.840.1.113883.1.3\" extension=\"POCD_HD000040\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.2\"/>\n";
    String result = CdaGeneratorUtils.getHeaderTemplatesXML("TestVersion");
    assertEquals(expectedResult, result);
  }

  @Test
  public void getHeaderTemplatesXMLTest_versionR51() {
    String expectedResult =
        "<typeId root=\"2.16.840.1.113883.1.3\" extension=\"POCD_HD000040\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.1\" extension=\"2015-08-01\"/>\n"
            + "<templateId root=\"2.16.840.1.113883.10.20.22.1.2\" extension=\"2015-08-01\"/>\n";
    String result = CdaGeneratorUtils.getHeaderTemplatesXML("R51");
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
        "<value xsi:type=\"CD\" code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\" sdtc:valueSet=\"ValueName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForValueCDWithValueSet(
            "codeName", "CodeSystem", "CodeSystemName", "DisplayName", "ValueName");
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
        "<elName  code=\"codeName\" codeSystem=\"CodeSystem\" codeSystemName=\"CodeSystemName\" displayName=\"DisplayName\"/>\n";
    String result =
        CdaGeneratorUtils.getXmlForCDWithValueSet(
            "elName", "codeName", "CodeSystem", "CodeSystemName", "DisplayName", "");
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
}
