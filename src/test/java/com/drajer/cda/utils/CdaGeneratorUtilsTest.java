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
}
