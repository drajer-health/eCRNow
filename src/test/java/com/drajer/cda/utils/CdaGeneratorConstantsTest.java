package com.drajer.cda.utils;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.javatuples.Pair;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class CdaGeneratorConstantsTest {

  public String oid = "2.16.840.1.113883.6.96";
  public String uri = "http://loinc.org|LOINC";
  public String INTERPRETATION_CODE_EL_NAME = "interpretationCode";

  @Test
  public void getURITest() {
    Pair<String, String> uriValue = CdaGeneratorConstants.getURI(oid);
    assertNotNull(uriValue);
  }

  @Test
  public void getOIDTest() {
    Pair<String, String> oidValue = CdaGeneratorConstants.getOID(uri);
    assertNotNull(oidValue);
  }

  @Test
  public void getCodeSystemFromUrlTest() {
    Pair<String, String> uriValue = CdaGeneratorConstants.getCodeSystemFromUrl(uri);
    assertNotNull(uriValue);
  }

  @Test
  public void getMappedCodeFromFhirToCda() {
    String uriValue =
        CdaGeneratorConstants.getMappedCodeFromFhirToCda(INTERPRETATION_CODE_EL_NAME, "HH");
    assertNotNull(uriValue);
  }

  @Test
  public void getMappedCodeFromFhirToCda_invalidConceptDomain() {
    String uriValue =
        CdaGeneratorConstants.getMappedCodeFromFhirToCda("invalid_concept_domain", oid);
    assertNull(uriValue);
  }

  @Test
  public void getMappedCodeFromFhirToCda_invalidConcept() {
    String uriValue =
        CdaGeneratorConstants.getMappedCodeFromFhirToCda(
            INTERPRETATION_CODE_EL_NAME, "invalid_concept");
    assertNull(uriValue);
  }

  @Test
  public void getCodeForContactRelationshipWithValueAsFriend() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("friend");
    assertEquals("NOK", actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForContactRelationshipWithValueAsPartner() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("partner");
    assertEquals("NOK", actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForContactRelationshipWithValueAsParent() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("parent");
    assertEquals("NOK", actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForContactRelationshipWithValueAsFamily() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("family");
    assertEquals("NOK", actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForContactRelationshipWithValueAsN() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("N");
    assertEquals("NOK", actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForContactRelationshipWithValueAsPatient() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("patient");
    assertNull(actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForContactRelationshipWithNegationValue() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("");
    assertNull(actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForContactRelationshipWithValueAsC() {
    String actualCodeForContactRelationship =
        CdaGeneratorConstants.getCodeForContactRelationship("C");
    assertEquals("ECON", actualCodeForContactRelationship);
  }

  @Test
  public void getCodeForNameUseWithValueAsOfficial() {
    String actualCodeForNameUse = CdaGeneratorConstants.getCodeForNameUse("official");
    assertEquals("L", actualCodeForNameUse);
  }

  @Test
  public void getCodeForNameUseWithValueAsTemp() {
    String actualCodeForNameUse = CdaGeneratorConstants.getCodeForNameUse("temp");
    assertNull(actualCodeForNameUse);
  }

  @Test
  public void getCodeForNameUseWithNegationValue() {
    String actualCodeForNameUse = CdaGeneratorConstants.getCodeForNameUse("");
    assertNull(actualCodeForNameUse);
  }

  @Test
  public void getCodeForNameQualifierWithValueAsOfficial() {
    String actualCodeForNameQualifier = CdaGeneratorConstants.getCodeForNameQualifier("official");
    assertEquals("PR", actualCodeForNameQualifier);
  }

  @Test
  public void getCodeForNameQualifierWithValueANickName() {
    String actualCodeForNameQualifier = CdaGeneratorConstants.getCodeForNameQualifier("nickname");
    assertEquals("CL", actualCodeForNameQualifier);
  }

  @Test
  public void getCodeForNameQualifierWithValueAsMaiden() {
    String actualCodeForNameQualifier = CdaGeneratorConstants.getCodeForNameQualifier("maiden");
    assertEquals("BR", actualCodeForNameQualifier);
  }

  @Test
  public void getCodeForNameQualifierWithValueAsWork() {
    String actualCodeForNameQualifier = CdaGeneratorConstants.getCodeForNameQualifier("work");
    assertNull(actualCodeForNameQualifier);
  }

  @Test
  public void getCodeForNameQualifierWithNegationValue() {
    String actualCodeForNameQualifier = CdaGeneratorConstants.getCodeForNameQualifier("");
    assertNull(actualCodeForNameQualifier);
  }

  @Test
  public void testGetCodeForAddressUseWithValueAsHome() {
    String actualCodeForAddressUse = CdaGeneratorConstants.getCodeForAddressUse("home");
    assertEquals("HP", actualCodeForAddressUse);
  }

  @Test
  public void testGetCodeForAddressUseWithValueAsWork() {
    String actualCodeForAddressUse = CdaGeneratorConstants.getCodeForAddressUse("work");
    assertEquals("WP", actualCodeForAddressUse);
  }

  @Test
  public void testGetCodeForAddressUseWithValueAsTemp() {
    String actualCodeForAddressUse = CdaGeneratorConstants.getCodeForAddressUse("temp");
    assertEquals("TMP", actualCodeForAddressUse);
  }

  @Test
  public void testGetCodeForAddressUseWithNegationValue() {
    String actualCodeForAddressUse = CdaGeneratorConstants.getCodeForAddressUse("");
    assertNull(actualCodeForAddressUse);
  }

  @Test
  public void testGetCodeForAddressUseWithValueAsOffice() {
    String actualCodeForAddressUse = CdaGeneratorConstants.getCodeForAddressUse("office");
    assertNull(actualCodeForAddressUse);
  }

  @Test
  public void testGetCodeForTelecomUseWithValueAsHome() {
    String actualCodeForTelecomUse = CdaGeneratorConstants.getCodeForTelecomUse("home");
    assertEquals("HP", actualCodeForTelecomUse);
  }

  @Test
  public void testGetCodeForTelecomUseWithValueAsWork() {
    String actualCodeForTelecomUse = CdaGeneratorConstants.getCodeForTelecomUse("work");
    assertEquals("WP", actualCodeForTelecomUse);
  }

  @Test
  public void testGetCodeForTelecomUseWithValueAsMobile() {
    String actualCodeForTelecomUse = CdaGeneratorConstants.getCodeForTelecomUse("mobile");
    assertEquals("MC", actualCodeForTelecomUse);
  }

  @Test
  public void testGetCodeForTelecomUseWithValueAsPhone() {
    String actualCodeForTelecomUse = CdaGeneratorConstants.getCodeForTelecomUse("phone");
    assertEquals("WP", actualCodeForTelecomUse);
  }

  @Test
  public void testGetCodeForTelecomUseWithNegationValue() {
    String actualCodeForTelecomUse = CdaGeneratorConstants.getCodeForTelecomUse("");
    assertEquals("HP", actualCodeForTelecomUse);
  }

  @Test
  public void getCodeSystemFromUrlForDstu2NegationURL() {
    Pair<String, String> expectedCodeSystemFromUrlForDstu2 = new Pair<>("", "");
    Pair<String, String> actualCodeSystemFromUrlForDstu2 =
        CdaGeneratorConstants.getCodeSystemFromUrlForDstu2("");
    assertEquals(expectedCodeSystemFromUrlForDstu2, actualCodeSystemFromUrlForDstu2);
  }

  @Test
  public void testGetCodeSystemFromUrlForDstu2() {
    Pair<String, String> expectedCodeSystemFromUrlForDstu2 =
        new Pair<>("2.16.840.1.133883.18.31", "0078");
    Pair<String, String> actualCodeSystemFromUrlForDstu2 =
        CdaGeneratorConstants.getCodeSystemFromUrlForDstu2("http://hl7.org/fhir/v2/0078");
    assertEquals(expectedCodeSystemFromUrlForDstu2, actualCodeSystemFromUrlForDstu2);
  }

  @Test
  public void testGetCodeSystemFromUrlForDstu2InvalidUrl() {
    Pair<String, String> expectedCodeSystemFromUrlForDstu2 = new Pair<>("", "");
    Pair<String, String> actualCodeSystemFromUrlForDstu2 =
        CdaGeneratorConstants.getCodeSystemFromUrlForDstu2(
            "http://terminology.hl7.org/CodeSystem/v2-0203");
    assertEquals(expectedCodeSystemFromUrlForDstu2, actualCodeSystemFromUrlForDstu2);
  }

  @Test
  public void getCodeSystemFromUrl() {
    Pair<String, String> expectedCodeSystemFromUrl =
        new Pair<>("2.16.840.1.113883.11.11555", "RoleClass");
    Pair<String, String> actualCodeSystemFromUrl =
        CdaGeneratorConstants.getCodeSystemFromUrl("http://terminology.hl7.org/RoleClass");
    assertEquals(expectedCodeSystemFromUrl, actualCodeSystemFromUrl);
  }

  @Test
  public void getCodeSystemFromUrlWithInvalidURL() {
    Pair<String, String> expectedCodeSystemFromUrl =
        new Pair<>("2.16.840.1.113883.5.1117", "v3-IdentifierReliability");
    Pair<String, String> actualCodeSystemFromUrl =
        CdaGeneratorConstants.getCodeSystemFromUrl(
            "http://terminology.hl7.org/CodeSystem/v3-IdentifierReliability");
    assertEquals(expectedCodeSystemFromUrl, actualCodeSystemFromUrl);
  }

  @Test
  public void getCodeSystemFromUrlWithNegationUrl() {
    Pair<String, String> expectedCodeSystemFromUrl = new Pair<>("", "");
    Pair<String, String> actualCodeSystemFromUrl = CdaGeneratorConstants.getCodeSystemFromUrl("");
    assertEquals(expectedCodeSystemFromUrl, actualCodeSystemFromUrl);
  }

  @Test
  public void getCodeSystemFromUrlWithOID() {
    Pair<String, String> expectedCodeSystemFromUrl = new Pair<>("", "");
    Pair<String, String> actualCodeSystemFromUrl =
        CdaGeneratorConstants.getCodeSystemFromUrl("urn:oid:");
    assertEquals(expectedCodeSystemFromUrl, actualCodeSystemFromUrl);
  }

  @Test
  public void getOID() {
    Pair<String, String> expectedOID =
        new Pair<>("2.16.840.1.113883.5.1117", "v3-IdentifierReliability");
    Pair<String, String> actualOID =
        CdaGeneratorConstants.getOID(
            "http://terminology.hl7.org/CodeSystem/v3-IdentifierReliability");
    assertEquals(expectedOID, actualOID);
  }

  @Test
  public void getURIWithNegationOID() {
    Pair<String, String> expectedURI = new Pair<>("", "");
    Pair<String, String> actualURI = CdaGeneratorConstants.getURI("");
    assertEquals(expectedURI, actualURI);
  }

  @Test
  public void getSplitValueURL() {
    String expectedName = "Condition";
    String actualName =
        CdaGeneratorConstants.getSplitValueURL(
            "http://hl7.org/fhir/us/core/StructureDefinition/us-core-condition|Condition");
    assertEquals(expectedName, actualName);
  }

  @Test
  public void getSplitValueURLWithNegationURL() {
    String expectedName = "vitalsigns";
    String actualName =
        CdaGeneratorConstants.getSplitValueURL(
            "http://hl7.org/fhir/StructureDefinition/vitalsigns");
    assertEquals(expectedName, actualName);
  }
}
