package com.drajer.cda.utils;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.javatuples.Pair;
import org.junit.Test;

public class CdaGeneratorConstantsTest {

  public String oid = "2.16.840.1.113883.6.96";
  public String uri = "http://loinc.org|LOINC";
  public String INTERPRETATION_CODE_EL_NAME = "interpretationCode";

  @Test
  public void getURITest() {
    Pair<String, String> uriValue = CdaGeneratorConstants.getURI(oid);
    assertNotNull(uriValue);
    // assertEquals(uriValue.getValue(1), "SNOMED-CT");
  }

  @Test
  public void getOIDTest() {
    Pair<String, String> oidValue = CdaGeneratorConstants.getOID(uri);
    assertNotNull(oidValue);
    // assertEquals(oidValue.getValue(1), "LOINC");
  }

  @Test
  public void getCodeSystemFromUrlTest() {
    Pair<String, String> uriValue = CdaGeneratorConstants.getCodeSystemFromUrl(uri);
    assertNotNull(uriValue);
    // assertEquals(uriValue.getValue(1), "LOINC");
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
}
