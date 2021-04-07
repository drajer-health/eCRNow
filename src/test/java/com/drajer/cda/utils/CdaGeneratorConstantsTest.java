package com.drajer.cda.utils;

import static org.junit.Assert.assertNotNull;

import org.javatuples.Pair;
import org.junit.Test;

public class CdaGeneratorConstantsTest {

  public String oid = "2.16.840.1.113883.6.96";
  public String uri = "http://loinc.org|LOINC";

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
}
