package com.drajer.bsa.model;

import static org.junit.Assert.*;

import org.junit.Test;

public class BsaTypesTest {

  @Test
  public void testGetOutputContentTypeEnum() {
    // Test enum to string
    assertEquals("FHIR", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.FHIR));
    assertEquals(
        "FHIR",
        BsaTypes.getOutputContentType(BsaTypes.OutputContentType.TEST_FHIR_NOT_FOR_PRODUCTION));
    assertEquals("CDA_R11", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.CDA_R11));
    assertEquals("CDA_R30", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.CDA_R30));
    assertEquals("CDA_R31", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.CDA_R31));
    assertEquals("Both", BsaTypes.getOutputContentType(BsaTypes.OutputContentType.BOTH));
  }

  @Test
  public void testGetOutputContentTypeString() {
    // Test string to enum
    assertEquals(BsaTypes.OutputContentType.FHIR, BsaTypes.getOutputContentType("FHIR"));
    assertEquals(
        BsaTypes.OutputContentType.FHIR,
        BsaTypes.getOutputContentType("TEST_FHIR_NOT_FOR_PRODUCTION"));
    assertEquals(BsaTypes.OutputContentType.CDA_R11, BsaTypes.getOutputContentType("CDA_R11"));
    assertEquals(BsaTypes.OutputContentType.CDA_R30, BsaTypes.getOutputContentType("CDA_R30"));
    assertEquals(BsaTypes.OutputContentType.CDA_R31, BsaTypes.getOutputContentType("CDA_R31"));
    assertEquals(BsaTypes.OutputContentType.BOTH, BsaTypes.getOutputContentType("Both"));
    assertEquals(BsaTypes.OutputContentType.UNKNOWN, BsaTypes.getOutputContentType("UNKNOWN_TYPE"));
  }
}
