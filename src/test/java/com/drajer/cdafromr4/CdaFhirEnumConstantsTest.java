package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class CdaFhirEnumConstantsTest {

  @Test
  public void CdaFhirEnumConstantTest() {
    assertEquals("MR", CdaFhirEnumConstants.FHIR_ID_TYPE_MR);
  }
}
