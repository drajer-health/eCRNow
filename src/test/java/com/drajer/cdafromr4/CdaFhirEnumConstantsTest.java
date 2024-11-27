package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.powermock.core.classloader.annotations.PowerMockIgnore;

@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaFhirEnumConstantsTest {

  @Test
  public void CdaFhirEnumConstantTest() {
    assertEquals("MR", CdaFhirEnumConstants.FHIR_ID_TYPE_MR);
  }
}
