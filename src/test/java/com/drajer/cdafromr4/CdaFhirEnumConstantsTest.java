package com.drajer.cdafromr4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import org.junit.Test;
import org.powermock.core.classloader.annotations.PowerMockIgnore;

@PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*", "javax.management.*"})
public class CdaFhirEnumConstantsTest {

  @Test
  public void CdaFhirEnumConstantTest() {
    assertEquals("MR", CdaFhirEnumConstants.FHIR_ID_TYPE_MR);
  }

  @Test
  public void testPrivateConstructorThrowsIllegalState() throws Exception {
    Constructor<CdaFhirEnumConstants> ctor = CdaFhirEnumConstants.class.getDeclaredConstructor();
    assertTrue("constructor must be private", Modifier.isPrivate(ctor.getModifiers()));
    ctor.setAccessible(true);

    try {
      ctor.newInstance();
      fail("Utility class constructor must not be instantiable");
    } catch (InvocationTargetException e) {
      assertTrue(
          "cause must be IllegalStateException", e.getCause() instanceof IllegalStateException);
      assertEquals("Utility class", e.getCause().getMessage());
    }
  }
}
