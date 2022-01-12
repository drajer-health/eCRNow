package com.drajer.ecrapp.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.slf4j.MDC;

public class MDCUtilsTest {

  @Test
  public void testAddAndRemoveMDC() {
    MDCUtils.addCorrelationId("test");
    assertEquals("test", MDC.get("correlationId"));
    MDCUtils.removeCorrelationId();
    assertNull(MDC.get("correlationId"));
  }

  @Test
  public void testAddAndRemoveMdcKeyEicrDocId() {
    MDCUtils.addEicrDocId("test");
    assertEquals("test", MDC.get("eicrDocId"));
    MDCUtils.removeEicrDocId();
    assertNull(MDC.get("eicrDocId"));
  }
}
