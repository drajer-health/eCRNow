package com.drajer.ecrapp.util;

import org.slf4j.MDC;

public class MDCUtils {
  private static final String MDC_KEY_CORRELATION_ID = "correlationId";

  public static void addCorrelationId(String correlationId) {
    MDC.put(MDC_KEY_CORRELATION_ID, correlationId);
  }

  public static void removeCorrelationId() {
    MDC.remove(MDC_KEY_CORRELATION_ID);
  }
}
