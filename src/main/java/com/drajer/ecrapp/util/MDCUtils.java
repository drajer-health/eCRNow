package com.drajer.ecrapp.util;

import org.slf4j.MDC;

public class MDCUtils {
  private static final String MDC_KEY_CORRELATION_ID = "correlationId";
  private static final String MDC_KEY_EICR_DOC_ID = "eicrDocId";

  public static void addCorrelationId(String correlationId) {
    MDC.put(MDC_KEY_CORRELATION_ID, correlationId);
  }

  public static void removeCorrelationId() {
    MDC.remove(MDC_KEY_CORRELATION_ID);
  }

  public static void addEicrDocId(String eicrDocId) {
    MDC.put(MDC_KEY_EICR_DOC_ID, eicrDocId);
  }

  public static void removeEicrDocId() {
    MDC.remove(MDC_KEY_EICR_DOC_ID);
  }
}
