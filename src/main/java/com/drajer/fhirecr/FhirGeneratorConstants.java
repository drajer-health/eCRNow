package com.drajer.fhirecr;

public class FhirGeneratorConstants {

  // FHIR Resource Type Constants.

  // EICR Metadata
  public static String COMP_CLIN_DOC_VERSION_NUM_URL =
      "http://hl7.org/fhir/R4/extension-composition-clinicaldocument-versionnumber";
  public static int VERSION_NUM = 1;
  public static String UNKNOWN_VALUE = "UNKNOWN";

  // Code Systems
  public static String LOINC_CS_URL = "http://loinc.org";

  // Composition Codes
  public static String COMP_TYPE_CODE = "55751-2";
  public static String COMP_TYPE_CODE_DISPLAY = "Public Health Case Report";

  // Section Details
  public static String REASON_FOR_VISIT_CODE = "29299-5";
  public static String REASON_FOR_VISIT_CODE_DISPLAY = "Reason For Visit Narrative";

  public static String HISTORY_OF_ILLNESS_CODE = "10164-2";
  public static String HISTORY_OF_ILLNESS_CODE_DISPLAY = "History of Illness Narrative";
}
