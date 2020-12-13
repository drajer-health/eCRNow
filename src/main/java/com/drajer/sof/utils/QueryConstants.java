package com.drajer.sof.utils;

public class QueryConstants {

  private QueryConstants() {}

  public static final String LOINC_CODE_SYSTEM = "http://loinc.org";
  public static final String PREGNANCY_CODE = "90767-5";
  public static final String TRAVEL_CODE = "29762-2";

  private static final String[] TRAVEL_HISTORY_SNOMED_CODES = {
    "161085007", "443846001", "34831000175105", "161086008"
  };

  public static final String SNOMED_CODE_SYSTEM = "http://snomed.info/sct";

  private static final String[] OCCUPATION_LOINC_CODES = {"11295-3"};

  private static final String[] OCCUPATION_SNOMED_CODES = {"224362002", "364703007"};
  private static final String[] PREGNANCY_SNOMED_CODES = {"77386006"};

  public static String[] getTravelHistorySmtCodes() {
    return TRAVEL_HISTORY_SNOMED_CODES;
  }

  public static String[] getOccupationSmtCodes() {
    return OCCUPATION_SNOMED_CODES;
  }

  public static String[] getOccupationLoincCodes() {
    return OCCUPATION_LOINC_CODES;
  }

  public static String[] getPregnancySmtCodes() {
    return PREGNANCY_SNOMED_CODES;
  }
}
