package com.drajer.fhirecr;

public class FhirGeneratorConstants {

  private FhirGeneratorConstants() {}

  // FHIR Resource Type Constants.

  // Extension urls
  public static final String COMP_CLIN_DOC_VERSION_NUM_URL =
      "http://hl7.org/fhir/R4/extension-composition-clinicaldocument-versionnumber";
  public static final int VERSION_NUM = 1;
  public static final String UNKNOWN_VALUE = "UNKNOWN";

  // Code Systems
  public static final String LOINC_CS_URL = "http://loinc.org";
  public static final String HL7_OBSERVATION_CATEGORY =
      "http://terminology.hl7.org/CodeSystem/observation-category";

  // Composition Codes
  public static final String COMP_TYPE_CODE = "55751-2";
  public static final String COMP_TYPE_CODE_DISPLAY = "Public Health Case Report";

  // Composition section codes commonly used.
  public static final String CHIEF_COMPLAINT_SECTION_LOINC_CODE = "46239-0";
  public static final String CHIEF_COMPLAINT_SECTION_LOINC_CODE_DISPLAY =
      "Chief complaint+Reason for visit Narrative";
  public static final String HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE = "10164-2";
  public static final String HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE_DISPLAY =
      "History of Present illness Narrative";
  public static final String REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE = "10187-3";
  public static final String REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE_DISPLAY =
      "Review of Systems narrative";
  public static final String PROBLEM_SECTION_LOINC_CODE = "11450-4";
  public static final String PROBLEM_SECTION_LOINC_CODE_DISPLAY = "Problem Section Narrative";
  public static final String PAST_MEDICAL_HISTORY_SECTION_LOINC_CODE = "11348-0";
  public static final String PAST_MEDICAL_HISTORY_SECTION_LOINC_CODE_DISPLAY =
      "Past Medical History Narrative";
  public static final String MEDICATION_ADMINISTERED_SECTION_LOINC_CODE = "29549-3";
  public static final String MEDICATION_ADMINISTERED_SECTION_LOINC_CODE_DISPLAY =
      "Medications Administered Narrative";
  public static final String RESULTS_SECTION_LOINC_CODE = "30954-2";
  public static final String RESULTS_SECTION_LOINC_CODE_DISPLAY = "Results Section Narrative";
  public static final String PLAN_OF_TREATMENT_SECTION_LOINC_CODE = "18776-5";
  public static final String PLAN_OF_TREATMENT_SECTION_LOINC_CODE_DISPLAY =
      "Plan of Treatment Narrative";
  public static final String IMMUNIZATION_SECTION_LOINC_CODE = "11369-6";
  public static final String IMMUNIZATION_SECTION_LOINC_CODE_DISPLAY = "Immunizations Narrative";
  public static final String PROCEDURE_SECTION_LOINC_CODE = "47519-4";
  public static final String PROCEDURE_SECTION_LOINC_CODE_DISPLAY = "Procedure Section Narrative";
  public static final String VITAL_SIGNS_SECTION_LOINC_CODE = "8716-3";
  public static final String VITAL_SIGNS_SECTION_LOINC_CODE_DISPLAY = "Vital Signs Narrative";
  public static final String SOCIAL_HISTORY_SECTION_LOINC_CODE = "29762-2";
  public static final String SOCIAL_HISTORY_SECTION_LOINC_CODE_DISPLAY = "Social History Narrative";
  public static final String PREGNANCY_SECTION_LOINC_CODE = "90767-5";
  public static final String PREGNANCY_SECTION_LOINC_CODE_DISPLAY = "Pregnancy Section Narrative";
  public static final String REPORTABILITY_RESPONSE_INFORMATION_SECTION_LOINC_CODE = "88085-6";
  public static final String REPORTABILITY_RESPONSE_INFORMATION_SECTION_LOINC_CODE_DISPLAY =
      "Reportability Response Information Section Narrative";
  public static final String EMERGENCY_OUTBREAK_SECTION_LOINC_CODE = "83910-0";
  public static final String EMERGENCY_OUTBREAK_SECTION_LOINC_CODE_DISPLAY =
      "Emergency Outbreak Section Narrative";

  public static final String REASON_FOR_VISIT_CODE = "29299-5";
  public static final String REASON_FOR_VISIT_CODE_DISPLAY = "Reason For Visit Narrative";
}
