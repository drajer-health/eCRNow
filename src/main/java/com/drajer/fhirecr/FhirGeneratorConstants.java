package com.drajer.fhirecr;

public class FhirGeneratorConstants {

  private FhirGeneratorConstants() {}

  // FHIR Resource Type Constants.

  // ECR RCTC OID
  public static final String RCTC_OID = "2.16.840.1.114222.4.11.7508";

  // Document Bundle constants
  public static final String DOC_ID_SYSTEM = "urn:ietf:rfc:3986";

  // Extension urls
  public static final String COMP_CLIN_DOC_VERSION_NUM_URL =
      "http://hl7.org/fhir/R4/extension-composition-clinicaldocument-versionnumber";
  public static final int VERSION_NUM = 1;
  public static final String UNKNOWN_VALUE = "UNKNOWN";

  // Code Systems
  public static final String LOINC_CS_URL = "http://loinc.org";
  public static final String SNOMED_CS_URL = "http://snomed.info/sct";
  public static final String HL7_OBSERVATION_CATEGORY =
      "http://terminology.hl7.org/CodeSystem/observation-category";
  public static final String TERMINOLOGY_V2_0074_URL =
      "http://terminology.hl7.org/CodeSystem/v2-0074";

  // Composition Codes
  public static final String ECR_COMP_TYPE_CODE = "55751-2";
  public static final String ECR_COMP_TYPE_CODE_DISPLAY = "Public Health Case Report";
  public static final String HCS_COMP_TYPE_CODE = "75619-7";
  public static final String HCS_COMP_TYPE_CODE_DISPLAY = "National Health Care Surveys report";
  public static final String CCRR_COMP_TYPE_CODE = "72134-0";
  public static final String CCRR_COMP_TYPE_CODE_DISPLAY = "Cancer event report";
  public static final String CCRR_COMP_SECTION_TITLE = "Central Cancer Registry Report";

  // Composition section codes commonly used.
  public static final String CHIEF_COMPLAINT_SECTION_LOINC_CODE = "10154-3";
  public static final String CHIEF_COMPLAINT_SECTION_LOINC_CODE_DISPLAY =
      "Chief complaint Narrative";
  public static final String HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE = "10164-2";
  public static final String HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE_DISPLAY =
      "History of Present illness Narrative";
  public static final String REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE = "10187-3";
  public static final String REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE_DISPLAY =
      "Review of Systems narrative";
  public static final String PROBLEM_SECTION_LOINC_CODE = "11450-4";
  public static final String PROBLEM_SECTION_LOINC_CODE_DISPLAY = "Problem list - Reported";
  public static final String ALLERGIES_SECTION_LOINC_CODE = "48765-2";
  public static final String ALLERGIES_SECTION_LOINC_CODE_DISPLAY =
      "Allergies and adverse reactions Document";
  public static final String ADMISSION_MEDICATIONS_SECTION_LOINC_CODE = "42346-7";
  public static final String ADMISSION_MEDICATIONS_SECTION_LOINC_CODE_DISPLAY =
      "Medications on admission (narrative)";
  public static final String MEDICATIONS_SECTION_LOINC_CODE = "10160-0";
  public static final String MEDICATIONS_SECTION_LOINC_CODE_DISPLAY =
      "History of Medication use Narrative";
  public static final String NOTES_SECTION_LOINC_CODE = "28650-0";
  public static final String NOTES_SECTION_LOINC_CODE_DISPLAY =
      "Clinical notes and chart sections Set";
  public static final String MEDICAL_EQUIPMENT_SECTION_LOINC_CODE = "46264-8";
  public static final String MEDICAL_EQUIPMENT_SECTION_LOINC_CODE_DISPLAY =
      "History of medical device use";
  public static final String CARE_TEAM_SECTION_LOINC_CODE = "85847-2";
  public static final String CARE_TEAM_SECTION_LOINC_CODE_DISPLAY = "Patient Care team information";
  public static final String GOALS_SECTION_LOINC_CODE = "61146-7";
  public static final String GOALS_SECTION_LOINC_CODE_DISPLAY = "Goals Narrative";
  public static final String PAST_MEDICAL_HISTORY_SECTION_LOINC_CODE = "11348-0";
  public static final String PAST_MEDICAL_HISTORY_SECTION_LOINC_CODE_DISPLAY =
      "Past Medical History Narrative";
  public static final String MEDICATION_ADMINISTERED_SECTION_LOINC_CODE = "29549-3";
  public static final String MEDICATION_ADMINISTERED_SECTION_LOINC_CODE_DISPLAY =
      "Medication administered Narrative";
  public static final String RESULTS_SECTION_LOINC_CODE = "30954-2";
  public static final String RESULTS_SECTION_LOINC_CODE_DISPLAY =
      "Relevant diagnostic tests/laboratory data Narrative";
  public static final String PLAN_OF_TREATMENT_SECTION_LOINC_CODE = "18776-5";
  public static final String PLAN_OF_TREATMENT_SECTION_LOINC_CODE_DISPLAY = "Plan of care note";
  public static final String PLAN_OF_ASSESSMENT_SECTION_LOINC_CODE = "51847-2";
  public static final String PLAN_OF_ASSESSMENT_SECTION_LOINC_CODE_DISPLAY =
      "Evaluation + Plan note";
  public static final String IMMUNIZATION_SECTION_LOINC_CODE = "11369-6";
  public static final String IMMUNIZATION_SECTION_LOINC_CODE_DISPLAY =
      "History of Immunization Narrative";
  public static final String PROCEDURE_SECTION_LOINC_CODE = "47519-4";
  public static final String PROCEDURE_SECTION_LOINC_CODE_DISPLAY =
      "History of Procedures Document";
  public static final String VITAL_SIGNS_SECTION_LOINC_CODE = "8716-3";
  public static final String VITAL_SIGNS_SECTION_LOINC_CODE_DISPLAY = "Vital signs";
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

  public static final String PRIMARY_CANCER_CONDITION_SECTION_CODE = "363346000";
  public static final String PRIMARY_CANCER_CONDITION_SECTION_CODE_DISPLAY = "Malignant tumour";
  public static final String SECONDARY_CANCER_CONDITION_SECTION_CODE = "128462008";
  public static final String SECONDARY_CANCER_CONDITION_SECTION_CODE_DISPLAY = "Secondary cancer";
  public static final String CANCER_STAGE_GROUP_SECTION_CODE = "21908-9";
  public static final String CANCER_STAGE_GROUP_SECTION_CODE_DISPLAY =
      "Stage group.clinical Cancer";
  public static final String CANCER_RADIO_THERAPY_COURSE_SUMMARY_SECTION_CODE = "1217123003";
  public static final String CANCER_RADIO_THERAPY_COURSE_SUMMARY_SECTION_CODE_DISPLAY =
      "Radiotherapy course of treatment";
  public static final String ODH_SECTION_CODE = "74165-2";
  public static final String ODH_SECTION_CODE_DISPLAY = "History of employment status NIOSH";
  public static final String COVERAGE_CODE = "48768-6";
  public static final String COVERAGE_CODE_DISPLAY = "Payment sources Document";
  public static final String LAB_CODE = "LAB";
}
