package com.drajer.cda.utils;

public class CdaGeneratorConstants {
	
	// XML Related
    public static String RIGHT_ANGLE_BRACKET = ">";
    public static String START_XMLTAG = "<";
    public static String END_XMLTAG = "/>";
    public static  String END_XMLTAG_NEWLN = "/>" + "\n";
    public static String FORWARD_SLASH = "/";
    public static String BACKWARD_SLASH = "\\";
    public static String DOUBLE_QUOTE = "\"";
    public static String SPACE = " ";
    public static String COLON = ":";
    public static String HYPHEN = "-";
    public static String EQUALS = "=";
    public static String XSI_TYPE = "xsi:type=";
    public static String XML_FILE_PATTERN = "*.xml";

    // CCDA Header Releated
    public static  String DOC_HEADER_XML =
        "<?xml version=\"1.0\"?>" + "\n";

    public static String CLINICAL_DOC_HEADER_XML =
        "<ClinicalDocument xmlns:xsi=" + DOUBLE_QUOTE + "http:" + FORWARD_SLASH + FORWARD_SLASH + "www.w3.org" + FORWARD_SLASH + "2001" +
        FORWARD_SLASH + "XMLSchema-instance" + DOUBLE_QUOTE + "\n" +
        "xmlns=" + DOUBLE_QUOTE + "urn:hl7-org:v3" + DOUBLE_QUOTE + "\n" +
        "xmlns:cda=" + DOUBLE_QUOTE + "urn:hl7-org:v3" + DOUBLE_QUOTE + "\n" +
        "xmlns:sdtc=" + DOUBLE_QUOTE + "urn:hl7-org:sdtc" + DOUBLE_QUOTE + RIGHT_ANGLE_BRACKET + "\n";

    public static String END_HEADER_CLINICAL_DOC = START_XMLTAG + FORWARD_SLASH + "ClinicalDocument" + RIGHT_ANGLE_BRACKET;

    // Template Ids, Section Headers
    // CCDA Header Related Information
    public static String CDA_DOC_ROOT = "2.16.840.1.113883.1.3";
    public static String CDA_DOC_EXT = "POCD_HD000040";
    public static String US_REALM_HEADER = "2.16.840.1.113883.10.20.22.1.1";
    public static String CCDA_CCD_TEMPLATE_ID1 = "2.16.840.1.113883.10.20.22.1.1";
    public static String CCDA_CCD_TEMPLATE_ID1_EXT = "2015-08-01";
    public static String JUNE_2014_RELEASE_EXT = "2014-06-09";
    public static String CCDA_CCD_TEMPLATE_ID2 = "2.16.840.1.113883.10.20.22.1.2";
    public static String CCDA_CCD_TEMPLATE_ID2_EXT = "2015-08-01";
    public static String CCD_DOC_CODE = "34133-9";
    public static String CCD_DOC_NAME = "Summarization of episode note";
    public static String QRDA_CATEGORY_I_DOC_CODE = "55182-0";
    public static String QRDA_CATEGORY_I_DOC_CODE_DISP_NAME = "Quality Measure Report";
    public static String QRDA_CATEGORY_I_DOC_TITLE = "QRDA Incidence Report";
    public static String QRDA_CAT_I_TEMPLATEID = "2.16.840.1.113883.10.20.24.1.1";
    public static String DEC_2014_RELEASE_EXT = "2014-12-01";
    public static String QDM_QRDA_CAT_I_TEMPLATEID = "2.16.840.1.113883.10.20.24.1.2";
    public static String CMS_QRDA_CAT_I_TEMPLATEID = "2.16.840.1.113883.10.20.24.1.3";
    public static String JULY_2015_RELEASE_EXT = "2015-07-01";

    // Encounter Related Information
    public static String EO_ENC_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.22";
    public static String ENC_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.22.1";
    public static String ENC_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String ENC_SEC_CODE = "46240-8";
    public static String ENC_SEC_NAME = "History of Encounters";
    public static String ENC_SEC_TITLE = "ENCOUNTERS";
    public static String QRDA_ENC_PERF_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.133";
    public static String QRDA_ENC_PERF_ACT_TEMPLATE_ID_EXT = "2017-08-01";
    public static String ENC_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.49";
    public static String ENC_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
    public static String ENC_DIAGNOSIS_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.80";
    public static String ENC_DIAGNOSIS_ACT_TEMPLATE_ID_EXT = "2015-08-01";
    public static String ENC_DIAGNOSIS_ACT_CODE = "29308-4";
    public static String ENC_DIAGNOSIS_ACT_CODE_DISPLAY_NAME = "DIAGNOSIS";
    public static String QRDA_ENC_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.23";
    public static String QRDA_ENC_PERF_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_ENC_ACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.21";
    public static String ENC_SDLOC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.32";
    public static String FACILITY_LOC_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.100";
    public static String FACILITY_LOC_TEMPLATE_ID_EXT = "2017-08-01";
    public static String FACILITY_CODE_SYSTEM = "2.16.840.1.113883.5.111";
    public static String FACILTIY_CODE_SYSTEM_NAME = "HL7RoleCode";
    public static String PRINCIPAL_DIAGNOSIS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.152";
    public static String PRINCIPAL_DIAGNOSIS_TEMPLATE_ID_EXT = "2017-08-01";
    public static String PRINCIPAL_DIAGNOSIS_CODE = "8319008";
    public static String PRINCIPAL_DIAGNOSIS_DISPLAY = "Principal Diagnosis";

    // Problem Related Information
    public static String PROB_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.5.1";
    public static String PROB_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String PROB_SEC_CODE = "11450-4";
    public static String PROB_SEC_NAME = "PROBLEM LIST";
    public static String PROB_SEC_TITLE = "PROBLEMS - DIAGNOSES";
    public static String PROB_CONCERN_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.3";
    public static String PROB_CONCERN_TEMPLATE_ID_EXT = "2015-08-01";
    public static String PROB_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.4";
    public static String PROB_OBS_TEMPALTE_ID_EXT = "2015-08-01";
    public static String PROB_CONC_ACT_CODE = "CONC";
    public static String PROB_CONC_ACT_NAME = "Concern";
    public static String PROB_OBS_CODE = "55607006";
    public static String PROB_OBS_CODE_NAME = "Problem";
    public static String DIAGNOSIS_SNOMED = "282291009";
    public static String DIAGNOSIS_LOINC = "29308-4";
    public static String SYMPTOMS_SNOMED = "418799008";
    public static String SYMPTOPMS_LOINC = "75325-1";
    public static String SYMPTOMS_DISPLAY_NAME = "Symptoms";
    public static String DIAGNOSIS_DISPLAY_NAME = "Diagnosis";
    public static String QRDA_DIAG_CONCERN_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.137";
    public static String QRDA_DIAG_CONCERN_ACT_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_SYMPTOM_CONCERN_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.138";
    public static String QRDA_SYMPTOM_CONCERN_ACT_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_DIAG_ACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.11";
    public static String QRDA_DIAG_INACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.13";
    public static String QRDA_DIAG_RESOLVED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.14";
    public static String QRDA_DIAGNOSIS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.135";
    public static String QRDA_DIAGNOSIS_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_SYMPTOM_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.136";
    public static String QRDA_SYMPTOM_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_DIAG_CODE = "282291009";
    public static String QRDA_DIAG_CODE_NAME = "Diagnosis";
    public static String QRDA_DIAG_LOINC_CODE = "29308-4";
    public static String QRDA_SYMPTOM_CODE = "418799008";
    public static String QRDA_SYMPTOM_CODE_NAME = "Symptom";
    public static String QRDA_SYMPTOM_LOINC_CODE = "75325-1";
    public static String QRDA_PRIORITY_DIAG_CODE = "63161005";
    public static String QRDA_PRIORITY_DIAG_CODE_NAME = "Principal";
    public static String PROB_STATUS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.6";
    public static String QRDA_DIAG_ACT_STATUS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.94";
    public static String QRDA_DIAG_RESOLVED_STATUS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.96";
    public static String STATUS_ACTIVE_CODE = "55561003";
    public static String STATUS_RESOLVED_CODE = "413322009";
    public static String QRDA_DIAG_STATUS_CODE = "33999-4";
    public static String QRDA_DIAG_STATUS_CODE_NAME = "status";
    public static String SEVERITY_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.8";
    public static String SEVERITY_OBS_TEMPLATE_ID_EXT = "2014-06-09";
    public static String SEV_CODE = "SEV";
    public static String SEV_CODE_DISPLAY_NAME = "SeverityObservation";
    public static String ACT_CODE_SYSTEM = "2.16.840.1.113883.5.4";
    public static String ACT_CODE_SYSTEM_NAME = "ActCode";


    // Plan of Care, Intervention Data, Risk Category  
    public static String PLAN_OF_CARE_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.39";
    public static String PLAN_OF_CARE_ACT_TEMPLATE_ID_EXT = "2014-06-09";
    public static String PLAN_OF_CARE_SUBS_ADM_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.42";
    public static String PLAN_OF_CARE_SUBS_ADM_TEMPLATE_ID_EXT = "2014-06-09";
    public static String PLAN_OF_CARE_ACTIVITY_PROC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.41";
    public static String PLAN_OF_CARE_ACTIVITY_PROC_TEMPLATE_ID_EXT = "2014-06-09";
    public static String PLAN_OF_CARE_ACTIVITY_OBSERVATION_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.44";
    public static String PLAN_OF_CARE_ACTIVITY_OBSERVATION_TEMPLATE_ID_EXT = "2014-06-09";
    public static String ASSESSMENT_SCALE_OBSERVATION_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.69";
    public static String ASSESSMENT_RECOMMENDED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.145";
    public static String ASSESSMENT_RECOMMENDED_TEMPLATE_ID_EXT = "2017-08-01";
    public static String ASSESSMENT_PERFORMED_TEMPALTE_ID = "2.16.840.1.113883.10.20.24.3.144";
    public static String ASSESSMENT_PERFORMED_TEMPALTE_ID_EXT = "2017-08-01";
    public static String ASSESSMENT_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.158";
    public static String ASSESSMENT_ORDER_TEMPLATE_ID_EXT = "2018-10-01";
    public static String QRDA_RISK_CATEGORY_ASSESSMENT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.69";
    public static String QRDA_INTERVENTION_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.32";
    public static String QRDA_INTERVENTION_PERF_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_INTERVENTION_PERF_TEMPLATE_ID_EXT_R31 = "2014-12-01";
    public static String QRDA_INTERVENTION_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.31";
    public static String QRDA_INTERVENTION_ORD_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_INTERVENTION_ORD_TEMPLATE_ID_EXT_R31 = "2014-12-01";
    public static String INTERVENTION_SECTION_TEMPLATE_ID = "2.16.840.1.113883.10.20.21.2.3";
    public static String INTERVENTION_SECTION_TEMPLATE_EXT = "2015-08-01";
    public static String INTERVENTION_SECTION_CODE = "62387-6";
    public static String INTERVENTION_SECTION_CODE_DISPLAY_NAME = "Interventions Provided";
    public static String INTERVENTION_SECTION_TITLE = "Interventions";
    public static String NOTES_SECTION_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.65";
    public static String NOTES_SECTION_TEMPLATE_ID_EXT = "2016-11-01";
    public static String NOTES_SECTION_CODE = "11488-4";
    public static String NOTES_SECTION_CODE_DISPLAY_NAME = "Consultation Note";
    public static String NOTES_SECTION_TITLE = "CONSULTATION NOTES";
    public static String NOTES_CODE_FOR_NCQA = "371530004";
    public static String NOTES_CODE_FOR_NCQA_TEXT = "Clinical consultation report";


    // Procedure Related Information
    public static String PROC_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.7.1";
    public static String PROC_SEC_TEMPLATE_ID_EXT = "2014-06-09";
    public static String PROC_SEC_CODE = "47519-4";
    public static String PROC_SEC_NAME = "History of Procedures";
    public static String PROC_SEC_TITLE = "PROCEDURES";
    public static String PROC_ACTIVITY_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.12";
    public static String PROC_ACTIVITY_ACT_TEMPLATE_ID_EXT = "2014-06-09";
    public static String PROC_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.14";
    public static String PROC_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
    public static String QRDA_DEV_APPLIED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.7";
    public static String QRDA_DEV_APPLIED_TEMPLATE_ID_EXT = "2014-12-01";
    public static String QRDA_DEV_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.9";
    public static String QRDA_DEV_ORDER_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_DEV_APPLIED_CODE = "360030002";
    public static String QRDA_DEV_APPLIED_CODE_DISPLAY = "Application of Device";
    public static String PLANNED_SUPPLY_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.43";
    public static String PLANNED_SUPPLY_ACT_TEMPLATE_ID_EXT = "2014-06-09";
    public static String PATIENT_REFERAL_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.140";
    public static String INTERVENTION_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.131";
    public static String INTERVENTION_ACT_TEMPLATE_ID_EXT = "2015-08-01";
    public static String PLANNED_INTERVENTION_ACT = "2.16.840.1.113883.10.20.22.4.146";
    public static String PLANNED_INTERVENTION_ACT_EXT = "2015-08-01";
    public static String INTERVENTION_ACT_CODE = "362956003";
    public static String INTERVENTION_ACT_CODE_DISPLAY_NAME = "Procedure Intervention Concept";
    public static String NOTES_ACTIVITY_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.202";
    public static String AUTHOR_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.119";
    public static String AUTHOR_NPI_AA = "2.16.840.1.113883.4.6";
    public static String PROVIDER_SPECIALTY_CODE_SYSTEM = "2.16.840.1.113883.6.101";
    public static String PROVIDER_SPECIALTY_CODE_SYSTEM_NAME = "NUCC";
    public static String PROC_ACT_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.13";
    public static String PROC_ACT_OBS_TEMPLATE_ID_EXT = "2014-06-09";
    public static String QRDA_PHYS_EXAM_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.59";
    public static String QRDA_PHYS_EXAM_PERF_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_PHYS_EXAM_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.58";
    public static String QRDA_PHYS_EXAM_ORD_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_PHYS_EXAM_PERF_TEMPLATE_ID_EXT_R31 = "2014-12-01";
    public static String QRDA_PHYS_EXAM_COMP_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.149";
    public static String QRDA_PHYS_EXAM_COMP_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_PHYS_EXAM_CODE = "29545-1";
    public static String QRDA_PHYS_EXAM_CODE_DISPLAY_NAME = "Physical Exam";
    public static String QRDA_PHYS_EXAM_VALUESET = "2.16.840.1.113883.3.88.12.80.62";
    public static String QRDA_PROC_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.64";
    public static String QRDA_PROC_PERF_TEMPLATE_ID_EXT = "2018-10-01";
    public static String QRDA_PROC_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.63";
    public static String QRDA_PROC_ORD_TEMPLATE_ID_EXT = "2018-10-01";
    public static String QRDA_PROC_REC_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.65";
    public static String QRDA_PROC_RESULT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.66";
    public static String QRDA_PROC_INTOL_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.62";
    public static String PRODUCT_INSTANCE_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.37";
    public static String PAP_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.14";
    public static String PAP_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
    public static String NON_MED_ACTIVITY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.50";
    public static String NON_MED_ACTIVITY_TEMPLATE_ID_EXT = "2014-06-09";

    // Medical Equipment Related Information
    public static String MED_EQUIP_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.23";
    public static String MED_EQUIP_SEC_TEMPLATE_ID_EXT = "2014-06-09";
    public static String MED_EQUIP_SEC_CODE = "46264-8";
    public static String MED_EQUIP_SEC_TITLE = "Medical Equipment";

    // Medication Related Information
    public static String MED_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.1.1";
    public static String MED_SEC_TEMPLATE_ID_EXT = "2014-06-09";
    public static String MED_SEC_CODE = "10160-0";
    public static String MED_SEC_NAME = "History of Medication Use";
    public static String MED_SEC_TITLE = "MEDICATIONS";
    public static String MED_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.16";
    public static String MED_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
    public static String MED_SUPPLY_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.17";
    public static String MED_SUPPLY_ORDER_TEMPLATE_ID_EXT = "2014-06-09";
    public static String MED_DISPENSE_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.18";
    public static String MED_DISPENSE_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
    public static String CONSUMABLE_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.23";
    public static String CONSUMABLE_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
    public static String SUPPLY_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.17";
    public static String SUPPLY_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
    public static String QRDA_MED_DISPENSE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.45";
    public static String QRDA_MED_DISPENSE_TEMPLATE_ID_EXT = "2018-10-01";
    public static String QRDA_MED_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.47";
    public static String QRDA_MED_ORDER_TEMPLATE_ID_EXT = "2018-10-01";
    public static String QRDA_MED_ADMINISTERED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.42";
    public static String QRDA_MED_ADMINISTERED_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_MED_ACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.41";
    public static String QRDA_MED_ACTIVE_TEMPLATE_ID_EXT = "2017-08-01";
    public static String DISCHARGE_MED_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.11.1";
    public static String DISCHARGE_MED_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String DISCHARGE_MED_SEC_CODE = "10183-2";
    public static String DISCHARGE_MED_SEC_NAME = "Hospital Dischage Medications";
    public static String DISCHARGE_MED_SEC_TITLE = "Discharge Medications";
    public static String DISCHARGE_MED_SEC_CODE_TRANSLATION = "75311-1";
    public static String DISCHARGE_MED_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.35";
    public static String DISCHARGE_MED_ENTRY_TEMPLATE_ID_EXT = "2016-03-01";
    public static String DISCHARGE_MED_ACT_CODE = "10183-2";
    public static String QRDA_DISCHARGE_MED_ACT_CODE = "75311-1";
    public static String QRDA_DISCHARGE_MED_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.105";
    public static String QRDA_DISCHARGE_MED_ENTRY_TEMPLATE_ID_EXT = "2018-10-01";

    // Lab Results Related Information
    public static String LAB_RESULTS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.3.1";
    public static String LAB_RESULTS_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String LAB_RESULTS_SEC_CODE = "30954-2";
    public static String LAB_RESULTS_SEC_NAME = "RESULTS";
    public static String LAB_RESULTS_SEC_TITLE = "RESULTS";
    public static String LAB_RESULTS_ORG_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.1";
    public static String LAB_RESULTS_ORG_TEMPLATE_ID_EXT = "2015-08-01";
    public static String LAB_RESULTS_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.2";
    public static String LAB_RESULTS_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
    public static String QRDA_LAB_RESULTS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.40";
    public static String QRDA_LAB_TEST_PERFORMED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.38";
    public static String QRDA_LAB_TEST_PERFORMED_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_LAB_TEST_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.37";
    public static String QRDA_LAB_TEST_ORD_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_DIAG_STUDY_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.17";
    public static String QRDA_DIAG_STUDY_ORD_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_DIAG_STUDY_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.18";
    public static String QRDA_DIAG_STUDY_PERF_TEMPLATE_ID_EXT = "2017-08-01";
    public static String QRDA_DIAG_STUDY_RESULT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.20";

    // Vital Signs Related Information
    public static String VITAL_SIGNS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.4.1";
    public static String VITAL_SIGNS_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String VITAL_SIGNS_SEC_CODE = "8716-3";
    public static String VITAL_SIGNS_SEC_NAME = "Vital Signs";
    public static String VITAL_SIGNS_SEC_TITLE = "Vital Signs";
    public static String VITAL_SIGNS_ORG_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.26";
    public static String VITAL_SIGNS_ORG_TEMPLATE_ID_EXT = "2015-08-01";
    public static String VITAL_SIGNS_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.27";
    public static String VITAL_SIGNS_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
    public static String VITAL_SIGNS_ORG_CODE = "46680005";
    public static String VITAL_SIGNS_ORG_CODE_LOINC = "74728-7";
    public static String VITAL_SIGNS_ORG_CODE_NAME = "Vital Signs";

    // Allergies Related Information
    public static String ALLERGIES_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.6.1";
    public static String ALLERGIES_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String ALLERGIES_SEC_CODE = "48765-2";
    public static String ALLERGIES_SEC_NAME = "Allergies";
    public static String ALLERGIES_SEC_TITLE = "ALLERGIES, ADVERSE EVENTS and ALERTS";
    public static String ALLERGY_CONCERN_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.30";
    public static String ALLERGY_CONCERN_TEMPLATE_ID_EXT = "2015-08-01";
    public static String ALLERGY_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.7";
    public static String ALLERGY_OBS_TEMPLATE_ID_EXT = "2014-06-09";
    public static String ALLERGY_CONC_ACT_CODE = "CONC";
    public static String ALLERGY_CONC_ACT_NAME = "Concern";
    public static String ALLERGY_OBS_ASSERTION = "ASSERTION";
    public static String ALLERGY_OBS_ASSERTION_CODESYSTEM = "2.16.840.1.113883.5.4";
    public static String MED_ALLERGY_CODE = "419511003";
    public static String MED_ALLERGY_CODE_DISPLAY_NAME = "Propensity to adverse reaction to drug";
    public static String QRDA_MED_ALLERGY_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.44";
    public static String QRDA_MED_ALLERGY_ENTRY_TEXT = "Medication Allergy";
    public static String QRDA_MED_ALLERGY_CODE = "419199007";
    public static String QRDA_MED_ALLERGY_CODE_DISPLAY_NAME = "Allergy to Substance";
    public static String QRDA_ALLERGY_INTOLERANCE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.147";
    public static String QRDA_ALLERGY_INTOLERANCE_TEMPLATE_ID_EXT = "2017-08-01";


    // Plan Of Care Related Information
    public static String CAREPLAN_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.10";
    public static String CAREPLAN_SEC_TEMPLATE_ID_EXT = "2014-06-09";
    public static String CAREPLAN_SEC_CODE = "18776-5";
    public static String CAREPLAN_SEC_NAME = "Treatment Plan";
    public static String CAREPLAN_SEC_TITLE = "CARE PLAN";

    // Immunizations Related Information
    public static String IMMUNIZATION_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.2.1";
    public static String IMMUNIZATION_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String IMMUNIZATION_SEC_CODE = "11369-6";
    public static String IMMUNIZATION_SEC_NAME = "History of immunizations";
    public static String IMMUNIZATION_SEC_TITLE = "IMMUNIZATIONS";
    public static String IMMUNIZATION_ACTIVITY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.52";
    public static String IMMUNIZATION_ACTIVITY_TEMPLATE_ID_EXT = "2015-08-01";
    public static String IMMUNIZATION_MEDICATION_INFORMATION = "2.16.840.1.113883.10.20.22.4.54";
    public static String IMMUNIZATION_MEDICATION_INFORMATION_EXT = "2014-06-09";
    public static String IMMUNIZATION_ADMINISTERED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.140";
    public static String IMMUNIZATION_ADMINISTERED_TEMPLATE_ID_EXT = "2017-08-01";

    // SOCIAL History Related Information
    public static String SOC_HISTORY_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.17";
    public static String SOC_HISTORY_SEC_CODE = "29762-2";
    public static String SOC_HISTORY_SEC_NAME = "Social History";
    public static String SOC_HISTORY_SEC_TITLE = "SOCIAL HISTORY";
    public static String SOC_HISTORY_SEC_TEMPLATE_ID_EXT = "2015-08-01";

    // Payer Related Information.
    public static String PAYERS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.18";
    public static String PAYERS_SEC_TEMPLATE_ID_EXT = "2015-08-01";
    public static String PAYERS_SEC_CODE = "48768-6";
    public static String PAYERS_SEC_NAME = "Payer";
    public static String PAYERS_SEC_TITLE = "INSURANCE PROVIDERS";
    public static String COVERAGE_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.60";
    public static String COVERAGE_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
    public static String POLICY_ACTIVITY_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.61";
    public static String POLICY_ACTIVITY_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
    public static String POLICY_PERF_TYPE = "PRF";
    public static String POLICY_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.87";
    public static String COV_TYPE_CODE = "COV";
    public static String COV_PARTY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.89";
    public static String PART_ROLE_CLASS_CODE = "PAT";
    public static String PARTICIPANT_ROLE_CODE = "SELF";
    public static String PARTICIPANT_ROLE_CODE_SYSTEM = "2.16.840.1.113883.5.111";
    public static String PARTICIPANT_ROLE_CODE_SYSTEM_NAME = "RoleCode";
    public static String PARTICIPANT_ROLE_CODE_DISPLAY_NAME = "Self";
    public static String AUTH_ACTIVITY_TEMPLATE_ID = "2.16.840.1.113883.10.20.1.19";
    public static String AUTH_PROC_MOOD_CODE = "PRMS";

    // Functional and Cognitive History Related Information
    public static String FUNC_STATUS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.14";
    public static String FUNC_STATUS_SEC_CODE = "47420-5";
    public static String FUNC_STATUS_SEC_NAME = "Functional and Cognitive Status";
    public static String FUNC_STATUS_SEC_TITLE = "FUNCTIONAL and COGNITIVE STATUS";
    public static String FUNC_STATUS_SEC_TEMPALTE_ID_EXT = "2014-06-09";
    public static String ASSESSMENT_SCALE_OBSERVATION = "2.16.840.1.113883.10.20.22.4.69";

    // Author Function Code
    public static String AUTH_FUNC_CODE = "PCP";
    public static String AUTH_FUNC_CODE_NAME = "Primary Care Provider";
    public static String AUTH_FUNC_CODE_SYSTEM = "2.16.840.1.113883.5.88";
    public static String AUTH_FUNC_CODE_SYSTEM_NAME = "Provider Role";
    public static String AUTH_FUNC_CODE_ORIG_TEXT = "Primary Care Provider";
    public static String AUTH_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.155";
    public static String AUTH_TEMPLATE_ID_EXT = "2017-08-01";

    // NCQA Related
    public static String NCQA_OID = "2.16.840.1.113883.3.464.1005.1";
    public static String NCQA_OID_EXT = "001";

    // CodeSystems
    public static String LOINC_CODESYSTEM_OID = "2.16.840.1.113883.6.1";
    public static String LOINC_CODESYSTEM_NAME = "LOINC";
    public static String CONFIDENTIALITY_CODE_SYTEM = "2.16.840.1.113883.5.25";
    public static String ADMIN_GEN_CODE_SYSTEM = "2.16.840.1.113883.5.1";
    public static String ADMIN_GEN_CODE_SYSTEM_NAME = "HL7AdministrativeGenderCode";
    public static String RACE_CODE_SYSTEM = "2.16.840.1.113883.6.238";
    public static String RACE_CODE_SYSTEM_NAME = "Race &amp; Ethnicity - CDC";
    public static String CPT_CODESYSTEM_OID = "2.16.840.1.113883.6.12";
    public static String CPT_CODESYSTEM_NAME = "CPT-4";
    public static String SNOMED_CODESYSTEM_OID = "2.16.840.1.113883.6.96";
    public static String SNOMED_CODESYSTEM_NAME = "SNOMED-CT";
    public static String HCPCS_CODESYSTEM_OID = "2.16.840.1.113883.6.285";
    public static String HCPCS_CODESYSTEM_NAME = "HCPCS";
    public static String ICD9_CM_CODESYSTEM_OID = "2.16.840.1.113883.6.103";
    public static String ICD9_CM_CODESYSTEM_NAME = "ICD-9";
    public static String ICD10_CM_CODESYSTEM_OID = "2.16.840.1.113883.6.90";
    public static String ICD10_CM_CODESYSTEM_NAME = "ICD-10";
    public static String ICD10_PCS_CODESYSTEM_OID = "2.16.840.1.113883.6.4";
    public static String ICD10_PCS_CODESYSTEM_NAME = "ICD10-PCS";
    public static String LOCAL_CODESYSTEM_NAME = "LOCAL_CODE_SYSTEM";
    public static String HL7_ACT_CLASS_OID = "2.16.840.1.113883.5.6";
    public static String HL7_ACT_CLASS_NAME = "HL7ActClass";
    public static String RXNORM_CODESYSTEM_OID = "2.16.840.1.113883.6.88";
    public static String RXNORM_CODESYSTEM_NAME = "RxNorm";
    public static String CVX_CODESYSTEM_OID = "2.16.840.1.113883.12.292";
    public static String CVX_CODESYSTEM_NAME = "CVX";
    public static String CDT_CODESYSTEM_OID = "2.16.840.1.113883.6.13";
    public static String CDT_CODESYSTEM_NAME = "CDT";
    public static String HL7_DISCHARGE_CODE_SYSTEM_OID = "2.16.840.1.113883.12.112";
    public static String HL7_DISCHARGE_CODE_SYSTEM_NAME = "HL7 Discharge Disposition";
    public static String FDA_DEVICE_ID_AA = "2.16.840.1.113883.3.3719";
    public static String NUBC_CODE_SYSTEM_OID = "2.16.840.1.113883.6.301";
    public static String NUBC_CODE_SYSTEM_DISPLAY_NAME = "National Uniform Billing Committee (NUBC)";
    public static String HL7_SERVICE_LOCATION_CODESYSTEM = "2.16.840.1.113883.6.259";
    public static String HL7_SERVICE_LOCATION_CODESYSTEM_NAME = "HL7 Healthcare Service Location";


    // Value Sets
    public static String CONFIDENTIALITY_CODE = "N";
    public static String LANGUAGE_CODE = "en-US";
    public static String CMS_LANG_CODE = "en";
    public static String DEF_COUNTRY = "US";
    public static String PAT_LANGUAGE = "en";
    public static String CCDA_TRUE = "true";
    public static String CCDA_FALSE = "false";
    public static String PIVL_TS_OPERATOR_VAL = "A";
    public static String SIGNATURE_CODE_VAL = "S";

    // Status Codes
    public static String ACTIVE_STATUS = "active";
    public static String COMPLETED_STATUS = "completed";
    public static String NEW_STATUS = "new";
    public static String RESOLVED_STATUS = "resolved";
    public static String TABLE_RESOLVED_STATUS = "Resolved";
    public static String TABLE_ACTIVE_STATUS = "Active";

    // Default Attribute Values
    public static String TYPE_CODE_DEF = "DRIV";
    public static String MOOD_CODE_DEF = "EVN";
    public static String MOOD_CODE_INT = "INT";
    public static String MOOD_CODE_RQO = "RQO";
    public static String DOC_CLASS_CODE = "DOC";
    public static String DEV_CLASS_CODE = "DEV";
    public static String ENC_CLASS_CODE = "ENC";
    public static String ENC_CLASS_CODE_NAME = "Encounter";
    public static String MED_CLASS_CODE = "SBADM";
    public static String OBS_CLASS_CODE = "OBS";
    public static String SUPPLY_CLASS_CODE = "SPLY";
    public static String PROC_CLASS_CODE = "PROC";
    public static String PCPR_CLASS_CODE = "PCPR";
    public static String ACT_CLASS_CODE = "ACT";
    public static String ORGANIZER_CLASS_CODE_CLUSTER = "CLUSTER";
    public static String ENTRY_REL_SUBJ_CODE = "SUBJ";
    public static String MANU_CLASS_CODE = "MANU";
    public static String MANU_MATERIAL_CLASS_CODE = "MMAT";
    public static String SDLOC_CLASS_CODE = "SDLOC";
    public static String ENTRY_REL_REFR_CODE = "REFR";
    public static String ENTRY_REL_RSON_CODE = "RSON";
    public static String ENTRY_REL_COMP_CODE = "COMP";
    public static String TYPE_CODE_REFR = "REFR";
    public static String TYPE_CODE_DEV = "DEV";
    public static String TYPE_CODE_RSON = "RSON";
    public static String TYPE_CODE_SUBJ = "SUBJ";
    public static String TYPE_CODE_CONS = "CSM";
    public static String TYPE_CODE_LOC = "LOC";
    public static String TYPE_CODE_PRD = "PRD";

    // Element Names
    public static String ENTRY_REL_NAME = "entryRelationship";
    public static String CODE_EL_NAME = "code";
    public static String TARGET_SITE_CODE_EL_NAME = "targetSiteCode";
    public static String QUALIFIER_EL_NAME = "qualifier";
    public static String METHOD_CODE_EL_NAME = "methodCode";
    public static String TITLE_EL_NAME = "title";
    public static String SET_ID_EL_NAME = "setId";
    public static String CONFIDENTIALITY_EL_NAME = "confidentialityCode";
    public static String LANGUAGE_CODE_EL_NAME = "languageCode";
    public static String RECORD_TARGET_EL_NAME = "recordTarget";
    public static String PATIENT_ROLE_EL_NAME = "patientRole";
    public static String ADDR_EL_NAME = "addr";
    public static String ST_ADDR_LINE_EL_NAME = "streetAddressLine";
    public static String CITY_EL_NAME = "city";
    public static String STATE_EL_NAME = "state";
    public static String COUNTRY_EL_NAME = "country";
    public static String POSTAL_CODE_EL_NAME = "postalCode";
    public static String TELECOM_EL_NAME = "telecom";
    public static String PATIENT_EL_NAME = "patient";
    public static String NAME_EL_NAME = "name";
    public static String FIRST_NAME_EL_NAME = "given";
    public static String LAST_NAME_EL_NAME = "family";
    public static String ADMIN_GENDER_CODE_EL_NAME = "administrativeGenderCode";
    public static String BIRTH_TIME_EL_NAME = "birthTime";
    public static String MARITAL_STATUS_CODE_EL_NAME = "maritalStatusCode";
    public static String RELIGION_CODE_EL_NAME = "religiousAffiliationCode";
    public static String RACE_CODE_EL_NAME = "raceCode";
    public static String ETHNIC_CODE_EL_NAME = "ethnicGroupCode";
    public static String TEL_EL_NAME = "telecom";
    public static String EFF_TIME_EL_NAME = "effectiveTime";
    public static String LANGUAGE_COMM_EL_NAME = "languageCommunication";
    public static String AUTHOR_EL_NAME = "author";
    public static String ASSIGNED_AUTHOR_EL_NAME = "assignedAuthor";
    public static String TIME_EL_NAME = "time";
    public static String FUNCTION_CODE_EL_NAME = "functionCode";
    public static String COMP_EL_NAME = "component";
    public static String STRUC_BODY_EL_NAME = "structuredBody";
    public static String SECTION_EL_NAME = "section";
    public static String ENTRY_EL_NAME = "entry";
    public static String OBS_EL_NAME = "observation";
    public static String ENTRY_REL_EL_NAME = "entryRelationship";
    public static String TEXT_EL_NAME = "text";
    public static String ORIGINAL_TEXT_EL_NAME = "originalText";
    public static String TIME_LOW_EL_NAME = "low";
    public static String TIME_HIGH_EL_NAME = "high";
    public static String STATUS_CODE_EL_NAME = "statusCode";
    public static String VAL_EL_NAME = "value";
    public static String TRANSLATION_EL_NAME = "translation";
    public static String ORGANIZER_EL_NAME = "organizer";
    public static String CONSUMABLE_EL_NAME = "consumable";
    public static String MAN_PROD_EL_NAME = "manufacturedProduct";
    public static String PROD_EL_NAME = "product";
    public static String MANU_MAT_EL_NAME = "manufacturedMaterial";
    public static String LOT_NUM_TEXT = "lotNumberText";
    public static String PERF_EL_NAME = "performer";
    public static String ASSIGNED_ENTITY_EL_NAME = "assignedEntity";
    public static String ASSOCIATED_ENTITY_EL_NAME = "associatedEntity";
    public static String PARTICIPANT_EL_NAME = "participant";
    public static String PARTICIPANT_ROLE_EL_NAME = "participantRole";
    public static String TABLE_EL_NAME = "table";
    public static String TABLE_HEAD_EL_NAME = "thead";
    public static String TABLE_BODY_EL_NAME = "tbody";
    public static String TABLE_ROW_EL_NAME = "tr";
    public static String TABLE_HEAD_CONTENT_EL_NAME = "th";
    public static String TABLE_BODY_ROW_EL_NAME = "td";
    public static String TABLE_BODY_CONTENT_SUB_EL_NAME = "content";
    public static String TABLE_BODY_CONTENT_ID_EL_NAME = "ID";
    public static String LEGAL_AUTH_EL_NAME = "legalAuthenticator";
    public static String SIGNATURE_EL_NAME = "signatureCode";
    public static String REFR_EL_NAME = "reference";
    public static String ASSIGNED_AUTHORING_DEVICE_EL_NAME = "assignedAuthoringDevice";
    public static String SOFTWARE_NAME_EL_NAME = "softwareName";
    public static String MANU_MODEL_NAME_EL_NAME = "manufacturerModelName";
    public static String CUSTODIAN_EL_NAME = "custodian";
    public static String ASSGND_CUST_EL_NAME = "assignedCustodian";
    public static String REP_CUST_ORG_EL_NAME = "representedCustodianOrganization";
    public static String ASSIGNED_PERSON_EL_NAME = "assignedPerson";
    public static String REP_ORG_EL_NAME = "representedOrganization";
    public static String PRIORITY_CODE_EL_NAME = "priorityCode";
    public static String INFORMATION_RECIPIENT_EL_NAME = "informationRecipient";
    public static String INTENDED_RECIPIENT_EL_NAME = "intendedRecipient";
    public static String DOSE_QUANTITY_EL_NAME = "doseQuantity";
    public static String SDTC_DECEASED_IND = "sdtc:deceasedInd";
    public static String SDTC_DECEASED_TIME = "sdtc:deceasedTime";
    public static String SDTC_DISCHARGE_DISPOSITION = "sdtc:dischargeDispositionCode";
    public static String PLAYING_DEVICE = "playingDevice";
    public static String SCOPING_ENTITY = "scopingEntity";
    public static String PLAYING_ENTITY = "playingEntity";

    // Acts
    public static String ENC_ACT_EL_NAME = "encounter";
    public static String OBS_ACT_EL_NAME = "observation";
    public static String MED_ACT_EL_NAME = "substanceAdministration";
    public static String PROC_ACT_EL_NAME = "procedure";
    public static String ACT_EL_NAME = "act";
    public static String SUPPLY_ACT_EL_NAME = "supply";
    public static String EXT_DOC_ACT_EL_NAME = "externalDocument";
    public static String EXT_OBS_ACT_EL_NAME = "externalObservation";
    public static String EXT_ACT_EL_NAME = "externalAct";

    // ATTRIBUTE NAMES
    public static String TYPECODE_ATTR_NAME = "typeCode";
    public static String CLASSCODE_ATTR_NAME = "classCode";
    public static String MOODCODE_ATTR_NAME = "moodCode";
    public static String NEG_IND_ATTR_NAME = "negationInd";
    public static String INV_IND_ATTR_NAME = "inversionInd";
    public static String INSTITUTION_SPECIFIED_ATTR_NAME = "institutionSpecified";
    public static String OPERATOR_NAME = "operator";
    public static String PERIOD_NAME = "period";
    public static String HOURS_UNITS_NAME = "h";
    public static String NULL_FLAVOR_NAME = "nullFlavor";
    public static String TABLE_BORDER_ATTR_NAME = "border";
    public static String TABLE_WIDTH_ATTR_NAME = "width";

    // Null Flavors
    public static String NF_NI = "NI";
    public static String NF_NA = "NA";
    public static String NF_UNK = "UNK";
    public static String NF_OTH = "OTH";
    public static String NCQA_PROV = "NCQA Provider";

    // Data Types
    public static String CD_TYPE = "CD";
    public static String PQ_TYPE = "PQ";
    public static String CO_TYPE = "CO";
    public static String PIVL_TS_TYPE = "PIVL_TS";
    public static String IVL_TS_TYPE = "IVL_TS";
    public static String INT_TYPE = "INT";
    public static String ED_TYPE = "ED";

    // Table Values
    public static int TABLE_BORDER = 1;
    public static int TABLE_WIDTH = 100;
    public static String ENC_TABLE_COL_1_TITLE = "Encounter Reason";
    public static String ENC_TABLE_COL_1_BODY_CONTENT = "encounter";
    public static String ENC_TABLE_COL_2_TITLE = "Date of Encounter";
    public static String ENC_TABLE_COL_2_BODY_CONTENT = "encounterDate";
    public static String PROB_TABLE_COL_1_TITLE = "Problem or Diagnosis";
    public static String PROB_TABLE_COL_1_BODY_CONTENT = "problem";
    public static String PROB_TABLE_COL_2_TITLE = "Problem Status";
    public static String PROB_TABLE_COL_2_BODY_CONTENT = "problemStatus";
    public static String ALLERGY_TABLE_COL_1_TITLE = "Allergy Substance";
    public static String ALLERGY_TABLE_COL_1_BODY_CONTENT = "allergySubstance";
    public static String ALLERGY_TABLE_COL_2_TITLE = "Allergy Status";
    public static String ALLERGY_TABLE_COL_2_BODY_CONTENT = "allergyStatus";
    public static String PROC_TABLE_COL_1_TITLE = "Procedure Name";
    public static String PROC_TABLE_COL_1_BODY_CONTENT = "procedure";
    public static String PROC_TABLE_COL_2_TITLE = "Procedure Date";
    public static String PROC_TABLE_COL_2_BODY_CONTENT = "procedureDate";
    public static String INTER_TABLE_COL_1_TITLE = "Intervention Name";
    public static String INTER_TABLE_COL_1_BODY_CONTENT = "intervention";
    public static String INTER_TABLE_COL_2_TITLE = "Intervention Date";
    public static String INTER_TABLE_COL_2_BODY_CONTENT = "interDate";
    public static String REFERAL_TABLE_COL_1_TITLE = "Referal Name";
    public static String REFERAL_TABLE_COL_1_BODY_CONTENT = "referal";
    public static String REFERAL_TABLE_COL_2_TITLE = "Referal Date";
    public static String REFERAL_TABLE_COL_2_BODY_CONTENT = "referalDate";
    public static String VS_TABLE_COL_1_TITLE = "Vital Sign";
    public static String VS_TABLE_COL_1_BODY_CONTENT = "vitalSign";
    public static String VS_TABLE_COL_2_TITLE = "Measured Value";
    public static String VS_TABLE_COL_2_BODY_CONTENT = "vitalSignValue";
    public static String VS_TABLE_COL_3_TITLE = "Measurement Date";
    public static String VS_TABLE_COL_3_BODY_CONTENT = "vitalSignDate";
    public static String DIAGNOSTICSTUDY_TABLE_COL_1_TITLE = "Diagnostic Study";
    public static String DIAGNOSTICSTUDY_COL_1_BODY_CONTENT = "diagnosticStudy";
    public static String LABTEST_TABLE_COL_1_TITLE = "Lab Result";
    public static String LABTEST_TABLE_COL_1_BODY_CONTENT = "labResult";
    public static String LABTEST_TABLE_COL_2_TITLE = "Lab Result Value";
    public static String LABTEST_TABLE_COL_2_BODY_CONTENT = "labResultValue";
    public static String LABTEST_TABLE_COL_3_TITLE = "Lab Result Date";
    public static String LABTEST_TABLE_COL_3_BODY_CONTENT = "labResultDate";
    public static String MED_TABLE_COL_1_TITLE = "Medication Name";
    public static String MED_TABLE_COL_1_BODY_CONTENT = "medication";
    public static String MED_TABLE_COL_2_TITLE = "Medication Start Date";
    public static String MED_TABLE_COL_2_BODY_CONTENT = "medicationDate";
    public static String IMM_TABLE_COL_1_TITLE = "Vaccine Name";
    public static String IMM_TABLE_COL_1_BODY_CONTENT = "vaccine";
    public static String IMM_TABLE_COL_2_TITLE = "Vaccination Date";
    public static String IMM_TABLE_COL_2_BODY_CONTENT = "vaccinationDate";
    public static String FUNC_STATUS_TABLE_COL_1_TITLE = "Functional Status Observation";
    public static String FUNC_STATUS_TABLE_COL_1_BODY_CONTENT = "observation";
    public static String FUNC_STATUS_TABLE_COL_2_TITLE = "Observation Result";
    public static String FUNC_STATUS_TABLE_COL_2_BODY_CONTENT = "observationResult";
    public static String PAYOR_TABLE_COL_1_TITLE = "Insurance Provider";
    public static String PAYOR_TABLE_COL_1_BODY_CONTENT = "payor";
    public static String DEVICE_TABLE_COL_1_TITLE = "Device Name";
    public static String DEVICE_TABLE_COL_1_BODY_CONTENT = "device";
    public static String DEVICE_TABLE_COL_2_TITLE = "Device Date";
    public static String DEVICE_TABLE_COL_2_BODY_CONTENT = "deviceDate";
    public static String UNKNOWN_VALUE = "Unknown";

}
