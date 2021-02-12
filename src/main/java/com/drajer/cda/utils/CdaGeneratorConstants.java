package com.drajer.cda.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Properties;
import org.apache.commons.lang3.StringUtils;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaGeneratorConstants {

  private CdaGeneratorConstants() {
    // not called
  }

  private static final Logger logger = LoggerFactory.getLogger(CdaGeneratorConstants.class);

  // XML Related
  public static final String RIGHT_ANGLE_BRACKET = ">";
  public static final String START_XMLTAG = "<";
  public static final String END_XMLTAG = "/>";
  public static final String END_XMLTAG_NEWLN = "/>" + "\n";
  public static final String FORWARD_SLASH = "/";
  public static final String BACKWARD_SLASH = "\\";
  public static final String DOUBLE_QUOTE = "\"";
  public static final String SPACE = " ";
  public static final String PIPE = "|";
  public static final String COLON = ":";
  public static final String HYPHEN = "-";
  public static final String EQUAL = "=";
  public static final String XSI_TYPE = "xsi:type=";
  public static final String XML_FILE_PATTERN = "*.xml";
  public static final String TYPEID_ROOT = "typeId root=";
  public static final String EXTENSION = "extension=";
  public static final String ID_ROOT = "id root=";
  public static final String CODE_WITH_EQUAL = "code=";
  public static final String CODESYSTEM_WITH_EQUAL = "codeSystem=";
  public static final String CODESYSTEMNAME_WITH_EQUAL = "codeSystemName=";
  public static final String DISPLAYNAME_WITH_EQUAL = "displayName=";
  public static final String VALUE_WITH_EQUAL = "value=";
  public static final String NULLFLAVOR_WITH_EQUAL = "nullFlavor=";
  public static final String UNIT_WITH_EQUAL = "unit=";
  public static final String VALUESET = "sdtc:valueSet=";
  public static final String VALUESET_VERSION = "sdtc:valueSetVersion=";

  // CCDA Header Releated
  public static final String DOC_HEADER_XML = "<?xml version=\"1.0\"?>" + "\n";

  public static final String CLINICAL_DOC_HEADER_XML =
      "<ClinicalDocument xmlns:xsi="
          + DOUBLE_QUOTE
          + "http:"
          + FORWARD_SLASH
          + FORWARD_SLASH
          + "www.w3.org"
          + FORWARD_SLASH
          + "2001"
          + FORWARD_SLASH
          + "XMLSchema-instance"
          + DOUBLE_QUOTE
          + "\n"
          + " xmlns="
          + DOUBLE_QUOTE
          + "urn:hl7-org:v3"
          + DOUBLE_QUOTE
          + "\n"
          + " xmlns:cda="
          + DOUBLE_QUOTE
          + "urn:hl7-org:v3"
          + DOUBLE_QUOTE
          + "\n"
          + " xmlns:sdtc="
          + DOUBLE_QUOTE
          + "urn:hl7-org:sdtc"
          + DOUBLE_QUOTE
          + RIGHT_ANGLE_BRACKET
          + "\n";

  public static final String END_HEADER_CLINICAL_DOC =
      START_XMLTAG + FORWARD_SLASH + "ClinicalDocument" + RIGHT_ANGLE_BRACKET;

  // Template Ids, Section Headers
  // CCDA Header Related Information
  public static final String CDA_DOC_ROOT = "2.16.840.1.113883.1.3";
  public static final String CDA_DOC_EXT = "POCD_HD000040";
  // public static final String US_REALM_HEADER_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.1.1";
  public static final String US_REALM_HEADER_EXT = "2015-08-01";
  public static final String PUBLIC_HEALTH_TEMPLATE_ID = "2.16.840.1.113883.10.20.15.2";
  public static final String PUBLIC_HEALTH_EXT = "2016-12-01";
  public static final String PH_DOC_CODE = "55751-2";
  public static final String PH_DOC_DISPLAY_NAME = "Initial Public Health Case Report";
  // public static final String PH_REPORT_TITLE = "Initial Public Health Case Report";

  // FHIR Types

  // FHIR Value Set URLS and values
  public static final String FHIR_IDTYPE_SYSTEM = "http://terminology.hl7.org/CodeSystem/v2-0203";
  public static final String FHIR_IDENTIFIER_TYPE_SYSTEM =
      "http://hl7.org/fhir/ValueSet/identifier-type";
  public static final String FHIR_ARGO_RACE_EXT_URL =
      "http://fhir.org/guides/argonaut/StructureDefinition/argo-race";
  public static final String FHIR_ARGO_ETHNICITY_EXT_URL =
      "http://fhir.org/guides/argonaut/StructureDefinition/argo-ethnicity";
  public static final String FHIR_ARGO_BIRTHSEX_EXT_URL =
      "http://fhir.org/guides/argonaut/StructureDefinition/argo-birthsex";

  public static final String FHIR_USCORE_RACE_EXT_URL =
      "http://hl7.org/fhir/us/core/StructureDefinition/us-core-race";
  public static final String FHIR_USCORE_ETHNICITY_EXT_URL =
      "http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity";
  public static final String FHIR_USCORE_BIRTHSEX_EXT_URL =
      "http://hl7.org/fhir/us/core/StructureDefinition/us-core-birthsex";

  public static final String OMB_RACE_CATEGORY_URL = "ombCategory";
  public static final String OMB_RACE_DETAILED_URL = "detailed";
  public static final String FHIR_NPI_URL = "http://hl7.org/fhir/sid/us-npi";
  public static final String FHIR_CPT_URL = "http://www.ama-assn.org/go/cpt";
  public static final String FHIR_SNOMED_URL = "http://snomed.info/sct";
  public static final String FHIR_ICD10_CM_URL = "http://hl7.org/fhir/sid/icd-10-cm";
  public static final String FHIR_ICD9_CM_URL = "http://hl7.org/fhir/sid/icd-9-cm";
  public static final String FHIR_LOINC_URL = "http://loinc.org";

  public static final String FHIR_SERVICE_DELIVERY_TYPE_URL =
      "http://terminology.hl7.org/ValueSet/v3-ServiceDeliveryLocationRoleType";
  public static final String FHIR_CONDITION_CATEGORY_URL = "http://hl7.org/fhir/condition-category";
  public static final String FHIR_CONDITION_PROBLEM_CATEGORY = "problem";
  public static final String FHIR_CONDITION_HEALTH_CONCERN_CATEGORY = "health-concern";
  public static final String FHIR_LAB_RESULT_CATEGORY = "laboratory";
  public static final String FHIR_OBSERVATION_CATEGORY_URL =
      "http://hl7.org/fhir/observation-category";
  public static final String FHIR_DIAG_REPORT_CATEGORY = "LAB";
  public static final String FHIR_DIAG_REPORT_CATEGORY_URL =
      "http://hl7.org/fhir/ValueSet/diagnostic-service-sections";
  public static final String FHIR_CVX_URL = "http://hl7.org/fhir/sid/cvx";
  public static final String FHIR_RXNORM_URL = "http://www.nlm.nih.gov/research/umls/rxnorm";
  public static final String FHIR_LANGUAGE_CODESYSTEM_URL = "urn:ietf:bcp:47";
  public static final String FHIR_ENCOUNTER_CLASS_URL =
      "http://terminology.hl7.org/CodeSystem/v3-ActCode";
  public static final String FHIR_PARTICIPANT_TYPE =
      "http://terminology.hl7.org/CodeSystem/participant-type";
  public static final String FHIR_PARTICIPANT_TYPE_V3 =
      "http://terminology.hl7.org/CodeSystem/v3-ParticipationType";
  public static final String FHIR_LOC_ROLE_CODE_TYPE_V3 =
      "http://terminology.hl7.org/CodeSystem/v3-RoleCode";
  public static final String FHIR_CONTAINED_REFERENCE = "#";

  public static final String FHIR_MR_IDTYPE_CODE = "MR";
  public static final String CDA_MALE_CODE = "M";
  public static final String CDA_FEMALE_CODE = "F";
  public static final String CDA_UNK_GENDER = "UN";

  public static final String RCTC_OID = "2.16.840.1.114222.4.11.7508";

  public static final String CCDA_CCD_TEMPLATE_ID1 = "2.16.840.1.113883.10.20.22.1.1";
  public static final String CCDA_CCD_TEMPLATE_ID1_EXT = "2015-08-01";
  public static final String JUNE_2014_RELEASE_EXT = "2014-06-09";
  public static final String CCDA_CCD_TEMPLATE_ID2 = "2.16.840.1.113883.10.20.22.1.2";
  public static final String CCDA_CCD_TEMPLATE_ID2_EXT = "2015-08-01";
  public static final String CCD_DOC_CODE = "34133-9";
  public static final String CCD_DOC_NAME = "Summarization of episode note";
  public static final String QRDA_CATEGORY_I_DOC_CODE = "55182-0";
  public static final String QRDA_CATEGORY_I_DOC_CODE_DISP_NAME = "Quality Measure Report";
  public static final String QRDA_CATEGORY_I_DOC_TITLE = "QRDA Incidence Report";
  public static final String QRDA_CAT_I_TEMPLATEID = "2.16.840.1.113883.10.20.24.1.1";
  public static final String DEC_2014_RELEASE_EXT = "2014-12-01";
  public static final String QDM_QRDA_CAT_I_TEMPLATEID = "2.16.840.1.113883.10.20.24.1.2";
  public static final String CMS_QRDA_CAT_I_TEMPLATEID = "2.16.840.1.113883.10.20.24.1.3";
  public static final String JULY_2015_RELEASE_EXT = "2015-07-01";

  // Reason for Visit and History of Present Illnees
  public static final String REASON_FOR_VISIT_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.12";
  public static final String REASON_FOR_VISIT_SEC_CODE = "29299-5";
  public static final String REASON_FOR_VISIT_SEC_CODE_NAME = "Reason For Visit";
  public static final String REASON_FOR_VISIT_SEC_TITLE = "Reason For Visit";

  public static final String HISTORY_OF_PRESENT_ILLNESS_SEC_TEMPLATE_ID =
      "1.3.6.1.4.1.19376.1.5.3.1.3.4";
  public static final String HISTORY_OF_PRESENT_ILLNESS_SEC_CODE = "10164-2";
  public static final String HISTORY_OF_PRESENT_ILLNESS_SEC_CODE_NAME =
      "History of Present Illness";
  public static final String HISTORY_OF_PRESENT_ILLNESS_SEC_TITLE = "History of Present Illness";

  // Encounter Related Information
  public static final String EO_ENC_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.22";
  public static final String ENC_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.22.1";
  public static final String ENC_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String ENC_SEC_CODE = "46240-8";
  public static final String ENC_SEC_NAME = "History of Encounters";
  public static final String ENC_SEC_TITLE = "ENCOUNTERS";
  public static final String QRDA_ENC_PERF_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.133";
  public static final String QRDA_ENC_PERF_ACT_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String ENC_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.49";
  public static final String ENC_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String ENC_DIAGNOSIS_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.80";
  public static final String ENC_DIAGNOSIS_ACT_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String ENC_DIAGNOSIS_ACT_CODE = "29308-4";
  public static final String ENC_DIAGNOSIS_ACT_CODE_DISPLAY_NAME = "DIAGNOSIS";
  public static final String QRDA_ENC_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.23";
  public static final String QRDA_ENC_PERF_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_ENC_ACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.21";
  public static final String ENC_SDLOC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.32";
  public static final String FACILITY_LOC_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.100";
  public static final String FACILITY_LOC_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String FACILITY_CODE_SYSTEM = "2.16.840.1.113883.5.111";
  public static final String FACILTIY_CODE_SYSTEM_NAME = "HL7RoleCode";
  public static final String PRINCIPAL_DIAGNOSIS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.152";
  public static final String PRINCIPAL_DIAGNOSIS_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String PRINCIPAL_DIAGNOSIS_CODE = "8319008";
  public static final String PRINCIPAL_DIAGNOSIS_DISPLAY = "Principal Diagnosis";

  // Problem Related Information
  public static final String PROB_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.5.1";
  public static final String PROB_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String PROB_SEC_CODE = "11450-4";
  public static final String PROB_SEC_NAME = "PROBLEM LIST";
  public static final String PROB_SEC_TITLE = "PROBLEMS - DIAGNOSES";
  public static final String PROB_CONCERN_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.3";
  public static final String PROB_CONCERN_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String PROB_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.4";
  public static final String PROB_OBS_TEMPALTE_ID_EXT = "2015-08-01";
  public static final String TRIGGER_CODE_PROB_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.15.2.3.3";
  public static final String TRIGGER_CODE_PROB_OBS_TEMPLATE_ID_EXT = "2016-12-01";
  public static final String PROB_CONC_ACT_CODE = "CONC";
  public static final String PROB_CONC_ACT_NAME = "Concern";
  public static final String PROB_OBS_CODE = "55607006";
  public static final String PROB_OBS_CODE_NAME = "Problem";
  public static final String DIAGNOSIS_SNOMED = "282291009";
  public static final String DIAGNOSIS_LOINC = "29308-4";
  public static final String SYMPTOMS_SNOMED = "418799008";
  public static final String SYMPTOPMS_LOINC = "75325-1";
  public static final String SYMPTOMS_DISPLAY_NAME = "Symptoms";
  public static final String DIAGNOSIS_DISPLAY_NAME = "Diagnosis";
  public static final String QRDA_DIAG_CONCERN_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.137";
  public static final String QRDA_DIAG_CONCERN_ACT_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_SYMPTOM_CONCERN_ACT_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.138";
  public static final String QRDA_SYMPTOM_CONCERN_ACT_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_DIAG_ACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.11";
  public static final String QRDA_DIAG_INACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.13";
  public static final String QRDA_DIAG_RESOLVED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.14";
  public static final String QRDA_DIAGNOSIS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.135";
  public static final String QRDA_DIAGNOSIS_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_SYMPTOM_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.136";
  public static final String QRDA_SYMPTOM_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_DIAG_CODE = "282291009";
  public static final String QRDA_DIAG_CODE_NAME = "Diagnosis";
  public static final String QRDA_DIAG_LOINC_CODE = "29308-4";
  public static final String QRDA_SYMPTOM_CODE = "418799008";
  public static final String QRDA_SYMPTOM_CODE_NAME = "Symptom";
  public static final String QRDA_SYMPTOM_LOINC_CODE = "75325-1";
  public static final String QRDA_PRIORITY_DIAG_CODE = "63161005";
  public static final String QRDA_PRIORITY_DIAG_CODE_NAME = "Principal";
  public static final String PROB_STATUS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.6";
  public static final String QRDA_DIAG_ACT_STATUS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.94";
  public static final String QRDA_DIAG_RESOLVED_STATUS_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.96";
  public static final String STATUS_ACTIVE_CODE = "55561003";
  public static final String STATUS_RESOLVED_CODE = "413322009";
  public static final String QRDA_DIAG_STATUS_CODE = "33999-4";
  public static final String QRDA_DIAG_STATUS_CODE_NAME = "status";
  public static final String SEVERITY_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.8";
  public static final String SEVERITY_OBS_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String SEV_CODE = "SEV";
  public static final String SEV_CODE_DISPLAY_NAME = "SeverityObservation";
  public static final String ACT_CODE_SYSTEM = "2.16.840.1.113883.5.4";
  public static final String ACT_CODE_SYSTEM_NAME = "ActCode";

  // Plan of Care, Intervention Data, Risk Category
  public static final String PLAN_OF_CARE_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.39";
  public static final String PLAN_OF_CARE_ACT_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String PLAN_OF_CARE_SUBS_ADM_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.42";
  public static final String PLAN_OF_CARE_SUBS_ADM_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String PLAN_OF_CARE_ACTIVITY_PROC_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.22.4.41";
  public static final String PLAN_OF_CARE_ACTIVITY_PROC_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String PLAN_OF_CARE_ACTIVITY_OBSERVATION_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.22.4.44";
  public static final String PLAN_OF_CARE_ACTIVITY_OBSERVATION_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String ASSESSMENT_SCALE_OBSERVATION_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.22.4.69";
  public static final String ASSESSMENT_RECOMMENDED_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.145";
  public static final String ASSESSMENT_RECOMMENDED_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String ASSESSMENT_PERFORMED_TEMPALTE_ID = "2.16.840.1.113883.10.20.24.3.144";
  public static final String ASSESSMENT_PERFORMED_TEMPALTE_ID_EXT = "2017-08-01";
  public static final String ASSESSMENT_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.158";
  public static final String ASSESSMENT_ORDER_TEMPLATE_ID_EXT = "2018-10-01";
  public static final String QRDA_RISK_CATEGORY_ASSESSMENT_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.69";
  public static final String QRDA_INTERVENTION_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.32";
  public static final String QRDA_INTERVENTION_PERF_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_INTERVENTION_PERF_TEMPLATE_ID_EXT_R31 = "2014-12-01";
  public static final String QRDA_INTERVENTION_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.31";
  public static final String QRDA_INTERVENTION_ORD_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_INTERVENTION_ORD_TEMPLATE_ID_EXT_R31 = "2014-12-01";
  public static final String INTERVENTION_SECTION_TEMPLATE_ID = "2.16.840.1.113883.10.20.21.2.3";
  public static final String INTERVENTION_SECTION_TEMPLATE_EXT = "2015-08-01";
  public static final String INTERVENTION_SECTION_CODE = "62387-6";
  public static final String INTERVENTION_SECTION_CODE_DISPLAY_NAME = "Interventions Provided";
  public static final String INTERVENTION_SECTION_TITLE = "Interventions";
  public static final String NOTES_SECTION_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.65";
  public static final String NOTES_SECTION_TEMPLATE_ID_EXT = "2016-11-01";
  public static final String NOTES_SECTION_CODE = "11488-4";
  public static final String NOTES_SECTION_CODE_DISPLAY_NAME = "Consultation Note";
  public static final String NOTES_SECTION_TITLE = "CONSULTATION NOTES";

  // Procedure Related Information
  public static final String PROC_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.7.1";
  public static final String PROC_SEC_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String PROC_SEC_CODE = "47519-4";
  public static final String PROC_SEC_NAME = "History of Procedures";
  public static final String PROC_SEC_TITLE = "PROCEDURES";
  public static final String PROC_ACTIVITY_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.12";
  public static final String PROC_ACTIVITY_ACT_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String PROC_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.14";
  public static final String PROC_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String QRDA_DEV_APPLIED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.7";
  public static final String QRDA_DEV_APPLIED_TEMPLATE_ID_EXT = "2014-12-01";
  public static final String QRDA_DEV_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.9";
  public static final String QRDA_DEV_ORDER_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_DEV_APPLIED_CODE = "360030002";
  public static final String QRDA_DEV_APPLIED_CODE_DISPLAY = "Application of Device";
  public static final String PLANNED_SUPPLY_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.43";
  public static final String PLANNED_SUPPLY_ACT_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String PATIENT_REFERAL_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.140";
  public static final String INTERVENTION_ACT_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.131";
  public static final String INTERVENTION_ACT_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String PLANNED_INTERVENTION_ACT = "2.16.840.1.113883.10.20.22.4.146";
  public static final String PLANNED_INTERVENTION_ACT_EXT = "2015-08-01";
  public static final String INTERVENTION_ACT_CODE = "362956003";
  public static final String INTERVENTION_ACT_CODE_DISPLAY_NAME = "Procedure Intervention Concept";
  public static final String NOTES_ACTIVITY_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.202";
  public static final String AUTHOR_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.119";
  public static final String AUTHOR_NPI_AA = "2.16.840.1.113883.4.6";
  public static final String PROVIDER_SPECIALTY_CODE_SYSTEM = "2.16.840.1.113883.6.101";
  public static final String PROVIDER_SPECIALTY_CODE_SYSTEM_NAME = "NUCC";
  public static final String PROC_ACT_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.13";
  public static final String PROC_ACT_OBS_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String QRDA_PHYS_EXAM_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.59";
  public static final String QRDA_PHYS_EXAM_PERF_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_PHYS_EXAM_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.58";
  public static final String QRDA_PHYS_EXAM_ORD_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_PHYS_EXAM_PERF_TEMPLATE_ID_EXT_R31 = "2014-12-01";
  public static final String QRDA_PHYS_EXAM_COMP_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.149";
  public static final String QRDA_PHYS_EXAM_COMP_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_PHYS_EXAM_CODE = "29545-1";
  public static final String QRDA_PHYS_EXAM_CODE_DISPLAY_NAME = "Physical Exam";
  public static final String QRDA_PHYS_EXAM_VALUESET = "2.16.840.1.113883.3.88.12.80.62";
  public static final String QRDA_PROC_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.64";
  public static final String QRDA_PROC_PERF_TEMPLATE_ID_EXT = "2018-10-01";
  public static final String QRDA_PROC_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.63";
  public static final String QRDA_PROC_ORD_TEMPLATE_ID_EXT = "2018-10-01";
  public static final String QRDA_PROC_REC_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.65";
  public static final String QRDA_PROC_RESULT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.66";
  public static final String QRDA_PROC_INTOL_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.62";
  public static final String PRODUCT_INSTANCE_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.37";
  public static final String PAP_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.14";
  public static final String PAP_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String NON_MED_ACTIVITY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.50";
  public static final String NON_MED_ACTIVITY_TEMPLATE_ID_EXT = "2014-06-09";

  // Medical Equipment Related Information
  public static final String MED_EQUIP_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.23";
  public static final String MED_EQUIP_SEC_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String MED_EQUIP_SEC_CODE = "46264-8";
  public static final String MED_EQUIP_SEC_TITLE = "Medical Equipment";

  // Medication Related Information
  public static final String MED_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.1.1";
  public static final String MED_SEC_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String MED_SEC_CODE = "10160-0";
  public static final String MED_SEC_NAME = "History of Medication Use";
  public static final String MED_SEC_TITLE = "MEDICATIONS";
  public static final String MED_ADM_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.38";
  public static final String MED_ADM_SEC_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String MED_ADM_SEC_CODE = "29549-3";
  public static final String MED_ADM_SEC_TITLE = "Medications Administered";
  public static final String MED_ADM_SEC_NAME = "Medications Administered";
  public static final String MED_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.16";
  public static final String MED_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String MED_SUPPLY_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.17";
  public static final String MED_SUPPLY_ORDER_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String MED_DISPENSE_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.18";
  public static final String MED_DISPENSE_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String CONSUMABLE_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.23";
  public static final String CONSUMABLE_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String SUPPLY_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.17";
  public static final String SUPPLY_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String QRDA_MED_DISPENSE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.45";
  public static final String QRDA_MED_DISPENSE_TEMPLATE_ID_EXT = "2018-10-01";
  public static final String QRDA_MED_ORDER_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.47";
  public static final String QRDA_MED_ORDER_TEMPLATE_ID_EXT = "2018-10-01";
  public static final String QRDA_MED_ADMINISTERED_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.42";
  public static final String QRDA_MED_ADMINISTERED_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_MED_ACTIVE_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.41";
  public static final String QRDA_MED_ACTIVE_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String DISCHARGE_MED_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.11.1";
  public static final String DISCHARGE_MED_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String DISCHARGE_MED_SEC_CODE = "10183-2";
  public static final String DISCHARGE_MED_SEC_NAME = "Hospital Dischage Medications";
  public static final String DISCHARGE_MED_SEC_TITLE = "Discharge Medications";
  public static final String DISCHARGE_MED_SEC_CODE_TRANSLATION = "75311-1";
  public static final String DISCHARGE_MED_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.35";
  public static final String DISCHARGE_MED_ENTRY_TEMPLATE_ID_EXT = "2016-03-01";
  public static final String DISCHARGE_MED_ACT_CODE = "10183-2";
  public static final String QRDA_DISCHARGE_MED_ACT_CODE = "75311-1";
  public static final String QRDA_DISCHARGE_MED_ENTRY_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.105";
  public static final String QRDA_DISCHARGE_MED_ENTRY_TEMPLATE_ID_EXT = "2018-10-01";

  // Lab Results Related Information
  public static final String LAB_RESULTS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.3.1";
  public static final String LAB_RESULTS_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String LAB_RESULTS_SEC_CODE = "30954-2";
  public static final String LAB_RESULTS_SEC_NAME = "RESULTS";
  public static final String LAB_RESULTS_SEC_TITLE = "RESULTS";
  public static final String LAB_RESULTS_ORG_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.1";
  public static final String LAB_RESULTS_ORG_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String LAB_RESULTS_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.2";
  public static final String LAB_RESULTS_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String TRIGGER_CODE_LAB_RESULT_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.15.2.3.2";
  public static final String TRIGGER_CODE_LAB_RESULT_TEMPLATE_ID_EXT = "2016-12-01";
  public static final String QRDA_LAB_RESULTS_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.40";
  public static final String QRDA_LAB_TEST_PERFORMED_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.38";
  public static final String QRDA_LAB_TEST_PERFORMED_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_LAB_TEST_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.37";
  public static final String QRDA_LAB_TEST_ORD_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_DIAG_STUDY_ORD_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.17";
  public static final String QRDA_DIAG_STUDY_ORD_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_DIAG_STUDY_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.18";
  public static final String QRDA_DIAG_STUDY_PERF_TEMPLATE_ID_EXT = "2017-08-01";
  public static final String QRDA_DIAG_STUDY_RESULT_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.20";

  // Vital Signs Related Information
  public static final String VITAL_SIGNS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.4.1";
  public static final String VITAL_SIGNS_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String VITAL_SIGNS_SEC_CODE = "8716-3";
  public static final String VITAL_SIGNS_SEC_NAME = "Vital Signs";
  public static final String VITAL_SIGNS_SEC_TITLE = "Vital Signs";
  public static final String VITAL_SIGNS_ORG_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.26";
  public static final String VITAL_SIGNS_ORG_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String VITAL_SIGNS_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.27";
  public static final String VITAL_SIGNS_ENTRY_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String VITAL_SIGNS_ORG_CODE = "46680005";
  public static final String VITAL_SIGNS_ORG_CODE_LOINC = "74728-7";
  public static final String VITAL_SIGNS_ORG_CODE_NAME = "Vital Signs";

  // Allergies Related Information
  public static final String ALLERGIES_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.6.1";
  public static final String ALLERGIES_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String ALLERGIES_SEC_CODE = "48765-2";
  public static final String ALLERGIES_SEC_NAME = "Allergies";
  public static final String ALLERGIES_SEC_TITLE = "ALLERGIES, ADVERSE EVENTS and ALERTS";
  public static final String ALLERGY_CONCERN_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.30";
  public static final String ALLERGY_CONCERN_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String ALLERGY_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.7";
  public static final String ALLERGY_OBS_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String ALLERGY_CONC_ACT_CODE = "CONC";
  public static final String ALLERGY_CONC_ACT_NAME = "Concern";
  public static final String OBS_ASSERTION = "ASSERTION";
  public static final String OBS_ASSERTION_DISPLAY = "Assertion";
  public static final String OBS_ASSERTION_CODESYSTEM = "2.16.840.1.113883.5.4";
  public static final String OBS_ASSERTION_CODESYSTEM_NAME = "HL7ActCode";
  public static final String MED_ALLERGY_CODE = "419511003";
  public static final String MED_ALLERGY_CODE_DISPLAY_NAME =
      "Propensity to adverse reaction to drug";
  public static final String QRDA_MED_ALLERGY_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.44";
  public static final String QRDA_MED_ALLERGY_ENTRY_TEXT = "Medication Allergy";
  public static final String QRDA_MED_ALLERGY_CODE = "419199007";
  public static final String QRDA_MED_ALLERGY_CODE_DISPLAY_NAME = "Allergy to Substance";
  public static final String QRDA_ALLERGY_INTOLERANCE_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.147";
  public static final String QRDA_ALLERGY_INTOLERANCE_TEMPLATE_ID_EXT = "2017-08-01";

  // Plan Of Care Related Information
  public static final String CAREPLAN_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.10";
  public static final String CAREPLAN_SEC_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String CAREPLAN_SEC_CODE = "18776-5";
  public static final String CAREPLAN_SEC_NAME = "Treatment Plan";
  public static final String CAREPLAN_SEC_TITLE = "Plan of Treatment";

  // Immunizations Related Information
  public static final String IMMUNIZATION_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.2.1";
  public static final String IMMUNIZATION_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String IMMUNIZATION_SEC_CODE = "11369-6";
  public static final String IMMUNIZATION_SEC_NAME = "History of immunizations";
  public static final String IMMUNIZATION_SEC_TITLE = "IMMUNIZATIONS";
  public static final String IMMUNIZATION_ACTIVITY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.52";
  public static final String IMMUNIZATION_ACTIVITY_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String IMMUNIZATION_MEDICATION_INFORMATION =
      "2.16.840.1.113883.10.20.22.4.54";
  public static final String IMMUNIZATION_MEDICATION_INFORMATION_EXT = "2014-06-09";
  public static final String IMMUNIZATION_ADMINISTERED_TEMPLATE_ID =
      "2.16.840.1.113883.10.20.24.3.140";
  public static final String IMMUNIZATION_ADMINISTERED_TEMPLATE_ID_EXT = "2017-08-01";

  // SOCIAL History Related Information
  public static final String SOC_HISTORY_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.17";
  public static final String SOC_HISTORY_SEC_CODE = "29762-2";
  public static final String SOC_HISTORY_SEC_NAME = "Social History";
  public static final String SOC_HISTORY_SEC_TITLE = "SOCIAL HISTORY";
  public static final String SOC_HISTORY_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String SOC_HISTORY_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.38";
  public static final String SOC_HISTORY_OBS_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String SNOMED_OCC_HISTORY_CODE = "364703007";
  public static final String SNOMED_OCC_HISTORY_CODE_DISPLAY = "Employment Detail";
  public static final String LOINC_OCC_HISTORY_CODE = "11295-3";
  public static final String LOINC_OCC_HISTORY_CODE_DISPLAY = "Current employment - Reported";
  public static final String BIRTH_SEX_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.200";
  public static final String BIRTH_SEX_OBS_TEMPLATE_ID_EXT = "2016-06-01";
  public static final String PREGNANCY_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.15.3.8";
  public static final String TRAVEL_HISTORY_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.15.2.3.1";
  public static final String PLANNED_OBS_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.44";
  public static final String PLANNED_OBS_TEMPLATE_ID_EXT = "2014-06-09";
  public static final String LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE =
      "2.16.840.1.113883.10.20.15.2.3.4";
  public static final String LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT = "2016-12-01";
  public static final String LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE =
      "2.16.840.1.113883.10.20.15.2.3.2";
  public static final String LAB_TEST_RESULT_OBSERVATION_TRIGGER_TEMPLATE_EXT = "2016-12-01";
  public static final String TRAVEL_HISTORY_OBS_TEMPLATE_ID_EXT = "2016-12-01";
  public static final String TRAVEL_HISTORY_SNOMED_CODE = "420008001";
  public static final String TRAVEL_HISTORY_SNOMED_CODE_DISPLAY = "Travel";
  public static final String BIRTH_SEX_CODE = "76689-9";
  public static final String BIRTH_SEX_DISPLAY = "Birth Sex";
  public static final String PREGNANCY_CONDITION_DISPLAY = "Pregnancy Condition";
  public static final String PREGNANCY_OBSERVATION_DISPLAY = "Pregnancy Observation";
  public static final String OCCUPATION_HISTORY_DISPLAY = "Occupation History";
  public static final String TRAVEL_HISTORY_DISPLAY = "Travel History";
  public static final String BIRTH_SEX_CODESYSTEM_OID = "2.16.840.1.113883.5.1";
  public static final String BIRTH_SEX_CODESYSTEM_NAME = "Administrative Gender";

  // Payer Related Information.
  public static final String PAYERS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.18";
  public static final String PAYERS_SEC_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String PAYERS_SEC_CODE = "48768-6";
  public static final String PAYERS_SEC_NAME = "Payer";
  public static final String PAYERS_SEC_TITLE = "INSURANCE PROVIDERS";
  public static final String COVERAGE_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.60";
  public static final String COVERAGE_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String POLICY_ACTIVITY_ENTRY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.61";
  public static final String POLICY_ACTIVITY_ENTRY_TEMPLATE_ID_EXT = "2015-08-01";
  public static final String POLICY_PERF_TYPE = "PRF";
  public static final String POLICY_PERF_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.87";
  public static final String COV_TYPE_CODE = "COV";
  public static final String COV_PARTY_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.4.89";
  public static final String PART_ROLE_CLASS_CODE = "PAT";
  public static final String PARTICIPANT_ROLE_CODE = "SELF";
  public static final String PARTICIPANT_ROLE_CODE_SYSTEM = "2.16.840.1.113883.5.111";
  public static final String PARTICIPANT_ROLE_CODE_SYSTEM_NAME = "RoleCode";
  public static final String PARTICIPANT_ROLE_CODE_DISPLAY_NAME = "Self";
  public static final String AUTH_ACTIVITY_TEMPLATE_ID = "2.16.840.1.113883.10.20.1.19";
  public static final String AUTH_PROC_MOOD_CODE = "PRMS";

  // Functional and Cognitive History Related Information
  public static final String FUNC_STATUS_SEC_TEMPLATE_ID = "2.16.840.1.113883.10.20.22.2.14";
  public static final String FUNC_STATUS_SEC_CODE = "47420-5";
  public static final String FUNC_STATUS_SEC_NAME = "Functional and Cognitive Status";
  public static final String FUNC_STATUS_SEC_TITLE = "FUNCTIONAL and COGNITIVE STATUS";
  public static final String FUNC_STATUS_SEC_TEMPALTE_ID_EXT = "2014-06-09";
  public static final String ASSESSMENT_SCALE_OBSERVATION = "2.16.840.1.113883.10.20.22.4.69";

  // Author Function Code
  public static final String AUTH_FUNC_CODE = "PCP";
  public static final String AUTH_FUNC_CODE_NAME = "Primary Care Provider";
  public static final String AUTH_FUNC_CODE_SYSTEM = "2.16.840.1.113883.5.88";
  public static final String AUTH_FUNC_CODE_SYSTEM_NAME = "Provider Role";
  public static final String AUTH_FUNC_CODE_ORIG_TEXT = "Primary Care Provider";
  public static final String AUTH_TEMPLATE_ID = "2.16.840.1.113883.10.20.24.3.155";
  public static final String AUTH_TEMPLATE_ID_EXT = "2017-08-01";

  // CodeSystems
  public static final String LOINC_CODESYSTEM_OID = "2.16.840.1.113883.6.1";
  public static final String LOINC_CODESYSTEM_NAME = "LOINC";
  public static final String CONFIDENTIALITY_CODE_SYTEM = "2.16.840.1.113883.5.25";
  public static final String ADMIN_GEN_CODE_SYSTEM = "2.16.840.1.113883.5.1";
  public static final String ADMIN_GEN_CODE_SYSTEM_NAME = "HL7AdministrativeGenderCode";
  public static final String RACE_CODE_SYSTEM = "2.16.840.1.113883.6.238";
  public static final String RACE_CODE_SYSTEM_NAME = "Race &amp; Ethnicity - CDC";
  public static final String CPT_CODESYSTEM_OID = "2.16.840.1.113883.6.12";
  public static final String CPT_CODESYSTEM_NAME = "CPT-4";
  public static final String SNOMED_CODESYSTEM_OID = "2.16.840.1.113883.6.96";
  public static final String SNOMED_CODESYSTEM_NAME = "SNOMED-CT";
  public static final String HCPCS_CODESYSTEM_OID = "2.16.840.1.113883.6.285";
  public static final String HCPCS_CODESYSTEM_NAME = "HCPCS";
  public static final String ICD9_CM_CODESYSTEM_OID = "2.16.840.1.113883.6.103";
  public static final String ICD9_CM_CODESYSTEM_NAME = "ICD-9";
  public static final String ICD10_CM_CODESYSTEM_OID = "2.16.840.1.113883.6.90";
  public static final String ICD10_CM_CODESYSTEM_NAME = "ICD-10";
  public static final String ICD10_PCS_CODESYSTEM_OID = "2.16.840.1.113883.6.4";
  public static final String ICD10_PCS_CODESYSTEM_NAME = "ICD10-PCS";
  public static final String LOCAL_CODESYSTEM_NAME = "LOCAL_CODE_SYSTEM";
  public static final String HL7_ACT_CLASS_OID = "2.16.840.1.113883.5.6";
  public static final String HL7_ACT_CLASS_NAME = "HL7ActClass";
  public static final String RXNORM_CODESYSTEM_OID = "2.16.840.1.113883.6.88";
  public static final String RXNORM_CODESYSTEM_NAME = "RxNorm";
  public static final String CVX_CODESYSTEM_OID = "2.16.840.1.113883.12.292";
  public static final String CVX_CODESYSTEM_NAME = "CVX";
  public static final String CDT_CODESYSTEM_OID = "2.16.840.1.113883.6.13";
  public static final String CDT_CODESYSTEM_NAME = "CDT";
  public static final String HL7_DISCHARGE_CODE_SYSTEM_OID = "2.16.840.1.113883.12.112";
  public static final String HL7_DISCHARGE_CODE_SYSTEM_NAME = "HL7 Discharge Disposition";
  public static final String FDA_DEVICE_ID_AA = "2.16.840.1.113883.3.3719";
  public static final String NUBC_CODE_SYSTEM_OID = "2.16.840.1.113883.6.301";
  public static final String NUBC_CODE_SYSTEM_DISPLAY_NAME =
      "National Uniform Billing Committee (NUBC)";
  public static final String HL7_SERVICE_LOCATION_CODESYSTEM = "2.16.840.1.113883.6.259";
  public static final String HL7_SERVICE_LOCATION_CODESYSTEM_NAME =
      "HL7 Healthcare Service Location";
  public static final String SERVICE_DELIVERY_LOCATION_CODESYSTEM = "2.16.840.1.11388 3.5.111";
  public static final String SERVICE_DELIVERY_LOCATION_CODESYSTEM_NAME =
      "ServiceDelveryLocationRoleType";

  // Value Sets
  public static final String CONFIDENTIALITY_CODE = "N";
  public static final String LANGUAGE_CODE = "en-US";
  public static final String CMS_LANG_CODE = "en";
  public static final String DEF_COUNTRY = "US";
  public static final String PAT_LANGUAGE = "en";
  public static final String CCDA_TRUE = "true";
  public static final String CCDA_FALSE = "false";
  public static final String PIVL_TS_OPERATOR_VAL = "A";
  public static final String SIGNATURE_CODE_VAL = "S";

  // Status Codes
  public static final String ACTIVE_STATUS = "active";
  public static final String COMPLETED_STATUS = "completed";
  public static final String NEW_STATUS = "new";
  public static final String RESOLVED_STATUS = "resolved";
  public static final String TABLE_RESOLVED_STATUS = "Resolved";
  public static final String TABLE_ACTIVE_STATUS = "Active";

  // Default Attribute Values
  public static final String TYPE_CODE_DEF = "DRIV";
  public static final String MOOD_CODE_DEF = "EVN";
  public static final String MOOD_CODE_INT = "INT";
  public static final String MOOD_CODE_RQO = "RQO";
  public static final String DOC_CLASS_CODE = "DOC";
  public static final String DEV_CLASS_CODE = "DEV";
  public static final String ENC_CLASS_CODE = "ENC";
  public static final String ENC_CLASS_CODE_NAME = "Encounter";
  public static final String MED_CLASS_CODE = "SBADM";
  public static final String OBS_CLASS_CODE = "OBS";
  public static final String SUPPLY_CLASS_CODE = "SPLY";
  public static final String PROC_CLASS_CODE = "PROC";
  public static final String PCPR_CLASS_CODE = "PCPR";
  public static final String ACT_CLASS_CODE = "ACT";
  public static final String ORGANIZER_CLASS_CODE_CLUSTER = "CLUSTER";
  public static final String ENTRY_REL_SUBJ_CODE = "SUBJ";
  public static final String MANU_CLASS_CODE = "MANU";
  public static final String MANU_MATERIAL_CLASS_CODE = "MMAT";
  public static final String SDLOC_CLASS_CODE = "SDLOC";
  public static final String ENTRY_REL_REFR_CODE = "REFR";
  public static final String ENTRY_REL_RSON_CODE = "RSON";
  public static final String ENTRY_REL_COMP_CODE = "COMP";
  public static final String TYPE_CODE_REFR = "REFR";
  public static final String TYPE_CODE_DEV = "DEV";
  public static final String TYPE_CODE_RSON = "RSON";
  public static final String TYPE_CODE_SUBJ = "SUBJ";
  public static final String TYPE_CODE_CONS = "CSM";
  public static final String TYPE_CODE_LOC = "LOC";
  public static final String TYPE_CODE_PRD = "PRD";
  public static final String US_REALM_CODE_VAL = "US";

  // Element Names
  public static final String REALM_CODE_EL_NAME = "realmCode";
  public static final String ENTRY_REL_NAME = "entryRelationship";
  public static final String CODE_EL_NAME = "code";
  public static final String TARGET_SITE_CODE_EL_NAME = "targetSiteCode";
  public static final String QUALIFIER_EL_NAME = "qualifier";
  public static final String METHOD_CODE_EL_NAME = "methodCode";
  public static final String TITLE_EL_NAME = "title";
  public static final String SET_ID_EL_NAME = "setId";
  public static final String VERSION_EL_NAME = "versionNumber";
  public static final String CONFIDENTIALITY_EL_NAME = "confidentialityCode";
  public static final String LANGUAGE_CODE_EL_NAME = "languageCode";
  public static final String RECORD_TARGET_EL_NAME = "recordTarget";
  public static final String GUARDIAN_EL_NAME = "guardian";
  public static final String GUARDIAN_PERSON_EL_NAME = "guardianPerson";
  public static final String PATIENT_ROLE_EL_NAME = "patientRole";
  public static final String ADDR_EL_NAME = "addr";
  public static final String ST_ADDR_LINE_EL_NAME = "streetAddressLine";
  public static final String CITY_EL_NAME = "city";
  public static final String STATE_EL_NAME = "state";
  public static final String COUNTRY_EL_NAME = "country";
  public static final String POSTAL_CODE_EL_NAME = "postalCode";
  public static final String TELECOM_EL_NAME = "telecom";
  public static final String PATIENT_EL_NAME = "patient";
  public static final String NAME_EL_NAME = "name";
  public static final String FIRST_NAME_EL_NAME = "given";
  public static final String LAST_NAME_EL_NAME = "family";
  public static final String ADMIN_GENDER_CODE_EL_NAME = "administrativeGenderCode";
  public static final String BIRTH_TIME_EL_NAME = "birthTime";
  public static final String MARITAL_STATUS_CODE_EL_NAME = "maritalStatusCode";
  public static final String RELIGION_CODE_EL_NAME = "religiousAffiliationCode";
  public static final String RACE_CODE_EL_NAME = "raceCode";
  public static final String ETHNIC_CODE_EL_NAME = "ethnicGroupCode";
  public static final String TEL_EL_NAME = "telecom";
  public static final String EFF_TIME_EL_NAME = "effectiveTime";
  public static final String LANGUAGE_COMM_EL_NAME = "languageCommunication";
  public static final String AUTHOR_EL_NAME = "author";
  public static final String ASSIGNED_AUTHOR_EL_NAME = "assignedAuthor";
  public static final String TIME_EL_NAME = "time";
  public static final String FUNCTION_CODE_EL_NAME = "functionCode";
  public static final String COMP_EL_NAME = "component";
  public static final String STRUC_BODY_EL_NAME = "structuredBody";
  public static final String SECTION_EL_NAME = "section";
  public static final String ENTRY_EL_NAME = "entry";
  public static final String OBS_EL_NAME = "observation";
  public static final String ENTRY_REL_EL_NAME = "entryRelationship";
  public static final String TEXT_EL_NAME = "text";
  public static final String NARRATIVE_TEXT_EL_NAME = "Narrative Text";
  public static final String ORIGINAL_TEXT_EL_NAME = "originalText";
  public static final String TIME_LOW_EL_NAME = "low";
  public static final String TIME_HIGH_EL_NAME = "high";
  public static final String STATUS_CODE_EL_NAME = "statusCode";
  public static final String VAL_EL_NAME = "value";
  public static final String TRANSLATION_EL_NAME = "translation";
  public static final String INTERPRETATION_CODE_EL_NAME = "interpretationCode";
  public static final String ORGANIZER_EL_NAME = "organizer";
  public static final String CONSUMABLE_EL_NAME = "consumable";
  public static final String MAN_PROD_EL_NAME = "manufacturedProduct";
  public static final String PROD_EL_NAME = "product";
  public static final String MANU_MAT_EL_NAME = "manufacturedMaterial";
  public static final String LOT_NUM_TEXT = "lotNumberText";
  public static final String PERF_EL_NAME = "performer";
  public static final String ASSIGNED_ENTITY_EL_NAME = "assignedEntity";
  public static final String ASSOCIATED_ENTITY_EL_NAME = "associatedEntity";
  public static final String PARTICIPANT_EL_NAME = "participant";
  public static final String PARTICIPANT_ROLE_EL_NAME = "participantRole";
  public static final String TABLE_EL_NAME = "table";
  public static final String TABLE_HEAD_EL_NAME = "thead";
  public static final String TABLE_BODY_EL_NAME = "tbody";
  public static final String TABLE_ROW_EL_NAME = "tr";
  public static final String TABLE_HEAD_CONTENT_EL_NAME = "th";
  public static final String TABLE_BODY_ROW_EL_NAME = "td";
  public static final String TABLE_BODY_CONTENT_SUB_EL_NAME = "content";
  public static final String TABLE_BODY_CONTENT_ID_EL_NAME = "ID";
  public static final String LEGAL_AUTH_EL_NAME = "legalAuthenticator";
  public static final String SIGNATURE_EL_NAME = "signatureCode";
  public static final String REFR_EL_NAME = "reference";
  public static final String ASSIGNED_AUTHORING_DEVICE_EL_NAME = "assignedAuthoringDevice";
  public static final String SOFTWARE_NAME_EL_NAME = "softwareName";
  public static final String MANU_MODEL_NAME_EL_NAME = "manufacturerModelName";
  public static final String CUSTODIAN_EL_NAME = "custodian";
  public static final String ASSGND_CUST_EL_NAME = "assignedCustodian";
  public static final String REP_CUST_ORG_EL_NAME = "representedCustodianOrganization";
  public static final String ASSIGNED_PERSON_EL_NAME = "assignedPerson";
  public static final String REP_ORG_EL_NAME = "representedOrganization";
  public static final String ENCOMPASSING_ENC_EL_NAME = "encompassingEncounter";
  public static final String ENCOUNTER_PARTICIPANT_EL_NAME = "encounterParticipant";
  public static final String COMPONENT_OF_EL_NAME = "componentOf";
  public static final String RESP_PARTY_EL_NAME = "responsibleParty";
  public static final String LOCATION_EL_NAME = "location";
  public static final String HEALTHCARE_FACILITY_EL_NAME = "healthCareFacility";
  public static final String SERVICE_PROVIDER_ORG_EL_NAME = "serviceProviderOrganization";
  public static final String PRIORITY_CODE_EL_NAME = "priorityCode";
  public static final String INFORMATION_RECIPIENT_EL_NAME = "informationRecipient";
  public static final String INTENDED_RECIPIENT_EL_NAME = "intendedRecipient";
  public static final String DOSE_QUANTITY_EL_NAME = "doseQuantity";
  public static final String SDTC_DECEASED_IND = "sdtc:deceasedInd";
  public static final String SDTC_DECEASED_TIME = "sdtc:deceasedTime";
  public static final String SDTC_DISCHARGE_DISPOSITION = "sdtc:dischargeDispositionCode";
  public static final String PLAYING_DEVICE = "playingDevice";
  public static final String SCOPING_ENTITY = "scopingEntity";
  public static final String PLAYING_ENTITY = "playingEntity";

  // Acts
  public static final String ENC_ACT_EL_NAME = "encounter";
  public static final String OBS_ACT_EL_NAME = "observation";
  public static final String MED_ACT_EL_NAME = "substanceAdministration";
  public static final String PROC_ACT_EL_NAME = "procedure";
  public static final String ACT_EL_NAME = "act";
  public static final String SUPPLY_ACT_EL_NAME = "supply";
  public static final String EXT_DOC_ACT_EL_NAME = "externalDocument";
  public static final String EXT_OBS_ACT_EL_NAME = "externalObservation";
  public static final String EXT_ACT_EL_NAME = "externalAct";

  // ATTRIBUTE NAMES
  public static final String TYPECODE_ATTR_NAME = "typeCode";
  public static final String CLASSCODE_ATTR_NAME = "classCode";
  public static final String MOODCODE_ATTR_NAME = "moodCode";
  public static final String NEG_IND_ATTR_NAME = "negationInd";
  public static final String INV_IND_ATTR_NAME = "inversionInd";
  public static final String INSTITUTION_SPECIFIED_ATTR_NAME = "institutionSpecified";
  public static final String OPERATOR_NAME = "operator";
  public static final String PERIOD_NAME = "period";
  public static final String HOURS_UNITS_NAME = "h";
  public static final String NULL_FLAVOR_NAME = "nullFlavor";
  public static final String TABLE_BORDER_ATTR_NAME = "border";
  public static final String TABLE_WIDTH_ATTR_NAME = "width";

  // Null Flavors
  public static final String NF_NI = "NI";
  public static final String NF_NA = "NA";
  public static final String NF_UNK = "UNK";
  public static final String NF_OTH = "OTH";

  // Data Types
  public static final String CD_TYPE = "CD";
  public static final String PQ_TYPE = "PQ";
  public static final String CO_TYPE = "CO";
  public static final String PIVL_TS_TYPE = "PIVL_TS";
  public static final String IVL_TS_TYPE = "IVL_TS";
  public static final String INT_TYPE = "INT";
  public static final String ED_TYPE = "ED";
  public static final String ST_TYPE = "ST";

  // Table Values
  public static final int TABLE_BORDER = 1;
  public static final int TABLE_WIDTH = 100;
  public static final String ENC_TABLE_COL_1_TITLE = "Encounter Reason";
  public static final String ENC_TABLE_COL_1_BODY_CONTENT = "encounter";
  public static final String ENC_TABLE_COL_2_TITLE = "Date of Encounter";
  public static final String ENC_TABLE_COL_2_BODY_CONTENT = "encounterDate";
  public static final String PROB_TABLE_COL_1_TITLE = "Problem or Diagnosis";
  public static final String PROB_TABLE_COL_1_BODY_CONTENT = "problem";
  public static final String PROB_TABLE_COL_2_TITLE = "Problem Status";
  public static final String PROB_TABLE_COL_2_BODY_CONTENT = "problemStatus";
  public static final String HISTORY_OF_PRESENT_ILLNESS_BODY_CONTENT = "historyOfPresentIllness";
  public static final String REASON_FOR_VISIT_BODY_CONTENT = "reasonForVisit";
  public static final String ALLERGY_TABLE_COL_1_TITLE = "Allergy Substance";
  public static final String ALLERGY_TABLE_COL_1_BODY_CONTENT = "allergySubstance";
  public static final String ALLERGY_TABLE_COL_2_TITLE = "Allergy Status";
  public static final String ALLERGY_TABLE_COL_2_BODY_CONTENT = "allergyStatus";
  public static final String PROC_TABLE_COL_1_TITLE = "Procedure Name";
  public static final String PROC_TABLE_COL_1_BODY_CONTENT = "procedure";
  public static final String PROC_TABLE_COL_2_TITLE = "Procedure Date";
  public static final String PROC_TABLE_COL_2_BODY_CONTENT = "procedureDate";
  public static final String INTER_TABLE_COL_1_TITLE = "Intervention Name";
  public static final String INTER_TABLE_COL_1_BODY_CONTENT = "intervention";
  public static final String INTER_TABLE_COL_2_TITLE = "Intervention Date";
  public static final String INTER_TABLE_COL_2_BODY_CONTENT = "interDate";
  public static final String REFERAL_TABLE_COL_1_TITLE = "Referal Name";
  public static final String REFERAL_TABLE_COL_1_BODY_CONTENT = "referal";
  public static final String REFERAL_TABLE_COL_2_TITLE = "Referal Date";
  public static final String REFERAL_TABLE_COL_2_BODY_CONTENT = "referalDate";
  public static final String VS_TABLE_COL_1_TITLE = "Vital Sign";
  public static final String VS_TABLE_COL_1_BODY_CONTENT = "vitalSign";
  public static final String VS_TABLE_COL_2_TITLE = "Measured Value";
  public static final String VS_TABLE_COL_2_BODY_CONTENT = "vitalSignValue";
  public static final String VS_TABLE_COL_3_TITLE = "Measurement Date";
  public static final String VS_TABLE_COL_3_BODY_CONTENT = "vitalSignDate";
  public static final String DIAGNOSTICSTUDY_TABLE_COL_1_TITLE = "Diagnostic Study";
  public static final String DIAGNOSTICSTUDY_COL_1_BODY_CONTENT = "diagnosticStudy";
  public static final String LABTEST_TABLE_COL_1_TITLE = "Lab Test Name";
  public static final String LABTEST_TABLE_COL_1_BODY_CONTENT = "labTestName";
  public static final String LABTEST_TABLE_COL_2_TITLE = "Lab Test Result Value";
  public static final String LABTEST_TABLE_COL_2_BODY_CONTENT = "labTestResultValue";
  public static final String LABTEST_TABLE_COL_3_TITLE = "Lab Test Result Date";
  public static final String LABTEST_TABLE_COL_3_BODY_CONTENT = "labTestResultValueDate";
  public static final String MED_TABLE_COL_1_TITLE = "Medication Name";
  public static final String MED_TABLE_COL_1_BODY_CONTENT = "medication";
  public static final String MED_TABLE_COL_2_TITLE = "Medication Start Date";
  public static final String MED_TABLE_COL_2_BODY_CONTENT = "medicationDate";
  public static final String IMM_TABLE_COL_1_TITLE = "Vaccine Name";
  public static final String IMM_TABLE_COL_1_BODY_CONTENT = "vaccine";
  public static final String IMM_TABLE_COL_2_TITLE = "Vaccination Date";
  public static final String IMM_TABLE_COL_2_BODY_CONTENT = "vaccinationDate";
  public static final String FUNC_STATUS_TABLE_COL_1_TITLE = "Functional Status Observation";
  public static final String FUNC_STATUS_TABLE_COL_1_BODY_CONTENT = "observation";
  public static final String FUNC_STATUS_TABLE_COL_2_TITLE = "Observation Result";
  public static final String FUNC_STATUS_TABLE_COL_2_BODY_CONTENT = "observationResult";
  public static final String SOC_HISTORY_TABLE_COL_1_TITLE = "Social History Observation";
  public static final String SOC_HISTORY_TABLE_COL_1_BODY_CONTENT = "socContent";
  public static final String SOC_HISTORY_TABLE_COL_2_TITLE = "Social History Observation Result";
  public static final String SOC_HISTORY_TABLE_COL_2_BODY_CONTENT = "socObservationResult";
  public static final String POT_OBS_TABLE_COL_1_TITLE = "Planned Observation";
  public static final String POT_OBS_TABLE_COL_1_BODY_CONTENT = "potObsContent";
  public static final String POT_OBS_TABLE_COL_2_TITLE = "Planned Observation Date";
  public static final String POT_OBS_TABLE_COL_2_BODY_CONTENT = "potObsDate";
  public static final String PAYOR_TABLE_COL_1_TITLE = "Insurance Provider";
  public static final String PAYOR_TABLE_COL_1_BODY_CONTENT = "payor";
  public static final String DEVICE_TABLE_COL_1_TITLE = "Device Name";
  public static final String DEVICE_TABLE_COL_1_BODY_CONTENT = "device";
  public static final String DEVICE_TABLE_COL_2_TITLE = "Device Date";
  public static final String DEVICE_TABLE_COL_2_BODY_CONTENT = "deviceDate";
  public static final String UNKNOWN_VALUE = "Unknown";
  public static final String UNKNOWN_HISTORY_OF_PRESENT_ILLNESS =
      "Unknown History of Present Illness";
  public static final String UNKNOWN_REASON_FOR_VISIT = "Unknown Reason For Visit";

  // OID to URI Mapping
  private static HashMap<String, Pair<String, String>> oidMap = new HashMap<>();
  private static HashMap<String, Pair<String, String>> uriMap = new HashMap<>();

  // Static block to load OID to URI mapping from property file
  static {
    try (InputStream input =
        CdaGeneratorConstants.class
            .getClassLoader()
            .getResourceAsStream("oid-uri-mapping-r4.properties")) {

      Properties prop = new Properties();
      prop.load(input);
      prop.forEach(
          (key, value) -> {
            String name = StringUtils.substringAfterLast((String) value, "/");
            oidMap.put((String) key, new Pair<>((String) value, name));
            uriMap.put((String) value, new Pair<>((String) key, name));
          });
    } catch (IOException ex) {
      logger.error("Error while loading OID to URI from properties files", ex);
    }
  }

  /**
   * @param oid
   * @return URI|Name
   */
  public static Pair<String, String> getURI(String oid) {
    if (oidMap.containsKey(oid)) {
      return oidMap.get(oid);
    } else {
      return new Pair<>("", "");
    }
  }

  /**
   * @param uri
   * @return OID|Name
   */
  public static Pair<String, String> getOID(String uri) {
    if (uriMap.containsKey(uri)) {
      return uriMap.get(uri);
    } else {
      return new Pair<>("", "");
    }
  }

  public static Pair<String, String> getCodeSystemFromUrl(String url) {

    logger.debug(" Url passed = {}", url);

    if (StringUtils.isEmpty(url)) {
      return new Pair<>("", "");
    } else if (uriMap.containsKey(url)) {
      return uriMap.get(url);
    } else {
      return new Pair<>("", "");
    }
  }

  public static String getCodeForTelecomUse(String val) {

    if (!StringUtils.isEmpty(val)) {

      if (val.contentEquals("home")) {
        return "HP";
      } else if (val.contentEquals("work")) {
        return "WP";
      } else if (val.contentEquals("mobile")) {
        return "MC";
      } else {
        return "WP";
      }

    } else {
      return "WP";
    }
  }
}
