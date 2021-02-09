package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.eca.model.ActionRepo;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.ServiceRequest.ServiceRequestStatus;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaPlanOfTreatmentGenerator {

  private static final Logger logger = LoggerFactory.getLogger(CdaPlanOfTreatmentGenerator.class);

  private CdaPlanOfTreatmentGenerator() {}

  public static String generatePlanOfTreatmentSection(R4FhirData data, LaunchDetails details) {

    StringBuilder sb = new StringBuilder(2000);

    List<ServiceRequest> sr = getValidServiceRequests(data);

    if (sr != null && !sr.isEmpty()) {
      logger.info(" Found a total of {} service request objects to translate to CDA.", sr.size());

      // Generate the component and section end tags
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID));
      sb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID_EXT));

      sb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.CAREPLAN_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.CAREPLAN_SEC_NAME));

      // Add Title
      sb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.CAREPLAN_SEC_TITLE));

      // Add Narrative Text
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.POT_OBS_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.POT_OBS_TABLE_COL_2_TITLE);
      sb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int rowNum = 1;
      StringBuilder potObsXml = new StringBuilder();
      for (ServiceRequest s : sr) {

        Pair<String, Boolean> srDisplayName =
            CdaFhirUtilities.getCodeableConceptDisplayForCodeSystem(
                s.getCode(), CdaGeneratorConstants.FHIR_LOINC_URL, false);

        if (srDisplayName.getValue0().isEmpty()) {
          srDisplayName.setAt0(CdaGeneratorConstants.UNKNOWN_VALUE);
        }

        String serviceDate = CdaFhirUtilities.getStringForType(s.getOccurrence());
        logger.info(" Service Date for display {} ", serviceDate);

        if (serviceDate.isEmpty() && s.getAuthoredOn() != null) {
          serviceDate = CdaGeneratorUtils.getStringForDate(s.getAuthoredOn());
        }

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(
            CdaGeneratorConstants.POT_OBS_TABLE_COL_1_BODY_CONTENT, srDisplayName.getValue0());
        bodyvals.put(CdaGeneratorConstants.POT_OBS_TABLE_COL_2_BODY_CONTENT, serviceDate);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        rowNum++;

        potObsXml.append(getPlannedObservationXml(s, details));
      }

      // Close the Text Element
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Add Entries
      sb.append(potObsXml);

      // Complete the section end tags.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {

      sb.append(generateEmptyPlanOfTreatmentSection());
    }

    return sb.toString();
  }

  public static String getPlannedObservationXml(ServiceRequest sr, LaunchDetails details) {

    StringBuilder sb = new StringBuilder();

    // Generate the entry
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.ENTRY_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_RQO));

    sb.append(CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID,
            CdaGeneratorConstants.PLANNED_OBS_TEMPLATE_ID_EXT));

    List<String> matchedTriggerCodes =
        CdaFhirUtilities.getMatchedCodesForResourceAndUrl(
            details, "ServiceRequest", CdaGeneratorConstants.FHIR_LOINC_URL);

    String codeXml = "";
    // Add Trigger code template if the code matched the Url in the Service Request.
    if (matchedTriggerCodes != null && !matchedTriggerCodes.isEmpty()) {
      logger.info(" Found a Matched Code that is for Service Request ");

      String mCd =
          CdaFhirUtilities.getMatchingCodeFromCodeableConceptForCodeSystem(
              matchedTriggerCodes, sr.getCode(), CdaGeneratorConstants.FHIR_LOINC_URL);

      if (!mCd.isEmpty()) {

        sb.append(
            CdaGeneratorUtils.getXmlForTemplateId(
                CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE,
                CdaGeneratorConstants.LAB_TEST_ORDER_TRIGGER_CODE_TEMPLATE_EXT));

        codeXml =
            CdaGeneratorUtils.getXmlForCDWithValueSetAndVersion(
                CdaGeneratorConstants.CODE_EL_NAME,
                mCd,
                CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
                CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
                CdaGeneratorConstants.RCTC_OID,
                ActionRepo.getInstance().getRctcVersion(),
                "");
      } else {
        logger.info(
            " Did not find the code value in the matched codes, make it a regular Planned Observation ");
      }
    }

    // add Id
    sb.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    if (codeXml.isEmpty()) {
      List<CodeableConcept> ccs = new ArrayList<>();
      ccs.add(sr.getCode());
      codeXml =
          CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
              ccs,
              CdaGeneratorConstants.CODE_EL_NAME,
              false,
              CdaGeneratorConstants.FHIR_LOINC_URL,
              false);

      if (!codeXml.isEmpty()) {
        sb.append(codeXml);
      } else {
        sb.append(
            CdaFhirUtilities.getCodeableConceptXml(ccs, CdaGeneratorConstants.CODE_EL_NAME, false));
      }
    } else {
      sb.append(codeXml);
    }

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.ACTIVE_STATUS));

    Date effDate = CdaFhirUtilities.getActualDate(sr.getOccurrence());
    if (effDate == null) {
      logger.info(" Use Authored Date ");

      effDate = sr.getAuthoredOn();
    }

    if (effDate != null) {}

    sb.append(
        CdaGeneratorUtils.getXmlForEffectiveTime(CdaGeneratorConstants.EFF_TIME_EL_NAME, effDate));

    // End Tag for Entry
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return sb.toString();
  }

  public static List<ServiceRequest> getValidServiceRequests(R4FhirData data) {

    List<ServiceRequest> sr = new ArrayList<>();

    if (data.getServiceRequests() != null && !data.getServiceRequests().isEmpty()) {

      logger.info(
          " Total num of Service Requests available for Patient {}",
          data.getServiceRequests().size());

      for (ServiceRequest s : data.getServiceRequests()) {

        if (s.getCode() != null
            && s.getCode().getCoding() != null
            && !s.getCode().getCoding().isEmpty()
            && CdaFhirUtilities.isCodingPresentForCodeSystem(
                s.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL)
            && s.getStatus() != null
            && s.getStatus() == ServiceRequestStatus.ACTIVE) {

          logger.info(" Found a Service Request with a LOINC code ");
          sr.add(s);
        }
      }
    } else {
      logger.info(" No Valid Service Requests in the bundle to process ");
    }

    return sr;
  }

  public static String generateEmptyPlanOfTreatmentSection() {

    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    // Add Plan of Treatment Template Id
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.CAREPLAN_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.CAREPLAN_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.CAREPLAN_SEC_TITLE));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Plan Of Treatment Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }
}
