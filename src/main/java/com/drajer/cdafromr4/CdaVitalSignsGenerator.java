package com.drajer.cdafromr4;

import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cda.utils.CdaGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.Quantity;
import org.hl7.fhir.r4.model.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaVitalSignsGenerator {

  private CdaVitalSignsGenerator() {}

  private static final Logger logger = LoggerFactory.getLogger(CdaVitalSignsGenerator.class);

  public static String generateVitalsSection(
      R4FhirData data, LaunchDetails details, String version) {

    StringBuilder hsb = new StringBuilder(5000);
    StringBuilder sb = new StringBuilder(2000);
    StringBuilder vitalEntries = new StringBuilder();

    List<Observation> allVitals = data.getVitalObs();
    // Check if there is a LOINC code for the observation to be translated.
    List<Observation> vitals = getValidVitals(data);

    if (vitals != null && !vitals.isEmpty()) {

      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.SECTION_EL_NAME));

      hsb.append(
          CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.VITAL_SIGNS_SEC_TEMPLATE_ID));
      hsb.append(
          CdaGeneratorUtils.getXmlForTemplateId(
              CdaGeneratorConstants.VITAL_SIGNS_SEC_TEMPLATE_ID,
              CdaGeneratorConstants.VITAL_SIGNS_SEC_TEMPLATE_ID_EXT));

      hsb.append(
          CdaGeneratorUtils.getXmlForCD(
              CdaGeneratorConstants.CODE_EL_NAME,
              CdaGeneratorConstants.VITAL_SIGNS_SEC_CODE,
              CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
              CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
              CdaGeneratorConstants.VITAL_SIGNS_SEC_NAME));

      // Add Title
      hsb.append(
          CdaGeneratorUtils.getXmlForText(
              CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.VITAL_SIGNS_SEC_TITLE));

      // Add Narrative Text
      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TEXT_EL_NAME));

      // Create Table Header.
      List<String> list = new ArrayList<>();
      list.add(CdaGeneratorConstants.VS_TABLE_COL_1_TITLE);
      list.add(CdaGeneratorConstants.VS_TABLE_COL_2_TITLE);
      list.add(CdaGeneratorConstants.VS_TABLE_COL_3_TITLE);
      hsb.append(
          CdaGeneratorUtils.getXmlForTableHeader(
              list, CdaGeneratorConstants.TABLE_BORDER, CdaGeneratorConstants.TABLE_WIDTH));

      // Add Table Body
      hsb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      int rowNum = 1;
      for (Observation obs : vitals) {

        String obsDisplayName = CdaGeneratorConstants.UNKNOWN_VALUE;

        if (obs.hasCode()
            && obs.getCode() != null
            && obs.getCode().hasCoding()
            && obs.getCode().getCodingFirstRep() != null) {

          if (!StringUtils.isEmpty(obs.getCode().getCodingFirstRep().getDisplay())) {
            obsDisplayName = obs.getCode().getCodingFirstRep().getDisplay();
          } else if (!StringUtils.isEmpty(obs.getCode().getText())) {
            obsDisplayName = obs.getCode().getText();
          } else if (!StringUtils.isEmpty(obs.getCode().getCodingFirstRep().getCode())
              && (!StringUtils.isEmpty(obs.getCode().getCodingFirstRep().getSystem()))) {
            obsDisplayName =
                obs.getCode().getCodingFirstRep().getSystem()
                    + "|"
                    + obs.getCode().getCodingFirstRep().getCode();
          }
        } else if (obs.hasCode()
            && obs.getCode() != null
            && !StringUtils.isEmpty(obs.getCode().getText())) {
          obsDisplayName = obs.getCode().getText();
        }

        Map<String, String> bodyvals = new LinkedHashMap<>();
        bodyvals.put(CdaGeneratorConstants.VS_TABLE_COL_1_BODY_CONTENT, obsDisplayName);

        String val = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (obs.getValue() != null) {

          val = CdaFhirUtilities.getStringForType(obs.getValue());
        }

        bodyvals.put(CdaGeneratorConstants.VS_TABLE_COL_2_BODY_CONTENT, val);

        String dt = CdaGeneratorConstants.UNKNOWN_VALUE;
        if (obs.getEffective() != null) {

          dt = CdaFhirUtilities.getStringForType(obs.getEffective());
        }
        bodyvals.put(CdaGeneratorConstants.VS_TABLE_COL_3_BODY_CONTENT, dt);

        sb.append(CdaGeneratorUtils.addTableRow(bodyvals, rowNum));

        vitalEntries.append(getXmlForVitalOrganizer(data, obs, details, rowNum));
        rowNum++;
      }

      // End the Sb string.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_BODY_EL_NAME));

      // End Table.
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TABLE_EL_NAME));
      sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.TEXT_EL_NAME));

      hsb.append(sb);

      // Add lab results
      hsb.append(vitalEntries);

      // Complete the section end tags.
      hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
      hsb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    } else {
      hsb.append(generateEmptyLabVitals());
    }

    return hsb.toString();
  }

  private static Object getXmlForVitalOrganizer(
      R4FhirData data, Observation obs, LaunchDetails details, int rowNum) {
    // Setup the Organizer and Entries
    StringBuilder vsEntry = new StringBuilder();

    // Add the Entries.
    vsEntry.append(CdaGeneratorUtils.getXmlForActEntry(CdaGeneratorConstants.TYPE_CODE_DEF));

    // Add the Organizer Act
    vsEntry.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.ORGANIZER_EL_NAME,
            CdaGeneratorConstants.ORGANIZER_CLASS_CODE_CLUSTER,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    vsEntry.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.VITAL_SIGNS_ORG_TEMPLATE_ID));
    vsEntry.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.VITAL_SIGNS_ORG_TEMPLATE_ID,
            CdaGeneratorConstants.VITAL_SIGNS_ORG_TEMPLATE_ID_EXT));

    vsEntry.append(CdaGeneratorUtils.getXmlForIIUsingGuid());

    vsEntry.append(
        CdaGeneratorUtils.getXmlForCDWithoutEndTag(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.VITAL_SIGNS_ORG_CODE,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_OID,
            CdaGeneratorConstants.SNOMED_CODESYSTEM_NAME,
            CdaGeneratorConstants.VITAL_SIGNS_ORG_CODE_LOINC));
    vsEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.TRANSLATION_EL_NAME,
            CdaGeneratorConstants.VITAL_SIGNS_ORG_CODE_LOINC,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.VITAL_SIGNS_ORG_CODE_LOINC));
    vsEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.CODE_EL_NAME));

    vsEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    vsEntry.append(
        CdaFhirUtilities.getXmlForType(
            obs.getEffective(), CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    vsEntry.append(
        getXmlForObservation(
            details, obs, CdaGeneratorConstants.VS_TABLE_COL_1_BODY_CONTENT, rowNum));

    // End Tags for Entries
    vsEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ORGANIZER_EL_NAME));
    vsEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.ENTRY_EL_NAME));

    return vsEntry.toString();
  }

  public static String getXmlForObservation(
      LaunchDetails details, Observation obs, String contentId, int row) {

    StringBuilder lrEntry = new StringBuilder(2000);
    String contentRef = contentId + Integer.toString(row);

    Boolean foundComponent = false;

    if (obs.hasComponent()) {

      CodeableConcept cc = obs.getCode();
      Type val = obs.getValue();
      List<CodeableConcept> interpretation = obs.getInterpretation();
      StringBuilder id = new StringBuilder(200);
      id.append(obs.getIdElement().getIdPart());
      int rowNum = 1;

      for (ObservationComponentComponent oc : obs.getComponent()) {

        logger.debug("Found Observation Components ");
        if (oc.hasCode()) {
          cc = oc.getCode();
        }

        if (oc.hasValue() && oc.getValue() instanceof Quantity) {
          val = oc.getValue();
        }

        if (oc.hasInterpretation()) {
          interpretation = oc.getInterpretation();
        }

        id.append("-");
        id.append(Integer.toBinaryString(rowNum));

        String compString =
            getXmlForObservationComponent(
                details,
                cc,
                val,
                id.toString(),
                obs.getEffective(),
                interpretation,
                contentRef,
                null);

        if (!compString.isEmpty()) {
          foundComponent = true;
          lrEntry.append(compString);
        }

        rowNum++;
      }
    }

    if (Boolean.FALSE.equals(foundComponent)) {

      logger.debug("No component found , so directly adding the observation code ");
      lrEntry.append(
          getXmlForObservationComponent(
              details,
              obs.getCode(),
              obs.getValue(),
              obs.getId(),
              obs.getEffective(),
              obs.getInterpretation(),
              contentRef,
              null));
    }

    logger.debug("Lr Entry = {}", lrEntry);

    return lrEntry.toString();
  }

  public static String getXmlForObservationComponent(
      LaunchDetails details,
      CodeableConcept cd,
      Type val,
      String id,
      Type effective,
      List<CodeableConcept> interpretation,
      String contentRef,
      CodeableConcept altCode) {

    StringBuilder lrEntry = new StringBuilder(2000);

    // Add the actual Result Observation
    lrEntry.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    lrEntry.append(
        CdaGeneratorUtils.getXmlForAct(
            CdaGeneratorConstants.OBS_ACT_EL_NAME,
            CdaGeneratorConstants.OBS_CLASS_CODE,
            CdaGeneratorConstants.MOOD_CODE_DEF));

    lrEntry.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.VITAL_SIGNS_ENTRY_TEMPLATE_ID));
    lrEntry.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.VITAL_SIGNS_ENTRY_TEMPLATE_ID,
            CdaGeneratorConstants.VITAL_SIGNS_ENTRY_TEMPLATE_ID_EXT));

    lrEntry.append(CdaGeneratorUtils.getXmlForII(details.getAssigningAuthorityId(), id));

    List<CodeableConcept> cds = new ArrayList<CodeableConcept>();
    cds.add(cd);
    lrEntry.append(
        CdaFhirUtilities.getCodeableConceptXmlForCodeSystem(
            cds,
            CdaGeneratorConstants.CODE_EL_NAME,
            false,
            CdaGeneratorConstants.FHIR_LOINC_URL,
            false,
            contentRef));

    lrEntry.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.STATUS_CODE_EL_NAME, CdaGeneratorConstants.COMPLETED_STATUS));

    lrEntry.append(
        CdaFhirUtilities.getXmlForType(effective, CdaGeneratorConstants.EFF_TIME_EL_NAME, false));

    if (val instanceof Quantity) {
      Quantity dt = (Quantity) val;

      lrEntry.append(CdaFhirUtilities.getQuantityXml(dt, CdaGeneratorConstants.VAL_EL_NAME, true));
    } else {
      lrEntry.append(
          CdaGeneratorUtils.getXmlForNfQuantity(
              CdaGeneratorConstants.VAL_EL_NAME, CdaGeneratorConstants.NF_NI, true));
    }

    // Add interpretation code.
    if (interpretation != null) {

      logger.debug("Adding Interpretation Code");

      String interpretXml =
          CdaFhirUtilities.getCodeableConceptXmlForMappedConceptDomain(
              CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
              interpretation,
              CdaGeneratorConstants.INTERPRETATION_CODE_EL_NAME,
              false,
              false);

      if (interpretXml != null && !interpretXml.isEmpty()) lrEntry.append(interpretXml);
    }

    // End Tag for Entry Relationship
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.OBS_ACT_EL_NAME));
    lrEntry.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return lrEntry.toString();
  }

  public static String generateEmptyLabVitals() {
    StringBuilder sb = new StringBuilder();

    // Generate the component and section end tags
    sb.append(CdaGeneratorUtils.getXmlForStartElement(CdaGeneratorConstants.COMP_EL_NAME));
    sb.append(
        CdaGeneratorUtils.getXmlForNFSection(
            CdaGeneratorConstants.SECTION_EL_NAME, CdaGeneratorConstants.NF_NI));

    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(CdaGeneratorConstants.VITAL_SIGNS_SEC_TEMPLATE_ID));
    sb.append(
        CdaGeneratorUtils.getXmlForTemplateId(
            CdaGeneratorConstants.VITAL_SIGNS_SEC_TEMPLATE_ID,
            CdaGeneratorConstants.VITAL_SIGNS_SEC_TEMPLATE_ID_EXT));

    sb.append(
        CdaGeneratorUtils.getXmlForCD(
            CdaGeneratorConstants.CODE_EL_NAME,
            CdaGeneratorConstants.VITAL_SIGNS_SEC_CODE,
            CdaGeneratorConstants.LOINC_CODESYSTEM_OID,
            CdaGeneratorConstants.LOINC_CODESYSTEM_NAME,
            CdaGeneratorConstants.VITAL_SIGNS_SEC_NAME));

    // Add Title
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TITLE_EL_NAME, CdaGeneratorConstants.VITAL_SIGNS_SEC_TITLE));

    // Add Narrative Text
    sb.append(
        CdaGeneratorUtils.getXmlForText(
            CdaGeneratorConstants.TEXT_EL_NAME, "No Vitals Information"));

    // Complete the section end tags.
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.SECTION_EL_NAME));
    sb.append(CdaGeneratorUtils.getXmlForEndElement(CdaGeneratorConstants.COMP_EL_NAME));

    return sb.toString();
  }

  public static List<Observation> getValidVitals(R4FhirData data) {

    List<Observation> sr = new ArrayList<>();

    if (data.getVitalObs() != null && !data.getVitalObs().isEmpty()) {

      logger.info("Total num of Vitals available for Patient {}", data.getVitalObs().size());

      for (Observation s : data.getVitalObs()) {

        if (s.hasCode()
            && s.getCode().hasCoding()
            && Boolean.TRUE.equals(
                CdaFhirUtilities.isCodingPresentForCodeSystem(
                    s.getCode().getCoding(), CdaGeneratorConstants.FHIR_LOINC_URL))) {

          logger.debug("Found a Lab Vitals with a LOINC code");
          sr.add(s);
        } else {
          logger.info(
              " Ignoring observation with id {} because it is not coded with LOINC code",
              s.getId());
        }
      }
    } else {
      logger.debug("No Valid Lab Vitals in the bundle to process");
    }

    logger.info("Total num of Vitals available for Patient after filtering {}", sr.size());
    return sr;
  }
}
