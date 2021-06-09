package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.fhirecr.FhirGeneratorConstants;
import com.drajer.fhirecr.FhirGeneratorUtils;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Composition;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.Device.DeviceDeviceNameComponent;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Narrative;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.StringType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EcrReportCreator extends ReportCreator {

  private static String DEFAULT_VERSION = "1";
  private static String VERSION_NUM_URL =
      "http://hl7.org/fhir/StructureDefinition/composition-clinicaldocument-versionNumber";
  private static String DEVICE_NAME = "eCRNow/Backend Service App";

  private final Logger logger = LoggerFactory.getLogger(EcrReportCreator.class);

  public enum SectionTypeEnum {
    CHIEF_COMPLAINT,
    HISTORY_OF_PRESENT_ILLNESS,
    REVIEW_OF_SYSTEMS,
    PROBLEM,
    MEDICAL_HISTORY,
    MEDICATION_ADMINISTERED,
    RESULTS,
    PLAN_OF_TREATMENT,
    SERVICE_REQUEST,
    IMMUNIZATIONS,
    PROCEDURES,
    VITAL_SIGNS,
    SOCIAL_HISTORY,
    PREGNANCY,
    REPORTABILITY_RESPONSE,
    EMERGENCY_OUTBREAK_SECTION
  }

  @Override
  public Resource createReport(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile) {

    // Create the report as needed by the Ecr FHIR IG
    Bundle returnBundle = new Bundle();
    returnBundle.setId(id);
    returnBundle.setType(BundleType.DOCUMENT);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    logger.info(" Creating Composition Resource ");
    List<Resource> resourcesTobeAdded = new ArrayList<>();
    Composition comp = createComposition(kd, resourcesTobeAdded);

    returnBundle.addEntry(new BundleEntryComponent().setResource(comp));

    for (Resource res : resourcesTobeAdded) {

      returnBundle.addEntry(new BundleEntryComponent().setResource(res));
    }

    return returnBundle;
  }

  public Composition createComposition(KarProcessingData kd, List<Resource> resTobeAdded) {

    Composition comp = new Composition();
    comp.setId(UUID.randomUUID().toString());

    // Add clinical document version number extension.
    comp.setExtension(getExtensions());

    // Add Identifier.
    Identifier val = new Identifier();
    val.setValue(comp.getId());
    comp.setIdentifier(val);

    // Add Type
    comp.setType(
        FhirGeneratorUtils.getCodeableConcept(
            FhirGeneratorConstants.LOINC_CS_URL,
            FhirGeneratorConstants.COMP_TYPE_CODE,
            FhirGeneratorConstants.COMP_TYPE_CODE_DISPLAY));

    // Set Patient
    Set<Resource> patients = kd.getResourcesByType(ResourceType.Patient.toString());
    if (patients != null && patients.size() >= 1) {

      logger.info(" Setting up the patient for the composition ");
      Resource patient = patients.iterator().next();

      comp.getSubject().setResource(patient);
      resTobeAdded.add(patient);
    } else {

      logger.error(
          " Cannot setup the patient for Composition, need to determine best approach to deal with the error. ");
    }

    // Set Encounter
    Set<Resource> encounters = kd.getResourcesByType(ResourceType.Encounter.toString());
    if (encounters != null && encounters.size() >= 1) {

      logger.info(" Setting up the patient for the composition ");
      Resource encounter = encounters.iterator().next();
      comp.getEncounter().setResource(encounters.iterator().next());
      resTobeAdded.add(encounter);

    } else {

      logger.error(
          "Received more than one encounter for processing which is erroneous, using the first one.");
      comp.getEncounter().setResource(encounters.iterator().next());
    }

    // Set Date
    comp.setDate(Date.from(Instant.now()));

    // Set Author
    comp.getAuthorFirstRep().setResource(getDeviceAuthor());

    List<SectionComponent> scs = new ArrayList<SectionComponent>();

    // Add chief complaint section.
    SectionComponent sc = getSection(SectionTypeEnum.CHIEF_COMPLAINT, kd);
    if (sc != null) scs.add(sc);

    // Add History of Present Illness section.
    sc = getSection(SectionTypeEnum.HISTORY_OF_PRESENT_ILLNESS, kd);
    if (sc != null) scs.add(sc);

    // Add Review of Systems Section
    sc = getSection(SectionTypeEnum.REVIEW_OF_SYSTEMS, kd);
    if (sc != null) scs.add(sc);

    // Add Problem section.
    sc = getSection(SectionTypeEnum.PROBLEM, kd);
    if (sc != null) scs.add(sc);

    // Add Past Medical History section.
    sc = getSection(SectionTypeEnum.MEDICAL_HISTORY, kd);
    if (sc != null) scs.add(sc);

    // Add Medications Administered section.
    sc = getSection(SectionTypeEnum.MEDICATION_ADMINISTERED, kd);
    if (sc != null) scs.add(sc);

    // Add Results section.
    sc = getSection(SectionTypeEnum.RESULTS, kd);
    if (sc != null) scs.add(sc);

    // Add Plan Of Treatment section.
    sc = getSection(SectionTypeEnum.PLAN_OF_TREATMENT, kd);
    if (sc != null) scs.add(sc);

    // Add Immunizations section.
    sc = getSection(SectionTypeEnum.IMMUNIZATIONS, kd);
    if (sc != null) scs.add(sc);

    // Add Procedures section.
    sc = getSection(SectionTypeEnum.PROCEDURES, kd);
    if (sc != null) scs.add(sc);

    // Add Vital Signs section.
    sc = getSection(SectionTypeEnum.VITAL_SIGNS, kd);
    if (sc != null) scs.add(sc);

    // Add Social History section.
    sc = getSection(SectionTypeEnum.SOCIAL_HISTORY, kd);
    if (sc != null) scs.add(sc);

    // Add Pregnancy section.
    sc = getSection(SectionTypeEnum.PREGNANCY, kd);
    if (sc != null) scs.add(sc);

    // Add Emergency Outbreak section.
    sc = getSection(SectionTypeEnum.EMERGENCY_OUTBREAK_SECTION, kd);
    if (sc != null) scs.add(sc);

    // Finalize the sections.
    comp.setSection(scs);

    return comp;
  }

  public SectionComponent getSection(SectionTypeEnum st, KarProcessingData kd) {

    SectionComponent sc = getSectionComponent(st, kd);

    return sc;
  }

  public SectionComponent getSectionComponent(SectionTypeEnum st, KarProcessingData kd) {

    SectionComponent sc = null;

    switch (st) {
      case CHIEF_COMPLAINT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.CHIEF_COMPLAINT_SECTION_LOINC_CODE,
                FhirGeneratorConstants.CHIEF_COMPLAINT_SECTION_LOINC_CODE_DISPLAY);
        populateChiefComplaintNarrative(sc, kd);
        break;

      case HISTORY_OF_PRESENT_ILLNESS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE_DISPLAY);
        break;

      case REVIEW_OF_SYSTEMS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE_DISPLAY);
        break;

      case PROBLEM:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PROBLEM_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PROBLEM_SECTION_LOINC_CODE_DISPLAY);
        break;

      case MEDICAL_HISTORY:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PAST_MEDICAL_HISTORY_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PAST_MEDICAL_HISTORY_SECTION_LOINC_CODE_DISPLAY);
        break;

      case MEDICATION_ADMINISTERED:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.MEDICATION_ADMINISTERED_SECTION_LOINC_CODE,
                FhirGeneratorConstants.MEDICATION_ADMINISTERED_SECTION_LOINC_CODE_DISPLAY);
        break;

      case RESULTS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.RESULTS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.RESULTS_SECTION_LOINC_CODE_DISPLAY);
        break;

      case PLAN_OF_TREATMENT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PLAN_OF_TREATMENT_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PLAN_OF_TREATMENT_SECTION_LOINC_CODE_DISPLAY);
        break;

      case IMMUNIZATIONS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.IMMUNIZATION_SECTION_LOINC_CODE,
                FhirGeneratorConstants.IMMUNIZATION_SECTION_LOINC_CODE_DISPLAY);
        break;

      case PROCEDURES:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PROCEDURE_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PROCEDURE_SECTION_LOINC_CODE_DISPLAY);
        break;

      case VITAL_SIGNS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE_DISPLAY);
        break;

      case SOCIAL_HISTORY:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE,
                FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE_DISPLAY);
        break;

      case PREGNANCY:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PREGNANCY_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PREGNANCY_SECTION_LOINC_CODE_DISPLAY);
        break;

      case EMERGENCY_OUTBREAK_SECTION:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.EMERGENCY_OUTBREAK_SECTION_LOINC_CODE,
                FhirGeneratorConstants.EMERGENCY_OUTBREAK_SECTION_LOINC_CODE_DISPLAY);
        break;

      default:
        sc = null;
        break;
    }

    return sc;
  }

  public List<Extension> getExtensions() {

    Extension ext = new Extension();
    ext.setUrl(VERSION_NUM_URL);
    StringType st = new StringType();
    st.setValue(DEFAULT_VERSION);
    ext.setValue(st);
    List<Extension> exts = new ArrayList<Extension>();
    exts.add(ext);

    return exts;
  }

  public Device getDeviceAuthor() {

    Device dev = new Device();
    DeviceDeviceNameComponent dnc = new DeviceDeviceNameComponent();
    dnc.setName(DEVICE_NAME);
    List<DeviceDeviceNameComponent> dncs = new ArrayList<DeviceDeviceNameComponent>();
    dncs.add(dnc);
    dev.setDeviceName(dncs);

    return dev;
  }

  void populateChiefComplaintNarrative(SectionComponent sc, KarProcessingData kd) {

    Narrative val = new Narrative();
    val.setDivAsString("No Information");
    sc.setText(val);
  }
}
