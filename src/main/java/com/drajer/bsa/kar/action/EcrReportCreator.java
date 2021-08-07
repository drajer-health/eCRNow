package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.fhirecr.FhirGeneratorConstants;
import com.drajer.fhirecr.FhirGeneratorUtils;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Composition;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.Device.DeviceDeviceNameComponent;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.Narrative;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EcrReportCreator extends ReportCreator {

  private static final String DEFAULT_VERSION = "1";
  private static final String VERSION_NUM_URL =
      "http://hl7.org/fhir/StructureDefinition/composition-clinicaldocument-versionNumber";
  private static final String DEVICE_NAME = "eCRNow/Backend Service App";
  private static final String TRIGGER_CODE_EXT_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-trigger-code-flag-extension";
  private static final String TRIGGER_CODE_VALUESET_EXT_URL = "triggerCodeValueSet";
  private static final String TRIGGER_CODE_VALUESET_VERSION_EXT_URL = "triggerCodeValueSetVersion";
  private static final String TRIGGER_CODE_VALUE_EXT_URL = "triggerCode";

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
    Set<Resource> resourcesTobeAdded = new HashSet<>();
    Composition comp = createComposition(kd, resourcesTobeAdded);

    returnBundle.addEntry(new BundleEntryComponent().setResource(comp));

    for (Resource res : resourcesTobeAdded) {

      returnBundle.addEntry(new BundleEntryComponent().setResource(res));
    }

    return returnBundle;
  }

  public Composition createComposition(KarProcessingData kd, Set<Resource> resTobeAdded) {

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
    addEntries(ResourceType.Condition, kd, sc, resTobeAdded);

    // Add Past Medical History section.
    sc = getSection(SectionTypeEnum.MEDICAL_HISTORY, kd);
    if (sc != null) scs.add(sc);

    // Add Medications Administered section.
    sc = getSection(SectionTypeEnum.MEDICATION_ADMINISTERED, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.MedicationAdministration, kd, sc, resTobeAdded);

    // Add Results section.
    sc = getSection(SectionTypeEnum.RESULTS, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);

    // Add Plan Of Treatment section.
    sc = getSection(SectionTypeEnum.PLAN_OF_TREATMENT, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.ServiceRequest, kd, sc, resTobeAdded);
    addEntries(ResourceType.MedicationRequest, kd, sc, resTobeAdded);

    // Add Immunizations section.
    sc = getSection(SectionTypeEnum.IMMUNIZATIONS, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Immunization, kd, sc, resTobeAdded);

    // Add Procedures section.
    sc = getSection(SectionTypeEnum.PROCEDURES, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Procedure, kd, sc, resTobeAdded);

    // Add Vital Signs section.
    sc = getSection(SectionTypeEnum.VITAL_SIGNS, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);

    // Add Social History section.
    sc = getSection(SectionTypeEnum.SOCIAL_HISTORY, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);

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

  public void populateChiefComplaintNarrative(SectionComponent sc, KarProcessingData kd) {

    Narrative val = new Narrative();
    val.setDivAsString("No Information");
    sc.setText(val);
  }

  public void addEntries(
      ResourceType rt, KarProcessingData kd, SectionComponent sc, Set<Resource> resTobeAdded) {

    Set<Resource> resourcesByType = kd.getResourcesByType(rt.toString());
    Set<Resource> res = null;

    if (rt == ResourceType.Observation && isResultsSection(sc)) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.LABORATORY.toCode());
    } else if (rt == ResourceType.Observation && isVitalsSection(sc)) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.VITALSIGNS.toCode());
    } else if (rt == ResourceType.Observation && isSocialHistorySection(sc)) {
      res =
          filterObservationsByCategory(resourcesByType, ObservationCategory.SOCIALHISTORY.toCode());
    } else res = resourcesByType;

    if (res != null && res.size() >= 1) {

      logger.info(" Addding resources of type {}", rt.toString());

      for (Resource r : res) {

        Reference refRes = new Reference();
        refRes.setResource(r);

        // Add Trigger Code extension if appropriate.
        addExtensionIfAppropriate(refRes, r, kd, rt);

        // Add Reference to the entry.
        sc.addEntry(refRes);

        // add the resource to the set
        resTobeAdded.add(r);
      }
    }
  }

  public void addExtensionIfAppropriate(
      Reference ref, Resource res, KarProcessingData kd, ResourceType rt) {

    BsaActionStatus status = kd.getActionStatusByType(ActionType.CheckTriggerCodes);

    if (status != null) {

      CheckTriggerCodeStatus ctcs = (CheckTriggerCodeStatus) (status);
      ctcs.log();

      if (ctcs.containsMatches(rt)) {

        logger.info(" Trigger codes have been found for resource {}", rt.toString());

        // Check to see if the resource being added has the same codes, if so add the extension.
        Pair<Boolean, ReportableMatchedTriggerCode> matchCode = resourceHasMatchedCode(res, ctcs);

        if (matchCode.getValue0() && matchCode.getValue1() != null) {

          Extension ext = new Extension();
          ext.setUrl(TRIGGER_CODE_EXT_URL);

          // Add Value Set Url Extension.
          Extension vsExt = new Extension();
          vsExt.setUrl(TRIGGER_CODE_VALUESET_EXT_URL);
          StringType url = new StringType(matchCode.getValue1().getValueSet());
          vsExt.setValue(url);
          ext.addExtension(vsExt);

          // Add Value Set Version Extension.
          Extension vsVerExt = new Extension();
          vsVerExt.setUrl(TRIGGER_CODE_VALUESET_VERSION_EXT_URL);
          StringType vsVer = new StringType(matchCode.getValue1().getValueSetVersion());
          vsVerExt.setValue(vsVer);
          ext.addExtension(vsVerExt);

          // Add Trigger Code
          Extension tcExt = new Extension();
          tcExt.setUrl(TRIGGER_CODE_VALUE_EXT_URL);

          Coding code = new Coding();
          code.setSystem(matchCode.getValue1().getCodeSystem());
          code.setCode(matchCode.getValue1().getCode());
          tcExt.setValue(code);
          ext.addExtension(tcExt);

          // Add Extension to the Reference .
          ref.addExtension(ext);

        } else {

          logger.info(" Resource {} does not match any trigger code or value.", res.getId());
        }

      } else {
        logger.info("Trigger Matches not found, hence nothing to add");
      }
    } else {

      logger.error("No Trigger codes can be added, as there is no status report from the action ");
    }
  }

  public Pair<Boolean, ReportableMatchedTriggerCode> resourceHasMatchedCode(
      Resource res, CheckTriggerCodeStatus ctcs) {

    Pair<Boolean, ReportableMatchedTriggerCode> mtc = new Pair<>(false, null);

    if (res instanceof Condition) {

      Condition cond = (Condition) res;
      mtc = ctcs.getMatchedCode(cond.getCode());

    } else if (res instanceof Observation) {

    } else if (res instanceof MedicationRequest) {

    } else if (res instanceof ServiceRequest) {

    } else if (res instanceof Immunization) {

    } else if (res instanceof Procedure) {

    } else {

      logger.info(" Resource not being processed for matched codes ");
    }

    return mtc;
  }

  public Boolean isResultsSection(SectionComponent sc) {

    if (sc.getCode() != null
        && sc.getCode().getCodingFirstRep() != null
        && sc.getCode().getCodingFirstRep().getSystem() != null
        && sc.getCode()
            .getCodingFirstRep()
            .getSystem()
            .contentEquals(FhirGeneratorConstants.LOINC_CS_URL)
        && sc.getCode().getCodingFirstRep().getCode() != null
        && sc.getCode()
            .getCodingFirstRep()
            .getCode()
            .contentEquals(FhirGeneratorConstants.RESULTS_SECTION_LOINC_CODE)) {
      return true;
    }

    return false;
  }

  public Boolean isVitalsSection(SectionComponent sc) {

    if (sc.getCode() != null
        && sc.getCode().getCodingFirstRep() != null
        && sc.getCode().getCodingFirstRep().getSystem() != null
        && sc.getCode()
            .getCodingFirstRep()
            .getSystem()
            .contentEquals(FhirGeneratorConstants.LOINC_CS_URL)
        && sc.getCode().getCodingFirstRep().getCode() != null
        && sc.getCode()
            .getCodingFirstRep()
            .getCode()
            .contentEquals(FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE)) {
      return true;
    }

    return false;
  }

  public Boolean isSocialHistorySection(SectionComponent sc) {

    if (sc.getCode() != null
        && sc.getCode().getCodingFirstRep() != null
        && sc.getCode().getCodingFirstRep().getSystem() != null
        && sc.getCode()
            .getCodingFirstRep()
            .getSystem()
            .contentEquals(FhirGeneratorConstants.LOINC_CS_URL)
        && sc.getCode().getCodingFirstRep().getCode() != null
        && sc.getCode()
            .getCodingFirstRep()
            .getCode()
            .contentEquals(FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE)) {
      return true;
    }

    return false;
  }

  public Set<Resource> filterObservationsByCategory(Set<Resource> res, String category) {

    Set<Resource> returnVal = new HashSet<Resource>();
    for (Resource r : res) {

      Observation obs = (Observation) (r);

      if (obs.getCategoryFirstRep() != null
          && obs.getCategoryFirstRep().getCodingFirstRep() != null
          && obs.getCategoryFirstRep().getCodingFirstRep().getSystem() != null
          && obs.getCategoryFirstRep()
              .getCodingFirstRep()
              .getSystem()
              .contentEquals(FhirGeneratorConstants.HL7_OBSERVATION_CATEGORY)
          && obs.getCategoryFirstRep().getCodingFirstRep().getCode() != null
          && obs.getCategoryFirstRep().getCodingFirstRep().getCode().contentEquals(category)) {

        returnVal.add(r);
      }
    }

    return returnVal;
  }
}
