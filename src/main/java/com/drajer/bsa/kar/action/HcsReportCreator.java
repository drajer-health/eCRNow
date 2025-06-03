package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.BsaTypes.SectionTypeEnum;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.ReportGenerationUtils;
import com.drajer.fhirecr.FhirGeneratorConstants;
import com.drajer.fhirecr.FhirGeneratorUtils;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.Address;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Composition;
import org.hl7.fhir.r4.model.Composition.CompositionStatus;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.ContactPoint;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointUse;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.Device.DeviceDeviceNameComponent;
import org.hl7.fhir.r4.model.DomainResource;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MessageHeader;
import org.hl7.fhir.r4.model.MessageHeader.MessageDestinationComponent;
import org.hl7.fhir.r4.model.MessageHeader.MessageSourceComponent;
import org.hl7.fhir.r4.model.Narrative;
import org.hl7.fhir.r4.model.Narrative.NarrativeStatus;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Procedure;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.ServiceRequest;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.UriType;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HcsReportCreator extends ReportCreator {

  private final Logger logger = LoggerFactory.getLogger(HcsReportCreator.class);

  public static final String DEFAULT_VERSION = "1";
  public static final String BUNDLE_REL_URL = "Bundle/";
  public static final String MESSAGE_HEADER_PROFILE =
      "http://hl7.org/fhir/us/ph-library/StructureDefinition/us-ph-messageheader";
  public static final String SENDER_ORG_PROFILE =
      "http://hl7.org/fhir/us/core/StructureDefinition/us-core-organization";
  public static final String MESSAGE_TYPE =
      "http://hl7.org/fhir/us/ph-library/CodeSystem/us-ph-codesystem-message-types";
  public static final String NAMED_EVENT_URL =
      "http://hl7.org/fhir/us/ph-library/ValueSet/us-ph-valueset-triggerdefinition-namedevent";
  public static final String HCS_REPORTING_BUNDLE =
      "http://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-reporting-bundle";
  public static final String HCS_CONTENT_BUNDLE =
      "http://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-content-bundle";
  public static final String HCS_COMPOSITION =
      "http://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-composition";

  private static final String VERSION_NUM_URL =
      "http://hl7.org/fhir/StructureDefinition/composition-clinicaldocument-versionNumber";
  public static final String REPORT_ENCRYPTION_EXT_URL =
      "http://hl7.org/fhir/us/ph-library/StructureDefinition/us-ph-data-encrypted-extension";
  private static final String DEVICE_NAME = "eCRNow/Backend Service App";
  public static final String REPORT_INITIATION_TYPE_EXT_URL =
      "http://hl7.org/fhir/us/ph-library/StructureDefinition/us-ph-report-initiation-type-extension";
  public static final String REPORT_INITIATION_TYPE_CODE_SYSTEM_URL =
      "http://hl7.org/fhir/us/ph-library/CodeSystem/us-ph-codesystem-report-initiation-types";
  public static final String REPORT_INITIATION_TYPE_CODE = "subscription-notification";
  public static final String MESSAGE_PROCESSING_CATEGORY_EXT_URL =
      "http://hl7.org/fhir/us/ph-library/StructureDefinition/us-ph-message-processing-category-extension";
  public static final String MESSAGE_PROCESSING_CATEGORY_EXT_CODE = "consequence";
  public static final String MESSAGE_SIGNIFICANCE_CATEGORY =
      "http://hl7.org/fhir/ValueSet/message-significance-category";

  @Override
  public Resource createReport(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile, BsaAction act) {
    Set<Resource> resources = new HashSet<>();
    kd.getFhirInputDataByType().entrySet().stream().forEach(es -> resources.addAll(es.getValue()));
    return createReport(kd, ehrService, resources, id, profile, act);
  }

  @Override
  public Resource createReport(
      KarProcessingData kd,
      EhrQueryService ehrService,
      Set<Resource> inputData,
      String id,
      String profile,
      BsaAction act) {
    // Create the report as needed by the HCS FHIR IG
    Bundle returnBundle = new Bundle();

    logger.info("Creating report for {}", kd.getKar().getKarId());
    returnBundle.setId(id);
    returnBundle.setType(BundleType.MESSAGE);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    logger.info("HCS Report Resource Count {}", inputData.size());

    // Create the Content Bundle.
    Bundle contentBundle = createContentBundle(kd, ehrService, id, profile);
    BundleEntryComponent contentBundlebec = new BundleEntryComponent().setResource(contentBundle);
    contentBundlebec.setFullUrl(
        StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
            + "/"
            + contentBundle.getResourceType().toString()
            + "/"
            + contentBundle.getIdElement().getIdPart());

    // Create the Message Header resource.
    MessageHeader header = createMessageHeader(kd);
    BundleEntryComponent headerbec = new BundleEntryComponent().setResource(header);
    headerbec.setFullUrl(
        StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
            + "/"
            + header.getResourceType().toString()
            + "/"
            + header.getIdElement().getIdPart());

    Organization sender = createSender(kd);

    // Setup Message Header to Content Bundle Linkage.
    Reference ref = new Reference();
    ref.setReference(BUNDLE_REL_URL + contentBundle.getId());
    List<Reference> refs = new ArrayList<>();
    refs.add(ref);
    header.setFocus(refs);

    // Reference the sender.
    header.setSender(getFullReference(sender, kd.getHealthcareSetting().getFhirServerBaseURL()));

    // Add the Message Header Resource
    returnBundle.addEntry(headerbec);

    // Add the Content Bundle.
    returnBundle.addEntry(contentBundlebec);

    // Add additional resources to the content bundle
    Set<Resource> resourcesTobeAdded = new HashSet<>();
    resourcesTobeAdded.add(sender);
    addResourcesToBundle(kd, contentBundle, resourcesTobeAdded);

    // Add sender to the outer bundle also to resolve the reference issue
    resourcesTobeAdded.clear();
    resourcesTobeAdded.add(sender);
    addResourcesToBundle(kd, returnBundle, resourcesTobeAdded);

    return returnBundle;
  }

  public Reference referenceTo(Resource dest) {
    Reference ref = new Reference();
    ref.setReference(dest.fhirType() + "/" + dest.getId());
    return ref;
  }

  public Reference getFullReference(Resource res, String baseUrl) {

    Reference ref = new Reference();
    ref.setReference(baseUrl + "/" + res.fhirType() + "/" + res.getIdElement().getIdPart());

    return ref;
  }

  public Organization createSender(KarProcessingData kd) {

    HealthcareSetting hs = kd.getHealthcareSetting();
    Organization org = null;

    if (kd.getNotificationContext().getNotificationResourceType()
        == ResourceType.Encounter.toString()) {

      org = new Organization();
      org.setId(UUID.randomUUID().toString());
      // org.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, SENDER_ORG_PROFILE));
      org.setName(hs.getOrgName());
      org.setActive(true);

      List<ContactPoint> cts = new ArrayList<>();
      ContactPoint ct = new ContactPoint();
      ct.setUse(ContactPointUse.WORK);
      ct.setSystem(ContactPointSystem.EMAIL);
      ct.setValue("+1-777-555-1111");
      cts.add(ct);
      org.setTelecom(cts);

      Address addr = new Address();
      addr.setCountry("US");
      org.addAddress();
      org.addIdentifier().setSystem(hs.getOrgIdSystem()).setValue(hs.getOrgId());
    } else {

      org = new Organization();
      org.setId(UUID.randomUUID().toString());
      org.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, SENDER_ORG_PROFILE));
      org.setName(hs.getOrgName());
      org.setActive(true);

      List<ContactPoint> cts = new ArrayList<>();
      ContactPoint ct = new ContactPoint();
      ct.setUse(ContactPointUse.WORK);
      ct.setSystem(ContactPointSystem.EMAIL);
      ct.setValue("+1-777-555-1111");
      cts.add(ct);
      org.setTelecom(cts);

      Address addr = new Address();
      addr.setCountry("US");
      org.addAddress(addr);
      org.addIdentifier().setSystem(hs.getOrgIdSystem()).setValue(hs.getOrgId());
    }

    return org;
  }

  public MessageHeader createMessageHeader(KarProcessingData kd) {

    MessageHeader header = new MessageHeader();

    header.setId(UUID.randomUUID().toString());
    header.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, MESSAGE_HEADER_PROFILE));

    // Add extensions
    Extension ext2 = new Extension();
    ext2.setUrl(REPORT_INITIATION_TYPE_EXT_URL);
    CodeableConcept rptCd = new CodeableConcept();
    Coding rptCoding = new Coding();
    rptCoding.setCode(REPORT_INITIATION_TYPE_CODE);
    rptCoding.setSystem(REPORT_INITIATION_TYPE_CODE_SYSTEM_URL);
    rptCd.addCoding(rptCoding);
    ext2.setValue(rptCd);

    Extension ext3 = new Extension();
    ext3.setUrl(MESSAGE_PROCESSING_CATEGORY_EXT_URL);
    ext3.setValue(new CodeType(MESSAGE_PROCESSING_CATEGORY_EXT_CODE));

    List<Extension> exts = new ArrayList<>();
    exts.add(ext2);
    exts.add(ext3);

    header.setExtension(exts);

    // Set message type.
    Coding c = new Coding();
    c.setSystem(MESSAGE_TYPE);
    c.setCode(BsaTypes.getMessageTypeString(MessageType.HEALTHCARE_SURVEY_REPORT_MESSAGE));
    header.setEvent(c);

    // set destination
    Set<UriType> dests = kd.getKar().getReceiverAddresses();
    List<MessageDestinationComponent> mdcs = new ArrayList<>();
    for (UriType i : dests) {
      MessageDestinationComponent mdc = new MessageDestinationComponent();
      mdc.setEndpoint(i.asStringValue());
      mdcs.add(mdc);
    }
    header.setDestination(mdcs);

    // Set source.
    MessageSourceComponent msc = new MessageSourceComponent();
    msc.setEndpoint(kd.getHealthcareSetting().getFhirServerBaseURL());
    header.setSource(msc);

    // Set Reason.
    CodeableConcept cd = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(NAMED_EVENT_URL);
    coding.setCode(kd.getNotificationContext().getTriggerEvent());
    cd.addCoding(coding);
    header.setReason(cd);

    return header;
  }

  public Bundle createContentBundle(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile) {

    Bundle returnBundle = new Bundle();

    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.COLLECTION);
    returnBundle.setTimestamp(Date.from(Instant.now()));
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, HCS_CONTENT_BUNDLE));

    logger.info(" Creating Composition Resource ");
    Set<Resource> resourcesTobeAdded = new HashSet<>();
    Composition comp = createComposition(kd, resourcesTobeAdded);

    BundleEntryComponent bec = new BundleEntryComponent().setResource(comp);
    bec.setFullUrl(
        StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
            + "/"
            + comp.getResourceType().toString()
            + "/"
            + comp.getIdElement().getIdPart());
    returnBundle.addEntry(bec);

    addResourcesToBundle(kd, returnBundle, resourcesTobeAdded);

    return returnBundle;
  }

  public void addResourcesToBundle(
      KarProcessingData kd, Bundle b, Set<Resource> resourcesTobeAdded) {

    for (Resource res : resourcesTobeAdded) {

      removeExtensions(res);
      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(res);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + res.getResourceType().toString()
              + "/"
              + res.getIdElement().getIdPart());

      b.addEntry(bec);
    }
  }

  public Composition createComposition(KarProcessingData kd, Set<Resource> resTobeAdded) {

    Composition comp = new Composition();
    comp.setId(UUID.randomUUID().toString());
    comp.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, HCS_COMPOSITION));

    // Add clinical document version number extension.
    comp.setExtension(getExtensions());

    // Add Identifier.
    Identifier val = new Identifier();
    val.setValue(comp.getId());
    comp.setIdentifier(val);

    // Add status
    comp.setStatus(CompositionStatus.FINAL);

    // Add Type
    comp.setType(
        FhirGeneratorUtils.getCodeableConcept(
            FhirGeneratorConstants.LOINC_CS_URL,
            FhirGeneratorConstants.HCS_COMP_TYPE_CODE,
            FhirGeneratorConstants.HCS_COMP_TYPE_CODE_DISPLAY));

    // Set Patient
    Set<Resource> patients = kd.getResourcesByType(ResourceType.Patient.toString());
    if (patients != null && !patients.isEmpty()) {

      logger.info(" Setting up the patient for the composition ");
      Resource patient = patients.iterator().next();

      removeExtensions(patient);

      comp.getSubject().setResource(patient);
      resTobeAdded.add(patient);
    } else {

      logger.error(
          " Cannot setup the patient for Composition, need to determine best approach to deal with the error. ");
    }

    // Set Encounter
    Set<Resource> encounters = kd.getResourcesByType(ResourceType.Encounter.toString());
    if (encounters != null && !encounters.isEmpty()) {

      logger.info(" Setting up the patient for the composition ");
      Resource encounter = encounters.iterator().next();
      removeExtensions(encounter);
      comp.getEncounter().setResource(encounter);
      resTobeAdded.add(encounter);

    } else {

      logger.error(
          "Received more than one encounter for processing which is erroneous, using the first one.");
      comp.getEncounter().setResource(encounters.iterator().next());
    }

    // Set Date
    comp.setDate(Date.from(Instant.now()));

    // Set Author
    List<Practitioner> practs = addAuthors(kd, comp);
    if (practs != null && !practs.isEmpty()) resTobeAdded.addAll(practs);

    // Add title
    comp.setTitle(FhirGeneratorConstants.HCS_COMP_TYPE_CODE_DISPLAY);

    // Add Organization
    Organization org = ReportCreationUtilities.getOrganization(kd);

    if (org != null) {
      Reference orgRef = new Reference();
      orgRef.setResource(org);
      comp.setCustodian(orgRef);
      resTobeAdded.add(org);
    }

    // Add sections
    List<SectionComponent> scs = new ArrayList<>();

    SectionComponent sc = null;

    // Add reason for visit section
    sc = getSection(SectionTypeEnum.REASON_FOR_VISIT, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Encounter, kd, sc, resTobeAdded);

    // Add Problem section.
    sc = getSection(SectionTypeEnum.PROBLEM, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Condition, kd, sc, resTobeAdded);

    // Add Allergies section.
    sc = getSection(SectionTypeEnum.ALLERGIES, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.AllergyIntolerance, kd, sc, resTobeAdded);

    // Add Medications Administered section.
    sc = getSection(SectionTypeEnum.MEDICATION_ADMINISTERED, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.MedicationAdministration, kd, sc, resTobeAdded);

    // Add Admission Medications Section
    sc = getSection(SectionTypeEnum.ADMISSION_MEDICATIONS, kd);
    if (sc != null) scs.add(sc);
    // No way to figure out admission medications currently
    // addEntries(ResourceType.MedicationStatement, kd, sc, resTobeAdded);

    // Add Medications Section
    sc = getSection(SectionTypeEnum.MEDICATIONS, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.MedicationDispense, kd, sc, resTobeAdded);

    // Add Results section.
    sc = getSection(SectionTypeEnum.RESULTS, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);
    addEntries(ResourceType.DiagnosticReport, kd, sc, resTobeAdded);

    // Add Notes section.
    sc = getSection(SectionTypeEnum.NOTES, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.DocumentReference, kd, sc, resTobeAdded);
    addEntries(ResourceType.DiagnosticReport, kd, sc, resTobeAdded);

    // Add Plan Of Assessment Treatment section.
    sc = getSection(SectionTypeEnum.PLAN_OF_ASSESSMENT, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);

    // Add Plan Of Treatment section.
    sc = getSection(SectionTypeEnum.PLAN_OF_TREATMENT, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.ServiceRequest, kd, sc, resTobeAdded);
    addEntries(ResourceType.MedicationRequest, kd, sc, resTobeAdded);
    // CarePlan is now part of Plan of Treatment section
    addEntries(ResourceType.CarePlan, kd, sc, resTobeAdded);

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

    // Add Medical Equipment section.
    sc = getSection(SectionTypeEnum.MEDICAL_EQUIPMENT, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Device, kd, sc, resTobeAdded);

    // Add Care Team section.
    sc = getSection(SectionTypeEnum.CARE_TEAM, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.CareTeam, kd, sc, resTobeAdded);

    // Add Goals section.
    sc = getSection(SectionTypeEnum.GOAL, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Goal, kd, sc, resTobeAdded);

    // Add Pregnancy Section
    sc = getSection(SectionTypeEnum.PREGNANCY, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);

    // Add coverage section
    sc = getSection(SectionTypeEnum.COVERAGE, kd);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Coverage, kd, sc, resTobeAdded);

    // Finalize the sections.
    comp.setSection(scs);

    // Add Locations
    Set<Resource> locs = kd.getResourcesByType(ResourceType.Location);
    if (locs != null && !locs.isEmpty()) {
      resTobeAdded.addAll(locs);
    }

    // Add Organizations
    Set<Resource> orgs = kd.getResourcesByType(ResourceType.Organization);
    if (orgs != null && !orgs.isEmpty()) {
      resTobeAdded.addAll(orgs);
    }

    // Add Practitioners
    Set<Resource> practitioners = kd.getResourcesByType(ResourceType.Practitioner);
    if (practitioners != null && !practitioners.isEmpty()) {
      resTobeAdded.addAll(practitioners);
    }

    return comp;
  }

  public List<Practitioner> addAuthors(KarProcessingData kd, Composition comp) {

    List<Practitioner> authors =
        ReportCreationUtilities.getPractitioners(kd, V3ParticipationType.AUT);

    if (authors != null && !authors.isEmpty()) {

      Practitioner author = authors.get(0);
      Reference authReference = new Reference();
      authReference.setResource(author);
      List<Reference> authRefs = new ArrayList<>();
      authRefs.add(authReference);
      comp.setAuthor(authRefs);
    }

    return authors;
  }

  public SectionComponent getSection(SectionTypeEnum st, KarProcessingData kd) {

    return getSectionComponent(st, kd);
  }

  public SectionComponent getSectionComponent(SectionTypeEnum st, KarProcessingData kd) {

    SectionComponent sc = null;

    switch (st) {
      case REASON_FOR_VISIT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.REASON_FOR_VISIT_CODE,
                FhirGeneratorConstants.REASON_FOR_VISIT_CODE_DISPLAY);
        populateReasonForVisitNarrative(sc, kd);
        break;

      case PROBLEM:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PROBLEM_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PROBLEM_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case ALLERGIES:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.ALLERGIES_SECTION_LOINC_CODE,
                FhirGeneratorConstants.ALLERGIES_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case MEDICATION_ADMINISTERED:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.MEDICATION_ADMINISTERED_SECTION_LOINC_CODE,
                FhirGeneratorConstants.MEDICATION_ADMINISTERED_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case ADMISSION_MEDICATIONS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.ADMISSION_MEDICATIONS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.ADMISSION_MEDICATIONS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case MEDICATIONS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.MEDICATIONS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.MEDICATIONS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case RESULTS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.RESULTS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.RESULTS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case PLAN_OF_ASSESSMENT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PLAN_OF_ASSESSMENT_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PLAN_OF_ASSESSMENT_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case PLAN_OF_TREATMENT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PLAN_OF_TREATMENT_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PLAN_OF_TREATMENT_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case NOTES:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.NOTES_SECTION_LOINC_CODE,
                FhirGeneratorConstants.NOTES_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case IMMUNIZATIONS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.IMMUNIZATION_SECTION_LOINC_CODE,
                FhirGeneratorConstants.IMMUNIZATION_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case PROCEDURES:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PROCEDURE_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PROCEDURE_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case VITAL_SIGNS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case SOCIAL_HISTORY:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE,
                FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case MEDICAL_EQUIPMENT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.MEDICAL_EQUIPMENT_SECTION_LOINC_CODE,
                FhirGeneratorConstants.MEDICAL_EQUIPMENT_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case CARE_TEAM:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.CARE_TEAM_SECTION_LOINC_CODE,
                FhirGeneratorConstants.CARE_TEAM_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case GOAL:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.GOALS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.GOALS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case PREGNANCY:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.PREGNANCY_SECTION_LOINC_CODE,
                FhirGeneratorConstants.PREGNANCY_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case COVERAGE:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.COVERAGE_CODE,
                FhirGeneratorConstants.COVERAGE_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
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
    List<Extension> exts = new ArrayList<>();
    exts.add(ext);

    return exts;
  }

  public Device getDeviceAuthor() {

    Device dev = new Device();
    DeviceDeviceNameComponent dnc = new DeviceDeviceNameComponent();
    dnc.setName(DEVICE_NAME);
    List<DeviceDeviceNameComponent> dncs = new ArrayList<>();
    dncs.add(dnc);
    dev.setDeviceName(dncs);

    return dev;
  }

  public void populateReasonForVisitNarrative(SectionComponent sc, KarProcessingData kd) {
    logger.info(" Generating Reason For Visit Narrative");
    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);
    val.setDivAsString("No Information");
    sc.setText(val);
  }

  public void populateDefaultNarrative(SectionComponent sc, KarProcessingData kd) {
    logger.info("KarProcessingData:{}", kd);

    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);
    val.setDivAsString("Not Generated automatically in this version");
    sc.setText(val);
  }

  public void addEntries(
      ResourceType rt, KarProcessingData kd, SectionComponent sc, Set<Resource> resTobeAdded) {

    Set<Resource> resourcesByType = kd.getResourcesByType(rt.toString());
    Set<Resource> res = null;

    if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isResultsSection(sc))) {
      res = filterObservationsForResultsSection(resourcesByType);
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isVitalsSection(sc))) {
      res = ReportGenerationUtils.getVitalSectionObservation(resourcesByType);
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isSocialHistorySection(sc))) {
      res = ReportGenerationUtils.getSmokingStatusObservation(resourcesByType);
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isPregnancySection(sc))) {
      res = ReportGenerationUtils.getPregnancySectionObservation(resourcesByType);
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isAssessmentSection(sc))) {
      res = filterObservationsForAssessmentSection(resourcesByType);
    } else if (resourcesByType != null
        && rt == ResourceType.DiagnosticReport
        && Boolean.TRUE.equals(isResultsSection(sc))) {
      res = filterDiagnosticReportsForResultsSection(resourcesByType);
    } else if (resourcesByType != null
        && rt == ResourceType.DiagnosticReport
        && Boolean.TRUE.equals(isNotesSection(sc))) {
      res = filterDiagnosticReportsForNotesSection(resourcesByType);
    } else res = resourcesByType;

    if (res != null && !res.isEmpty()) {

      logger.info(" Adding resources of type {}", rt);

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
      Reference ref, Resource res, KarProcessingData kd, ResourceType rt) {}

  public Pair<Boolean, ReportableMatchedTriggerCode> resourceHasMatchedCode(
      Resource res, CheckTriggerCodeStatus ctcs) {

    Pair<Boolean, ReportableMatchedTriggerCode> mtc = new Pair<>(false, null);

    if (res instanceof Condition) {

      Condition cond = (Condition) res;
      mtc = ctcs.getMatchedCode(cond.getCode());

    } else if (res instanceof Observation) {
      logger.info(" Observation Resource ");

    } else if (res instanceof MedicationRequest) {
      logger.info(" MedicationRequest Resource ");

    } else if (res instanceof ServiceRequest) {
      logger.info(" ServiceRequest Resource ");

    } else if (res instanceof Immunization) {

      logger.info(" Immunization Resource ");
    } else if (res instanceof Procedure) {
      logger.info(" Procedure Resource ");

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
      logger.info("ResultsSection");
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
      logger.info("VitalsSection");
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
      logger.info("SocialHistorySection");
      return true;
    }

    return false;
  }

  public Boolean isPregnancySection(SectionComponent sc) {

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
            .contentEquals(FhirGeneratorConstants.PREGNANCY_SECTION_LOINC_CODE)) {
      return true;
    }

    return false;
  }

  public Boolean isAssessmentSection(SectionComponent sc) {

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
            .contentEquals(FhirGeneratorConstants.PLAN_OF_ASSESSMENT_SECTION_LOINC_CODE)) {
      return true;
    }

    return false;
  }

  public Boolean isNotesSection(SectionComponent sc) {

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
            .contentEquals(FhirGeneratorConstants.NOTES_SECTION_LOINC_CODE)) {
      return true;
    }

    return false;
  }

  public void removeExtensions(Resource res) {

    if (res instanceof DomainResource) {

      DomainResource r = (DomainResource) res;

      List<Extension> exts = r.getExtension();
      List<Extension> newExts = new ArrayList<>();

      for (Extension ext : exts) {

        if (ext.getUrl().contains("us/core")
            || ext.getUrl().contains("us/medmorph")
            || ext.getUrl().contains("us/healthcare-survey")
            || ext.getUrl().contains("us/ecr")
            || ext.getUrl().contains("us/ph")) {
          newExts.add(ext);
        }
      }

      r.setExtension(newExts);
    }

    // remove observation extensions
    if (res instanceof Observation) {

      Observation obs = (Observation) res;

      if (obs.hasPerformer()) {

        List<Reference> perfs = obs.getPerformer();

        List<Extension> newExts = new ArrayList<>();
        for (Reference perfRef : perfs) {
          newExts.clear();
          if (perfRef.hasExtension()) {

            List<Extension> existingExts = perfRef.getExtension();
            logger.info(" Existing Exts {} ", existingExts.size());
            Boolean found = false;

            for (Extension e : existingExts) {

              if (!found
                  && e.getUrl()
                      .contentEquals(
                          "http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
                newExts.add(e);
                found = true;
              } else if (!e.getUrl()
                  .contentEquals(
                      "http://hl7.org/fhir/StructureDefinition/event-performerFunction")) {
                newExts.add(e);
              }
            }
            logger.info(" New Exts {} ", newExts.size());
            perfRef.setExtension(newExts);
          }
        }
      }
    }
  }

  public Set<Resource> filterObservationsForResultsSection(Set<Resource> res) {
    return ReportGenerationUtils.filterObservationsByCategory(
        res, ObservationCategory.LABORATORY.toCode());
  }

  public Set<Resource> filterObservationsForAssessmentSection(Set<Resource> res) {
    return ReportGenerationUtils.getAssessmentObservations(res);
  }

  public Set<Resource> filterDiagnosticReportsForResultsSection(Set<Resource> res) {
    return ReportGenerationUtils.filterDiagnosticReportsByCategories(
        res, FhirGeneratorConstants.TERMINOLOGY_V2_0074_URL, FhirGeneratorConstants.LAB_CODE);
  }

  public Set<Resource> filterDiagnosticReportsForNotesSection(Set<Resource> res) {
    return ReportGenerationUtils.getNotesDiagnosticReports(res);
  }
}
