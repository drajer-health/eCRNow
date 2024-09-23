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
import org.hl7.fhir.r4.model.CanonicalType;
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
import org.springframework.beans.factory.annotation.Value;

public class CcrrReportCreator extends ReportCreator {

  private final Logger logger = LoggerFactory.getLogger(CcrrReportCreator.class);

  public static final String DEFAULT_VERSION = "1";
  public static final String BUNDLE_REL_URL = "Bundle/";
  public static final String MESSAGE_HEADER_PROFILE =
      "http://hl7.org/fhir/us/central-cancer-registry-reporting/StructureDefinition/central-cancer-registry-reporting-messageheader";
  public static final String SENDER_ORG_PROFILE =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-organization";
  public static final String MESSAGE_TYPE =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-messageheader-message-types";
  public static final String NAMED_EVENT_URL =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents";
  public static final String CCRR_REPORTING_BUNDLE =
      "http://hl7.org/fhir/us/central-cancer-registry-reporting/StructureDefinition/ccrr-reporting-bundle";
  public static final String CCRR_CONTENT_BUNDLE =
      "http://hl7.org/fhir/us/central-cancer-registry-reporting/StructureDefinition/ccrr-content-bundle";
  public static final String CCRR_COMPOSITION =
      "http://hl7.org/fhir/us/central-cancer-registry-reporting/StructureDefinition/ccrr-composition";

  private static final String VERSION_NUM_URL =
      "http://hl7.org/fhir/StructureDefinition/composition-clinicaldocument-versionNumber";
  private static final String DEVICE_NAME = "eCRNow/Backend Service App";
  public static final String REPORT_INITIATION_TYPE_EXT_URL =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-report-initiation-type";
  public static final String REPORT_INITIATION_TYPE_CODE_SYSTEM_URL =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-report-initiation-types";
  public static final String REPORT_INITIATION_TYPE_CODE = "subscription-notification";
  public static final String MESSAGE_SIGNIFICANCE_CATEGORY =
      "http://hl7.org/fhir/ValueSet/message-significance-category";

  public static final String PRIMARY_CANCER_CONDITION_PROFILE =
      "http://hl7.org/fhir/us/central-cancer-registry-reporting/StructureDefinition/central-cancer-registry-primary-cancer-condition";
  public static final String SECONDARY_CANCER_CONDITION_PROFILE =
      "http://hl7.org/fhir/us/mcode/StructureDefinition/mcode-secondary-cancer-condition";
  public static final String TNM_STAGE_GROUP_PROFILE =
      "http://hl7.org/fhir/us/mcode/StructureDefinition/mcode-tnm-stage-group";
  public static final String RADIO_THERAPY_COURSE_SUMMARY_PROFILE =
      "http://hl7.org/fhir/us/mcode/StructureDefinition/mcode-radiotherapy-course-summary";
  public static final String ODH_USUAL_WORK_PROFILE =
      "http://hl7.org/fhir/us/odh/StructureDefinition/odh-UsualWork";

  @Value("${ehr.product.name:Unknown-Ehr-Product}")
  private String ehrProductName;

  @Value("${ehr.product.version:Unknown-Version}")
  private String ehrProductVersion;

  @Value("${ehr.name:Unknown-Ehr}")
  private String ehrName;

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

    logger.info("CCRR Report Resource Count {}", inputData.size());

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

    List<Extension> exts = new ArrayList<>();
    exts.add(ext2);

    header.setExtension(exts);

    // Set message type.
    Coding c = new Coding();
    c.setSystem(MESSAGE_TYPE);
    c.setCode(BsaTypes.getMessageTypeString(MessageType.CANCER_REPORT_MESSAGE));
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

    msc.setName(ehrName);
    msc.setSoftware(ehrProductName);
    msc.setVersion(ehrProductVersion);
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
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, CCRR_CONTENT_BUNDLE));

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
    comp.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, CCRR_COMPOSITION));

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
            FhirGeneratorConstants.CCRR_COMP_TYPE_CODE,
            FhirGeneratorConstants.CCRR_COMP_TYPE_CODE_DISPLAY));

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

      logger.error("Received no encounter for processing which is erroneous.");
    }

    // Set Date
    comp.setDate(Date.from(Instant.now()));

    // Set Author
    List<Practitioner> practs = addAuthors(kd, comp);
    if (practs != null && !practs.isEmpty()) resTobeAdded.addAll(practs);

    // Add title
    comp.setTitle(FhirGeneratorConstants.CCRR_COMP_TYPE_CODE_DISPLAY);

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
    Set<String> profilesToIgnore = new HashSet<>();

    SectionComponent sc = null;

    // Add primary cancer condition report section
    sc = getSection(SectionTypeEnum.PRIMARY_CANCER_CONDITION, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(
        ResourceType.Condition,
        kd,
        sc,
        resTobeAdded,
        PRIMARY_CANCER_CONDITION_PROFILE,
        profilesToIgnore);

    // Add secondary cancer condition report section
    sc = getSection(SectionTypeEnum.SECONDARY_CANCER_CONDITION, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(
        ResourceType.Condition,
        kd,
        sc,
        resTobeAdded,
        SECONDARY_CANCER_CONDITION_PROFILE,
        profilesToIgnore);

    // Cancer Stage Group
    sc = getSection(SectionTypeEnum.CANCER_STAGE_GROUP, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(
        ResourceType.Observation, kd, sc, resTobeAdded, TNM_STAGE_GROUP_PROFILE, profilesToIgnore);

    // Radio Therapy Course Summary
    sc = getSection(SectionTypeEnum.RADIO_THERAPY_COURSE_SUMMARY, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(
        ResourceType.Procedure,
        kd,
        sc,
        resTobeAdded,
        RADIO_THERAPY_COURSE_SUMMARY_PROFILE,
        profilesToIgnore);

    // Odh Section
    sc = getSection(SectionTypeEnum.ODH, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(
        ResourceType.Observation, kd, sc, resTobeAdded, ODH_USUAL_WORK_PROFILE, profilesToIgnore);

    // Add Problem section.
    sc = getSection(SectionTypeEnum.PROBLEM, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    profilesToIgnore.add(PRIMARY_CANCER_CONDITION_PROFILE);
    profilesToIgnore.add(SECONDARY_CANCER_CONDITION_PROFILE);
    addEntries(ResourceType.Condition, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Allergies section.
    sc = getSection(SectionTypeEnum.ALLERGIES, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(ResourceType.AllergyIntolerance, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Medications Administered section.
    sc = getSection(SectionTypeEnum.MEDICATION_ADMINISTERED, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(ResourceType.MedicationAdministration, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Medications Section
    sc = getSection(SectionTypeEnum.MEDICATIONS, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(ResourceType.MedicationStatement, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Results section.
    sc = getSection(SectionTypeEnum.RESULTS, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    profilesToIgnore.add(TNM_STAGE_GROUP_PROFILE);
    profilesToIgnore.add(ODH_USUAL_WORK_PROFILE);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded, "", profilesToIgnore);
    addEntries(ResourceType.DiagnosticReport, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Notes section.
    sc = getSection(SectionTypeEnum.NOTES, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(ResourceType.DocumentReference, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Plan Of Treatment section.
    sc = getSection(SectionTypeEnum.PLAN_OF_TREATMENT, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    addEntries(ResourceType.ServiceRequest, kd, sc, resTobeAdded, "", profilesToIgnore);
    addEntries(ResourceType.MedicationRequest, kd, sc, resTobeAdded, "", profilesToIgnore);
    addEntries(ResourceType.CarePlan, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Procedures section.
    sc = getSection(SectionTypeEnum.PROCEDURES, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.add(RADIO_THERAPY_COURSE_SUMMARY_PROFILE);
    addEntries(ResourceType.Procedure, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Vital Signs section.
    sc = getSection(SectionTypeEnum.VITAL_SIGNS, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    profilesToIgnore.add(TNM_STAGE_GROUP_PROFILE);
    profilesToIgnore.add(ODH_USUAL_WORK_PROFILE);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded, "", profilesToIgnore);

    // Add Social History section.
    sc = getSection(SectionTypeEnum.SOCIAL_HISTORY, kd);
    if (sc != null) scs.add(sc);
    profilesToIgnore.clear();
    profilesToIgnore.add(TNM_STAGE_GROUP_PROFILE);
    profilesToIgnore.add(ODH_USUAL_WORK_PROFILE);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded, "", profilesToIgnore);

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
      case PRIMARY_CANCER_CONDITION:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.SNOMED_CS_URL,
                FhirGeneratorConstants.PRIMARY_CANCER_CONDITION_SECTION_CODE,
                FhirGeneratorConstants.PRIMARY_CANCER_CONDITION_SECTION_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case SECONDARY_CANCER_CONDITION:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.SNOMED_CS_URL,
                FhirGeneratorConstants.SECONDARY_CANCER_CONDITION_SECTION_CODE,
                FhirGeneratorConstants.SECONDARY_CANCER_CONDITION_SECTION_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case CANCER_STAGE_GROUP:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.CANCER_STAGE_GROUP_SECTION_CODE,
                FhirGeneratorConstants.CANCER_STAGE_GROUP_SECTION_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case RADIO_THERAPY_COURSE_SUMMARY:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.SNOMED_CS_URL,
                FhirGeneratorConstants.CANCER_RADIO_THERAPY_COURSE_SUMMARY_SECTION_CODE,
                FhirGeneratorConstants.CANCER_RADIO_THERAPY_COURSE_SUMMARY_SECTION_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case ODH:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.ODH_SECTION_CODE,
                FhirGeneratorConstants.ODH_SECTION_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
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
      ResourceType rt,
      KarProcessingData kd,
      SectionComponent sc,
      Set<Resource> resTobeAdded,
      String profile,
      Set<String> profilesToIgnore) {

    Set<Resource> resourcesByType = kd.getResourcesByType(rt.toString());
    Set<Resource> res = null;

    if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isResultsSection(sc))) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.LABORATORY.toCode());
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isVitalsSection(sc))) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.VITALSIGNS.toCode());
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isSocialHistorySection(sc))) {
      res =
          filterObservationsByCategory(resourcesByType, ObservationCategory.SOCIALHISTORY.toCode());
    } else res = resourcesByType;

    res = filterResourcesByProfile(res, profile, profilesToIgnore);

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

  public Set<Resource> filterResourcesByProfile(
      Set<Resource> resources, String profile, Set<String> profilesToIgnore) {

    Set<Resource> resToReturn = new HashSet<>();

    for (Resource r : resources) {

      if (!profile.isEmpty() && r.hasMeta() && r.getMeta().hasProfile()) {

        List<CanonicalType> profiles = r.getMeta().getProfile();

        // Check for Profile irrespective of what needs to be ignored
        for (CanonicalType c : profiles) {
          if (c.hasValue() && c.getValue().contentEquals(profile)) {
            resToReturn.add(r);
            break;
          }
        }
      } else if (profile.isEmpty() && !profilesToIgnore.isEmpty()) {

        // Ensure to add only if it is not in the ignore list
        if (r.hasMeta() && r.getMeta().hasProfile()) {

          List<CanonicalType> profiles = r.getMeta().getProfile();

          // Check for Profile irrespective of what needs to be ignored
          for (CanonicalType c : profiles) {
            if (c.hasValue() && c.getValue().contentEquals(profile)) {
              resToReturn.add(r);
              break;
            }
          }
        } else {
          // Cannot compare, so assume it is not one of the ones to be ignored.
          resToReturn.add(r);
        }

      } else if ((profile.isEmpty() && profilesToIgnore.isEmpty())) {

        // Ensure to add when there is no filtering needed.
        resToReturn.add(r);
      }
    }
    return resToReturn;
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
            || ext.getUrl().contains("us/ccrr")
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

  public Set<Resource> filterObservationsByCategory(Set<Resource> res, String category) {

    if (ObservationCategory.SOCIALHISTORY.toCode().contentEquals(category)) {
      return ReportGenerationUtils.getSmokingStatusObservation(res);
    } else {
      return ReportGenerationUtils.filterObservationsByCategory(res, category);
    }
  }
}
