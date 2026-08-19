package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.BsaTypes.OutputContentType;
import com.drajer.bsa.model.BsaTypes.SectionTypeEnum;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.bsa.utils.ReportGenerationUtils;
import com.drajer.cda.utils.CdaGeneratorConstants;
import com.drajer.cdafromr4.CdaEicrGeneratorFromR4;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.fhirecr.FhirGeneratorConstants;
import com.drajer.fhirecr.FhirGeneratorUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Composition.CompositionStatus;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Device.DeviceDeviceNameComponent;
import org.hl7.fhir.r4.model.MessageHeader.MessageDestinationComponent;
import org.hl7.fhir.r4.model.MessageHeader.MessageSourceComponent;
import org.hl7.fhir.r4.model.Narrative.NarrativeStatus;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EcrReportCreator extends ReportCreator {

  private static final String DEFAULT_VERSION = "1";

  // FIX 1: Corrected extension URL constants
  private static final String VERSION_NUM_URL =
      "http://hl7.org/fhir/StructureDefinition/composition-clinicaldocument-versionNumber";

  private static final String EICR_INITIATION_TYPE_EXT_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-initiation-type-extension";

  private static final String DEVICE_NAME = "eCRNow/Backend Service App";
  private static final String TRIGGER_CODE_EXT_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-trigger-code-flag-extension";
  private static final String TRIGGER_CODE_VALUESET_EXT_URL = "triggerCodeValueSet";
  private static final String TRIGGER_CODE_VALUESET_VERSION_EXT_URL = "triggerCodeValueSetVersion";
  private static final String TRIGGER_CODE_VALUE_EXT_URL = "triggerCode";

  private static final String EICR_REPORT_LOINC_CODE = "55751-2";
  private static final String EICR_REPORT_LOINC_CODE_SYSTEM = "http://loinc.org";
  public static final String EICR_REPORT_LOINC_CODE_DISPLAY_NAME = "Public Health Case Report";
  public static final String EICR_DOCUMENT_BUNDLE =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-document-bundle";
  public static final String EICR_DOC_CONTENT_TYPE = "application/xml;charset=utf-8";
  public static final String BUNDLE_REL_URL = "Bundle/";
  public static final String EICR_COMPOSITION_PROFILE_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-composition";
  public static final String MESSAGE_PROCESSING_CATEGORY_EXT_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-message-processing-category-extension";
  public static final String MESSAGE_PROCESSING_CATEGORY_CODE = "notification";
  public static final String MESSAGE_HEADER_PROFILE = "";
  public static final String MESSAGE_TYPE_URL =
      "http://hl7.org/fhir/us/ecr/CodeSystem/us-ph-message-types-codesystem";
  private static final String ADDRESS_MEDICATION_REFERENCES_FUTURE =
      " Address Medication References in future ";
  public static final String NAMED_EVENT_URL =
      "http://hl7.org/fhir/us/ecr/CodeSystem/us-ph-triggerdefinition-namedevents";

  private final Logger logger = LoggerFactory.getLogger(EcrReportCreator.class);

  @Override
  public Resource createReport(
      KarProcessingData kd,
      EhrQueryService ehrService,
      Set<Resource> inputData,
      String id,
      String profile,
      BsaAction act) {
    return createReport(kd, ehrService, id, profile, act);
  }

  @Override
  public Resource createReport(
      KarProcessingData kd,
      EhrQueryService ehrService,
      String dataRequirementId,
      String profile,
      BsaAction act) {

    Bundle reportingBundle = null;

    if (kd.getKarStatus().getOutputFormat() == OutputContentType.FHIR) {

      logger.info(" Creating a FHIR Eicr Report ");
      reportingBundle = createReportingBundle(profile);
      Bundle contentBundle = getFhirReport(kd, dataRequirementId, EICR_DOCUMENT_BUNDLE, act);
      MessageHeader mh = createMessageHeader(kd, false, contentBundle);

      BundleEntryComponent messageComponent = new BundleEntryComponent();
      messageComponent.setResource(mh);
      messageComponent.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      BundleEntryComponent contentBundleEntry = new BundleEntryComponent();
      contentBundleEntry.setResource(contentBundle);
      contentBundleEntry.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + contentBundle.getResourceType().toString()
              + "/"
              + contentBundle.getIdElement().getIdPart());

      reportingBundle.addEntry(messageComponent);
      reportingBundle.addEntry(contentBundleEntry);
    } else if (kd.getKarStatus().getOutputFormat() == OutputContentType.CDA_R11) {

      logger.info(" Creating a CDA R11 Eicr Report ");
      reportingBundle = createReportingBundle(profile);
      Bundle contentBundle = getCdaR11Report(kd, dataRequirementId, profile, act);
      MessageHeader mh = createMessageHeader(kd, true, contentBundle);

      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(mh);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      reportingBundle.addEntry(bec);
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle));
    } else if (kd.getKarStatus().getOutputFormat() == OutputContentType.CDA_R30
        || kd.getKarStatus().getOutputFormat() == OutputContentType.CDA_R31) {

      logger.info(" Creating a CDA R31 Eicr Report ");
      reportingBundle = createReportingBundle(profile);
      Bundle contentBundle = getCdaR31Report(kd, ehrService, dataRequirementId, profile, act);
      MessageHeader mh = createMessageHeader(kd, true, contentBundle);

      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(mh);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      reportingBundle.addEntry(bec);
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle));

    } else if (kd.getKarStatus().getOutputFormat() == OutputContentType.BOTH) {

      logger.info(" Creating an Eicr for each of the above formats ");
      reportingBundle = createReportingBundle(profile);
      Bundle contentBundle1 = getCdaR11Report(kd, dataRequirementId, profile, act);
      Bundle contentBundle2 = getFhirReport(kd, dataRequirementId, profile, act);
      Bundle contentBundle3 = getCdaR31Report(kd, ehrService, dataRequirementId, profile, act);
      MessageHeader mh = createMessageHeader(kd, true, contentBundle1);

      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(mh);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      reportingBundle.addEntry(bec);
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle1));
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle2));
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle3));
    }

    return reportingBundle;
  }

  public MessageHeader createMessageHeader(
      KarProcessingData kd, Boolean cdaFlag, Bundle contentBundle) {

    MessageHeader header = new MessageHeader();

    header.setId(UUID.randomUUID().toString());
    header.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, MESSAGE_HEADER_PROFILE));

    Extension ext = new Extension();
    ext.setUrl(MESSAGE_PROCESSING_CATEGORY_EXT_URL);
    ext.setValue(new CodeType(MESSAGE_PROCESSING_CATEGORY_CODE));

    List<Extension> exts = new ArrayList<>();
    exts.add(ext);
    header.setExtension(exts);

    Coding c = new Coding();
    c.setSystem(MESSAGE_TYPE_URL);
    if (Boolean.TRUE.equals(cdaFlag)) {
      c.setCode(BsaTypes.getMessageTypeString(MessageType.CDA_EICR_MESSAGE));
    } else {
      c.setCode(BsaTypes.getMessageTypeString(MessageType.EICR_CASE_REPORT_MESSAGE));
    }
    header.setEvent(c);

    Set<UriType> dests = kd.getKar().getReceiverAddresses();
    List<MessageDestinationComponent> mdcs = new ArrayList<>();
    for (UriType i : dests) {
      MessageDestinationComponent mdc = new MessageDestinationComponent();
      mdc.setEndpoint(i.asStringValue());
      mdcs.add(mdc);
    }
    header.setDestination(mdcs);

    MessageSourceComponent msgComp = new MessageSourceComponent();
    msgComp.setEndpoint(kd.getHealthcareSetting().getFhirServerBaseURL());
    header.setSource(msgComp);

    CodeableConcept codeCpt = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(NAMED_EVENT_URL);
    String triggerEvent = kd.getNotificationContext().getTriggerEvent();
    coding =
        triggerEvent.equalsIgnoreCase("encounter-end")
            ? coding.setCode("encounter-close")
            : coding.setCode(triggerEvent);
    codeCpt.addCoding(coding);
    header.setReason(codeCpt);

    Organization org = ReportCreationUtilities.getOrganization(kd);
    if (org != null) {
      Reference orgRef = new Reference();
      orgRef.setResource(org);
      header.setSender(orgRef);
    }

    Reference ref = new Reference();
    ref.setReference(BUNDLE_REL_URL + contentBundle.getId());
    List<Reference> refs = new ArrayList<>();
    refs.add(ref);
    header.setFocus(refs);

    return header;
  }

  public Bundle createReportingBundle(String profile) {

    Bundle returnBundle = new Bundle();
    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.MESSAGE);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));
    return returnBundle;
  }

  public Bundle getCdaR11Report(
      KarProcessingData kd, String dataRequirementId, String profile, BsaAction act) {

    Bundle returnBundle = new Bundle();
    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.DOCUMENT);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    Eicr ecr = new Eicr();
    Integer submittedVersionNumber = 0;
    if (kd.getPhm() != null) {
      submittedVersionNumber = kd.getPhm().getSubmittedVersionNumber();
    }
    Pair<R4FhirData, LaunchDetails> data =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(kd, act);

    String outputFileName =
        KarProcessingData.LOADING_QUERY_FILE_NAME
            + "_"
            + kd.getNotificationContext().getPatientId()
            + "_"
            + kd.getNotificationContext().getNotificationResourceId();

    BsaServiceUtils.saveFhirResourceToFile(data.getValue0().getData(), outputFileName);

    String eicr =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            data.getValue0(),
            data.getValue1(),
            ecr,
            submittedVersionNumber,
            CdaGeneratorConstants.CDA_EICR_VERSION_R11);

    DocumentReference docref = createR4DocumentReference(kd, eicr, ecr, dataRequirementId);
    returnBundle.addEntry(new BundleEntryComponent().setResource(docref));

    return returnBundle;
  }

  public Bundle getCdaR31Report(
      KarProcessingData kd,
      EhrQueryService ehrService,
      String dataRequirementId,
      String profile,
      BsaAction act) {

    Bundle returnBundle = new Bundle();
    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.DOCUMENT);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));
    logger.info("Ehr Query Service:{}", ehrService);

    logger.info(" Creating Document Reference Resource ");
    Eicr ecr = new Eicr();
    Pair<R4FhirData, LaunchDetails> data =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(kd, act);

    Integer submittedVersionNumber = 0;
    if (kd.getPhm() != null) {
      submittedVersionNumber = kd.getPhm().getSubmittedVersionNumber();
    }

    String outputFileName =
        KarProcessingData.LOADING_QUERY_FILE_NAME
            + "_"
            + kd.getNotificationContext().getPatientId()
            + "_"
            + kd.getNotificationContext().getNotificationResourceId();

    BsaServiceUtils.saveFhirResourceToFile(data.getValue0().getData(), outputFileName);

    String eicr =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            data.getValue0(),
            data.getValue1(),
            ecr,
            submittedVersionNumber,
            CdaGeneratorConstants.CDA_EICR_VERSION_R31);

    DocumentReference docref = createR4DocumentReference(kd, eicr, ecr, dataRequirementId);
    returnBundle.addEntry(new BundleEntryComponent().setResource(docref));

    return returnBundle;
  }

  public DocumentReference createR4DocumentReference(
      KarProcessingData kd, String xmlPayload, Eicr ecr, String dataRequirementId) {
    logger.info("Data Requirement ID:{}", dataRequirementId);

    DocumentReference documentReference = new DocumentReference();
    documentReference.setId(ecr.getEicrDocId());

    documentReference.setStatus(Enumerations.DocumentReferenceStatus.CURRENT);
    documentReference.setDocStatus(DocumentReference.ReferredDocumentStatus.FINAL);

    CodeableConcept typeCode = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    Coding typeCoding = new Coding();
    typeCoding.setSystem(EICR_REPORT_LOINC_CODE_SYSTEM);
    typeCoding.setCode(EICR_REPORT_LOINC_CODE);
    typeCoding.setDisplay(EICR_REPORT_LOINC_CODE_DISPLAY_NAME);
    codingList.add(typeCoding);
    typeCode.setCoding(codingList);
    typeCode.setText(EICR_REPORT_LOINC_CODE_DISPLAY_NAME);
    documentReference.setType(typeCode);

    Reference patientReference = new Reference();
    patientReference.setReference("Patient/" + kd.getNotificationContext().getPatientId());
    documentReference.setSubject(patientReference);

    List<DocumentReference.DocumentReferenceContentComponent> contentList = new ArrayList<>();
    DocumentReference.DocumentReferenceContentComponent contentComp =
        new DocumentReference.DocumentReferenceContentComponent();
    Attachment attachment = new Attachment();
    attachment.setTitle("Initial Public Health Case Report");
    attachment.setContentType(EICR_DOC_CONTENT_TYPE);

    if (xmlPayload != null && !xmlPayload.isEmpty()) {
      attachment.setData(xmlPayload.getBytes());
    }
    contentComp.setAttachment(attachment);
    contentList.add(contentComp);
    documentReference.setContent(contentList);

    if (kd.getNotificationContext()
        .getNotificationResourceType()
        .equals(ResourceType.Encounter.toString())) {
      DocumentReference.DocumentReferenceContextComponent docContextComp =
          new DocumentReference.DocumentReferenceContextComponent();
      List<Reference> encounterRefList = new ArrayList<>();
      Reference encounterReference = new Reference();
      encounterReference.setReference(
          "Encounter/" + kd.getNotificationContext().getNotificationResourceId());
      encounterRefList.add(encounterReference);
      docContextComp.setEncounter(encounterRefList);

      Period period = new Period();
      period.setStart(new Date());
      period.setEnd(new Date());
      docContextComp.setPeriod(period);
      documentReference.setContext(docContextComp);
    }

    logger.debug("DocumentReference Object created successfully ");
    return documentReference;
  }

  // ==========================================================================
  // FIX 12: getFhirReport — bundle profile fallback, identifier urn: prefix,
  //         dedupe entries by fullUrl
  // ==========================================================================
  public Bundle getFhirReport(KarProcessingData kd, String id, String profile, BsaAction act) {

    Bundle returnBundle = new Bundle();
    returnBundle.setId(id);
    returnBundle.setType(BundleType.DOCUMENT);
    returnBundle.setMeta(
        ActionUtils.getMeta(
            DEFAULT_VERSION,
            (profile != null && !profile.isEmpty()) ? profile : EICR_DOCUMENT_BUNDLE));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    // FIX 12: prefix bundle identifier value with urn:uuid:
    Identifier docId = new Identifier();
    docId.setSystem(FhirGeneratorConstants.DOC_ID_SYSTEM);
    docId.setValue("urn:uuid:" + UUID.randomUUID().toString());
    returnBundle.setIdentifier(docId);

    logger.info(" Creating R4FhirData");
    Pair<R4FhirData, LaunchDetails> data =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(kd, act);

    logger.info(" Creating Composition Resource ");
    Set<Resource> resourcesTobeAdded = new HashSet<>();
    Composition comp = createComposition(kd, resourcesTobeAdded, data);

    String baseUrl = StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/");

    // Composition entry must be the FIRST entry in a document bundle
    BundleEntryComponent becComp = new BundleEntryComponent();
    becComp.setResource(comp);
    String fullUrlComp =
        baseUrl + "/" + comp.getResourceType().toString() + "/" + comp.getIdElement().getIdPart();
    becComp.setFullUrl(fullUrlComp);
    returnBundle.addEntry(becComp);

    // FIX 12: dedupe entries by fullUrl to avoid duplicate-id validation errors
    Set<String> seenFullUrls = new HashSet<>();
    seenFullUrls.add(fullUrlComp);

    for (Resource res : resourcesTobeAdded) {
      String entryFullUrl =
          baseUrl + "/" + res.getResourceType().toString() + "/" + res.getIdElement().getIdPart();
      if (seenFullUrls.contains(entryFullUrl)) {
        continue;
      }
      seenFullUrls.add(entryFullUrl);

      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(res);
      bec.setFullUrl(entryFullUrl);
      returnBundle.addEntry(bec);
    }

    return returnBundle;
  }

  // ==========================================================================
  // createComposition — fixes 3, 4, 5, 6, 8, 9, 10
  // ==========================================================================
  public Composition createComposition(
      KarProcessingData kd, Set<Resource> resTobeAdded, Pair<R4FhirData, LaunchDetails> data) {

    Composition comp = new Composition();
    comp.setId(UUID.randomUUID().toString());

    // FIX 8: meta.profile declares eICR Composition canonical URL
    comp.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, EICR_COMPOSITION_PROFILE_URL));

    // Add clinical document version + initiation type extensions
    comp.setExtension(getExtensions());

    // FIX 3: identifier needs system (URN URI scheme) AND value
    Identifier compId = new Identifier();
    compId.setSystem(FhirGeneratorConstants.DOC_ID_SYSTEM); // urn:ietf:rfc:3986
    compId.setValue("urn:uuid:" + comp.getId());
    comp.setIdentifier(compId);

    // Status
    comp.setStatus(CompositionStatus.FINAL);

    // Type
    comp.setType(
        FhirGeneratorUtils.getCodeableConcept(
            FhirGeneratorConstants.LOINC_CS_URL,
            FhirGeneratorConstants.ECR_COMP_TYPE_CODE,
            FhirGeneratorConstants.ECR_COMP_TYPE_CODE_DISPLAY));

    // Subject
    setupSubject(comp, kd, resTobeAdded);

    // Encounter
    setupEncounter(comp, kd, resTobeAdded);

    comp.setDate(Date.from(Instant.now()));

    List<Practitioner> practs = addAuthors(kd, comp);
    if (practs != null && !practs.isEmpty()) {
      resTobeAdded.addAll(practs);
    }

    comp.setTitle(EICR_REPORT_LOINC_CODE_DISPLAY_NAME);
    setupCustodian(comp, kd, resTobeAdded);

    List<SectionComponent> scs = createSections(kd, data, resTobeAdded);
    comp.setSection(scs);

    populateSectionNarratives(scs);

    addLocationResources(kd, resTobeAdded);

    return comp;
  }

  private void setupSubject(Composition comp, KarProcessingData kd, Set<Resource> resTobeAdded) {
    Set<Resource> patients = kd.getResourcesByType(ResourceType.Patient.toString());
    if (patients == null || patients.isEmpty()) {
      logger.error(
          " Cannot setup the patient for Composition, need to determine best approach to deal with the error. ");
      return;
    }
    logger.info(" Setting up the patient for the composition ");
    Resource patient = patients.iterator().next();
    Reference patReference = new Reference();
    patReference.setReference(ResourceType.Patient + "/" + patient.getIdElement().getIdPart());
    comp.setSubject(patReference);
    resTobeAdded.add(patient);
  }

  private void setupEncounter(Composition comp, KarProcessingData kd, Set<Resource> resTobeAdded) {
    Set<Resource> encounters = kd.getResourcesByType(ResourceType.Encounter.toString());
    if (encounters == null || encounters.isEmpty()) {
      return;
    }
    if (encounters.size() == 1) {
      logger.info(" Setting up the encounter for the composition ");
      Resource encounter = encounters.iterator().next();
      Reference encounterRef = new Reference();
      encounterRef.setReference(
          ResourceType.Encounter + "/" + encounter.getIdElement().getIdPart());
      comp.setEncounter(encounterRef);
      resTobeAdded.add(encounter);
    } else {
      logger.error(
          "Received more than one encounter for processing which is erroneous, using the first one.");
      comp.getEncounter().setResource(encounters.iterator().next());
    }
  }

  private void setupCustodian(Composition comp, KarProcessingData kd, Set<Resource> resTobeAdded) {
    Organization org = ReportCreationUtilities.getOrganization(kd);
    if (org == null) {
      logger.info("No Organization from EHR — using fallback custodian Organization");
      org = new Organization();
      org.setId(UUID.randomUUID().toString());
      org.setName(
          kd.getHealthcareSetting() != null && kd.getHealthcareSetting().getOrgName() != null
              ? kd.getHealthcareSetting().getOrgName()
              : "Unknown Healthcare Organization");
    }
    Reference orgRef = new Reference();
    orgRef.setReference(org.fhirType() + "/" + org.getIdElement().getIdPart());
    comp.setCustodian(orgRef);
    resTobeAdded.add(org);
  }

  private List<SectionComponent> createSections(
      KarProcessingData kd, Pair<R4FhirData, LaunchDetails> data, Set<Resource> resTobeAdded) {
    List<SectionComponent> scs = new ArrayList<>();

    addSectionIfPresent(scs, SectionTypeEnum.REASON_FOR_VISIT, data);
    addSectionIfPresent(scs, SectionTypeEnum.CHIEF_COMPLAINT, data);
    addSectionIfPresent(scs, SectionTypeEnum.HISTORY_OF_PRESENT_ILLNESS, data);
    addSectionIfPresent(scs, SectionTypeEnum.REVIEW_OF_SYSTEMS, data);

    SectionComponent sc = getSection(SectionTypeEnum.PROBLEM, data);
    if (sc != null) {
      addEntries(null, ResourceType.Condition, kd, sc, resTobeAdded);
      scs.add(sc);
    }

    addSectionIfPresent(scs, SectionTypeEnum.MEDICAL_HISTORY, data);
    addSectionIfPresent(scs, SectionTypeEnum.ADMISSION_MEDICATIONS, data);

    sc = getSection(SectionTypeEnum.MEDICATION_ADMINISTERED, data);
    if (sc != null) {
      addEntries(null, ResourceType.MedicationAdministration, kd, sc, resTobeAdded);
      scs.add(sc);
    }

    sc = getSection(SectionTypeEnum.MEDICATIONS, data);
    if (sc != null) {
      addEntries(null, ResourceType.MedicationStatement, kd, sc, resTobeAdded);
      scs.add(sc);
    }

    sc = getSection(SectionTypeEnum.RESULTS, data);
    if (sc != null) {
      addEntries(data.getValue0(), ResourceType.Observation, kd, sc, resTobeAdded);
      addEntries(null, ResourceType.DiagnosticReport, kd, sc, resTobeAdded);
      scs.add(sc);
    }

    sc = getSection(SectionTypeEnum.PLAN_OF_TREATMENT, data);
    if (sc != null) {
      addEntries(data.getValue0(), ResourceType.ServiceRequest, kd, sc, resTobeAdded);
      addEntries(data.getValue0(), ResourceType.MedicationRequest, kd, sc, resTobeAdded);
      addEntries(null, ResourceType.DiagnosticReport, kd, sc, resTobeAdded);
      if (sc.hasEntry()) {
        scs.add(sc);
      }
    }

    sc = getSection(SectionTypeEnum.IMMUNIZATIONS, data);
    if (sc != null) {
      addEntries(null, ResourceType.Immunization, kd, sc, resTobeAdded);
      if (sc.hasEntry()) {
        scs.add(sc);
      }
    }

    sc = getSection(SectionTypeEnum.PROCEDURES, data);
    if (sc != null) {
      addEntries(data.getValue0(), ResourceType.Procedure, kd, sc, resTobeAdded);
      if (sc.hasEntry()) {
        scs.add(sc);
      }
    }

    sc = getSection(SectionTypeEnum.VITAL_SIGNS, data);
    if (sc != null) {
      addEntries(data.getValue0(), ResourceType.Observation, kd, sc, resTobeAdded);
      scs.add(sc);
    }

    sc = getSection(SectionTypeEnum.SOCIAL_HISTORY, data);
    if (sc != null) {
      addEntries(data.getValue0(), ResourceType.Observation, kd, sc, resTobeAdded);
      scs.add(sc);
    }

    sc = getSection(SectionTypeEnum.PREGNANCY, data);
    if (sc != null) {
      addEntries(data.getValue0(), ResourceType.Observation, kd, sc, resTobeAdded);
      if (sc.hasEntry()) {
        scs.add(sc);
      }
    }

    addSectionIfPresent(scs, SectionTypeEnum.EMERGENCY_OUTBREAK_SECTION, data);

    return scs;
  }

  private void addSectionIfPresent(
      List<SectionComponent> scs, SectionTypeEnum type, Pair<R4FhirData, LaunchDetails> data) {
    SectionComponent sc = getSection(type, data);
    if (sc != null) {
      scs.add(sc);
    }
  }

  private void populateSectionNarratives(List<SectionComponent> scs) {
    for (SectionComponent scomp : scs) {
      if (sectionHasNoEntries(scomp)) {
        addEmptyNarrative(scomp);
      }
    }
  }

  private boolean sectionHasNoEntries(SectionComponent scomp) {
    return scomp.getEntry() == null || scomp.getEntry().isEmpty();
  }

  private void addLocationResources(KarProcessingData kd, Set<Resource> resTobeAdded) {
    Set<Resource> locs = kd.getResourcesByType(ResourceType.Location);
    if (locs != null && !locs.isEmpty()) {
      resTobeAdded.addAll(locs);
    }
  }

  public List<Practitioner> addAuthors(KarProcessingData kd, Composition comp) {

    List<Practitioner> authors =
        ReportCreationUtilities.getPractitioners(kd, V3ParticipationType.AUT);

    if (authors != null && !authors.isEmpty()) {
      Practitioner author = authors.get(0);
      Reference authReference = new Reference();
      String reference = author.fhirType() + "/" + author.getIdElement().getIdPart();
      authReference.setReference(reference);
      List<Reference> authRefs = new ArrayList<>();
      authRefs.add(authReference);
      comp.setAuthor(authRefs);
    }

    return authors;
  }

  public SectionComponent getSection(SectionTypeEnum st, Pair<R4FhirData, LaunchDetails> data) {
    return getSectionComponent(st, data);
  }

  public SectionComponent getSectionComponent(
      SectionTypeEnum st, Pair<R4FhirData, LaunchDetails> data) {

    SectionComponent sc = null;

    switch (st) {
      case REASON_FOR_VISIT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.REASON_FOR_VISIT_CODE,
                FhirGeneratorConstants.REASON_FOR_VISIT_CODE_DISPLAY);
        populateReasonForVisitNarrative(sc, data);
        break;

      case CHIEF_COMPLAINT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.CHIEF_COMPLAINT_SECTION_LOINC_CODE,
                FhirGeneratorConstants.CHIEF_COMPLAINT_SECTION_LOINC_CODE_DISPLAY);
        populateChiefComplaintNarrative(sc);
        break;

      case HISTORY_OF_PRESENT_ILLNESS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc);
        break;

      case REVIEW_OF_SYSTEMS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc);
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
        populateDefaultNarrative(sc);
        break;

      case MEDICATION_ADMINISTERED:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.MEDICATION_ADMINISTERED_SECTION_LOINC_CODE,
                FhirGeneratorConstants.MEDICATION_ADMINISTERED_SECTION_LOINC_CODE_DISPLAY);
        break;

      case ADMISSION_MEDICATIONS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.ADMISSION_MEDICATIONS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.ADMISSION_MEDICATIONS_SECTION_LOINC_CODE_DISPLAY);
        break;

      case MEDICATIONS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.MEDICATIONS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.MEDICATIONS_SECTION_LOINC_CODE_DISPLAY);

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
        populateDefaultNarrative(sc);
        break;

      default:
        sc = null;
        break;
    }

    return sc;
  }

  // ==========================================================================
  // FIX 2: getExtensions — drop placeholder extensions that fail validation
  // ==========================================================================
  public List<Extension> getExtensions() {

    List<Extension> exts = new ArrayList<>();

    // (1) Clinical document version number — required
    Extension versionExt = new Extension();
    versionExt.setUrl(VERSION_NUM_URL);
    versionExt.setValue(new StringType(DEFAULT_VERSION));
    exts.add(versionExt);

    // (2) eICR Initiation Type — with valid PHIN VADS coding
    Extension initiationTypeExt = new Extension();
    initiationTypeExt.setUrl(EICR_INITIATION_TYPE_EXT_URL);
    CodeableConcept initiationTypeConcept = new CodeableConcept();
    Coding initiationTypeCoding = new Coding();
    initiationTypeCoding.setSystem("urn:oid:2.16.840.1.114222.4.5.274");
    initiationTypeCoding.setCode("1");
    initiationTypeCoding.setDisplay("Provider/Patient Initiated");
    initiationTypeConcept.addCoding(initiationTypeCoding);
    initiationTypeExt.setValue(initiationTypeConcept);
    exts.add(initiationTypeExt);

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

  public void populateReasonForVisitNarrative(
      SectionComponent sc, Pair<R4FhirData, LaunchDetails> data) {
    logger.info("Creating Reason for Visit Narrative ");
    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);

    Encounter encounter = data.getValue0().getEncounter();

    if (encounter != null && encounter.hasText()) {
      val.setDivAsString(encounter.getText().getDivAsString());
    } else if (encounter != null && encounter.hasReasonCode()) {
      val.setDivAsString(
          ReportGenerationUtils.getTextForCodeableConcepts(encounter.getReasonCode()));
    } else {
      val.setDivAsString(
          "<div xmlns=\"http://www.w3.org/1999/xhtml\">No Reason for Visit Information</div>");
    }
    sc.setText(val);
  }

  public void populateChiefComplaintNarrative(SectionComponent sc) {
    logger.info("Creating Chief Complaint Narrative ");

    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);
    val.setDivAsString(
        "<div xmlns=\"http://www.w3.org/1999/xhtml\">No Chief Complaint Information</div>");
    sc.setText(val);
  }

  public void populateDefaultNarrative(SectionComponent sc) {
    logger.info("Adding Narrative ");

    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);
    val.setDivAsString(
        "<div xmlns=\"http://www.w3.org/1999/xhtml\">No Narrative Information</div>");
    sc.setText(val);
  }

  public void populateTextNarrative(SectionComponent sc, Set<Resource> resTobeAdded) {
    logger.info("Adding Narrative ");

    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);

    String resultString =
        resTobeAdded.stream().map(dres -> (DomainResource) (dres)).toList().stream()
            .filter(tdres -> tdres.hasText())
            .map(strres -> strres.getText().getDivAsString())
            .collect(Collectors.joining(", "));

    if (resultString != null && !resultString.isEmpty()) {
      val.setDivAsString(resultString);
    } else {
      val.setDivAsString(
          "<div xmlns=\"http://www.w3.org/1999/xhtml\">No Text Elements found in resources to generate narrative</div>");
    }

    sc.setText(val);
  }

  // ==========================================================================
  // FIX 7: addEmptyNarrative — status must match content (ADDITIONAL not EMPTY)
  // ==========================================================================
  public void addEmptyNarrative(SectionComponent sc) {
    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);
    val.setDivAsString("<div xmlns=\"http://www.w3.org/1999/xhtml\">No Information</div>");
    sc.setText(val);
  }

  // ==========================================================================
  // FIX 11: addEntries — only populate narrative when textResources is non-empty
  // ==========================================================================
  public void addEntries(
      R4FhirData data,
      ResourceType rt,
      KarProcessingData kd,
      SectionComponent sc,
      Set<Resource> resTobeAdded) {

    if (sc == null) {
      return;
    }

    Set<Resource> resourcesByType = kd.getResourcesByType(rt.toString());

    Set<Resource> res = null;
    Set<Resource> textResources = new HashSet<>();
    ObservationCategory filteredCategory = ObservationCategory.NULL;
    if ((resourcesByType == null || resourcesByType.isEmpty())
        && data != null
        && data.getData() != null
        && data.getData().getEntry() != null) {
      resourcesByType =
          data.getData().getEntry().stream()
              .map(Bundle.BundleEntryComponent::getResource)
              .filter(resource -> resource.fhirType().equals(rt.toString()))
              .collect(Collectors.toSet());
    }

    if (ReportGenerationUtils.isPregnancySection(sc)) {
      res = ReportGenerationUtils.filterPregnancyObservations(resourcesByType);
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isResultsSection(sc))) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.LABORATORY.toCode());
      filteredCategory = ObservationCategory.LABORATORY;
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isVitalsSection(sc))) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.VITALSIGNS.toCode());
      if (data != null && data.getVitalObs() != null) {
        List<Resource> resources = data.getVitalObs().stream().map(obs -> (Resource) obs).toList();
        res.addAll(resources);
      }
      filteredCategory = ObservationCategory.VITALSIGNS;
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isSocialHistorySection(sc))) {
      res =
          filterObservationsByCategory(resourcesByType, ObservationCategory.SOCIALHISTORY.toCode());
      filteredCategory = ObservationCategory.SOCIALHISTORY;

      if (ReportGenerationUtils.isSocialHistorySection(sc)) {
        res = ReportGenerationUtils.filterSocialHistoryObservations(res);
      }
    } else if (resourcesByType != null
        && rt == ResourceType.DiagnosticReport
        && Boolean.TRUE.equals(isResultsSection(sc))) {
      res = filterDiagnosticReports(resourcesByType, true);
    } else if (resourcesByType != null
        && rt == ResourceType.DiagnosticReport
        && Boolean.TRUE.equals(isPlanOfTreatmentSection(sc))) {
      res = filterDiagnosticReports(resourcesByType, false);
    } else {
      res = resourcesByType;
    }

    if (res != null && !res.isEmpty()) {

      logger.info(" Adding resources of type {}", rt);

      for (Resource r : res) {
        Reference refRes = new Reference();

        String reference = r.fhirType() + "/" + r.getIdElement().getIdPart();

        refRes.setReference(reference);

        addExtensionIfAppropriate(refRes, r, kd, rt, filteredCategory);

        sc.addEntry(refRes);
        resTobeAdded.add(r);
        textResources.add(r);
      }

      // FIX 11: only build narrative when we actually have resources
      if (!textResources.isEmpty()) {
        populateTextNarrative(sc, textResources);
      }
    }
  }

  private Set<Resource> filterDiagnosticReports(Set<Resource> resourcesByType, boolean resultFlag) {
    return ReportGenerationUtils.filterDiagnosticReports(resourcesByType, resultFlag);
  }

  public void addExtensionIfAppropriate(
      Reference ref,
      Resource res,
      KarProcessingData kd,
      ResourceType rt,
      ObservationCategory filteredCategory) {

    List<BsaActionStatus> status = kd.getActionStatusByType(ActionType.CHECK_TRIGGER_CODES);

    if (status != null && !status.isEmpty()) {

      CheckTriggerCodeStatus ctcs = (CheckTriggerCodeStatus) (status.get(0));

      if (Boolean.TRUE.equals(ctcs.containsMatches(rt))) {

        logger.info(" Trigger codes have been found for resource {}", rt);

        Pair<Boolean, ReportableMatchedTriggerCode> matchCode =
            resourceHasMatchedCode(res, ctcs, filteredCategory);

        if (Boolean.TRUE.equals(matchCode.getValue0()) && matchCode.getValue1() != null) {

          Extension ext = new Extension();
          ext.setUrl(TRIGGER_CODE_EXT_URL);

          // Value Set URL — ensure urn:oid: prefix
          Extension vsExt = new Extension();
          vsExt.setUrl(TRIGGER_CODE_VALUESET_EXT_URL);
          String oidValue = matchCode.getValue1().getValueSetOid();
          if (oidValue != null && !oidValue.startsWith("urn:oid:")) {
            oidValue = "urn:oid:" + oidValue;
          }
          OidType oid = new OidType();
          oid.setValue(oidValue);
          vsExt.setValue(oid);
          ext.addExtension(vsExt);

          // Value Set Version
          Extension vsVerExt = new Extension();
          vsVerExt.setUrl(TRIGGER_CODE_VALUESET_VERSION_EXT_URL);
          StringType vsVer = new StringType(matchCode.getValue1().getValueSetVersion());
          vsVerExt.setValue(vsVer);
          ext.addExtension(vsVerExt);

          // Trigger Code
          Extension tcExt = new Extension();
          tcExt.setUrl(TRIGGER_CODE_VALUE_EXT_URL);
          Coding code = new Coding();
          code.setSystem(matchCode.getValue1().getCodeSystem());
          code.setCode(matchCode.getValue1().getCode());
          tcExt.setValue(code);
          ext.addExtension(tcExt);

          ref.addExtension(ext);

        } else {
          logger.debug(" Resource {} does not match any trigger code or value.", res.getId());
        }

      } else {
        logger.info("Trigger Matches not found, hence nothing to add");
      }
    } else {
      logger.error("No Trigger codes can be added, as there is no status report from the action ");
    }
  }

  public Pair<Boolean, ReportableMatchedTriggerCode> resourceHasMatchedCode(
      Resource res, CheckTriggerCodeStatus ctcs, ObservationCategory filteredCategory) {

    Pair<Boolean, ReportableMatchedTriggerCode> mtc = new Pair<>(false, null);

    if (res instanceof Condition) {
      Condition cond = (Condition) res;
      mtc = ctcs.getMatchedCode(cond.getCode());

    } else if (res instanceof Observation && filteredCategory == ObservationCategory.LABORATORY) {
      logger.info(" Observation Resource ");
      Observation obs = (Observation) res;
      mtc = ctcs.getMatchedCode(obs.getCode());

      if (!mtc.getValue0() && obs.hasComponent()) {
        for (ObservationComponentComponent ob : obs.getComponent()) {
          mtc = ctcs.getMatchedCode(ob.getCode());
          if (mtc.getValue0()) break;
        }
      }

    } else if (res instanceof DiagnosticReport) {
      logger.info(" DiagnosticReport Resource ");
      DiagnosticReport dr = (DiagnosticReport) res;
      mtc = ctcs.getMatchedCode(dr.getCode());

    } else if (res instanceof MedicationRequest) {
      logger.info(" MedicationRequest Resource ");
      MedicationRequest mr = (MedicationRequest) res;
      if (mr.hasMedicationCodeableConcept()) {
        mtc = ctcs.getMatchedCode(mr.getMedicationCodeableConcept());
      } else if (mr.hasMedicationReference()) {
        logger.info(ADDRESS_MEDICATION_REFERENCES_FUTURE);
      }

    } else if (res instanceof MedicationStatement) {
      logger.info(" MedicationStatement Resource ");
      MedicationStatement ms = (MedicationStatement) res;
      if (ms.hasMedicationCodeableConcept()) {
        mtc = ctcs.getMatchedCode(ms.getMedicationCodeableConcept());
      } else if (ms.hasMedicationReference()) {
        logger.info(ADDRESS_MEDICATION_REFERENCES_FUTURE);
      }

    } else if (res instanceof MedicationAdministration) {
      logger.info(" MedicationAdmininstration Resource ");
      MedicationAdministration ma = (MedicationAdministration) res;
      if (ma.hasMedicationCodeableConcept()) {
        mtc = ctcs.getMatchedCode(ma.getMedicationCodeableConcept());
      } else if (ma.hasMedicationReference()) {
        logger.info(ADDRESS_MEDICATION_REFERENCES_FUTURE);
      }

    } else if (res instanceof ServiceRequest) {
      logger.info(" ServiceRequest Resource ");
      ServiceRequest sr = (ServiceRequest) res;
      mtc = ctcs.getMatchedCode(sr.getCode());

    } else if (res instanceof Immunization) {
      logger.info(" Immunization Resource ");
      Immunization imm = (Immunization) res;
      mtc = ctcs.getMatchedCode(imm.getVaccineCode());

    } else if (res instanceof Procedure) {
      logger.info(" Procedure Resource ");
      Procedure pr = (Procedure) res;
      mtc = ctcs.getMatchedCode(pr.getCode());

    } else {
      logger.info(" Resource not being processed for matched codes ");
    }

    return mtc;
  }

  public Boolean isResultsSection(SectionComponent sc) {
    if (sc != null
        && sc.getCode() != null
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
    if (sc != null
        && sc.getCode() != null
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
    if (sc != null
        && sc.getCode() != null
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

  public Boolean isPlanOfTreatmentSection(SectionComponent sc) {
    if (sc != null
        && sc.getCode() != null
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
            .contentEquals(FhirGeneratorConstants.PLAN_OF_TREATMENT_SECTION_LOINC_CODE)) {
      logger.info("Plan Of Treatment Section");
      return true;
    }
    return false;
  }

  public Set<Resource> filterObservationsByCategory(Set<Resource> res, String category) {
    return ReportGenerationUtils.filterObservationsByCategory(res, category);
  }

  public Set<Resource> getMeasureReports(KarProcessingData kd) {

    Set<Resource> measureReports = kd.getResourcesByType(ResourceType.MeasureReport.toString());

    if (measureReports != null && !measureReports.isEmpty()) {
      logger.info("Measure Reports found for processing: {}", measureReports.size());
      return measureReports;
    }

    Set<Resource> outputMeasureReports =
        kd.getOutputDataById(ResourceType.MeasureReport.toString());

    if (outputMeasureReports == null || outputMeasureReports.isEmpty()) {
      outputMeasureReports = kd.getOutputDataById("measurereport");
    }

    if (outputMeasureReports != null && !outputMeasureReports.isEmpty()) {
      logger.info(
          "Measure Reports found in output data for processing: {}", outputMeasureReports.size());
      return outputMeasureReports;
    }

    logger.info("No Measure Reports found for processing");
    return new HashSet<>();
  }
}
