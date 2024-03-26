package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.BsaTypes.OutputContentType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.bsa.utils.ReportGenerationUtils;
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
import org.hl7.fhir.r4.model.Attachment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Composition;
import org.hl7.fhir.r4.model.Composition.CompositionStatus;
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Condition;
import org.hl7.fhir.r4.model.Device;
import org.hl7.fhir.r4.model.Device.DeviceDeviceNameComponent;
import org.hl7.fhir.r4.model.DiagnosticReport;
import org.hl7.fhir.r4.model.DocumentReference;
import org.hl7.fhir.r4.model.DomainResource;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.Identifier;
import org.hl7.fhir.r4.model.Immunization;
import org.hl7.fhir.r4.model.MedicationAdministration;
import org.hl7.fhir.r4.model.MedicationRequest;
import org.hl7.fhir.r4.model.MedicationStatement;
import org.hl7.fhir.r4.model.MessageHeader;
import org.hl7.fhir.r4.model.MessageHeader.MessageDestinationComponent;
import org.hl7.fhir.r4.model.MessageHeader.MessageSourceComponent;
import org.hl7.fhir.r4.model.Narrative;
import org.hl7.fhir.r4.model.Narrative.NarrativeStatus;
import org.hl7.fhir.r4.model.Observation;
import org.hl7.fhir.r4.model.Observation.ObservationComponentComponent;
import org.hl7.fhir.r4.model.OidType;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Period;
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
  public static final String MESSAGE_HEADER_PROFILE =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-messageheader";
  public static final String MESSAGE_TYPE_URL =
      "http://hl7.org/fhir/us/ecr/CodeSystem/us-ph-message-types-codesystem";
  public static final String NAMED_EVENT_URL =
      "http://hl7.org/fhir/us/ecr/CodeSystem/us-ph-triggerdefinition-namedevents";

  private final Logger logger = LoggerFactory.getLogger(EcrReportCreator.class);

  public enum SectionTypeEnum {
    REASON_FOR_VISIT,
    CHIEF_COMPLAINT,
    HISTORY_OF_PRESENT_ILLNESS,
    REVIEW_OF_SYSTEMS,
    PROBLEM,
    MEDICAL_HISTORY,
    MEDICATION_ADMINISTERED,
    ADMISSION_MEDICATIONS,
    MEDICATIONS,
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
      Bundle contentBundle =
          getFhirReport(kd, ehrService, dataRequirementId, EICR_DOCUMENT_BUNDLE, act);
      MessageHeader mh = createMessageHeader(kd, false, contentBundle);

      // Add the Message Header Resource
      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(mh);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      reportingBundle.addEntry(bec);

      // Add the Content Bundle.
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle));
    } else if (kd.getKarStatus().getOutputFormat() == OutputContentType.CDA_R11) {

      logger.info(" Creating a CDA R11 Eicr Report ");

      reportingBundle = createReportingBundle(profile);
      Bundle contentBundle = getCdaR11Report(kd, ehrService, dataRequirementId, profile, act);
      MessageHeader mh = createMessageHeader(kd, true, contentBundle);

      // Add the Message Header Resource
      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(mh);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      reportingBundle.addEntry(bec);

      // Add the Content Bundle.
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle));
    } else if (kd.getKarStatus().getOutputFormat() == OutputContentType.CDA_R30) {

      logger.info(" Creating a Test CDA R30 Not For Production Eicr Report ");
      reportingBundle = createReportingBundle(profile);
      Bundle contentBundle = getCdaR11Report(kd, ehrService, dataRequirementId, profile, act);
      MessageHeader mh = createMessageHeader(kd, true, contentBundle);

      // Add the Message Header Resource
      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(mh);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      reportingBundle.addEntry(bec);

      // Add the Content Bundle.
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle));

    } else if (kd.getKarStatus().getOutputFormat() == OutputContentType.BOTH) {

      logger.info(" Creating an Eicr for each of the above formats ");

      reportingBundle = createReportingBundle(profile);
      Bundle contentBundle1 = getCdaR11Report(kd, ehrService, dataRequirementId, profile, act);
      Bundle contentBundle2 = getFhirReport(kd, ehrService, dataRequirementId, profile, act);
      MessageHeader mh = createMessageHeader(kd, true, contentBundle1);

      // Add the Message Header Resource
      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(mh);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + mh.getResourceType().toString()
              + "/"
              + mh.getIdElement().getIdPart());

      reportingBundle.addEntry(bec);

      // Add the Content Bundles..
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle1));
      reportingBundle.addEntry(new BundleEntryComponent().setResource(contentBundle2));
    }

    return reportingBundle;
  }

  public MessageHeader createMessageHeader(
      KarProcessingData kd, Boolean cdaFlag, Bundle contentBundle) {

    MessageHeader header = new MessageHeader();

    header.setId(UUID.randomUUID().toString());
    header.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, MESSAGE_HEADER_PROFILE));

    // Add extensions
    Extension ext = new Extension();
    ext.setUrl(MESSAGE_PROCESSING_CATEGORY_EXT_URL);
    StringType st = new StringType();
    st.setValue(MESSAGE_PROCESSING_CATEGORY_CODE);
    ext.setValue(st);
    List<Extension> exts = new ArrayList<>();
    exts.add(ext);

    header.setExtension(exts);

    // Set message type.
    Coding c = new Coding();
    c.setSystem(MESSAGE_TYPE_URL);
    if (Boolean.TRUE.equals(cdaFlag)) {
      c.setCode(BsaTypes.getMessageTypeString(MessageType.CDA_EICR_MESSAGE));
    } else {
      c.setCode(BsaTypes.getMessageTypeString(MessageType.EICR_CASE_REPORT_MESSAGE));
    }

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
    MessageSourceComponent msgComp = new MessageSourceComponent();
    msgComp.setEndpoint(kd.getHealthcareSetting().getFhirServerBaseURL());
    header.setSource(msgComp);

    // Set Reason.
    CodeableConcept codeCpt = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(NAMED_EVENT_URL);
    coding.setCode(kd.getNotificationContext().getTriggerEvent());
    codeCpt.addCoding(coding);
    header.setReason(codeCpt);

    // Add sender
    Organization org = ReportCreationUtilities.getOrganization(kd);

    if (org != null) {
      Reference orgRef = new Reference();
      orgRef.setResource(org);
      header.setSender(orgRef);
    }

    // Setup Message Header to Content Bundle Linkage.
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

  public Resource getAllReports(
      KarProcessingData kd,
      EhrQueryService ehrService,
      String dataRequirementId,
      String profile,
      BsaAction act) {

    // Create the report as needed by the Ecr FHIR IG
    Bundle returnBundle = new Bundle();
    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.DOCUMENT);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    returnBundle.addEntry(
        new BundleEntryComponent()
            .setResource(getCdaR11Report(kd, ehrService, dataRequirementId, profile, act)));
    returnBundle.addEntry(
        new BundleEntryComponent()
            .setResource(getFhirReport(kd, ehrService, dataRequirementId, profile, act)));
    return returnBundle;
  }

  public Bundle getCdaR11Report(
      KarProcessingData kd,
      EhrQueryService ehrService,
      String dataRequirementId,
      String profile,
      BsaAction act) {

    // Create the report as needed by the Ecr FHIR IG
    Bundle returnBundle = new Bundle();
    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.DOCUMENT);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));
    logger.info("Ehr Query Service :{}", ehrService);

    Eicr ecr = new Eicr();
    Pair<R4FhirData, LaunchDetails> data =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(kd, act);

    // Save data to File for debugging.
    String outputFileName =
        KarProcessingData.LOADING_QUERY_FILE_NAME
            + "_"
            + kd.getNotificationContext().getPatientId()
            + "_"
            + kd.getNotificationContext().getNotificationResourceId();

    BsaServiceUtils.saveFhirResourceToFile(data.getValue0().getData(), outputFileName);

    String eicr =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            data.getValue0(), data.getValue1(), ecr);

    DocumentReference docref = createR4DocumentReference(kd, eicr, ecr, dataRequirementId);
    returnBundle.addEntry(new BundleEntryComponent().setResource(docref));

    return returnBundle;
  }

  public Resource getCdaR30Report(
      KarProcessingData kd,
      EhrQueryService ehrService,
      String dataRequirementId,
      String profile,
      BsaAction act) {

    // Create the report as needed by the Ecr FHIR IG
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
    String eicr =
        CdaEicrGeneratorFromR4.convertR4FhirBundletoCdaEicr(
            data.getValue0(), data.getValue1(), ecr);

    DocumentReference docref = createR4DocumentReference(kd, eicr, ecr, dataRequirementId);

    returnBundle.addEntry(new BundleEntryComponent().setResource(docref));

    return returnBundle;
  }

  public DocumentReference createR4DocumentReference(
      KarProcessingData kd, String xmlPayload, Eicr ecr, String dataRequirementId) {
    logger.info("Data Requirement ID:{}", dataRequirementId);

    DocumentReference documentReference = new DocumentReference();
    documentReference.setId(ecr.getEicrDocId());

    // Set Doc Ref Status
    documentReference.setStatus(Enumerations.DocumentReferenceStatus.CURRENT);
    documentReference.setDocStatus(DocumentReference.ReferredDocumentStatus.FINAL);

    // Set Doc Ref Type
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

    // Set Subject
    Reference patientReference = new Reference();
    patientReference.setReference("Patient/" + kd.getNotificationContext().getPatientId());
    documentReference.setSubject(patientReference);

    // Set Doc Ref Content
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

    // Set Doc Ref Context
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

  public Bundle getFhirReport(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile, BsaAction act) {

    // Create the report as needed by the Ecr FHIR IG
    Bundle returnBundle = new Bundle();
    returnBundle.setId(id);
    returnBundle.setType(BundleType.DOCUMENT);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    // Add Identifier
    Identifier docId = new Identifier();
    docId.setValue(java.util.UUID.randomUUID().toString());
    docId.setSystem(FhirGeneratorConstants.DOC_ID_SYSTEM);
    returnBundle.setIdentifier(docId);

    logger.info(" Creating R4FhirData");
    Pair<R4FhirData, LaunchDetails> data =
        R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(kd, act);

    logger.info(" Creating Composition Resource ");
    Set<Resource> resourcesTobeAdded = new HashSet<>();
    Composition comp = createComposition(kd, resourcesTobeAdded, data);

    BundleEntryComponent becComp = new BundleEntryComponent();
    becComp.setResource(comp);
    String fullUrlComp =
        StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
            + "/"
            + comp.getResourceType().toString()
            + "/"
            + comp.getIdElement().getIdPart();
    becComp.setFullUrl(fullUrlComp);
    returnBundle.addEntry(becComp);

    for (Resource res : resourcesTobeAdded) {

      BundleEntryComponent bec = new BundleEntryComponent();
      bec.setResource(res);
      bec.setFullUrl(
          StringUtils.stripEnd(kd.getNotificationContext().getFhirServerBaseUrl(), "/")
              + "/"
              + res.getResourceType().toString()
              + "/"
              + res.getIdElement().getIdPart());

      returnBundle.addEntry(bec);
    }

    return returnBundle;
  }

  public Composition createComposition(
      KarProcessingData kd, Set<Resource> resTobeAdded, Pair<R4FhirData, LaunchDetails> data) {

    Composition comp = new Composition();
    comp.setId(UUID.randomUUID().toString());

    // Add Meta
    comp.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, EICR_COMPOSITION_PROFILE_URL));

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
            FhirGeneratorConstants.ECR_COMP_TYPE_CODE,
            FhirGeneratorConstants.ECR_COMP_TYPE_CODE_DISPLAY));

    // Set Patient
    Set<Resource> patients = kd.getResourcesByType(ResourceType.Patient.toString());
    if (patients != null && !patients.isEmpty()) {

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
    if (encounters != null && !encounters.isEmpty() && encounters.size() == 1) {

      logger.info(" Setting up the patient for the composition ");
      Resource encounter = encounters.iterator().next();
      comp.getEncounter().setResource(encounters.iterator().next());
      resTobeAdded.add(encounter);

    } else if (encounters != null && !encounters.isEmpty() && encounters.size() > 1) {

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
    comp.setTitle(EICR_REPORT_LOINC_CODE_DISPLAY_NAME);

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

    // Add chief complaint section.
    SectionComponent sc = getSection(SectionTypeEnum.REASON_FOR_VISIT, kd, data);
    if (sc != null) scs.add(sc);

    // Add chief complaint section.
    sc = getSection(SectionTypeEnum.CHIEF_COMPLAINT, kd, data);
    if (sc != null) scs.add(sc);

    // Add History of Present Illness section.
    sc = getSection(SectionTypeEnum.HISTORY_OF_PRESENT_ILLNESS, kd, data);
    if (sc != null) scs.add(sc);

    // Add Review of Systems Section
    sc = getSection(SectionTypeEnum.REVIEW_OF_SYSTEMS, kd, data);
    if (sc != null) scs.add(sc);

    // Add Problem section.
    sc = getSection(SectionTypeEnum.PROBLEM, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Condition, kd, sc, resTobeAdded);

    // Add Past Medical History section.
    sc = getSection(SectionTypeEnum.MEDICAL_HISTORY, kd, data);
    if (sc != null) scs.add(sc);

    // Add Admission Medications section.
    sc = getSection(SectionTypeEnum.ADMISSION_MEDICATIONS, kd, data);
    if (sc != null) scs.add(sc);

    // Add Medications Administered section.
    sc = getSection(SectionTypeEnum.MEDICATION_ADMINISTERED, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.MedicationAdministration, kd, sc, resTobeAdded);

    // Add Medications Administered section.
    sc = getSection(SectionTypeEnum.MEDICATIONS, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.MedicationStatement, kd, sc, resTobeAdded);

    // Add Results section.
    sc = getSection(SectionTypeEnum.RESULTS, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);
    addEntries(ResourceType.DiagnosticReport, kd, sc, resTobeAdded);

    // Add Plan Of Treatment section.
    sc = getSection(SectionTypeEnum.PLAN_OF_TREATMENT, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.ServiceRequest, kd, sc, resTobeAdded);
    addEntries(ResourceType.MedicationRequest, kd, sc, resTobeAdded);
    addEntries(ResourceType.DiagnosticReport, kd, sc, resTobeAdded);

    // Add Immunizations section.
    sc = getSection(SectionTypeEnum.IMMUNIZATIONS, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Immunization, kd, sc, resTobeAdded);

    // Add Procedures section.
    sc = getSection(SectionTypeEnum.PROCEDURES, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Procedure, kd, sc, resTobeAdded);

    // Add Vital Signs section.
    sc = getSection(SectionTypeEnum.VITAL_SIGNS, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);

    // Add Social History section.
    sc = getSection(SectionTypeEnum.SOCIAL_HISTORY, kd, data);
    if (sc != null) scs.add(sc);
    addEntries(ResourceType.Observation, kd, sc, resTobeAdded);

    // Add Pregnancy section.
    sc = getSection(SectionTypeEnum.PREGNANCY, kd, data);
    if (sc != null) scs.add(sc);
    // addEntries(ResourceType.Observation, kd, sc, resTobeAdded, );

    // Add Emergency Outbreak section.
    sc = getSection(SectionTypeEnum.EMERGENCY_OUTBREAK_SECTION, kd, data);
    if (sc != null) scs.add(sc);

    // Finalize the sections.
    comp.setSection(scs);

    // Add Locations
    Set<Resource> locs = kd.getResourcesByType(ResourceType.Location);
    if (locs != null && !locs.isEmpty()) {
      resTobeAdded.addAll(locs);
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

  public SectionComponent getSection(
      SectionTypeEnum st, KarProcessingData kd, Pair<R4FhirData, LaunchDetails> data) {

    return getSectionComponent(st, kd, data);
  }

  public SectionComponent getSectionComponent(
      SectionTypeEnum st, KarProcessingData kd, Pair<R4FhirData, LaunchDetails> data) {

    SectionComponent sc = null;

    switch (st) {
      case REASON_FOR_VISIT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.REASON_FOR_VISIT_CODE,
                FhirGeneratorConstants.REASON_FOR_VISIT_CODE_DISPLAY);
        populateReasonForVisitNarrative(sc, kd, data);
        break;

      case CHIEF_COMPLAINT:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.CHIEF_COMPLAINT_SECTION_LOINC_CODE,
                FhirGeneratorConstants.CHIEF_COMPLAINT_SECTION_LOINC_CODE_DISPLAY);
        populateChiefComplaintNarrative(sc, kd, data);
        break;

      case HISTORY_OF_PRESENT_ILLNESS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.HISTORY_OF_PRESENT_ILLNESS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
        break;

      case REVIEW_OF_SYSTEMS:
        sc =
            FhirGeneratorUtils.getSectionComponent(
                FhirGeneratorConstants.LOINC_CS_URL,
                FhirGeneratorConstants.REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE,
                FhirGeneratorConstants.REVIEW_OF_SYSTEMS_SECTION_LOINC_CODE_DISPLAY);
        populateDefaultNarrative(sc, kd);
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
        populateDefaultNarrative(sc, kd);
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
        populateDefaultNarrative(sc, kd);
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

  public void populateReasonForVisitNarrative(
      SectionComponent sc, KarProcessingData kd, Pair<R4FhirData, LaunchDetails> data) {
    logger.info("Creating Reason for Visit Narrative ");
    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);

    Encounter encounter = data.getValue0().getEncounter();
    String text = "No Reason for Visit Information";

    if (encounter != null && encounter.hasText()) {
      val.setDivAsString(encounter.getText().getDivAsString());
    } else if (encounter != null && encounter.hasReasonCode()) {
      val.setDivAsString(
          ReportGenerationUtils.getTextForCodeableConcepts(encounter.getReasonCode()));
    } else {
      val.setDivAsString("No Reason for Visit Information");
    }
    sc.setText(val);
  }

  public void populateChiefComplaintNarrative(
      SectionComponent sc, KarProcessingData kd, Pair<R4FhirData, LaunchDetails> data) {
    logger.info("Creating Chief Complaint Narrative ");

    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);
    val.setDivAsString("No Chief Complaint Information");
    sc.setText(val);
  }

  public void populateDefaultNarrative(SectionComponent sc, KarProcessingData kd) {
    logger.info("Adding Narrative ");

    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);
    val.setDivAsString("No Narrative Information");
    sc.setText(val);
  }

  public void populateTextNarrative(
      SectionComponent sc, KarProcessingData kd, Set<Resource> resTobeAdded) {
    logger.info("Adding Narrative ");

    Narrative val = new Narrative();
    val.setStatus(NarrativeStatus.ADDITIONAL);

    String resultString =
        resTobeAdded
            .stream()
            .map(dres -> (DomainResource) (dres))
            .collect(Collectors.toList())
            .stream()
            .filter(tdres -> tdres.hasText())
            .map(strres -> strres.getText().getDivAsString())
            .collect(Collectors.joining(", "));

    if (resultString != null && !resultString.isEmpty()) val.setDivAsString(resultString);
    else val.setDivAsString("No Text Elements found in resources to generate narrative ");

    sc.setText(val);
  }

  public void addEntries(
      ResourceType rt, KarProcessingData kd, SectionComponent sc, Set<Resource> resTobeAdded) {

    Set<Resource> resourcesByType = kd.getResourcesByType(rt.toString());
    Set<Resource> res = null;
    Set<Resource> textResources = new HashSet<>();
    ObservationCategory filteredCategory = ObservationCategory.NULL;

    if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isResultsSection(sc))) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.LABORATORY.toCode());
      filteredCategory = ObservationCategory.LABORATORY;
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isVitalsSection(sc))) {
      res = filterObservationsByCategory(resourcesByType, ObservationCategory.VITALSIGNS.toCode());
      filteredCategory = ObservationCategory.VITALSIGNS;
    } else if (resourcesByType != null
        && rt == ResourceType.Observation
        && Boolean.TRUE.equals(isSocialHistorySection(sc))) {
      res =
          filterObservationsByCategory(resourcesByType, ObservationCategory.SOCIALHISTORY.toCode());
      filteredCategory = ObservationCategory.SOCIALHISTORY;
    } else if (resourcesByType != null
        && rt == ResourceType.DiagnosticReport
        && Boolean.TRUE.equals(isResultsSection(sc))) {
      res = filterDiagnosticReports(resourcesByType, true);
    } else if (resourcesByType != null
        && rt == ResourceType.DiagnosticReport
        && Boolean.TRUE.equals(isPlanOfTreatmentSection(sc))) {
      res = filterDiagnosticReports(resourcesByType, false);
    } else res = resourcesByType;

    if (res != null && !res.isEmpty()) {

      logger.info(" Adding resources of type {}", rt);

      for (Resource r : res) {

        Reference refRes = new Reference();
        refRes.setResource(r);

        // Add Trigger Code extension if appropriate.
        addExtensionIfAppropriate(refRes, r, kd, rt, filteredCategory);

        // Add Reference to the entry.
        sc.addEntry(refRes);

        // add the resource to the set
        resTobeAdded.add(r);
        textResources.add(r);
      }

      populateTextNarrative(sc, kd, textResources);
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

        // Check to see if the resource being added has the same codes, if so add the extension.
        Pair<Boolean, ReportableMatchedTriggerCode> matchCode =
            resourceHasMatchedCode(res, ctcs, filteredCategory);

        if (Boolean.TRUE.equals(matchCode.getValue0()) && matchCode.getValue1() != null) {

          Extension ext = new Extension();
          ext.setUrl(TRIGGER_CODE_EXT_URL);

          // Add Value Set Url Extension.
          Extension vsExt = new Extension();
          vsExt.setUrl(TRIGGER_CODE_VALUESET_EXT_URL);
          StringType url = new StringType(matchCode.getValue1().getValueSetOid());
          OidType oid = new OidType();
          oid.setValue(url.asStringValue());
          vsExt.setValue(oid);
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
        logger.info(" Address Medication References in future ");
      }

    } else if (res instanceof MedicationStatement) {
      logger.info(" MedicationStatement Resource ");

      MedicationStatement ms = (MedicationStatement) res;

      if (ms.hasMedicationCodeableConcept()) {
        mtc = ctcs.getMatchedCode(ms.getMedicationCodeableConcept());
      } else if (ms.hasMedicationReference()) {
        logger.info(" Address Medication References in future ");
      }

    } else if (res instanceof MedicationAdministration) {
      logger.info(" MedicationAdmininstration Resource ");

      MedicationAdministration ma = (MedicationAdministration) res;

      if (ma.hasMedicationCodeableConcept()) {
        mtc = ctcs.getMatchedCode(ma.getMedicationCodeableConcept());
      } else if (ma.hasMedicationReference()) {
        logger.info(" Address Medication References in future ");
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

  public Boolean isPlanOfTreatmentSection(SectionComponent sc) {

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
            .contentEquals(FhirGeneratorConstants.PLAN_OF_TREATMENT_SECTION_LOINC_CODE)) {
      logger.info("Plan Of Treatment Section");
      return true;
    }

    return false;
  }

  public Set<Resource> filterObservationsByCategory(Set<Resource> res, String category) {

    return ReportGenerationUtils.filterObservationsByCategory(res, category);
  }
}
