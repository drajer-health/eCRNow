package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
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
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.Extension;
import org.hl7.fhir.r4.model.MessageHeader;
import org.hl7.fhir.r4.model.MessageHeader.MessageDestinationComponent;
import org.hl7.fhir.r4.model.MessageHeader.MessageSourceComponent;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.StringType;
import org.hl7.fhir.r4.model.UriType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MedMorphReportCreator extends ReportCreator {

  private final Logger logger = LoggerFactory.getLogger(MedMorphReportCreator.class);

  public static final String DEFAULT_VERSION = "1";
  public static final String CONTENT_BUNDLE_PROFILE =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-content-bundle";
  public static final String BUNDLE_REL_URL = "Bundle/";
  public static final String MESSAGE_HEADER_PROFILE =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-messageheader";
  public static final String MESSAGE_TYPE =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-messageheader-message-types";
  public static final String NAMED_EVENT_URL =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents";
  public static final String MESSAGE_PROCESSING_CATEGORY_EXT_URL =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/us-ph-message-processing-category-extension";
  public static final String REPORT_INITIATION_TYPE_EXT_URL =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-report-initiation-types";
  public static final String REPORT_INITIATION_TYPE_CODE = "manually-initiated";
  public static final String REPORT_INITIATION_TYPE_DISPLAY_STRING =
      "Indicates a report was initiated to a manual action by the provider.";
  public static final String MESSAGE_PROCESSING_CATEGORY_CODE = "notification";

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
    // Create the report as needed by the Ecr FHIR IG
    Bundle returnBundle = new Bundle();

    logger.info("Creating report for {}", kd.getKar().getKarId());
    returnBundle.setId(id);
    returnBundle.setType(BundleType.MESSAGE);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    logger.info("MEDMORPH REPORT Resource Count {}", inputData.size());

    // Create the Content Bundle.
    Bundle contentBundle =
        createContentBundle(inputData, kd.getHealthcareSetting().getFhirServerBaseURL());

    // Create the Message Header resource.
    MessageHeader header = createMessageHeader(kd);

    Organization sender = createSender(kd);

    // Setup Message Header to Content Bundle Linkage.
    Reference ref = new Reference();
    ref.setReference(BUNDLE_REL_URL + contentBundle.getId());
    List<Reference> refs = new ArrayList<>();
    refs.add(ref);
    header.setFocus(refs);

    // Reference the sender.
    header.setSender(referenceTo(sender));

    // Add the Message Header Resource
    returnBundle.addEntry(new BundleEntryComponent().setResource(header));

    // Add the Content Bundle.
    returnBundle.addEntry(new BundleEntryComponent().setResource(contentBundle));

    return returnBundle;
  }

  public Reference referenceTo(Resource dest) {
    Reference ref = new Reference();
    ref.setReference(dest.fhirType() + "/" + dest.getId());
    return ref;
  }

  public Organization createSender(KarProcessingData kd) {
    HealthcareSetting hs = kd.getHealthcareSetting();
    Organization org = new Organization();
    org.setId(UUID.randomUUID().toString());
    org.setName(hs.getOrgName());
    org.addIdentifier().setSystem(hs.getOrgIdSystem()).setValue(hs.getOrgId());

    return org;
  }

  public MessageHeader createMessageHeader(KarProcessingData kd) {

    logger.info("creating message header ===> ", REPORT_INITIATION_TYPE_EXT_URL);

    MessageHeader header = new MessageHeader();

    header.setId(UUID.randomUUID().toString());
    header.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, MESSAGE_HEADER_PROFILE));

    // Add extensions
    Extension ext = new Extension();
    ext.setUrl(MESSAGE_PROCESSING_CATEGORY_EXT_URL);
    StringType st = new StringType();
    st.setValue(MESSAGE_PROCESSING_CATEGORY_CODE);
    ext.setValue(st);
    Extension ext2 = new Extension();
    ext2.setUrl(REPORT_INITIATION_TYPE_EXT_URL);
    CodeableConcept cc = new CodeableConcept();
    Coding coding1 = new Coding();
    coding1.setSystem(REPORT_INITIATION_TYPE_EXT_URL);
    coding1.setCode(REPORT_INITIATION_TYPE_CODE);
    coding1.setDisplay(REPORT_INITIATION_TYPE_DISPLAY_STRING);
    List<Coding> codeList = new ArrayList<>();
    codeList.add(coding1);
    cc.setCoding(codeList);
    ext2.setValue(cc);
    // ext2.setValue(st2);
    List<Extension> exts = new ArrayList<>();
    exts.add(ext);
    exts.add(ext2);

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

  public Bundle createContentBundle(Set<Resource> inputData, String fhirBase) {

    Bundle returnBundle = new Bundle();

    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.COLLECTION);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, CONTENT_BUNDLE_PROFILE));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    List<BundleEntryComponent> becs = new ArrayList<>();
    for (Resource resource : inputData) {
      String id = resource.getId();
      String resourceType = resource.getResourceType().toString();
      String fullUrl;
      if (id.startsWith("http")) {
        // id is already the full URL
        fullUrl = id;
      } else if (id.startsWith(resourceType)) {
        // id is something like "Observation/1235"
        fullUrl = fhirBase + "/" + id;
      } else {
        fullUrl = fhirBase + "/" + resourceType + "/" + id;
      }
      fullUrl = fullUrl.replaceAll("/_history/.*", "");
      logger.info(" Adding Resource Id : {} of Type {}", id, resourceType);
      becs.add(new BundleEntryComponent().setResource(resource).setFullUrl(fullUrl));
    }
    returnBundle.setEntry(becs);

    return returnBundle;
  }
}
