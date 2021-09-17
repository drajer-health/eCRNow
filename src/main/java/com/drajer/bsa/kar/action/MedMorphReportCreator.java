package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.MessageType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.MessageHeader;
import org.hl7.fhir.r4.model.MessageHeader.MessageDestinationComponent;
import org.hl7.fhir.r4.model.MessageHeader.MessageSourceComponent;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.UriType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MedMorphReportCreator extends ReportCreator {

  private final Logger logger = LoggerFactory.getLogger(MedMorphReportCreator.class);

  public static String DEFAULT_VERSION = "1";
  public static String CONTENT_BUNDLE_PROFILE =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-content-bundle";
  public static String BUNDLE_REL_URL = "Bundle/";
  public static String MESSAGE_HEADER_PROFILE =
      "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-messageheader";
  public static String MESSAGE_TYPE =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-messageheader-message-types";
  public static String NAMED_EVENT_URL =
      "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents";

  @Override
  public Resource createReport(
      KarProcessingData kd, EhrQueryService ehrService, String id, String profile) {
    // Create the report as needed by the Ecr FHIR IG
    Bundle returnBundle = new Bundle();

    returnBundle.setId(id);
    returnBundle.setType(BundleType.MESSAGE);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, profile));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    // Create the Content Bundle.
    Bundle contentBundle = createContentBundle(kd);

    // Create the Message Header resource.
    MessageHeader header = createMessageHeader(kd);

    Organization sender = createSender(kd);

    // Setup Message Header to Content Bundle Linkage.
    header.setFocus(Arrays.asList(referenceTo(contentBundle)));

    header.setSender(referenceTo(sender));

    // Add the Message Header Resource
    returnBundle.addEntry(new BundleEntryComponent().setResource(header));

    // Add the Content Bundle.
    returnBundle.addEntry(new BundleEntryComponent().setResource(contentBundle));

    returnBundle.addEntry(new BundleEntryComponent().setResource(sender));

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

    MessageHeader header = new MessageHeader();

    header.setId(UUID.randomUUID().toString());
    header.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, MESSAGE_HEADER_PROFILE));

    // Set message type.
    Coding c = new Coding();
    c.setSystem(MESSAGE_TYPE);
    c.setCode(BsaTypes.getMessageTypeString(MessageType.CancerReportMessage));
    header.setEvent(c);

    // set destination
    Set<UriType> dests = kd.getKar().getReceiverAddresses();
    List<MessageDestinationComponent> mdcs = new ArrayList<MessageDestinationComponent>();
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

  public Bundle createContentBundle(KarProcessingData kd) {

    Bundle returnBundle = new Bundle();

    returnBundle.setId(UUID.randomUUID().toString());
    returnBundle.setType(BundleType.COLLECTION);
    returnBundle.setMeta(ActionUtils.getMeta(DEFAULT_VERSION, CONTENT_BUNDLE_PROFILE));
    returnBundle.setTimestamp(Date.from(Instant.now()));

    List<BundleEntryComponent> becs = new ArrayList<BundleEntryComponent>();
    populateBundleEntries(becs, kd);
    returnBundle.setEntry(becs);

    return returnBundle;
  }

  public void populateBundleEntries(List<BundleEntryComponent> becs, KarProcessingData kd) {

    HashMap<ResourceType, Set<Resource>> resources = kd.getFhirInputData();

    for (Map.Entry<ResourceType, Set<Resource>> res : resources.entrySet()) {

      Set<Resource> entries = res.getValue();

      for (Resource r : entries) {

        logger.info(" Adding Resource Id : {} of Type {}", r.getId(), r.getResourceType());
        becs.add(new BundleEntryComponent().setResource(r));
      }
    }
  }
}
