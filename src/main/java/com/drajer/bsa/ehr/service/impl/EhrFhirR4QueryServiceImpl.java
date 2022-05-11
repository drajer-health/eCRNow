package com.drajer.bsa.ehr.service.impl;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import com.drajer.bsa.ehr.service.EhrAuthorizationService;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.ResourceUtils;
import java.util.EnumMap;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IIdType;
import org.hl7.fhir.r4.model.Attachment;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.CodeableConcept;
import org.hl7.fhir.r4.model.Coding;
import org.hl7.fhir.r4.model.DocumentReference;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Encounter.EncounterLocationComponent;
import org.hl7.fhir.r4.model.Encounter.EncounterParticipantComponent;
import org.hl7.fhir.r4.model.Enumerations;
import org.hl7.fhir.r4.model.IdType;
import org.hl7.fhir.r4.model.Location;
import org.hl7.fhir.r4.model.Organization;
import org.hl7.fhir.r4.model.Period;
import org.hl7.fhir.r4.model.Practitioner;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 *
 * <h1>EhrQueryService</h1>
 *
 * This class defines the implementation methods to access data from the Ehr for a set of resources.
 *
 * @author nbashyam
 */
@Service
@Transactional
public class EhrFhirR4QueryServiceImpl implements EhrQueryService {

  private final Logger logger = LoggerFactory.getLogger(EhrFhirR4QueryServiceImpl.class);

  /** The EHR Authorization Service class enables the BSA to get an access token. */
  @Qualifier("backendauth")
  @Autowired
  EhrAuthorizationService backendAuthorizationService;

  @Qualifier("ehrauth")
  @Autowired
  EhrAuthorizationService ehrAuthorizationService;

  private static final String R4 = "R4";
  private static final String PATIENT_RESOURCE = "Patient";
  private static final String PATIENT_ID_SEARCH_PARAM = "?patient=";
  private static final String LOG_FHIR_CTX_GET = " Getting FHIR Context for R4";
  private static final String LOG_INIT_FHIR_CLIENT = "Initializing FHIR Client";

  /** The FHIR Context Initializer necessary to retrieve FHIR resources */
  @Autowired FhirContextInitializer fhirContextInitializer;

  /**
   * The method is used to retrieve data from the Ehr.
   *
   * @param kd The processing context which contains information such as patient, encounter,
   *     previous data etc.
   * @return The Map of Resources to its type.
   */
  @Override
  public Map<ResourceType, Set<Resource>> getFilteredData(
      KarProcessingData kd, Map<String, ResourceType> resTypes) {

    String secret = kd.getHealthcareSetting().getClientSecret();
    if (secret == null || secret.isEmpty()) {
      backendAuthorizationService.getAuthorizationToken(kd);
    } else {
      ehrAuthorizationService.getAuthorizationToken(kd);
    }

    logger.info(LOG_FHIR_CTX_GET);
    FhirContext context = fhirContextInitializer.getFhirContext(R4);

    logger.info(LOG_INIT_FHIR_CLIENT);

    IGenericClient client = getClient(kd, context);

    // Get Patient by Id always
    Resource res =
        getResourceById(
            client, context, PATIENT_RESOURCE, kd.getNotificationContext().getPatientId());

    if (res != null) {

      logger.info(
          " Found Patient resource for Id : {}", kd.getNotificationContext().getPatientId());

      Set<Resource> resources = new HashSet<>();
      resources.add(res);
      Map<ResourceType, Set<Resource>> resMap = new EnumMap<>(ResourceType.class);
      resMap.put(res.getResourceType(), resources);
      kd.addResourcesByType(resMap);
    }

    if (kd.getNotificationContext()
        .getNotificationResourceType()
        .equals(ResourceType.Encounter.toString())) {

      logger.info(
          " Fetch Encounter resource for Id : {} ",
          kd.getNotificationContext().getNotificationResourceId());

      Resource enc =
          getResourceById(
              client,
              context,
              ResourceType.Encounter.toString(),
              kd.getNotificationContext().getNotificationResourceId());

      if (enc != null) {

        logger.info(
            " Found Encounter resource for Id : {}",
            kd.getNotificationContext().getNotificationResourceId());

        Set<Resource> resources = new HashSet<>();
        resources.add(enc);
        Map<ResourceType, Set<Resource>> resMap = new EnumMap<>(ResourceType.class);
        resMap.put(enc.getResourceType(), resources);
        kd.addResourcesByType(resMap);
      }
    }

    // Fetch Resources by Patient Id.
    for (Map.Entry<String, ResourceType> entry : resTypes.entrySet()) {

      logger.info(" Fetching Resource of type {}", entry.getValue());

      if (entry.getValue() != ResourceType.Patient && entry.getValue() != ResourceType.Encounter) {
        String url =
            kd.getNotificationContext().getFhirServerBaseUrl()
                + "/"
                + entry.getValue().toString()
                + PATIENT_ID_SEARCH_PARAM
                + kd.getNotificationContext().getPatientId();

        logger.info(" Resource Query Url : {}", url);

        getResourcesByPatientId(
            client,
            context,
            entry.getValue().toString(),
            url,
            kd,
            entry.getValue(),
            entry.getKey());
      }
    }

    // Get other resources for Patient
    return kd.getFhirInputData();
  }

  /**
   * @param kd The data object for getting the healthcareSetting and notification context from
   * @param context The HAPI FHIR context for making a FHIR client with
   * @return
   */
  public IGenericClient getClient(KarProcessingData kd, FhirContext context) {
    String secret = kd.getHealthcareSetting().getClientSecret();
    if (secret == null || secret.isEmpty()) {
      backendAuthorizationService.getAuthorizationToken(kd);
    } else {
      ehrAuthorizationService.getAuthorizationToken(kd);
    }

    return fhirContextInitializer.createClient(
        context,
        kd.getHealthcareSetting().getFhirServerBaseURL(),
        kd.getNotificationContext().getEhrAccessToken(),
        kd.getNotificationContext().getxRequestId());
  }

  /**
   * @param kd The KarProcessingData includes data about the fhir server to create a resource on
   * @param resource the resource to create on the fhir server
   */
  public void createResource(KarProcessingData kd, Resource resource) {

    logger.info(LOG_FHIR_CTX_GET);
    FhirContext context = fhirContextInitializer.getFhirContext(R4);

    logger.info(LOG_INIT_FHIR_CLIENT);
    IGenericClient client = getClient(kd, context);
    client.create().resource(resource).execute();
  }

  /**
   * @param kd The KarProcessingData which contains information about the fhir server
   * @param resourceType The resource type of the resource to be deleted
   * @param id The logical ID of the resource to be deleted
   */
  public void deleteResource(KarProcessingData kd, ResourceType resourceType, String id) {

    logger.info(LOG_FHIR_CTX_GET);
    FhirContext context = fhirContextInitializer.getFhirContext(R4);

    logger.info(LOG_INIT_FHIR_CLIENT);
    IGenericClient client = getClient(kd, context);
    client.delete().resourceById(resourceType.toString(), id).execute();
  }

  public HashMap<ResourceType, Set<Resource>> loadJurisdicationData(KarProcessingData kd) {

    String secret = kd.getHealthcareSetting().getClientSecret();
    if (secret == null || secret.isEmpty()) {
      backendAuthorizationService.getAuthorizationToken(kd);
    } else {
      ehrAuthorizationService.getAuthorizationToken(kd);
    }

    logger.info(LOG_FHIR_CTX_GET);
    FhirContext context = fhirContextInitializer.getFhirContext(R4);

    logger.info(LOG_INIT_FHIR_CLIENT);
    IGenericClient client =
        fhirContextInitializer.createClient(
            context,
            kd.getHealthcareSetting().getFhirServerBaseURL(),
            kd.getNotificationContext().getEhrAccessToken(),
            kd.getxRequestId());

    // Retrieve the encounter
    Set<Resource> res = kd.getResourcesByType(ResourceType.Encounter.toString());

    Set<Resource> practitioners = new HashSet<>();
    Set<Resource> locations = new HashSet<>();
    Set<Resource> organizations = new HashSet<>();
    Map<String, String> practitionerMap = new HashMap<>();

    for (Resource r : res) {

      Encounter encounter = (Encounter) r;

      // Load Practitioners
      if (encounter.getParticipant() != null) {

        List<EncounterParticipantComponent> participants = encounter.getParticipant();

        for (EncounterParticipantComponent participant : participants) {
          if (participant.getIndividual() != null) {
            Reference practitionerReference = participant.getIndividual();
            String practitionerID = practitionerReference.getReferenceElement().getIdPart();
            if (!practitionerMap.containsKey(practitionerID)) {
              Practitioner practitioner =
                  (Practitioner)
                      getResourceById(
                          client, context, ResourceType.Practitioner.toString(), practitionerID);
              if (practitioner != null && !practitionerMap.containsKey(practitionerID)) {
                practitioners.add(practitioner);
                practitionerMap.put(practitionerID, ResourceType.Practitioner.toString());
              }
            }
          } // Individual != null
        } // For all participants
      } // For participant != null

      // Load Organizations
      if (Boolean.TRUE.equals(encounter.hasServiceProvider())) {
        Reference organizationReference = encounter.getServiceProvider();
        if (organizationReference.hasReferenceElement()) {
          Organization organization =
              (Organization)
                  getResourceById(
                      client,
                      context,
                      "Organization",
                      organizationReference.getReferenceElement().getIdPart());
          if (organization != null) {
            organizations.add(organization);
          }
        }
      }

      // Load Locations
      if (Boolean.TRUE.equals(encounter.hasLocation())) {
        List<EncounterLocationComponent> enocunterLocations = encounter.getLocation();
        for (EncounterLocationComponent location : enocunterLocations) {
          if (location.getLocation() != null) {
            Reference locationReference = location.getLocation();
            Location locationResource =
                (Location)
                    getResourceById(
                        client,
                        context,
                        "Location",
                        locationReference.getReferenceElement().getIdPart());
            if (locationResource != null) {
              locations.add(locationResource);
            }
          }
        }
      }
    } // for all encounters

    if (!practitioners.isEmpty()) {

      Map<ResourceType, Set<Resource>> resMap = new EnumMap<>(ResourceType.class);
      resMap.put(ResourceType.Practitioner, practitioners);
      kd.addResourcesByType(resMap);
    }

    if (!locations.isEmpty()) {

      Map<ResourceType, Set<Resource>> resMap = new EnumMap<>(ResourceType.class);
      resMap.put(ResourceType.Location, locations);
      kd.addResourcesByType(resMap);
    }

    if (!organizations.isEmpty()) {

      Map<ResourceType, Set<Resource>> resMap = new EnumMap<>(ResourceType.class);
      resMap.put(ResourceType.Organization, organizations);
      kd.addResourcesByType(resMap);
    }

    return kd.getFhirInputData();
  }

  @Override
  public Resource getResourceById(KarProcessingData kd, String resourceName, String resourceId) {

    String secret = kd.getHealthcareSetting().getClientSecret();
    if (secret == null || secret.isEmpty()) {
      backendAuthorizationService.getAuthorizationToken(kd);
    } else {
      ehrAuthorizationService.getAuthorizationToken(kd);
    }

    logger.info(LOG_FHIR_CTX_GET);
    FhirContext context = fhirContextInitializer.getFhirContext(R4);

    logger.info(LOG_INIT_FHIR_CLIENT);
    IGenericClient client =
        fhirContextInitializer.createClient(
            context,
            kd.getHealthcareSetting().getFhirServerBaseURL(),
            kd.getNotificationContext().getEhrAccessToken(),
            kd.getxRequestId());

    return getResourceById(client, context, resourceName, resourceId);
  }

  public Resource getResourceById(
      IGenericClient genericClient, FhirContext context, String resourceName, String resourceId) {

    Resource resource = null;

    try {

      logger.info("Getting data for Resource : {} with Id : {}", resourceName, resourceId);

      resource =
          (Resource) (genericClient.read().resource(resourceName).withId(resourceId).execute());

    } catch (BaseServerResponseException responseException) {
      if (responseException.getOperationOutcome() != null) {
        logger.debug(
            context
                .newJsonParser()
                .encodeResourceToString(responseException.getOperationOutcome()));
      }
      logger.error(
          "Error in getting {} resource by Id: {}", resourceName, resourceId, responseException);
    } catch (Exception e) {
      logger.error("Error in getting {} resource by Id: {}", resourceName, resourceId, e);
    }
    return resource;
  }

  public void getResourcesByPatientId(
      IGenericClient genericClient,
      FhirContext context,
      String resourceName,
      String searchUrl,
      KarProcessingData kd,
      ResourceType resType,
      String id) {

    logger.info("Invoking search url : {}", searchUrl);
    Set<Resource> resources = null;
    Map<ResourceType, Set<Resource>> resMap = null;
    HashMap<String, Set<Resource>> resMapById = null;

    try {
      logger.info(
          "Getting {} data using Patient Id: {}",
          resourceName,
          kd.getNotificationContext().getPatientId());

      Bundle bundle = genericClient.search().byUrl(searchUrl).returnBundle(Bundle.class).execute();

      getAllR4RecordsUsingPagination(genericClient, bundle);

      if (bundle.getEntry() != null) {
        logger.info(
            "Total No of Entries {} retrieved : {}", resourceName, bundle.getEntry().size());

        List<BundleEntryComponent> bc = bundle.getEntry();

        if (bc != null) {

          resources = new HashSet<>();
          resMap = new EnumMap<>(ResourceType.class);
          resMapById = new HashMap<>();
          for (BundleEntryComponent comp : bc) {

            logger.debug(" Adding Resource Id : {}", comp.getResource().getId());
            resources.add(comp.getResource());
          }
          Set<Resource> uniqueResources =
              ResourceUtils.deduplicate(resources).stream().collect(Collectors.toSet());
          resMap.put(resType, uniqueResources);
          resMapById.put(id, uniqueResources);
          kd.addResourcesByType(resMap);
          kd.addResourcesById(resMapById);

          logger.info(" Adding {} resources of type : {}", uniqueResources.size(), resType);
        } else {
          logger.error(" No entries found for type : {}", resType);
        }
      } else {
        logger.error(" Unable to retrieve resources for type : {}", resType);
      }

    } catch (BaseServerResponseException responseException) {
      if (responseException.getOperationOutcome() != null) {
        logger.debug(
            context
                .newJsonParser()
                .encodeResourceToString(responseException.getOperationOutcome()));
      }
      logger.info(
          "Error in getting {} resource by Patient Id: {}",
          resourceName,
          kd.getNotificationContext().getPatientId(),
          responseException);
    } catch (Exception e) {
      logger.info(
          "Error in getting {} resource by Patient Id: {}",
          resourceName,
          kd.getNotificationContext().getPatientId(),
          e);
    }
  }

  private void getAllR4RecordsUsingPagination(IGenericClient genericClient, Bundle bundle) {

    if (bundle.hasEntry()) {
      List<BundleEntryComponent> entriesList = bundle.getEntry();
      if (bundle.hasLink() && bundle.getLink(IBaseBundle.LINK_NEXT) != null) {
        logger.info(
            "Found Next Page in Bundle :{}", bundle.getLink(IBaseBundle.LINK_NEXT).getUrl());
        Bundle nextPageBundleResults = genericClient.loadPage().next(bundle).execute();
        entriesList.addAll(nextPageBundleResults.getEntry());
        nextPageBundleResults.setEntry(entriesList);
        getAllR4RecordsUsingPagination(genericClient, nextPageBundleResults);
      }
    }
  }

  public DocumentReference constructR4DocumentReference(
      String payload,
      String patientId,
      String encounterId,
      String providerUuid,
      String rrDocRefMimeType,
      String title,
      String docCode,
      String docDisplayName,
      String docCodeSystem) {
    DocumentReference documentReference = new DocumentReference();

    // Set Doc Ref Status
    documentReference.setStatus(Enumerations.DocumentReferenceStatus.CURRENT);
    documentReference.setDocStatus(DocumentReference.ReferredDocumentStatus.FINAL);

    // Set Doc Ref Type
    CodeableConcept typeCode = new CodeableConcept();
    List<Coding> codingList = new ArrayList<>();
    Coding typeCoding = new Coding();
    typeCoding.setSystem(docCodeSystem);
    typeCoding.setCode(docCode);
    typeCoding.setDisplay(docDisplayName);
    codingList.add(typeCoding);
    typeCode.setCoding(codingList);
    typeCode.setText(docDisplayName);
    documentReference.setType(typeCode);

    // Set Subject
    Reference patientReference = new Reference();
    patientReference.setReference("Patient/" + patientId);
    documentReference.setSubject(patientReference);

    // Set Author
    if (providerUuid != null && !providerUuid.isEmpty()) {
      List<Reference> authorRefList = new ArrayList<>();
      Reference providerReference = new Reference();
      providerReference.setReference("Practitioner/" + providerUuid);
      authorRefList.add(providerReference);
      documentReference.setAuthor(authorRefList);
    }

    // Set Doc Ref Content
    List<DocumentReference.DocumentReferenceContentComponent> contentList = new ArrayList<>();
    DocumentReference.DocumentReferenceContentComponent contentComp =
        new DocumentReference.DocumentReferenceContentComponent();
    Attachment attachment = new Attachment();
    attachment.setTitle(title);
    attachment.setContentType(rrDocRefMimeType);

    if (payload != null && !payload.isEmpty()) {
      attachment.setData(payload.getBytes());
    }
    contentComp.setAttachment(attachment);
    contentList.add(contentComp);
    documentReference.setContent(contentList);

    DocumentReference.DocumentReferenceContextComponent docContextComp =
        new DocumentReference.DocumentReferenceContextComponent();

    // Set Doc Ref Context
    if (encounterId != null && !encounterId.isEmpty()) {

      List<Reference> encounterRefList = new ArrayList<>();
      Reference encounterReference = new Reference();
      encounterReference.setReference("Encounter/" + encounterId);
      encounterRefList.add(encounterReference);
      docContextComp.setEncounter(encounterRefList);
    }

    Period period = new Period();
    period.setStart(new Date());
    period.setEnd(new Date());
    docContextComp.setPeriod(period);
    documentReference.setContext(docContextComp);

    String docReference =
        FhirContext.forR4().newJsonParser().encodeResourceToString(documentReference);

    logger.debug("DocumentReference Object===========> {}", docReference);

    return documentReference;
  }

  @Override
  public JSONObject getAuthorizationToken(HealthcareSetting hs) {

    JSONObject tokenResponse = null;

    if (hs.getAuthType().equals(BsaTypes.getString(BsaTypes.AuthenticationType.System))) {

      tokenResponse = ehrAuthorizationService.getAuthorizationToken(hs);

    } else {

      // Handle other Auth Types
    }

    return tokenResponse;
  }
}
