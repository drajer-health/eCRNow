package com.drajer.bsa.utils;

import com.drajer.bsa.exceptions.InvalidLaunchContext;
import com.drajer.bsa.exceptions.InvalidNotification;
import com.drajer.bsa.model.BsaTypes.NotificationProcessingStatusType;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PatientLaunchContext;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.Properties;
import java.util.UUID;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>SubscriptionUtils</h1>
 *
 * The class provides utility methods for processing subscriptions.
 *
 * @author nbashyam
 */
public class SubscriptionUtils {

  private SubscriptionUtils() {}

  private static final String TOPIC_PARAMETER = "topic";
  private static final String SUBSCRIPTION_PARAMETER = "subscription";
  private static final String TYPE_PARAMETER = "type";
  private static final String EVENT_NOTIFICATION_CODE = "event-notification";
  private static final String SUBSCRIPTION_URL_SPLIT_REGEX = "/Subscription";
  private static final int FHIR_SERVER_URL_MIN_LENGTH = 8;

  private static HashMap<String, String> topicToNamedEventMap = new HashMap<>();
  private static HashMap<String, String> namedEventToResourceMap = new HashMap<>();

  private static final Logger logger = LoggerFactory.getLogger(SubscriptionUtils.class);

  // Load the Topic to Named Event Map. This is a map of the topic urls to the named events in the
  // MedMorph IGs.
  static {
    try (InputStream input =
        SubscriptionUtils.class
            .getClassLoader()
            .getResourceAsStream("subscription-topic-named-event.properties")) {

      Properties prop = new Properties();
      prop.load(input);

      prop.forEach((key, value) -> topicToNamedEventMap.put((String) key, (String) value));

    } catch (IOException ex) {
      logger.error("Error while loading Topic to Named Event Mapping from Proporties File ");
    }
  }

  // Load the Named Event to Resource Map indicates which Resource is expected to be received as
  // part of the notification.
  static {
    try (InputStream input =
        SubscriptionUtils.class
            .getClassLoader()
            .getResourceAsStream("named-event-resource.properties")) {

      Properties prop = new Properties();
      prop.load(input);

      prop.forEach((key, value) -> namedEventToResourceMap.put((String) key, (String) value));

    } catch (IOException ex) {
      logger.error("Error while loading Named Event to Resource Mapping from Proporties File ");
    }
  }

  public static NotificationContext getNotificationContext(
      Bundle bundle,
      HttpServletRequest request,
      HttpServletResponse response,
      Boolean relaunch,
      Boolean reprocess,
      PatientLaunchContext launchContext)
      throws InvalidLaunchContext, InvalidNotification {

    validateBundleStructure(bundle);
    Parameters parameters = getParametersResource(bundle);
    ParameterInfo paramInfo = extractParameters(parameters);
    validateParameters(paramInfo);

    String namedEvent = topicToNamedEventMap.get(paramInfo.subTopic.getValue());
    String resourceType = namedEventToResourceMap.get(namedEvent);

    logger.info(" Named Event Received : {}", namedEvent);
    logger.info(" ResourceType Expected : {}", resourceType);

    Resource res = bundle.getEntry().get(1).getResource();
    validateResource(res, resourceType);

    String fhirServerUrl = getFhirServerUrl(paramInfo.subsRef);
    validateFhirServerUrl(fhirServerUrl);

    NotificationContext nc = new NotificationContext();
    setupBasicNotificationContext(
        nc, namedEvent, resourceType, fhirServerUrl, res, relaunch, reprocess, launchContext);
    processEncounterIfApplicable(nc, res);
    setRequestHeaders(nc, request);
    setLaunchContext(nc, launchContext);

    return nc;
  }

  private static void validateBundleStructure(Bundle bundle) throws InvalidNotification {
    if (bundle == null
        || bundle.getType() != BundleType.HISTORY
        || bundle.getEntryFirstRep() == null
        || !(bundle.getEntryFirstRep().getResource() instanceof Parameters)
        || bundle.getEntry().size() < 2) {
      logger.error(" Bundle does not have necessary data to process the notification. ");
      throw new InvalidNotification(
          "Bundle does not have necessary data to process the notification.");
    }
    logger.info(" The notification passes the first level of checks for processing ");
  }

  private static Parameters getParametersResource(Bundle bundle) {
    return (Parameters) bundle.getEntryFirstRep().getResource();
  }

  private static class ParameterInfo {
    CodeType subsType;
    Reference subsRef;
    CanonicalType subTopic;
  }

  private static ParameterInfo extractParameters(Parameters p) {
    ParameterInfo info = new ParameterInfo();
    info.subsType =
        (p.getParameterValue(TYPE_PARAMETER) instanceof CodeType)
            ? (CodeType) p.getParameter(TYPE_PARAMETER).getValue()
            : null;
    info.subsRef =
        (p.getParameterValue(SUBSCRIPTION_PARAMETER) instanceof Reference)
            ? (Reference) p.getParameter(SUBSCRIPTION_PARAMETER).getValue()
            : null;
    info.subTopic =
        (p.getParameterValue(TOPIC_PARAMETER) instanceof CanonicalType)
            ? (CanonicalType) p.getParameter(TOPIC_PARAMETER).getValue()
            : null;
    return info;
  }

  private static void validateParameters(ParameterInfo info) throws InvalidNotification {
    if (info.subsType == null
        || !info.subsType.getCode().equals(EVENT_NOTIFICATION_CODE)
        || info.subsRef == null
        || info.subTopic == null
        || !topicToNamedEventMap.containsKey(info.subTopic.getValue())) {
      logger.error(" Bundle does not pass the necessary checks for processing. ");
      throw new InvalidNotification(
          "Bundle does not pass the necessary checks for processing, check the event-notification code.");
    }
  }

  private static void validateResource(Resource res, String resourceType)
      throws InvalidNotification {
    if (res == null) {
      String error = "Resource not found for type: " + resourceType;
      logger.error(error);
      String possibleCauses =
          error
              + ", Check for accurate PatientId, Encounter or Notified Resource Id or an Expired Authorization Token";
      throw new InvalidNotification(possibleCauses);
    }
    logger.info(" Resource Type Received : {}", res.getResourceType());
    if (!res.getResourceType().toString().equals(resourceType)) {
      String error =
          " Resource Type Received "
              + res.getResourceType().getDeclaringClass()
              + " , does not match Resource Type Expected "
              + ResourceType.fromCode(resourceType).getDeclaringClass();
      logger.error(error);
      throw new InvalidNotification(error);
    }
    logger.info(" Found the Resource Type Expected in the bundle : {}", res.getResourceType());
  }

  private static void validateFhirServerUrl(String fhirServerUrl) throws InvalidNotification {
    if (fhirServerUrl == null || fhirServerUrl.length() <= FHIR_SERVER_URL_MIN_LENGTH) {
      String error =
          "Fhir Server Url received is not valid for further processing, Url Value : "
              + ((fhirServerUrl != null) ? fhirServerUrl : "Null Value");
      logger.error(error);
      throw new InvalidNotification(error);
    }
  }

  private static void setupBasicNotificationContext(
      NotificationContext nc,
      String namedEvent,
      String resourceType,
      String fhirServerUrl,
      Resource res,
      Boolean relaunch,
      Boolean reprocess,
      PatientLaunchContext launchContext) {
    nc.setFhirServerBaseUrl(fhirServerUrl);
    nc.setPatientId(getPatientId(res));
    nc.setNotificationResourceId(res.getIdElement().getIdPart());
    nc.setNotificationResourceType(resourceType);
    nc.setLastUpdated(Date.from(Instant.now()));
    nc.setNotifiedResource(res);
    nc.setNotificationProcessingStatus(NotificationProcessingStatusType.IN_PROGRESS.toString());
    setTriggerEvent(nc, namedEvent, relaunch, reprocess, launchContext);
  }

  private static void setTriggerEvent(
      NotificationContext nc,
      String namedEvent,
      Boolean relaunch,
      Boolean reprocess,
      PatientLaunchContext launchContext) {
    if (!relaunch && !reprocess) {
      nc.setTriggerEvent(namedEvent);
    } else if (isForceReprocessingDisabled(launchContext)) {
      nc.setTriggerEvent(namedEvent + "|reprocessed:" + UUID.randomUUID().toString());
    } else if (reprocess) {
      nc.setTriggerEvent(namedEvent + "|reprocessed");
    } else {
      nc.setTriggerEvent(namedEvent + "|relaunch-id:" + UUID.randomUUID().toString());
    }
  }

  private static boolean isForceReprocessingDisabled(PatientLaunchContext launchContext) {
    return launchContext != null
        && launchContext.getEhrLaunchContext() != null
        && launchContext.getEhrLaunchContext().containsKey("forceReprocessing")
        && launchContext.getEhrLaunchContext().get("forceReprocessing").contains("false");
  }

  private static void processEncounterIfApplicable(NotificationContext nc, Resource res) {
    if (res.getResourceType() != ResourceType.Encounter) {
      return;
    }
    Encounter enc = (Encounter) res;
    setEncounterTimes(nc, enc);
    setEncounterClass(nc, enc);
  }

  private static void setEncounterTimes(NotificationContext nc, Encounter enc) {
    if (enc.getPeriod() != null && enc.getPeriod().getStart() != null) {
      logger.debug(" Encounter has a start date");
      nc.setEncounterStartTime(enc.getPeriod().getStart());
      if (enc.getPeriod().getEnd() != null) {
        logger.info(" Encounter has an end date, so it is a closed encounter ");
        nc.setEncounterEndTime(enc.getPeriod().getEnd());
      }
    } else {
      logger.debug(" Initializing Encounter Start time as current time ");
      nc.setEncounterStartTime(new Date());
    }
  }

  private static void setEncounterClass(NotificationContext nc, Encounter enc) {
    if (isAmbulatoryEncounter(enc)) {
      logger.info("Setting Encounter Class as Ambulatory ");
      nc.setEncounterClass("AMB");
    } else {
      logger.info("Setting Encounter Class as Inpatient ");
      nc.setEncounterClass("IMP");
    }
  }

  private static boolean isAmbulatoryEncounter(Encounter enc) {
    return enc.hasClass_()
        && enc.getClass_().hasCode()
        && (enc.getClass_().getCode().contentEquals("AMB")
            || enc.getClass_().getCode().contentEquals("VR")
            || enc.getClass_().getCode().contentEquals("HH"));
  }

  private static void setRequestHeaders(NotificationContext nc, HttpServletRequest request) {
    String xRequestId = request.getHeader("X-Request-ID");
    String xCorrelationId = request.getHeader("X-Correlation-ID");

    if (isBothHeadersPresent(xRequestId, xCorrelationId)) {
      nc.setxRequestId(xRequestId);
      nc.setxCorrelationId(xCorrelationId);
    } else if (xRequestId != null && xRequestId.length() > 0) {
      nc.setxRequestId(xRequestId);
      nc.setxCorrelationId(xRequestId);
    } else if (xCorrelationId != null && xCorrelationId.length() > 0) {
      nc.setxCorrelationId(xCorrelationId);
      nc.setxRequestId(xCorrelationId);
    } else {
      String guid = java.util.UUID.randomUUID().toString();
      nc.setxRequestId(guid);
      nc.setxCorrelationId(guid);
    }
  }

  private static boolean isBothHeadersPresent(String xRequestId, String xCorrelationId) {
    return xRequestId != null
        && xRequestId.length() > 0
        && xCorrelationId != null
        && xCorrelationId.length() > 0;
  }

  private static void setLaunchContext(NotificationContext nc, PatientLaunchContext launchContext)
      throws InvalidNotification {
    if (launchContext == null
        || launchContext.getEhrLaunchContext() == null
        || launchContext.getEhrLaunchContext().isEmpty()) {
      return;
    }
    ObjectMapper objectMapper = new ObjectMapper();
    try {
      nc.setEhrLaunchContext(objectMapper.writeValueAsString(launchContext.getEhrLaunchContext()));
    } catch (JsonProcessingException e) {
      String err = "Unable to set the Context in the Notification Context table";
      logger.error(err);
      throw new InvalidNotification(err);
    }
  }

  public static String getFhirServerUrl(Reference ref) {

    // This checks to see if the URL contains the "Subscription" keyword.
    // https://example.org/fhir/r4/Subscription/encounter-close
    if (ref != null
        && ref.getReference() != null
        && ref.getReference().contains(ResourceType.Subscription.toString())) {

      // Split the String based on the String "Subscription" to obtain the FHIR Base Url.
      logger.info(" Subscription Url : {}", ref.getReference());

      String[] fhirServerUrl = ref.getReference().split(SUBSCRIPTION_URL_SPLIT_REGEX);

      if (fhirServerUrl != null && fhirServerUrl.length >= 1) {

        logger.info(" Found the Fhir Server Url ");
        return fhirServerUrl[0];
      }

    } else {

      logger.error(
          " Subscription Notification Reference does not have the appropriate Subscription URL required ");
    }

    return null;
  }

  public static String getPatientId(Resource res) {

    if (res != null) {

      if (res instanceof Encounter) {

        Encounter en = (Encounter) res;

        if (en.getSubject() != null && en.getSubject().getReferenceElement() != null)
          return en.getSubject().getReferenceElement().getIdPart();
      }

      logger.error(" Need to add other resources here after connectathon ");
    }

    return null;
  }
}
