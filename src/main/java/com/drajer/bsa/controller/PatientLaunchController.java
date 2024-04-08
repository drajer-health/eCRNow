package com.drajer.bsa.controller;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.bsa.service.SubscriptionNotificationReceiver;
import java.io.IOException;
import java.time.Instant;
import java.util.Date;
import java.util.UUID;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Bundle.BundleEntryRequestComponent;
import org.hl7.fhir.r4.model.Bundle.BundleEntryResponseComponent;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.Bundle.HTTPVerb;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.CodeType;
import org.hl7.fhir.r4.model.IntegerType;
import org.hl7.fhir.r4.model.Meta;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Reference;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

/**
 * This controller is used to receive notifications from EHRs to launch specific patient instances
 * for processing within the app for public health reporting.
 *
 * @author nbashyam
 */
@RestController
public class PatientLaunchController {

  @Autowired HealthcareSettingsService hsService;

  @Autowired EhrQueryService ehrService;

  @Autowired SubscriptionNotificationReceiver notificationReceiver;

  private final Logger logger = LoggerFactory.getLogger(PatientLaunchController.class);

  private static final String FHIR_VERSION = "fhirVersion";
  private static final String X_REQUEST_ID = "X-Request-ID";

  /**
   * The method is the API to launch a patient instance within the app for processing. In addition
   * to the parameters listed below, the following HTTP headers are expected to be populated.
   * 'X-Request-ID' - This can be used for logging across the client and server. This is expected to
   * be a GUID. 'X-Correlation-ID' - This can be used for correlation across clients and server
   * request / responses. This is expected to be a GUID.
   *
   * @param launchContext - Contains the context of the launch.
   * @param request - Request parameters
   * @param response - Response parameters.
   * @return
   * @throws IOException
   */
  @CrossOrigin
  @PostMapping(value = "/api/launchPatient")
  public String launchPatient(
      @RequestBody PatientLaunchContext launchContext,
      HttpServletRequest request,
      HttpServletResponse response) {

    logger.info(
        "Patient launch request received for fhirServerUrl: {}, patientId: {}, encounterId: {}, ehrLaunchContext: {}, requestId: {},  throttleContext: {}",
        StringEscapeUtils.escapeJava(launchContext.getFhirServerURL()),
        StringEscapeUtils.escapeJava(launchContext.getPatientId()),
        StringEscapeUtils.escapeJava(launchContext.getEncounterId()),
        launchContext.getEhrLaunchContext().size(),
        StringEscapeUtils.escapeJava(request.getHeader(X_REQUEST_ID)),
        launchContext.getThrottleContext());

    logger.info(FHIR_VERSION);

    HealthcareSetting hs = hsService.getHealthcareSettingByUrl(launchContext.getFhirServerURL());

    // If the healthcare setting exists
    if (hs != null) {

      String requestId = request.getHeader(X_REQUEST_ID);

      if (!StringUtils.isEmpty(requestId)) {

        Bundle nb = getNotificationBundle(launchContext, hs, requestId, false);

        notificationReceiver.processNotification(nb, request, response, launchContext);

      } else {
        throw new ResponseStatusException(
            HttpStatus.BAD_REQUEST,
            "No Request Id set in the header. Add X-Request-ID parameter for request tracking. ");
      }

    } else {
      throw new ResponseStatusException(
          HttpStatus.BAD_REQUEST, "Unrecognized healthcare setting FHIR URL ");
    }
    logger.info(
        " Patient launch was successful for patientId: {}, encounterId: {}, requestId: {}",
        StringEscapeUtils.escapeJava(launchContext.getPatientId()),
        StringEscapeUtils.escapeJava(launchContext.getEncounterId()),
        StringEscapeUtils.escapeJava(request.getHeader("X-Request-ID")));

    return "Patient Instance launched for processing successfully";
  }

  /**
   * The method is the API to re-launch a patient instance within the app for processing. The
   * re-launch API is intended to be used when the original patientId/encounterId combination has
   * bee completed either by the eCRNow App or by the EHR. This is intended to be used for long
   * running encounters. In such cases, the encounters may be open for a long duration of time and
   * the eCRNow App will suspend reporting for the encounter after a configurable time period. Once
   * it is suspended, any changes to the patient record for the patient/encounter combination will
   * not be detected and reported. In these cases, the re-launch API can be used to perform the
   * reporting.
   *
   * <p>NOTE: The re-launch API is a one shot check to report on the patient/encounter combination
   * and does not setup any future timers to keep checking and reporting. So EHR vendors are advised
   * to invoke, when their internal workflows detect specific data such as diagnosis,labresults etc
   * have been changed on a closed or long running encounter.
   *
   * <p>In addition to the parameters listed below, the following HTTP headers are expected to be
   * populated. 'X-Request-ID' - This can be used for logging across the client and server. This is
   * expected to be a GUID. 'X-Correlation-ID' - This can be used for correlation across clients and
   * server request / responses. This is expected to be a GUID.
   *
   * @param launchContext - Contains the context of the launch.
   * @param request - Request parameters
   * @param response - Response parameters.
   * @return
   * @throws IOException
   */
  @CrossOrigin
  @PostMapping(value = "/api/reLaunchPatient")
  public String reLaunchPatient(
      @RequestBody PatientLaunchContext launchContext,
      HttpServletRequest request,
      HttpServletResponse response) {

    logger.info(
        "Patient launch request received for fhirServerUrl: {}, patientId: {}, encounterId: {}, requestId: {}, throttleContext: {}",
        StringEscapeUtils.escapeJava(launchContext.getFhirServerURL()),
        StringEscapeUtils.escapeJava(launchContext.getPatientId()),
        StringEscapeUtils.escapeJava(launchContext.getEncounterId()),
        StringEscapeUtils.escapeJava(request.getHeader(X_REQUEST_ID)),
        launchContext.getThrottleContext());

    logger.info(FHIR_VERSION);

    HealthcareSetting hs = hsService.getHealthcareSettingByUrl(launchContext.getFhirServerURL());

    // If the healthcare setting exists
    if (hs != null) {

      String requestId = request.getHeader(X_REQUEST_ID);

      if (!StringUtils.isEmpty(requestId)) {

        Bundle nb = getNotificationBundle(launchContext, hs, requestId, true);

        notificationReceiver.processRelaunchNotification(
            nb, request, response, launchContext, true);

      } else {
        throw new ResponseStatusException(
            HttpStatus.BAD_REQUEST,
            "No Request Id set in the header. Add X-Request-ID parameter for request tracking. ");
      }

    } else {
      throw new ResponseStatusException(
          HttpStatus.BAD_REQUEST, "Unrecognized healthcare setting FHIR URL ");
    }
    logger.info(
        " Patient launch was successful for patientId: {}, encounterId: {}, requestId: {}",
        StringEscapeUtils.escapeJava(launchContext.getPatientId()),
        StringEscapeUtils.escapeJava(launchContext.getEncounterId()),
        StringEscapeUtils.escapeJava(request.getHeader("X-Request-ID")));

    return "Patient Instance launched for processing successfully";
  }

  /**
   * Create a NotificationBundle using the patientId and encounterId parameters.
   *
   * @return
   */
  public Bundle getNotificationBundle(
      PatientLaunchContext context, HealthcareSetting hs, String requestId, Boolean relaunch) {

    Bundle nb = new Bundle();

    nb.setId("notification-full-resource");

    // Setup Meta
    Meta met = new Meta();
    met.setLastUpdated(Date.from(Instant.now()));
    met.addProfile(
        "http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-subscription-notification");
    nb.setMeta(met);

    // Setup other attributes
    nb.setType(BundleType.HISTORY);
    nb.setTimestamp(Date.from(Instant.now()));

    // Add parameters
    Parameters params = new Parameters();
    String paramsId = UUID.randomUUID().toString();
    params.setId(paramsId);
    Meta paramMeta = new Meta();
    met.setLastUpdated(Date.from(Instant.now()));
    met.addProfile(
        "http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-subscriptionstatus");
    params.setMeta(paramMeta);

    // Add Subscription
    if (!relaunch) {
      Reference subsRef = new Reference();
      String url = context.getFhirServerURL() + "/Subscription/encounter-start";
      subsRef.setReference(url);
      params.addParameter("subscription", subsRef);

      // Add topic
      CanonicalType topicRef = new CanonicalType();
      String topicUrl = "http://hl7.org/fhir/us/medmorph/SubscriptionTopic/encounter-start";
      topicRef.setValue(topicUrl);
      params.addParameter("topic", topicRef);
    } else {
      Reference subsRef = new Reference();
      String url = context.getFhirServerURL() + "/Subscription/encounter-modified";
      subsRef.setReference(url);
      params.addParameter("subscription", subsRef);

      // Add topic
      CanonicalType topicRef = new CanonicalType();
      String topicUrl = "http://hl7.org/fhir/us/medmorph/SubscriptionTopic/encounter-modified";
      topicRef.setValue(topicUrl);
      params.addParameter("topic", topicRef);
    }

    // Add Type and Status
    CodeType ev = new CodeType();
    ev.setValue("event-notification");
    params.addParameter("type", ev);
    CodeType status = new CodeType();
    status.setValue("active");
    params.addParameter("status", status);
    IntegerType it = new IntegerType();
    it.setValue(1);
    params.addParameter("events-since-subscription-start", it);
    IntegerType ite = new IntegerType();
    ite.setValue(1);
    params.addParameter("events-in-notification", ite);

    // Add Entry
    BundleEntryComponent bec = new BundleEntryComponent();
    bec.setResource(params);
    bec.setFullUrl(paramsId);
    BundleEntryRequestComponent berc = new BundleEntryRequestComponent();
    berc.setMethod(HTTPVerb.GET);
    berc.setUrl(context.getFhirServerURL() + "/Subscription/admission/$status");
    bec.setRequest(berc);
    BundleEntryResponseComponent berpc = new BundleEntryResponseComponent();
    berpc.setStatus("200");
    nb.addEntry(bec);

    KarProcessingData kd = new KarProcessingData();
    kd.setHealthcareSetting(hs);
    kd.setNotificationContext(new NotificationContext());
    kd.getNotificationContext().setxRequestId(requestId);
    Resource res =
        ehrService.getResourceById(kd, ResourceType.Encounter.toString(), context.getEncounterId());
    nb.addEntry(new BundleEntryComponent().setResource(res));

    return nb;
  }
}
