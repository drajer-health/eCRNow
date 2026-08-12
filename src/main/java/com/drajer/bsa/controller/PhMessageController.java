package com.drajer.bsa.controller;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.PhMessageService;
import com.drajer.sof.model.PublicHealthMessageData;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class PhMessageController {

  public static final String ERROR_IN_PROCESSING_THE_REQUEST = "Error in Processing the Request";
  private static final String PATIENT_ID = "patientId";
  private static final String NO_PH_MESSAGE_RECORDS_FOUND = "No phMessage records found.";
  private static final String FHIR_SERVER_BASE_URL = "fhirServerBaseUrl";
  private static final String ENCOUNTER_ID = "encounterId";
  private static final String X_REQUEST_ID = "xRequestId";
  private static final String SUBMITTED_DATA_ID = "submittedDataId";
  private static final String VERSION = "version";
  private static final String RESPONSE_DATA_ID = "responseDataId";
  private static final String RESPONSE_PROCESSING_INSTRUCTION = "responseProcessingInstruction";
  private static final String NOTIFIED_RESOURCE_ID = "notifiedResourceId";
  private static final String NOTIFIED_RESOURCE_TYPE = "notifiedResourceType";
  private static final String KAR_UNIQUE_ID = "karUniqueId";
  private static final String NOTIFICATION_ID = "notificationId";
  private static final String X_CORRELATION_ID = "xCorrelationId";
  private static final String SUBMISSION_TIME = "submissionTime";
  private static final String RESPONSE_RECEIVED_TIME = "responseReceivedTime";
  private final Logger logger = LoggerFactory.getLogger(PhMessageController.class);
  private final PhMessageService phMessageService;

  /**
   * Instantiates a new ph message controller.
   *
   * @param phMessageService the ph message service
   */
  public PhMessageController(PhMessageService phMessageService) {
    this.phMessageService = phMessageService;
  }

  @CrossOrigin
  @GetMapping(value = "/api/phMessage", produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> getPhMessageDetails(
      @RequestParam(name = FHIR_SERVER_BASE_URL, required = false) String fhirServerBaseUrl,
      @RequestParam(name = PATIENT_ID, required = false) String patientId,
      @RequestParam(name = ENCOUNTER_ID, required = false) String encounterId,
      @RequestParam(name = X_REQUEST_ID, required = false) String xRequestId,
      @RequestParam(name = SUBMITTED_DATA_ID, required = false) String submittedDataId,
      @RequestParam(name = VERSION, required = false) String version,
      @RequestParam(name = RESPONSE_DATA_ID, required = false) String responseDataId,
      @RequestParam(name = RESPONSE_PROCESSING_INSTRUCTION, required = false)
          String responseProcessingInstruction,
      @RequestParam(name = NOTIFIED_RESOURCE_ID, required = false) String notifiedResourceId,
      @RequestParam(name = NOTIFIED_RESOURCE_TYPE, required = false) String notifiedResourceType,
      @RequestParam(name = KAR_UNIQUE_ID, required = false) String karUniqueId,
      @RequestParam(name = NOTIFICATION_ID, required = false) String notificationId,
      @RequestParam(name = X_CORRELATION_ID, required = false) String xCorrelationId,
      @RequestParam(name = "startTime", required = false) String startTime,
      @RequestParam(name = "endTime", required = false) String endTime,
      @RequestParam(name = "summaryFlag", required = false, defaultValue = "false")
          boolean summaryFlag) {
    List<JSONObject> phMessageData = new ArrayList<>();
    try {
      logger.info(
          "Retrieving PublicHealthMessage based on request\n"
              + "fhirServerBaseUrl = {}\n"
              + "patientId = {}\n"
              + "encounterId = {}\n"
              + "xRequestId = {}\n"
              + "submittedDataId = {}\n"
              + "version = {}\n"
              + "responseDataId = {}\n"
              + "responseProcessingInstruction = {}\n"
              + "notifiedResourceId = {}\n"
              + "notifiedResourceType = {}\n"
              + "karUniqueId = {}\n"
              + "notificationId = {}\n",
          StringEscapeUtils.escapeJava(fhirServerBaseUrl),
          StringEscapeUtils.escapeJava(patientId),
          StringEscapeUtils.escapeJava(encounterId),
          xRequestId,
          submittedDataId,
          version,
          responseDataId,
          responseProcessingInstruction,
          notifiedResourceId,
          notifiedResourceType,
          karUniqueId,
          notificationId);

      Map<String, String> searchParams =
          buildPhMessageSearchParams(
              fhirServerBaseUrl,
              patientId,
              encounterId,
              xRequestId,
              submittedDataId,
              version,
              responseDataId,
              responseProcessingInstruction,
              notifiedResourceId,
              notifiedResourceType,
              karUniqueId,
              notificationId,
              xCorrelationId,
              startTime,
              endTime);

      List<PublicHealthMessage> phMessage =
          phMessageService.getPhMessageData(searchParams, summaryFlag);

      if (phMessage != null) {
        return new ResponseEntity<>(phMessage, HttpStatus.OK);
      }
      return new ResponseEntity<>("Failed to get ph message data:", HttpStatus.NOT_FOUND);

    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }
  }

  @CrossOrigin
  @GetMapping(value = "/api/getPhMessagesSummary", produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> getPhMessageDetailsSummary(
      @RequestParam(name = FHIR_SERVER_BASE_URL, required = false) String fhirServerBaseUrl,
      @RequestParam(name = PATIENT_ID, required = false) String patientId,
      @RequestParam(name = ENCOUNTER_ID, required = false) String encounterId,
      @RequestParam(name = X_REQUEST_ID, required = false) String xRequestId,
      @RequestParam(name = SUBMITTED_DATA_ID, required = false) String submittedDataId,
      @RequestParam(name = VERSION, required = false) String version,
      @RequestParam(name = RESPONSE_DATA_ID, required = false) String responseDataId,
      @RequestParam(name = RESPONSE_PROCESSING_INSTRUCTION, required = false)
          String responseProcessingInstruction,
      @RequestParam(name = NOTIFIED_RESOURCE_ID, required = false) String notifiedResourceId,
      @RequestParam(name = NOTIFIED_RESOURCE_TYPE, required = false) String notifiedResourceType,
      @RequestParam(name = KAR_UNIQUE_ID, required = false) String karUniqueId,
      @RequestParam(name = NOTIFICATION_ID, required = false) String notificationId,
      @RequestParam(name = X_CORRELATION_ID, required = false) String xCorrelationId,
      @RequestParam(name = "startTime", required = false) String startTime,
      @RequestParam(name = "endTime", required = false) String endTime) {
    List<JSONObject> phMessageData = new ArrayList<>();
    try {
      logger.info(
          "Retrieving PublicHealthMessage based on request\n"
              + "fhirServerBaseUrl = {}\n"
              + "patientId = {}\n"
              + "encounterId = {}\n"
              + "xRequestId = {}\n"
              + "submittedDataId = {}\n"
              + "version = {}\n"
              + "responseDataId = {}\n"
              + "responseProcessingInstruction = {}\n"
              + "notifiedResourceId = {}\n"
              + "notifiedResourceType = {}\n"
              + "karUniqueId = {}\n"
              + "notificationId = {}\n",
          fhirServerBaseUrl,
          patientId,
          encounterId,
          xRequestId,
          submittedDataId,
          version,
          responseDataId,
          responseProcessingInstruction,
          notifiedResourceId,
          notifiedResourceType,
          karUniqueId,
          notificationId);

      Map<String, String> searchParams =
          buildPhMessageSearchParams(
              fhirServerBaseUrl,
              patientId,
              encounterId,
              xRequestId,
              submittedDataId,
              version,
              responseDataId,
              responseProcessingInstruction,
              notifiedResourceId,
              notifiedResourceType,
              karUniqueId,
              notificationId,
              xCorrelationId,
              startTime,
              endTime);

      List<PublicHealthMessage> phMessage = phMessageService.getPhMessageDataSummary(searchParams);

      if (phMessage != null) {
        return new ResponseEntity<>(phMessage, HttpStatus.OK);
      }
      return new ResponseEntity<>("Failed to get ph message data:", HttpStatus.NOT_FOUND);

    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }
  }

  @PostMapping("/api/phMessage/batch")
  public ResponseEntity<Object> getByBatchXRequestIds(
      @RequestBody Map<String, Object> requestBody) {
    try {
      List<String> xRequestIds = extractXRequestIds(requestBody);
      boolean summaryFlag = (boolean) requestBody.getOrDefault("summaryFlag", false);

      List<PublicHealthMessage> phMessage =
          phMessageService.getPhMessageDataByXRequestIds(xRequestIds, summaryFlag);

      if (phMessage != null && !phMessage.isEmpty()) {
        return ResponseEntity.ok(phMessage);
      } else {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(NO_PH_MESSAGE_RECORDS_FOUND);
      }
    } catch (IllegalArgumentException e) {
      return ResponseEntity.badRequest().body(e.getMessage());
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }
  }

  @PostMapping("/api/getPhMessagesContainingXRequestIds")
  public ResponseEntity<Object> getPhMessagesContainingXRequestIds(
      @RequestBody Map<String, Object> requestBody) {
    try {
      List<String> xRequestIds = extractXRequestIds(requestBody);
      boolean summaryFlag = (boolean) requestBody.getOrDefault("summaryFlag", false);

      List<PublicHealthMessage> phMessage =
          phMessageService.getPhMessagesContainingXRequestIds(xRequestIds, summaryFlag);

      if (phMessage != null && !phMessage.isEmpty()) {
        return ResponseEntity.ok(phMessage);
      } else {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(NO_PH_MESSAGE_RECORDS_FOUND);
      }
    } catch (IllegalArgumentException e) {
      return ResponseEntity.badRequest().body(e.getMessage());
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }
  }

  @CrossOrigin
  @DeleteMapping("/api/phMessage")
  public ResponseEntity<String> deletePhMessages(
      @RequestBody PublicHealthMessageData publicHealthMessageData) {
    if (publicHealthMessageData == null) {
      return ResponseEntity.badRequest()
          .body("Invalid input. Provide either 'id' or a combination of parameters.");
    }

    try {
      logger.info(
          "Parameters received for deleting phMessages: id={}, fhirServerBaseUrl={}, notifiedResourceId={}, patientId={}, versionId={}",
          publicHealthMessageData.getId(),
          publicHealthMessageData.getFhirServerBaseUrl(),
          publicHealthMessageData.getNotifiedResourceId(),
          publicHealthMessageData.getPatientId(),
          publicHealthMessageData.getSubmittedVersionNumber());

      List<PublicHealthMessage> publicHealthMessages =
          phMessageService.getPhMessageByParameters(publicHealthMessageData);

      if (publicHealthMessages.isEmpty()) {

        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(NO_PH_MESSAGE_RECORDS_FOUND);
      }
      publicHealthMessages.forEach(
          publicHealthMessage -> {
            phMessageService.deletePhMessage(publicHealthMessage);
          });

      return ResponseEntity.ok("phMessages deleted successfully");
    } catch (Exception e) {
      logger.error("Error in processing the request", e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body("Error in processing the request");
    }
  }

  private Map<String, String> buildPhMessageSearchParams(
      String fhirServerBaseUrl,
      String patientId,
      String encounterId,
      String xRequestId,
      String submittedDataId,
      String version,
      String responseDataId,
      String responseProcessingInstruction,
      String notifiedResourceId,
      String notifiedResourceType,
      String karUniqueId,
      String notificationId,
      String xCorrelationId,
      String startTime,
      String endTime) {
    Map<String, String> searchParams = new HashMap<>();
    addParameterIfPresent(searchParams, FHIR_SERVER_BASE_URL, fhirServerBaseUrl);
    addParameterIfPresent(searchParams, PATIENT_ID, patientId);
    addParameterIfPresent(searchParams, ENCOUNTER_ID, encounterId);
    addParameterIfPresent(searchParams, X_REQUEST_ID, xRequestId);
    addParameterIfPresent(searchParams, SUBMITTED_DATA_ID, submittedDataId);
    addParameterIfPresent(searchParams, VERSION, version);
    addParameterIfPresent(searchParams, RESPONSE_DATA_ID, responseDataId);
    addParameterIfPresent(
        searchParams, RESPONSE_PROCESSING_INSTRUCTION, responseProcessingInstruction);
    addParameterIfPresent(searchParams, NOTIFIED_RESOURCE_ID, notifiedResourceId);
    addParameterIfPresent(searchParams, NOTIFIED_RESOURCE_TYPE, notifiedResourceType);
    addParameterIfPresent(searchParams, KAR_UNIQUE_ID, karUniqueId);
    addParameterIfPresent(searchParams, NOTIFICATION_ID, notificationId);
    addParameterIfPresent(searchParams, X_CORRELATION_ID, xCorrelationId);
    addTimeParameterIfPresent(searchParams, SUBMISSION_TIME, startTime);
    addTimeParameterIfPresent(searchParams, RESPONSE_RECEIVED_TIME, endTime);
    return searchParams;
  }

  private void addParameterIfPresent(Map<String, String> map, String key, String value) {
    if (value != null && !value.isEmpty()) {
      map.put(key, value);
    }
  }

  private void addTimeParameterIfPresent(Map<String, String> map, String key, String value) {
    if (value != null) {
      map.put(key, value);
    }
  }

  private List<String> extractXRequestIds(Map<String, Object> requestBody) {
    List<String> xRequestIds = (List<String>) requestBody.get("xRequestIds");

    if (xRequestIds == null || xRequestIds.isEmpty()) {
      throw new IllegalArgumentException(
          "The provided Xrequest IDs are out of range. "
              + "Please ensure that the number of Xrequest IDs is greater than 0 ");
    }

    return xRequestIds;
  }
}
