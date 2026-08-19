package com.drajer.bsa.controller;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.service.PhMessageService;
import com.drajer.sof.model.PublicHealthMessageData;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.text.StringEscapeUtils;
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
    try {
      if (logger.isInfoEnabled()) {
        logger.info(
            """
            Retrieving PublicHealthMessage based on request
            fhirServerBaseUrl = {}
            patientId = {}
            encounterId = {}
            xRequestId = {}
            submittedDataId = {}
            version = {}
            responseDataId = {}
            responseProcessingInstruction = {}
            notifiedResourceId = {}
            notifiedResourceType = {}
            karUniqueId = {}
            notificationId = {}
            """,
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
      }

      PhMessageSearchCriteria criteria =
          new PhMessageSearchCriteria.Builder()
              .fhirServerBaseUrl(fhirServerBaseUrl)
              .patientId(patientId)
              .encounterId(encounterId)
              .xRequestId(xRequestId)
              .submittedDataId(submittedDataId)
              .version(version)
              .responseDataId(responseDataId)
              .responseProcessingInstruction(responseProcessingInstruction)
              .notifiedResourceId(notifiedResourceId)
              .notifiedResourceType(notifiedResourceType)
              .karUniqueId(karUniqueId)
              .notificationId(notificationId)
              .xCorrelationId(xCorrelationId)
              .startTime(startTime)
              .endTime(endTime)
              .build();

      Map<String, String> searchParams = buildPhMessageSearchParams(criteria);

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
    try {
      if (logger.isInfoEnabled()) {
        logger.info(
            """
            Retrieving PublicHealthMessage based on request
            fhirServerBaseUrl = {}
            patientId = {}
            encounterId = {}
            xRequestId = {}
            submittedDataId = {}
            version = {}
            responseDataId = {}
            responseProcessingInstruction = {}
            notifiedResourceId = {}
            notifiedResourceType = {}
            karUniqueId = {}
            notificationId = {}
            """,
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
      }

      PhMessageSearchCriteria criteria =
          new PhMessageSearchCriteria.Builder()
              .fhirServerBaseUrl(fhirServerBaseUrl)
              .patientId(patientId)
              .encounterId(encounterId)
              .xRequestId(xRequestId)
              .submittedDataId(submittedDataId)
              .version(version)
              .responseDataId(responseDataId)
              .responseProcessingInstruction(responseProcessingInstruction)
              .notifiedResourceId(notifiedResourceId)
              .notifiedResourceType(notifiedResourceType)
              .karUniqueId(karUniqueId)
              .notificationId(notificationId)
              .xCorrelationId(xCorrelationId)
              .startTime(startTime)
              .endTime(endTime)
              .build();

      Map<String, String> searchParams = buildPhMessageSearchParams(criteria);

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

  private Map<String, String> buildPhMessageSearchParams(PhMessageSearchCriteria criteria) {
    Map<String, String> searchParams = new HashMap<>();
    addParameterIfPresent(searchParams, FHIR_SERVER_BASE_URL, criteria.fhirServerBaseUrl);
    addParameterIfPresent(searchParams, PATIENT_ID, criteria.patientId);
    addParameterIfPresent(searchParams, ENCOUNTER_ID, criteria.encounterId);
    addParameterIfPresent(searchParams, X_REQUEST_ID, criteria.xRequestId);
    addParameterIfPresent(searchParams, SUBMITTED_DATA_ID, criteria.submittedDataId);
    addParameterIfPresent(searchParams, VERSION, criteria.version);
    addParameterIfPresent(searchParams, RESPONSE_DATA_ID, criteria.responseDataId);
    addParameterIfPresent(
        searchParams, RESPONSE_PROCESSING_INSTRUCTION, criteria.responseProcessingInstruction);
    addParameterIfPresent(searchParams, NOTIFIED_RESOURCE_ID, criteria.notifiedResourceId);
    addParameterIfPresent(searchParams, NOTIFIED_RESOURCE_TYPE, criteria.notifiedResourceType);
    addParameterIfPresent(searchParams, KAR_UNIQUE_ID, criteria.karUniqueId);
    addParameterIfPresent(searchParams, NOTIFICATION_ID, criteria.notificationId);
    addParameterIfPresent(searchParams, X_CORRELATION_ID, criteria.xCorrelationId);
    addTimeParameterIfPresent(searchParams, SUBMISSION_TIME, criteria.startTime);
    addTimeParameterIfPresent(searchParams, RESPONSE_RECEIVED_TIME, criteria.endTime);
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

  private static class PhMessageSearchCriteria {
    final String fhirServerBaseUrl;
    final String patientId;
    final String encounterId;
    final String xRequestId;
    final String submittedDataId;
    final String version;
    final String responseDataId;
    final String responseProcessingInstruction;
    final String notifiedResourceId;
    final String notifiedResourceType;
    final String karUniqueId;
    final String notificationId;
    final String xCorrelationId;
    final String startTime;
    final String endTime;

    private PhMessageSearchCriteria(Builder builder) {
      this.fhirServerBaseUrl = builder.fhirServerBaseUrl;
      this.patientId = builder.patientId;
      this.encounterId = builder.encounterId;
      this.xRequestId = builder.xRequestId;
      this.submittedDataId = builder.submittedDataId;
      this.version = builder.version;
      this.responseDataId = builder.responseDataId;
      this.responseProcessingInstruction = builder.responseProcessingInstruction;
      this.notifiedResourceId = builder.notifiedResourceId;
      this.notifiedResourceType = builder.notifiedResourceType;
      this.karUniqueId = builder.karUniqueId;
      this.notificationId = builder.notificationId;
      this.xCorrelationId = builder.xCorrelationId;
      this.startTime = builder.startTime;
      this.endTime = builder.endTime;
    }

    static class Builder {
      String fhirServerBaseUrl;
      String patientId;
      String encounterId;
      String xRequestId;
      String submittedDataId;
      String version;
      String responseDataId;
      String responseProcessingInstruction;
      String notifiedResourceId;
      String notifiedResourceType;
      String karUniqueId;
      String notificationId;
      String xCorrelationId;
      String startTime;
      String endTime;

      Builder fhirServerBaseUrl(String val) {
        this.fhirServerBaseUrl = val;
        return this;
      }

      Builder patientId(String val) {
        this.patientId = val;
        return this;
      }

      Builder encounterId(String val) {
        this.encounterId = val;
        return this;
      }

      Builder xRequestId(String val) {
        this.xRequestId = val;
        return this;
      }

      Builder submittedDataId(String val) {
        this.submittedDataId = val;
        return this;
      }

      Builder version(String val) {
        this.version = val;
        return this;
      }

      Builder responseDataId(String val) {
        this.responseDataId = val;
        return this;
      }

      Builder responseProcessingInstruction(String val) {
        this.responseProcessingInstruction = val;
        return this;
      }

      Builder notifiedResourceId(String val) {
        this.notifiedResourceId = val;
        return this;
      }

      Builder notifiedResourceType(String val) {
        this.notifiedResourceType = val;
        return this;
      }

      Builder karUniqueId(String val) {
        this.karUniqueId = val;
        return this;
      }

      Builder notificationId(String val) {
        this.notificationId = val;
        return this;
      }

      Builder xCorrelationId(String val) {
        this.xCorrelationId = val;
        return this;
      }

      Builder startTime(String val) {
        this.startTime = val;
        return this;
      }

      Builder endTime(String val) {
        this.endTime = val;
        return this;
      }

      PhMessageSearchCriteria build() {
        return new PhMessageSearchCriteria(this);
      }
    }
  }
}
