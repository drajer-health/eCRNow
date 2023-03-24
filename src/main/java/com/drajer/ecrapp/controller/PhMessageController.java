package com.drajer.ecrapp.controller;

import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.ecrapp.service.PhMessageService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class PhMessageController {

  public static final String ERROR_IN_PROCESSING_THE_REQUEST = "Error in Processing the Request";
  private final Logger logger = LoggerFactory.getLogger(PhMessageController.class);

  @Autowired PhMessageService phMessageService;

  @CrossOrigin
  @GetMapping(value = "/api/phMessage", produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> getPhMessageDeatils(
      @RequestParam(name = "fhirServerBaseUrl", required = false) String fhirServerBaseUrl,
      @RequestParam(name = "patientId", required = false) String patientId,
      @RequestParam(name = "encounterId", required = false) String encounterId,
      @RequestParam(name = "xRequestId", required = false) String xRequestId,
      @RequestParam(name = "submittedDataId", required = false) String submittedDataId,
      @RequestParam(name = "version", required = false) String version,
      @RequestParam(name = "responseDataId", required = false) String responseDataId,
      @RequestParam(name = "responseProcessingInstruction", required = false)
          String responseProcessingInstruction,
      @RequestParam(name = "notifiedResourceId", required = false) String notifiedResourceId,
      @RequestParam(name = "notifiedResourceType", required = false) String notifiedResourceType,
      @RequestParam(name = "karUniqueId", required = false) String karUniqueId,
      @RequestParam(name = "notificationId", required = false) String notificationId) {
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

      Map<String, String> searchParams = new HashMap<>();
      if (fhirServerBaseUrl != null && !fhirServerBaseUrl.isEmpty()) {
        searchParams.put("fhirServerBaseUrl", fhirServerBaseUrl);
      }
      if (patientId != null && !patientId.isEmpty()) {
        searchParams.put("patientId", patientId);
      }
      if (encounterId != null && !encounterId.isEmpty()) {
        searchParams.put("encounterId", encounterId);
      }
      if (patientId != null && !patientId.isEmpty()) {
        searchParams.put("patientId", patientId);
      }

      if (xRequestId != null && !xRequestId.isEmpty()) {
        searchParams.put("xRequestId", xRequestId);
      }

      if (submittedDataId != null && !submittedDataId.isEmpty()) {
        searchParams.put("submittedDataId", submittedDataId);
      }
      if (version != null && !version.isEmpty()) {
        searchParams.put("version", version);
      }
      if (responseDataId != null && !responseDataId.isEmpty()) {
        searchParams.put("responseDataId", responseDataId);
      }

      if (responseProcessingInstruction != null && !responseProcessingInstruction.isEmpty()) {
        searchParams.put("responseProcessingInstruction", responseProcessingInstruction);
      }

      if (notifiedResourceId != null && !notifiedResourceId.isEmpty()) {
        searchParams.put("notifiedResourceId", notifiedResourceId);
      }

      if (notifiedResourceType != null && !notifiedResourceType.isEmpty()) {
        searchParams.put("notifiedResourceType", notifiedResourceType);
      }

      if (karUniqueId != null && !karUniqueId.isEmpty()) {
        searchParams.put("karUniqueId", karUniqueId);
      }

      if (notificationId != null && !notificationId.isEmpty()) {
        searchParams.put("notificationId", notificationId);
      }

      List<PublicHealthMessage> phMessage = phMessageService.getPhMessageData(searchParams);

      if (phMessage != null) {
        return new ResponseEntity<>(phMessage, HttpStatus.OK);
      } else {
        String message = "Failed to get ph message data:";
        return new ResponseEntity<>(message, HttpStatus.NOT_FOUND);
      }

    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }
  }
}
