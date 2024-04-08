package com.drajer.ecrapp.controller;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.EicrRRService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class EicrController {

  public static final String ERROR_IN_PROCESSING_THE_REQUEST = "Error in Processing the Request";
  private final Logger logger = LoggerFactory.getLogger(EicrController.class);

  @Autowired EicrRRService eicrRRService;

  @CrossOrigin
  @GetMapping(value = "/api/eicrData", produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> getEicrData(
      @RequestParam(name = "eicrId", required = false) String eicrId,
      @RequestParam(name = "eicrDocId", required = false) String eicrDocId,
      @RequestParam(name = "setId", required = false) String setId,
      @RequestParam(name = "patientId", required = false) String patientId,
      @RequestParam(name = "encounterId", required = false) String encounterId,
      @RequestParam(name = "version", required = false) String version,
      @RequestParam(name = "fhirServerUrl", required = false) String fhirServerUrl,
      @RequestParam(name = "xRequestId", required = false) String xRequestId) {
    List<JSONObject> eicrData = new ArrayList<>();
    try {
      logger.info(
          "Retrieving EICR based on request\n"
              + "eicrId = {}\n"
              + "eicrDocId = {}\n"
              + "setId = {}\n"
              + "patientId = {}\n"
              + "encounterId = {}\n"
              + "version = {}\n"
              + "fhirServerUrl = {}\n"
              + "xRequestId = {}",
          StringEscapeUtils.escapeJava(eicrId),
          StringEscapeUtils.escapeJava(eicrDocId),
          StringEscapeUtils.escapeJava(setId),
          StringEscapeUtils.escapeJava(patientId),
          StringEscapeUtils.escapeJava(encounterId),
          StringEscapeUtils.escapeJava(version),
          StringEscapeUtils.escapeJava(fhirServerUrl),
          StringEscapeUtils.escapeJava(xRequestId));

      Map<String, String> searchParams = new HashMap<>();
      if (eicrId != null && !eicrId.isEmpty()) {
        searchParams.put("eicrId", eicrId);
      }
      if (eicrDocId != null && !eicrDocId.isEmpty()) {
        searchParams.put("eicrDocId", eicrDocId);
      }
      if (setId != null && !setId.isEmpty()) {
        searchParams.put("setId", setId);
      }
      if (patientId != null && !patientId.isEmpty()) {
        searchParams.put("patientId", patientId);
      }
      if (encounterId != null && !encounterId.isEmpty()) {
        searchParams.put("encounterId", encounterId);
      }
      if (version != null && !version.isEmpty()) {
        searchParams.put("version", version);
      }
      if (fhirServerUrl != null && !fhirServerUrl.isEmpty()) {
        searchParams.put("fhirServerUrl", fhirServerUrl);
      }
      if (xRequestId != null && !xRequestId.isEmpty()) {
        searchParams.put("xRequestId", xRequestId);
      }
      eicrData = eicrRRService.getEicrData(searchParams);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }

    return new ResponseEntity<>(eicrData.toString(), HttpStatus.OK);
  }

  @CrossOrigin
  @GetMapping(value = "/api/responseData", produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> getRRData(
      @RequestParam(name = "responseDocId", required = false) String responseDocId,
      @RequestParam(name = "eicrDocId", required = false) String eicrDocId,
      @RequestParam(name = "setId", required = false) String setId,
      @RequestParam(name = "fhirServerUrl", required = false) String fhirServerUrl,
      @RequestParam(name = "patientId", required = false) String patientId,
      @RequestParam(name = "encounterId", required = false) String encounterId,
      @RequestParam(name = "version", required = false) String version) {
    List<JSONObject> rrData = new ArrayList<>();
    try {
      logger.info(
          "Retrieving EICR based on request\n"
              + "responseDocId = {}\n"
              + "eicrDocId = {}\n"
              + "setId = {}\n"
              + "patientId = {}\n"
              + "encounterId = {}\n"
              + "version = {}\n"
              + "fhirServerUrl = {}",
          responseDocId,
          eicrDocId,
          setId,
          patientId,
          encounterId,
          version,
          fhirServerUrl);

      Map<String, String> searchParams = new HashMap<>();
      if (responseDocId != null && !responseDocId.isEmpty()) {
        searchParams.put("responseDocId", responseDocId);
      }
      if (eicrDocId != null && !eicrDocId.isEmpty()) {
        searchParams.put("eicrDocId", eicrDocId);
      }
      if (setId != null && !setId.isEmpty()) {
        searchParams.put("setId", setId);
      }
      if (fhirServerUrl != null && !fhirServerUrl.isEmpty()) {
        searchParams.put("fhirServerUrl", fhirServerUrl);
      }
      if (patientId != null && !patientId.isEmpty()) {
        searchParams.put("patientId", patientId);
      }
      if (encounterId != null && !encounterId.isEmpty()) {
        searchParams.put("encounterId", encounterId);
      }
      if (version != null && !version.isEmpty()) {
        searchParams.put("version", version);
      }

      rrData = eicrRRService.getRRData(searchParams);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }

    return new ResponseEntity<>(rrData.toString(), HttpStatus.OK);
  }

  @CrossOrigin
  @GetMapping(value = "/api/eicrAndRRData")
  public ResponseEntity<Object> getEicrAndRRByRequestId(
      @RequestParam String xRequestId,
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID", required = false)
          String xCorrelationIdHttpHeaderValue) {
    List<JSONObject> eicrList = new ArrayList<>();
    try {
      logger.info(
          "X-Request-ID: {} and X-Correlation-ID: {} received for retrieving ECR",
          StringEscapeUtils.escapeJava(xRequestIdHttpHeaderValue),
          StringEscapeUtils.escapeJava(xCorrelationIdHttpHeaderValue));
      eicrList = eicrRRService.getEicrAndRRByXRequestId(xRequestId);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }

    return new ResponseEntity<>(eicrList.toString(), HttpStatus.OK);
  }

  @CrossOrigin
  @GetMapping(value = "/api/eicr")
  public ResponseEntity<Object> getEicrByEicrDocID(
      @RequestParam String eicrDocId,
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID", required = false)
          String xCorrelationIdHttpHeaderValue) {
    try {
      logger.info(
          "X-Request-ID: {} and X-Correlation-ID: {} received for retrieving ECR",
          StringEscapeUtils.escapeJava(xRequestIdHttpHeaderValue),
          StringEscapeUtils.escapeJava(xCorrelationIdHttpHeaderValue));

      if (eicrDocId == null || eicrDocId.isEmpty()) {
        logger.error("Eicr Doc Id is null ");
        return new ResponseEntity<>(
            "Requested eicrDocId is missing or empty", HttpStatus.BAD_REQUEST);
      }
      Eicr eicr = eicrRRService.getEicrByDocId(eicrDocId);
      if (eicr != null) {
        return new ResponseEntity<>(eicr, HttpStatus.OK);
      } else {
        String message = "Failed to find EICR by EICR_DOC_ID: " + eicrDocId;
        return new ResponseEntity<>(message, HttpStatus.NOT_FOUND);
      }
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      return new ResponseEntity<>(
          ERROR_IN_PROCESSING_THE_REQUEST, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @CrossOrigin
  @DeleteMapping(value = "/api/eicr")
  public ResponseEntity<String> deleteEicrByEicrDocID(
      @RequestParam String eicrDocId,
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID", required = false)
          String xCorrelationIdHttpHeaderValue,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {
      logger.info(
          "X-Request-ID: {} and X-Correlation-ID: {} received for deleting ECR",
          StringEscapeUtils.escapeJava(xRequestIdHttpHeaderValue),
          StringEscapeUtils.escapeJava(xCorrelationIdHttpHeaderValue));

      if (eicrDocId == null || eicrDocId.isEmpty()) {
        return new ResponseEntity<>(
            "Requested Eicr Doc Id is missing or empty", HttpStatus.BAD_REQUEST);
      }
      Eicr eicr = eicrRRService.getEicrByDocId(eicrDocId);
      if (eicr != null) {
        eicrRRService.deleteEicr(eicr);
        return new ResponseEntity<>("Eicr deleted successfully", HttpStatus.OK);
      }
      return new ResponseEntity<>("Eicr Not found", HttpStatus.NOT_FOUND);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      return new ResponseEntity<>(
          ERROR_IN_PROCESSING_THE_REQUEST, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @CrossOrigin
  @GetMapping(value = "/api/test2", produces = MediaType.APPLICATION_JSON_VALUE)
  public String test2(@RequestParam(value = "waitFor") Integer waitFor)
      throws InterruptedException {
    Thread.sleep(waitFor);
    return "Hello";
  }
}
