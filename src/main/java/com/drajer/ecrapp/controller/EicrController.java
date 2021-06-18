package com.drajer.ecrapp.controller;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.EicrRRService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
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
  @RequestMapping(
      value = "/api/eicrData",
      method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
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
      logger.info("Received EicrId::::: {}", eicrId);
      logger.info("Received EicrDocId::::: {}", eicrDocId);
      logger.info("Received SetId::::: {}", setId);

      Map<String, String> searchParams = new HashMap<>();
      searchParams.put("eicrId", eicrId);
      searchParams.put("eicrDocId", eicrDocId);
      searchParams.put("setId", setId);
      searchParams.put("patientId", patientId);
      searchParams.put("encounterId", encounterId);
      searchParams.put("version", version);
      searchParams.put("fhirServerUrl", fhirServerUrl);
      searchParams.put("xRequestId", xRequestId);
      eicrData = eicrRRService.getEicrData(searchParams);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }

    return new ResponseEntity<>(eicrData.toString(), HttpStatus.OK);
  }

  @CrossOrigin
  @RequestMapping(
      value = "/api/responseData",
      method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
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
      logger.info("Received ResponseDocId::::: {}", responseDocId);
      logger.info("Received SetId::::: {}", setId);
      logger.info("Received FHIRServerURL::::: {}", fhirServerUrl);

      Map<String, String> searchParams = new HashMap<>();
      searchParams.put("responseDocId", responseDocId);
      searchParams.put("eicrDocId", eicrDocId);
      searchParams.put("fhirServerUrl", fhirServerUrl);
      searchParams.put("setId", setId);
      searchParams.put("patientId", patientId);
      searchParams.put("encounterId", encounterId);
      searchParams.put("version", version);
      rrData = eicrRRService.getRRData(searchParams);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }

    return new ResponseEntity<>(rrData.toString(), HttpStatus.OK);
  }

  @CrossOrigin
  @RequestMapping(value = "/api/eicrAndRRData", method = RequestMethod.GET)
  public ResponseEntity<Object> redirectEndPoint(@RequestParam String xRequestId) {
    List<JSONObject> eicrList = new ArrayList<>();
    try {
      eicrList = eicrRRService.getEicrAndRRByXRequestId(xRequestId);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ERROR_IN_PROCESSING_THE_REQUEST);
    }

    return new ResponseEntity<>(eicrList.toString(), HttpStatus.OK);
  }

  @CrossOrigin
  @GetMapping(value = "/api/rrIdAndDocRefId")
  public ResponseEntity<String> getEicrAllAttributes(@RequestParam String eicrDocId) {
    JSONObject eicrObject = new JSONObject();
    try {
      if (eicrDocId == null || eicrDocId.isEmpty()) {
        logger.error("Eicr Doc Id is null ");
        return new ResponseEntity<>(
            "Requested eicrDocId is missing or empty", HttpStatus.BAD_REQUEST);
      }
      Eicr eicr = eicrRRService.getEicrByDocId(eicrDocId);
      if (eicr != null) {
        eicrObject.put("rrId", eicr.getResponseDocId());
        eicrObject.put("docRefId", eicr.getEhrDocRefId());
      } else {
        String message = "Failed to find EICR by EICR_DOC_ID: " + eicrDocId;
        return new ResponseEntity<>(message, HttpStatus.NOT_FOUND);
      }
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      return new ResponseEntity<>(
          ERROR_IN_PROCESSING_THE_REQUEST, HttpStatus.INTERNAL_SERVER_ERROR);
    }
    return new ResponseEntity<>(eicrObject.toString(), HttpStatus.OK);
  }

  @CrossOrigin
  @DeleteMapping(value = "/api/eicrDataByDocId")
  public ResponseEntity<String> deleteEicrByEicrDocID(
      @RequestParam String eicrDocId, HttpServletRequest request, HttpServletResponse response) {
    try {
      if (eicrDocId == null || eicrDocId.isEmpty()) {
        return new ResponseEntity<>(
            "Requested Eicr Doc Id is missing or empty", HttpStatus.BAD_REQUEST);
      }
      Eicr eicr = eicrRRService.getEicrByDocId(eicrDocId);
      if (eicr != null) {
        eicrRRService.deleteEicr(eicr);
        return new ResponseEntity<>("Eicr deleted successfully.", HttpStatus.OK);
      }
      return new ResponseEntity<>("Eicr Not found", HttpStatus.NOT_FOUND);
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      return new ResponseEntity<>(
          ERROR_IN_PROCESSING_THE_REQUEST, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }
}
