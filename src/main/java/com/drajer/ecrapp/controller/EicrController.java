package com.drajer.ecrapp.controller;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.EicrRRService;
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
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
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
  @RequestMapping(value = "/api/rrIdAndDocRefId", method = RequestMethod.GET)
  public ResponseEntity<Object> getEicrAllAttributes(@RequestParam String eicr_doc_id) {
    JSONObject eicrObject = new JSONObject();
    try {
      if (eicr_doc_id == null || eicr_doc_id.isEmpty()) {
        logger.error("Eicr Doc Id is null ");
        return new ResponseEntity(
            "Requested eicr_doc_id is missing or empty", HttpStatus.BAD_REQUEST);
      }
      Eicr eicr = eicrRRService.getEicrByDocId(eicr_doc_id);
      if (eicr != null) {
        eicrObject.put("rrId", eicr.getResponseDocId());
        eicrObject.put("docRefId", eicr.getEhrDocRefId());
      } else {
        String message = "Failed to find EICR by EICR_DOC_ID: " + eicr_doc_id;
        return new ResponseEntity<>(message, HttpStatus.NOT_FOUND);
      }
    } catch (Exception e) {
      logger.error(ERROR_IN_PROCESSING_THE_REQUEST, e);
      throw new ResponseStatusException(
          HttpStatus.INTERNAL_SERVER_ERROR, ERROR_IN_PROCESSING_THE_REQUEST);
    }
    return new ResponseEntity<>(eicrObject.toString(), HttpStatus.OK);
  }
}
