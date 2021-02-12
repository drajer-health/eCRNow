package com.drajer.ecrapp.controller;

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
      @RequestParam(name = "fhirServerUrl", required = false) String fhirServerUrl) {
    List<JSONObject> eicrData = new ArrayList<JSONObject>();
    try {
      logger.info("Received EicrId::::: {}", eicrId);
      logger.info("Received EicrDocId::::: {}", eicrDocId);
      logger.info("Received SetId::::: {}", setId);

      Map<String, String> searchParams = new HashMap<String, String>();
      searchParams.put("eicrId", eicrId);
      searchParams.put("eicrDocId", eicrDocId);
      searchParams.put("setId", setId);
      searchParams.put("patientId", patientId);
      searchParams.put("encounterId", encounterId);
      searchParams.put("version", version);
      searchParams.put("fhirServerUrl", fhirServerUrl);
      eicrData = eicrRRService.getEicrData(searchParams);
    } catch (Exception e) {
      logger.error("Error in Processing the request", e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Error in Processing the Request");
    }

    return new ResponseEntity<Object>(eicrData.toString(), HttpStatus.OK);
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
    List<JSONObject> rrData = new ArrayList<JSONObject>();
    try {
      logger.info("Received ResponseDocId::::: {}", responseDocId);
      logger.info("Received SetId::::: {}", setId);
      logger.info("Received FHIRServerURL::::: {}", fhirServerUrl);

      Map<String, String> searchParams = new HashMap<String, String>();
      searchParams.put("responseDocId", responseDocId);
      searchParams.put("eicrDocId", eicrDocId);
      searchParams.put("fhirServerUrl", fhirServerUrl);
      searchParams.put("setId", setId);
      searchParams.put("patientId", patientId);
      searchParams.put("encounterId", encounterId);
      searchParams.put("version", version);
      rrData = eicrRRService.getRRData(searchParams);
    } catch (Exception e) {
      logger.error("Error in Processing the request", e);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Error in Processing the Request");
    }

    return new ResponseEntity<Object>(rrData.toString(), HttpStatus.OK);
  }
}
