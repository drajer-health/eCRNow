package com.drajer.bsa.controller;

import com.drajer.bsa.model.RestApiBody;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

/**
 * This controller is a sample controller showing how to receive restful messages from eCRNow App.
 *
 * @author nbashyam
 */
@RestController
public class SampleRestApiReceiver {

  private static final Logger logger = LoggerFactory.getLogger(SampleRestApiReceiver.class);

  @CrossOrigin
  @PostMapping(value = "/api/receiveEicr")
  public JSONObject receiveEicr(
      @RequestBody RestApiBody body, HttpServletRequest request, HttpServletResponse response) {

    logger.info(
        "Payload received for fhirServerUrl: {}, patientId: {}, encounterId: {}, versionId: {}, requestId: {}, authorizationHeader: {}",
        body.getFhirServerURL(),
        body.getPatientId(),
        body.getEncounterId(),
        body.getSubmittedVersionId(),
        request.getHeader("X-Request-ID"),
        request.getHeader("Authorization"));

    logger.info(" Payload is : {}", body.getPayload());

    JSONObject responseObject = new JSONObject();
    responseObject.put("status", "Success");

    return responseObject;
  }
}
