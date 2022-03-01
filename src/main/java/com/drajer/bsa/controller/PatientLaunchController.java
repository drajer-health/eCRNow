package com.drajer.bsa.controller;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.PatientLaunchContext;
import com.drajer.bsa.service.HealthcareSettingsService;
import com.drajer.sof.launch.LaunchController;
import java.io.IOException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

/**
 * This controller is used to receive notifications from EHRs to launch specific patient instances
 * for processing within the app for public health reporting.
 *
 * @author nbashyam
 */
@RestController
public class PatientLaunchController {

  @Autowired HealthcareSettingsService hsService;

  private final Logger logger = LoggerFactory.getLogger(LaunchController.class);

  private static final String FHIR_VERSION = "fhirVersion";

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
      HttpServletResponse response)
      throws IOException {

    logger.info(
        "Patient launch request received for fhirServerUrl: {}, patientId: {}, encounterId: {}, requestId: {}",
        launchContext.getFhirServerURL(),
        launchContext.getPatientId(),
        launchContext.getEncounterId(),
        request.getHeader("X-Request-ID"));

    HealthcareSetting hs = hsService.getHealthcareSettingByUrl(launchContext.getFhirServerURL());

    // If the healthcare setting exists
    /*   if (hs != null) {

       	String requestId = request.getHeader("X-Request-ID");
           if(StringUtils.isEmpty(requestId)) {
           	requestId = UUID.randomUUID().toString();
           	request.
           }

           String correlationId = request.getHeader("X-Correlation-ID");

       } else {
         throw new ResponseStatusException(
             HttpStatus.BAD_REQUEST, "Unrecognized healthcare setting FHIR URL ");
       }
    */
    return "Patient Instance launched for processing successfully";
  }
}
