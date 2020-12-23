package com.drajer.sof.launch;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.annotation.OptionalParam;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.RRReceiverService;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.RefreshTokenScheduler;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.hl7.fhir.r4.model.DocumentReference;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class RRReceiverController {

  private final Logger logger = LoggerFactory.getLogger(RRReceiverController.class);

  private static final String ACCESS_TOKEN = "access_token";
  private static final String FHIR_VERSION = "fhirVersion";

  @Autowired RRReceiverService rrReceieverService;

  @Autowired ClientDetailsService clientDetailservice;

  @Autowired LaunchService launchService;

  @Autowired RefreshTokenScheduler tokenScheduler;

  @Autowired Authorization authorization;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @CrossOrigin
  @RequestMapping(value = "/api/rrReceiver", method = RequestMethod.POST)
  public ResponseEntity<String> rrReceiver(
      @RequestBody String obj,
      @OptionalParam(name = "type") String type,
      @OptionalParam(name = "xRequestIdHttpHeaderValue") String xRequestIdHttpHeaderValue,
      @OptionalParam(name = "fhirServerURL") String fhirServerURL,
      @OptionalParam(name = "patientId") String patientId,
      @OptionalParam(name = "encounter") String encounterId,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {
      logger.debug("Received Obj:::::" + obj);
      String xRequestId = null;
      String requestIdHeadervalue = request.getHeader("X-Request-ID");
      if (requestIdHeadervalue == null && xRequestIdHttpHeaderValue == null) {
        logger.error("Request Id is not present");
        throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Request Id is not present");
      } else {
        if ((requestIdHeadervalue != null && xRequestIdHttpHeaderValue != null)
            && !requestIdHeadervalue.equals(xRequestIdHttpHeaderValue)) {
          logger.error("Provided Request Id's are not matching");
          throw new ResponseStatusException(
              HttpStatus.BAD_REQUEST, "Provided Request Id's are not matching");
        } else {
          if (requestIdHeadervalue != null) {
            xRequestId = requestIdHeadervalue;
          } else if (xRequestIdHttpHeaderValue != null) {
            xRequestId = xRequestIdHttpHeaderValue;
          }
          LaunchDetails launchDetails = launchService.getLaunchDetailsByXRequestId(xRequestId);
          // Construct the DocumentReference Resource
          DocumentReference docRef =
              rrReceieverService.constructDocumentReference(
                  obj, type, launchDetails.getLaunchPatientId(), launchDetails.getEncounterId());

          // Get ClientDetails using the FHIR Server URL
          ClientDetails clientDetails =
              clientDetailservice.getClientDetailsByUrl(launchDetails.getEhrServerURL());

          // Get the AccessToken using the Client Details and read the Metadata Information to know
          // about the FHIR Server Version.
          if (clientDetails != null) {
            JSONObject tokenResponse = tokenScheduler.getSystemAccessToken(clientDetails);
            String access_token = tokenResponse.getString(ACCESS_TOKEN);
            String fhirVersion = "";
            JSONObject object = authorization.getMetadata(fhirServerURL + "/metadata");
            if (object != null) {
              logger.info("Reading Metadata information");
              if (object.getString(FHIR_VERSION).equals("1.0.2")) {
                fhirVersion = FhirVersionEnum.DSTU2.toString();
              }
              if (object.getString(FHIR_VERSION).equals("4.0.0")) {
                fhirVersion = FhirVersionEnum.R4.toString();
              }
            }

            // Initialize the FHIR Context based on FHIR Version
            FhirContext context = fhirContextInitializer.getFhirContext(fhirVersion);

            // Initialize the Client
            IGenericClient client =
                fhirContextInitializer.createClient(context, fhirServerURL, access_token);

            MethodOutcome outcome = fhirContextInitializer.submitResource(client, docRef);
            logger.info("DocumentReference Id::::: " + outcome.getId().getIdPart());
          } else {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unrecognized client");
          }
        }
      }
    } catch (Exception e) {
      logger.error("Error in Processing the request");
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage());
    }

    return new ResponseEntity<>("Success", HttpStatus.OK);
  }
}
