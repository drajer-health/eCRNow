package com.drajer.sof.launch;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.RRReceiver;
import com.drajer.sof.service.ClientDetailsService;
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
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class RRReceiverController {

  private final Logger logger = LoggerFactory.getLogger(RRReceiverController.class);

  private static final String ACCESS_TOKEN = "access_token";
  private static final String FHIR_VERSION = "fhirVersion";

  @Autowired RRReceiverService rrReceieverService;

  @Autowired ClientDetailsService clientDetailservice;

  @Autowired RefreshTokenScheduler tokenScheduler;

  @Autowired Authorization authorization;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @CrossOrigin
  @RequestMapping(value = "/api/rrReceiver", method = RequestMethod.POST)
  public ResponseEntity<String> rrReceiver(
      @RequestHeader(name = "X-Request-ID") String xRequestIdHttpHeaderValue,
      @RequestHeader(name = "X-Correlation-ID") String xCorrelationIdHttpHeaderValue,
      @RequestBody RRReceiver rrReceiver,
      HttpServletRequest request,
      HttpServletResponse response) {
    try {
      // Construct the DocumentReference Resource
      DocumentReference docRef = rrReceieverService.constructDocumentReference(rrReceiver);

      String docRefStr = FhirContext.forR4().newJsonParser().encodeResourceToString(docRef);
      logger.debug("docRef===========> {}", docRefStr);

      final String fhirServerURL = rrReceiver.getFhirServerURL();

      // Get ClientDetails using the FHIR Server URL
      ClientDetails clientDetails = clientDetailservice.getClientDetailsByUrl(fhirServerURL);

      // Get the AccessToken using the Client Details and read the Metadata Information to know
      // about the FHIR Server Version.
      if (clientDetails != null) {
        JSONObject tokenResponse = tokenScheduler.getSystemAccessToken(clientDetails);
        String accessToken = tokenResponse.getString(ACCESS_TOKEN);
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
            fhirContextInitializer.createClient(context, fhirServerURL, accessToken);

        MethodOutcome outcome = fhirContextInitializer.submitResource(client, docRef);

        if (outcome.getCreated()) {
          logger.info("Successfully processed the request");
          return new ResponseEntity<>("Success", HttpStatus.OK);
        } else {
          throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR);
        }
      } else {
        throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unrecognized client");
      }
    } catch (Exception e) {
      logger.error("Error in Processing the request", e);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage());
    }
  }
}
