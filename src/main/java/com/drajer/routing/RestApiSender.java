package com.drajer.routing;

import static org.apache.commons.text.StringEscapeUtils.escapeJson;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.security.AuthorizationService;
import com.drajer.sof.model.LaunchDetails;
import org.apache.http.client.utils.URIBuilder;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class RestApiSender {

  private final Logger logger = LoggerFactory.getLogger(RestApiSender.class);

  @Autowired private AuthorizationService authorizationService;

  @Autowired private RestTemplate restTemplate;

  public JSONObject sendEicrXmlDocument(
      final LaunchDetails launchDetails, final String eicrXml, Eicr ecr) {
    JSONObject bundleResponse = null;
    URIBuilder ub = null;
    try {
      HttpHeaders headers = new HttpHeaders();
      logger.info("In Initialization");

      if (authorizationService != null) {
        headers = authorizationService.getAuthorizationHeader(launchDetails);
      }

      // Set Content-Type Header
      headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);

      // Add X-Request-ID and X-Correlation-ID
      String newXReqId = java.util.UUID.randomUUID().toString();
      logger.info(
          " Launch Req Id: {}, X-Request-ID for Eicr Submission: {}",
          ecr.getxRequestId(),
          newXReqId);
      headers.add("X-Request-ID", newXReqId);

      headers.add("X-Correlation-ID", ecr.getxCoorrelationId());
      logger.info(
          " Launch Req Id: {}, X-Correlation-ID for Eicr Submission: {}",
          ecr.getxRequestId(),
          ecr.getxCoorrelationId());

      final String json = constructJson(eicrXml, ecr);

      if (logger.isDebugEnabled()) {
        logger.debug("Eicr Trigger request: {}", json);
      }

      final HttpEntity<String> request = new HttpEntity<>(json, headers);

      logger.info(launchDetails.getRestAPIURL());

      ub = new URIBuilder(launchDetails.getRestAPIURL());

      if (logger.isInfoEnabled()) {
        logger.info("Sending Eicr Trigger to Endpoint::::: {}", ub);
      }

      final ResponseEntity<String> response =
          restTemplate.exchange(ub.toString(), HttpMethod.POST, request, String.class);

      bundleResponse = new JSONObject(response.getBody());
      bundleResponse.put("status", response.getStatusCodeValue());

      if (logger.isInfoEnabled()) {
        logger.info("Received response: {}", bundleResponse.toString());
      }

    } catch (Exception e) {
      if (logger.isErrorEnabled()) {
        if (ub != null) {
          logger.error("RestApi Error in sending Eicr XML to Endpoint {}:", ub, e);
        } else {
          logger.error("RestApi Error in preparing to send Eicr XML:", e);
        }
      }

      throw new RuntimeException(e.getMessage());
    }
    return bundleResponse;
  }

  /**
   * Construct EICR Trigger JSON.
   *
   * @param launchDetails {@link LaunchDetails}
   * @param eicrXml EICR XML document
   * @return EICR Trigger JSON
   */
  private static String constructJson(final String eicrXml, final Eicr ecr) {
    return "{\"fhirServerURL\":\""
        + escapeJson(ecr.getFhirServerUrl())
        + "\",\"patientId\":\""
        + escapeJson(ecr.getLaunchPatientId())
        + "\",\"encounterId\":\""
        + escapeJson(ecr.getEncounterId())
        + "\",\"eicrSetId\":\""
        + escapeJson(ecr.getSetId())
        + "\",\"eicrXml\":\""
        + escapeJson(eicrXml)
        + "\"}";
  }
}
