package com.drajer.routing;

import static org.apache.commons.text.StringEscapeUtils.escapeJson;

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

  public JSONObject sendEicrXmlDocument(final LaunchDetails launchDetails, final String eicrXml) {
    JSONObject bundleResponse = null;
    URIBuilder ub = null;
    try {
      HttpHeaders headers = new HttpHeaders();
      logger.info("In Initialization");

      String accessToken = null;
      if (authorizationService != null) {
        accessToken = authorizationService.getAuthorizationHeader(launchDetails);
      }

      headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);

      if (accessToken != null && !accessToken.isEmpty()) {
        logger.info("Setting Access_token============> {} ", accessToken);
        headers.add("Authorization", accessToken);
      }

      final String json = constructJson(launchDetails, eicrXml);

      if (logger.isDebugEnabled()) {
        logger.debug("Eicr Trigger request: {}", json);
      }

      final HttpEntity<String> request = new HttpEntity<>(json, headers);

      logger.info(launchDetails.getRestAPIURL());

      ub = new URIBuilder(launchDetails.getRestAPIURL());

      if (logger.isInfoEnabled()) {
        logger.info("Sending Eicr Trigger to Endpoint::::: {}", ub.toString());
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
          logger.error("RestApi Error in sending Eicr XML to Endpoint {}:", ub.toString(), e);
        } else {
          logger.error("RestApi Error in preparing to send Eicr XML:", e);
        }
      }
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
  private static String constructJson(final LaunchDetails launchDetails, final String eicrXml) {
    return "{\"fhirServerURL\":\""
        + escapeJson(launchDetails.getEhrServerURL())
        + "\",\"patientId\":\""
        + escapeJson(launchDetails.getLaunchPatientId())
        + "\",\"encounterId\":\""
        + escapeJson(launchDetails.getEncounterId())
        + "\",\"ecrRequestId\":\""
        + escapeJson(launchDetails.getxRequestId())
        + "\",\"eicrXml\":\""
        + escapeJson(eicrXml)
        + "\"}";
  }
}
