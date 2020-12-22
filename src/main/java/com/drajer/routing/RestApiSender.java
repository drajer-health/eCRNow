package com.drajer.routing;

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

  public JSONObject sendEicrXmlDocument(LaunchDetails launchDetails, String eicrXml) {
    JSONObject bundleResponse = null;
    URIBuilder ub = null;
    String accessToken = null;
    try {
      HttpHeaders headers = new HttpHeaders();
      logger.info("In Initialization");

      if (authorizationService != null) {
        accessToken = authorizationService.getAuthorizationHeader(launchDetails);
      }

      headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);

      if (accessToken != null && !accessToken.isEmpty()) {
        logger.info("Setting Access_token============>" + accessToken);
        headers.add("Authorization", accessToken);
      }

      String json = constructJson(launchDetails);

      HttpEntity<String> request = new HttpEntity<>(json, headers);
      logger.info(launchDetails.getRestAPIURL());

      ub = new URIBuilder(launchDetails.getRestAPIURL());

      if (logger.isInfoEnabled()) {
        logger.info("Sending Eicr XML Document to Endpoint::::: {}", ub.toString());
      }

      ResponseEntity<String> response =
          restTemplate.exchange(ub.toString(), HttpMethod.POST, request, String.class);

      bundleResponse = new JSONObject(response.getBody());
      bundleResponse.put("status", response.getStatusCodeValue());

      if (logger.isInfoEnabled()) {
        logger.info("Received response: {}", bundleResponse.toString());
      }

    } catch (Exception e) {
      logger.error("RestAPI Exception", e);

      if (ub != null) {
        logger.error("Error in Sending Eicr XML to Endpoint: {}", ub.toString(), e);
      }
    }
    return bundleResponse;
  }

  private static String constructJson(LaunchDetails launchDetails) {
    StringBuilder sb = new StringBuilder(200);
    sb.append("{\"fhirServerURL\":\"");
    sb.append(launchDetails.getEhrServerURL());
    sb.append("\",\"patientId\":\"");
    sb.append(launchDetails.getLaunchPatientId());
    sb.append("\",\"encounterId\":\"");
    sb.append(launchDetails.getEncounterId());
    sb.append("\",\"ecrRequestId\":\"");
    sb.append(launchDetails.getxRequestId());
    sb.append("\"}");
    return sb.toString();
  }
}
