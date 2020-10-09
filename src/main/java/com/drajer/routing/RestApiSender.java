package com.drajer.routing;

import com.drajer.sof.model.LaunchDetails;
import org.apache.http.client.utils.URIBuilder;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

  public JSONObject sendEicrXmlDocument(LaunchDetails launchDetails, String eicrXml) {
    JSONObject bundleResponse = null;
    URIBuilder ub = null;
    try {
      RestTemplate restTemplate = new RestTemplate();
      HttpHeaders headers = new HttpHeaders();
      headers.add("Content-Type", MediaType.APPLICATION_XML_VALUE);
      HttpEntity<String> request = new HttpEntity<String>(eicrXml, headers);

      ub = new URIBuilder(launchDetails.getRestAPIURL());
      ub.addParameter("fhirServerURL", launchDetails.getEhrServerURL());
      ub.addParameter("patientId", launchDetails.getLaunchPatientId());
      ub.addParameter("encounterId", launchDetails.getEncounterId());
      ub.addParameter("setId", "123");

      logger.info("Sending Eicr XML Document to Endpoint:::::" + ub.toString());
      ResponseEntity<String> response =
          restTemplate.exchange(ub.toString(), HttpMethod.POST, request, String.class);

      bundleResponse = new JSONObject(response.getBody());
      bundleResponse.put("status", response.getStatusCodeValue());

      logger.info("Received response: " + bundleResponse.toString());

    } catch (Exception e) {
      e.printStackTrace();
      logger.error("Error in Sending Eicr XML to Endpoint: " + ub.toString());
    }
    return bundleResponse;
  }
}
