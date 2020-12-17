package com.drajer.routing;

import com.drajer.ecrapp.security.AuthorizationService;
import com.drajer.sof.model.LaunchDetails;
import org.apache.http.client.utils.URIBuilder;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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

  @Value("${authorization.service.impl.class}")
  private String authServiceImplClassName;

  @Autowired private RestTemplate restTemplate;

  public JSONObject sendEicrXmlDocument(LaunchDetails launchDetails, String eicrXml) {
    JSONObject bundleResponse = null;
    URIBuilder ub = null;
    String access_token = null;
    try {
      HttpHeaders headers = new HttpHeaders();
      logger.info("In Initialization");

      if (!authServiceImplClassName.isEmpty()) {

        Class<?> clazz = Class.forName(authServiceImplClassName);
        AuthorizationService authService =
            (AuthorizationService) clazz.getConstructor().newInstance();
        access_token = authService.getAuthorizationHeader(launchDetails);

        /*Class classInstance = Class.forName(authServiceImplClassName);
        Method authMethod = classInstance.getMethod("getAuthorizationHeader", LaunchDetails.class);
        logger.info(authMethod.getName());
        access_token = (String) authMethod.invoke(classInstance.newInstance(), launchDetails);*/
      }

      headers.add("Content-Type", MediaType.APPLICATION_XML_VALUE);
      headers.add("X-Request-ID", launchDetails.getxRequestId());

      if (access_token != null && !access_token.isEmpty()) {
        logger.info("Setting Access_token============>" + access_token);
        headers.add("Authorization", access_token);
      }

      HttpEntity<String> request = new HttpEntity<String>(eicrXml, headers);
      logger.info(launchDetails.getRestAPIURL());
      ub = new URIBuilder(launchDetails.getRestAPIURL());
      ub.addParameter("fhirServerURL", launchDetails.getEhrServerURL());
      ub.addParameter("patientId", launchDetails.getLaunchPatientId());
      ub.addParameter("encounterId", launchDetails.getEncounterId());
      ub.addParameter("setId", "123");

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
        logger.error("Error in Sending Eicr XML to Endpoint: {}", ub.toString());
      }
    }
    return bundleResponse;
  }

  /*
   * private String getAccessToken(LaunchDetails launchDetails) { String
   * access_token = ""; RestTemplate restTemplate = new RestTemplate(); JSONObject
   * requestBody = new JSONObject(); requestBody.put("client_id",
   * launchDetails.getClientId()); requestBody.put("client_secret",
   * launchDetails.getClientSecret()); HttpHeaders headers = new HttpHeaders();
   * headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
   * HttpEntity<String> request = new HttpEntity<String>(requestBody.toString(),
   * headers);
   *
   * logger.info( "Sending Authorization request to Endpoint::::: {}",
   * launchDetails.getEhrAuthorizationUrl()); ResponseEntity<String> response =
   * restTemplate.exchange( launchDetails.getEhrAuthorizationUrl(),
   * HttpMethod.POST, request, String.class);
   *
   * JSONObject responseObj = new JSONObject(response.getBody()); access_token =
   * responseObj.getString("access_token");
   * logger.info("received Access_token====>" + access_token); return
   * access_token; }
   */
}
