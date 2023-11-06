package com.drajer.routing;

import static org.apache.commons.text.StringEscapeUtils.escapeJson;

import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.security.AuthorizationService;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
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

  @Autowired private AuthorizationService authorizationService;

  @Autowired private RestTemplate restTemplate;

  @Value("${isMatchedPathsAndEicrDocIdRequired:false}")
  private boolean isMatchedPathsAndEicrDocIdRequired;

  private static final String MATCHED_PATHS_HEADER = "matchedPaths";
  private static final String EICR_DOC_ID_HEADER = "eicrDocId";

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
      headers.add("X-Request-ID", newXReqId);
      headers.add("X-Correlation-ID", ecr.getxCorrelationId());

      logger.info(
          " Launch ReqId: {} X-Request-ID for Eicr Submission: {} X-Correlation-ID for Eicr Submission: {}",
          ecr.getxRequestId(),
          newXReqId,
          ecr.getxCorrelationId());

      final String json = constructJson(eicrXml, ecr);

      if (logger.isDebugEnabled()) {
        logger.debug("Eicr Trigger request: {}", json);
      }
      if (isMatchedPathsAndEicrDocIdRequired) {
        String matchedPaths = "";
        matchedPaths = getTriggerMatchedCodes(launchDetails);
        headers.add(MATCHED_PATHS_HEADER, matchedPaths);
        headers.add(EICR_DOC_ID_HEADER, ecr.getEicrDocId());
      }

      final HttpEntity<String> request = new HttpEntity<>(json, headers);

      logger.info("Request Header : {}", headers);

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
        logger.info("Received response: {}", bundleResponse);
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

  public String getTriggerMatchedCodes(final LaunchDetails launchDetails) {
    String matchedPaths = "";
    PatientExecutionState state = ApplicationUtils.getDetailStatus(launchDetails);
    if (state != null) {
      if (state.getMatchTriggerStatus() != null
          && state.getMatchTriggerStatus().getMatchedCodes() != null) {
        List<MatchedTriggerCodes> codes = state.getMatchTriggerStatus().getMatchedCodes();
        matchedPaths =
            codes
                .stream()
                .filter(Objects::nonNull)
                .map(code -> code.getMatchedPath())
                .collect(Collectors.joining(","));
        logger.info("Matched Paths {}", matchedPaths);
      }
    } else {
      logger.info("No Matched Codes present");
    }
    return matchedPaths;
  }

  /**
   * Construct EICR Trigger JSON.
   *
   * @param eicrXml EICR XML document
   * @param ecr EICR object
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
