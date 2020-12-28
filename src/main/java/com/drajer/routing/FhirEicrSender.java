package com.drajer.routing;

import com.drajer.eca.model.ActionRepo;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
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
public class FhirEicrSender {

  private final Logger logger = LoggerFactory.getLogger(FhirEicrSender.class);

  @Autowired FhirContextInitializer contextInitializer;

  @Autowired Authorization authorization;

  @Value("${eicr.fhir.server.url}")
  private String fhirServerURL;

  public JSONObject submitBundle(String bundle) {
    JSONObject bundleResponse = null;
    logger.info("Submitting Bundle to FHIR Server Endpoint:::::{}", fhirServerURL);
    try {
      RestTemplate restTemplate = new RestTemplate();
      HttpHeaders headers = new HttpHeaders();
      headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
      HttpEntity<String> request = new HttpEntity<>(bundle, headers);

      ResponseEntity<String> response =
          restTemplate.exchange(fhirServerURL, HttpMethod.POST, request, String.class);

      bundleResponse = new JSONObject(response.getBody());

      logger.info("Received response ");

      String fileName = ActionRepo.getInstance().getLogFileDirectory() + "/BundleResponse.json";
      logger.info("Saving response to file:::::{}", fileName);
      ApplicationUtils.saveDataToFile(response.getBody(), fileName);

    } catch (Exception e) {
      logger.error("Error in Submitting Bundle to FHIR Endpoint: {}", fhirServerURL, e);
    }
    return bundleResponse;
  }
}
