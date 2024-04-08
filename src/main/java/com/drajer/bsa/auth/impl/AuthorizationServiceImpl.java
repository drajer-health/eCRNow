package com.drajer.bsa.auth.impl;

import com.drajer.bsa.auth.AuthorizationService;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.FhirServerDetails;
import com.drajer.sof.model.Response;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Objects;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

/**
 *
 *
 * <h1>EhrAuthorizationServiceImpl</h1>
 *
 * This class defines the implementation methods to get authorized with the EHRs.
 *
 * @author nbashyam
 */
@Service("ehrauth")
@Transactional
public class AuthorizationServiceImpl implements AuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(AuthorizationServiceImpl.class);

  private static final String GRANT_TYPE = "grant_type";

  @Override
  public JSONObject getAuthorizationToken(FhirServerDetails fsd) {

    JSONObject tokenResponse;
    logger.info(
        "Getting AccessToken for EHR FHIR URL : {}",
        StringEscapeUtils.escapeJava(fsd.getFhirServerBaseURL()));
    logger.info(
        "Getting AccessToken for Client Id : {}", StringEscapeUtils.escapeJava(fsd.getClientId()));

    try {

      if (fsd.getAuthType().equals(BsaTypes.getString(BsaTypes.AuthenticationType.SYSTEM))
          || fsd.getAuthType()
              .equals(BsaTypes.getString(BsaTypes.AuthenticationType.MULTI_TENANT_SYSTEM_LAUNCH))) {

        logger.info(
            " System Launch/Multi-tenant System Launch authorization is configured for EHR {}",
            StringEscapeUtils.escapeJava(fsd.getAuthType()));

        RestTemplate resTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();

        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        headers.add("Accept", MediaType.APPLICATION_JSON_VALUE);

        // Setup the Authorization Header.
        String authValues = fsd.getClientId() + ":" + fsd.getClientSecret();
        String base64EncodedString =
            Base64.getEncoder().encodeToString(authValues.getBytes(StandardCharsets.UTF_8));
        headers.add("Authorization", "Basic " + base64EncodedString);

        // Setup the OAuth Type flow.
        MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
        map.add(GRANT_TYPE, "client_credentials");
        map.add("scope", fsd.getScopes());

        if (Boolean.TRUE.equals(fsd.getRequireAud())) {
          map.add("aud", fsd.getFhirServerBaseURL());
        }

        HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
        ResponseEntity<?> response =
            resTemplate.exchange(fsd.getTokenUrl(), HttpMethod.POST, entity, Response.class);
        tokenResponse = new JSONObject(Objects.requireNonNull(response.getBody()));

        logger.debug(
            "Received AccessToken: {}", StringEscapeUtils.escapeJson(tokenResponse.toString()));

        return tokenResponse;
      } else {

        logger.error(
            " Wrong Auth Type provided for Authorization {}",
            StringEscapeUtils.escapeJava(fsd.getAuthType()));
      }

    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the client: {}",
          StringEscapeUtils.escapeJava(fsd.getFhirServerBaseURL()),
          e);
    }
    return null;
  }
}
