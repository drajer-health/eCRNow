package com.drajer.bsa.ehr.service.impl;

import com.drajer.bsa.ehr.service.EhrAuthorizationService;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.sof.model.Response;
import java.time.Instant;
import java.util.Base64;
import java.util.Date;
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
public class EhrAuthorizationServiceImpl implements EhrAuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(EhrAuthorizationServiceImpl.class);

  private static final String GRANT_TYPE = "grant_type";

  @Override
  public void getAuthorizationToken(KarProcessingData kd) {

    JSONObject tokenResponse = null;
    logger.info(
        "Getting AccessToken for EHR FHIR URL : {}",
        kd.getHealthcareSetting().getFhirServerBaseURL());
    logger.info("Getting AccessToken for Client Id : {}", kd.getHealthcareSetting().getClientId());

    try {

      if (kd.getHealthcareSetting()
          .getAuthType()
          .equals(BsaTypes.getString(BsaTypes.AuthenticationType.SofSystem))) {

        logger.info(" System Launch authorization is configured for EHR ");

        RestTemplate resTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();

        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        headers.add("Accept", MediaType.APPLICATION_JSON_VALUE);

        // Setup the Authorization Header.
        String authValues =
            kd.getHealthcareSetting().getClientId()
                + ":"
                + kd.getHealthcareSetting().getClientSecret();
        String base64EncodedString =
            Base64.getEncoder().encodeToString(authValues.getBytes("utf-8"));
        headers.add("Authorization", "Basic " + base64EncodedString);

        // Setup the OAuth Type flow.
        MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
        map.add(GRANT_TYPE, "client_credentials");
        map.add("scope", kd.getHealthcareSetting().getScopes());

        if (kd.getHealthcareSetting().getRequireAud())
          map.add("aud", kd.getHealthcareSetting().getFhirServerBaseURL());

        HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
        ResponseEntity<?> response =
            resTemplate.exchange(
                kd.getHealthcareSetting().getTokenUrl(), HttpMethod.POST, entity, Response.class);
        tokenResponse = new JSONObject(response.getBody());

        logger.info("Received AccessToken: {}", tokenResponse);

        kd.getNotificationContext().setEhrAccessToken(tokenResponse.getString("access_token"));
        kd.getNotificationContext()
            .setEhrAccessTokenExpiryDuration(tokenResponse.getInt("expires_in"));

        Integer expiresInSec = (Integer) tokenResponse.get("expires_in");
        Instant expireInstantTime = new Date().toInstant().plusSeconds(new Long(expiresInSec));
        kd.getNotificationContext()
            .setEhrAccessTokenExpirationTime(new Date().from(expireInstantTime));
      }

    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the client: {}",
          kd.getHealthcareSetting().getFhirServerBaseURL(),
          e);
    }
  }
}
