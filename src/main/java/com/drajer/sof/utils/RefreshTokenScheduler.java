package com.drajer.sof.utils;

import com.drajer.eca.model.ActionRepo;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.Response;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Base64;
import java.util.Date;
import java.util.concurrent.TimeUnit;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

/** The Class RefreshTokenScheduler. */
@Component
public class RefreshTokenScheduler {

  /** The task scheduler. */
  @Autowired ThreadPoolTaskScheduler taskScheduler;

  /** The logger. */
  private final Logger logger = LoggerFactory.getLogger(RefreshTokenScheduler.class);

  /** The Constant GRANT_TYPE. */
  private static final String GRANT_TYPE = "grant_type";

  /** The Constant SCOPE. */
  private static final String SCOPE = "scope";

  /** The Constant CLIENT_CREDENTIALS. */
  private static final String CLIENT_CREDENTIALS = "client_credentials";

  /** The Constant ACCEPT_HEADER. */
  private static final String ACCEPT_HEADER = "Accept";

  /** The Constant ACCESS_TOKEN. */
  private static final String ACCESS_TOKEN = "access_token";

  /** The Constant ACCESS_TOKEN_CAMEL_CASE. */
  private static final String ACCESS_TOKEN_CAMEL_CASE = "accessToken";

  /** The Constant EXPIRES_IN. */
  private static final String EXPIRES_IN = "expires_in";

  /** The Constant EXPIRES_IN_CAMEL_CASE. */
  private static final String EXPIRES_IN_CAMEL_CASE = "expiresIn";

  /** The Constant PROVIDER_UUID. */
  private static final String PROVIDER_UUID = "uuid";

  /**
   * Schedule job.
   *
   * @param authDetails the auth details
   */
  public void scheduleJob(LaunchDetails authDetails) {
    logger.info(
        "Scheduling Cron Job to Get AccessToken which expires every {} seconds",
        authDetails.getExpiry());
    long minutes = TimeUnit.SECONDS.toMinutes(authDetails.getExpiry());
    String cronExpression = "0 " + "0/" + minutes + " * * * ?";
    CronTrigger cronTrigger = new CronTrigger(cronExpression);
    taskScheduler.schedule(new RunnableTask(authDetails), cronTrigger);
    logger.info(
        "Job Scheduled to get AccessToken for every {} minutes for Client: {}",
        minutes,
        StringEscapeUtils.escapeJava(authDetails.getClientId()));
  }

  /** The Class RunnableTask. */
  class RunnableTask implements Runnable {

    /** The auth details. */
    private LaunchDetails authDetails;

    /**
     * Instantiates a new runnable task.
     *
     * @param authDetails the auth details
     */
    public RunnableTask(LaunchDetails authDetails) {
      this.authDetails = authDetails;
    }

    /** Run. */
    @Override
    public void run() {
      try {
        getAccessTokenUsingLaunchDetails(this.authDetails);
        Thread.currentThread().interrupt();
      } catch (Exception e) {
        logger.info("Error in Getting AccessToken=====>", e);
      }
    }
  }

  /**
   * Gets the access token using launch details.
   *
   * @param authDetails the auth details
   * @return the access token using launch details
   */
  public JSONObject getAccessTokenUsingLaunchDetails(LaunchDetails authDetails) {
    JSONObject tokenResponse = null;
    logger.trace(
        "Getting AccessToken for Client: {}",
        StringEscapeUtils.escapeJava(authDetails.getClientId()));
    try {
      RestTemplate resTemplate = new RestTemplate();
      HttpHeaders headers = new HttpHeaders();
      if (Boolean.FALSE.equals(authDetails.getIsSystem())
          && Boolean.FALSE.equals(authDetails.getIsUserAccountLaunch())
          && Boolean.FALSE.equals(authDetails.getIsMultiTenantSystemLaunch())) {
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
        map.add(GRANT_TYPE, "refresh_token");
        map.add("refresh_token", authDetails.getRefreshToken());
        HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
        ResponseEntity<?> response =
            resTemplate.exchange(
                authDetails.getTokenUrl(), HttpMethod.POST, entity, Response.class);
        tokenResponse = new JSONObject(response.getBody());
      } else if (authDetails.getIsSystem() || authDetails.getIsMultiTenantSystemLaunch()) {
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        headers.add(ACCEPT_HEADER, MediaType.APPLICATION_JSON_VALUE);
        String authValues = authDetails.getClientId() + ":" + authDetails.getClientSecret();
        String base64EncodedString =
            Base64.getEncoder().encodeToString(authValues.getBytes(StandardCharsets.UTF_8));
        headers.add("Authorization", "Basic " + base64EncodedString);
        MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
        map.add(GRANT_TYPE, CLIENT_CREDENTIALS);
        map.add(SCOPE, authDetails.getScope());
        if (Boolean.TRUE.equals(authDetails.getRequireAud())
            && Boolean.TRUE.equals(authDetails.getIsSystem())) {
          logger.debug("Adding Aud Parameter while getting AccessToken");
          map.add("aud", authDetails.getEhrServerURL());
        }
        HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
        ResponseEntity<?> response =
            resTemplate.exchange(
                authDetails.getTokenUrl(), HttpMethod.POST, entity, Response.class);
        tokenResponse = new JSONObject(response.getBody());
      } else if (Boolean.TRUE.equals(authDetails.getIsUserAccountLaunch())) {
        headers.setBasicAuth(authDetails.getClientId(), authDetails.getClientSecret());

        String tokenUrl =
            authDetails.getTokenUrl()
                + "?"
                + GRANT_TYPE
                + "="
                + CLIENT_CREDENTIALS
                + "&"
                + SCOPE
                + "="
                + authDetails.getScope();

        HttpEntity<?> entity = new HttpEntity<>(headers);

        ResponseEntity<?> response =
            resTemplate.exchange(tokenUrl, HttpMethod.GET, entity, String.class);
        String resBody = (String) response.getBody();
        if (resBody != null) {
          String responseBody =
              resBody
                  .replace(ACCESS_TOKEN_CAMEL_CASE, ACCESS_TOKEN)
                  .replace(EXPIRES_IN_CAMEL_CASE, EXPIRES_IN);
          tokenResponse = new JSONObject(responseBody);
        }
      }
      logger.trace(
          "Received AccessToken for Client {}",
          StringEscapeUtils.escapeJava(authDetails.getClientId()));
      if (Boolean.TRUE.equals(authDetails.getIsMultiTenantSystemLaunch())) {
        ClientDetails clientDetails =
            ActionRepo.getInstance()
                .getClientDetailsService()
                .getClientDetailsByUrl(authDetails.getEhrServerURL());
        updateAccessTokenInClientDetails(clientDetails, tokenResponse);
      }
      LaunchDetails existingAuthDetails =
          ActionRepo.getInstance().getLaunchService().getAuthDetailsById(authDetails.getId());
      updateAccessToken(existingAuthDetails, tokenResponse);

    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the Client: {}",
          StringEscapeUtils.escapeJava(authDetails.getClientId()),
          e);
    }
    return tokenResponse;
  }

  /**
   * Update access token.
   *
   * @param existingAuthDetails the existing auth details
   * @param tokenResponse the token response
   */
  private void updateAccessToken(LaunchDetails existingAuthDetails, JSONObject tokenResponse) {
    try {
      if (existingAuthDetails != null) {
        logger.trace("Updating the AccessToken value in LaunchDetails table");
        existingAuthDetails.setAccessToken(tokenResponse.getString(ACCESS_TOKEN));
        existingAuthDetails.setExpiry(tokenResponse.getInt(EXPIRES_IN));
        if (tokenResponse.has(PROVIDER_UUID)) {
          existingAuthDetails.setProviderUUID(tokenResponse.getString(PROVIDER_UUID));
        }
        existingAuthDetails.setLastUpdated(new Date());
        long expiresInSec = tokenResponse.getLong(EXPIRES_IN);
        Instant expireInstantTime = new Date().toInstant().plusSeconds(expiresInSec);
        existingAuthDetails.setTokenExpiryDateTime(new Date().from(expireInstantTime));
        ActionRepo.getInstance().getLaunchService().saveOrUpdate(existingAuthDetails);
        logger.trace("Successfully updated AccessToken value in LaunchDetails table");
      }
    } catch (Exception e) {
      logger.error("Error in Updating the AccessToken value into LaunchDetails: ", e);
    }
  }

  /**
   * Gets the access token for multi tenant launch. And updates the AccessToken in ClientDetails and
   * LaunchDetails tables.
   *
   * @param launchDetails the launch details
   * @return the access token for multi tenant launch
   */
  public JSONObject getAccessTokenForMultiTenantLaunch(LaunchDetails launchDetails) {
    JSONObject tokenResponse = null;
    try {
      ClientDetails clientDetails =
          ActionRepo.getInstance()
              .getClientDetailsService()
              .getClientDetailsByUrl(launchDetails.getEhrServerURL());

      Instant currentInstant = new Date().toInstant().plusSeconds(180);
      Date currentDate = Date.from(currentInstant);
      Date tokenExpiryTime = clientDetails.getTokenExpiryDateTime();
      int value = currentDate.compareTo(tokenExpiryTime);
      if (value > 0) {
        logger.info("AccessToken is Expired. Getting new AccessToken");
        tokenResponse = getAccessTokenUsingClientDetails(clientDetails);
      } else {
        logger.info("AccessToken is Valid. No need to get new AccessToken");
        tokenResponse = new JSONObject();
        tokenResponse.put(ACCESS_TOKEN, clientDetails.getAccessToken());
        tokenResponse.put(EXPIRES_IN, clientDetails.getTokenExpiry());
      }
    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the client: {}", launchDetails.getClientId(), e);
    }
    return tokenResponse;
  }

  /**
   * Gets the access token using client details.
   *
   * @param clientDetails the client details
   * @return the access token using client details
   */
  public JSONObject getAccessTokenUsingClientDetails(ClientDetails clientDetails) {
    JSONObject tokenResponse = null;
    logger.trace("Getting AccessToken for Client: {}", clientDetails.getClientId());
    try {
      RestTemplate resTemplate = new RestTemplate();
      if (clientDetails.getIsSystem() || clientDetails.getIsMultiTenantSystemLaunch()) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        headers.add(ACCEPT_HEADER, MediaType.APPLICATION_JSON_VALUE);
        String authValues = clientDetails.getClientId() + ":" + clientDetails.getClientSecret();
        String base64EncodedString =
            Base64.getEncoder().encodeToString(authValues.getBytes(StandardCharsets.UTF_8));
        headers.add("Authorization", "Basic " + base64EncodedString);
        MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
        map.add(GRANT_TYPE, CLIENT_CREDENTIALS);
        map.add(SCOPE, clientDetails.getScopes());
        if (Boolean.TRUE.equals(clientDetails.getRequireAud())) {
          logger.debug("Adding Aud Parameter while getting Access token");
          map.add("aud", clientDetails.getFhirServerBaseURL());
        }
        HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);

        ResponseEntity<?> response =
            resTemplate.exchange(
                clientDetails.getTokenURL(), HttpMethod.POST, entity, Response.class);
        tokenResponse = new JSONObject(response.getBody());
      } else if (Boolean.TRUE.equals(clientDetails.getIsUserAccountLaunch())) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBasicAuth(clientDetails.getClientId(), clientDetails.getClientSecret());

        String tokenUrl =
            clientDetails.getTokenURL()
                + "?"
                + GRANT_TYPE
                + "="
                + CLIENT_CREDENTIALS
                + "&"
                + SCOPE
                + "="
                + clientDetails.getScopes();

        HttpEntity<?> entity = new HttpEntity<>(headers);

        ResponseEntity<?> response =
            resTemplate.exchange(tokenUrl, HttpMethod.GET, entity, String.class);
        String resBody = (String) response.getBody();
        logger.info(resBody);
        if (resBody != null) {
          String responseBody =
              resBody
                  .replace(ACCESS_TOKEN_CAMEL_CASE, ACCESS_TOKEN)
                  .replace(EXPIRES_IN_CAMEL_CASE, EXPIRES_IN);
          tokenResponse = new JSONObject(responseBody);
        }
      }

      logger.trace(
          "Received AccessToken for Client: {}",
          StringEscapeUtils.escapeJava(clientDetails.getClientId()));
      ClientDetails existingClientDetails =
          ActionRepo.getInstance()
              .getClientDetailsService()
              .getClientDetailsById(clientDetails.getId());
      if (Boolean.TRUE.equals(existingClientDetails.getIsMultiTenantSystemLaunch())) {
        updateAccessTokenInClientDetails(existingClientDetails, tokenResponse);
      }
    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the client: {}", clientDetails.getClientId(), e);
    }
    return tokenResponse;
  }

  /**
   * Update access token in client details.
   *
   * @param existingClientDetails the existing client details
   * @param tokenResponse the token response
   */
  private void updateAccessTokenInClientDetails(
      ClientDetails existingClientDetails, JSONObject tokenResponse) {
    try {
      if (existingClientDetails != null) {
        logger.info("Updating the AccessToken value in ClientDetails table");
        existingClientDetails.setAccessToken(tokenResponse.getString(ACCESS_TOKEN));
        existingClientDetails.setTokenExpiry(tokenResponse.getInt(EXPIRES_IN));
        existingClientDetails.setLastUpdated(new Date());
        long expiresInSec = tokenResponse.getLong(EXPIRES_IN);
        Instant expireInstantTime = new Date().toInstant().plusSeconds(expiresInSec);
        existingClientDetails.setTokenExpiryDateTime(Date.from(expireInstantTime));
        ActionRepo.getInstance().getClientDetailsService().saveOrUpdate(existingClientDetails);
        logger.info("Successfully updated AccessToken value in ClientDetails table");
      }
    } catch (Exception e) {
      logger.error("Error in Updating the AccessToken value into ClientDetails: ", e);
    }
  }
}
