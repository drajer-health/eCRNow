package com.drajer.bsa.ehr.service.impl;

import com.drajer.bsa.ehr.service.EhrAuthorizationService;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.sof.model.Response;
import com.jayway.jsonpath.JsonPath;
import io.jsonwebtoken.Jwts;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.Key;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.time.Instant;
import java.util.Base64;
import java.util.Date;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import javax.transaction.Transactional;
import net.minidev.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

@Service("backendauth")
@Transactional
public class BackendAuthorizationServiceImpl implements EhrAuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(BackendAuthorizationServiceImpl.class);
  private final String OAUTH_URIS =
      "http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris";
  private final String WELL_KNOWN = ".well-known/smart-configuration";

  @Value("${jwks.keystore.location}")
  String jwksLocation;

  @Value("${jwks.keystore.password}")
  String password;

  @Value("${jwks.keystore.alias}")
  String alias;

  @Override
  public void getAuthorizationToken(KarProcessingData kd) {
    String baseUrl = kd.getHealthcareSetting().getFhirServerBaseURL();
    try {
      JSONObject tokenResponse = connectToServer(baseUrl, kd);
      kd.getNotificationContext().setEhrAccessToken(tokenResponse.getString("access_token"));
      kd.getNotificationContext()
          .setEhrAccessTokenExpiryDuration(tokenResponse.getInt("expires_in"));

      Integer expiresInSec = (Integer) tokenResponse.get("expires_in");
      Instant expireInstantTime = new Date().toInstant().plusSeconds(Long.valueOf(expiresInSec));
      kd.getNotificationContext().setEhrAccessTokenExpirationTime(Date.from(expireInstantTime));
    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the client: {}",
          kd.getHealthcareSetting().getFhirServerBaseURL(),
          e);
    }
  }

  public JSONObject connectToServer(String url, KarProcessingData kd) throws KeyStoreException {
    RestTemplate resTemplate = new RestTemplate();
    String tokenEndpoint;

    tokenEndpoint = kd.getHealthcareSetting().getTokenUrl();
    if (tokenEndpoint == null || tokenEndpoint.isEmpty() || tokenEndpoint.isBlank()) {
      tokenEndpoint = getTokenEndpoint(url);
    }
    String clientId = kd.getHealthcareSetting().getClientId();
    String scopes = kd.getHealthcareSetting().getScopes();
    String jwt = generateJwt(clientId, tokenEndpoint);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

    MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
    map.add("scope", scopes);
    map.add("grant_type", "client_credentials");

    map.add("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer");
    map.add("client_assertion", jwt);
    HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(map, headers);
    ResponseEntity<?> response = resTemplate.postForEntity(tokenEndpoint, request, Response.class);
    return new JSONObject(Objects.requireNonNull(response.getBody()));
  }

  public String getTokenEndpoint(String url) {
    RestTemplate resTemplate = new RestTemplate();
    try {
      ResponseEntity<String> res =
          resTemplate.getForEntity(String.format("%s/%s", url, WELL_KNOWN), String.class);
      JSONArray result = JsonPath.read(res.getBody(), "$.token_endpoint");
      return result.get(0).toString();
    } catch (Exception e1) {
      try {
        ResponseEntity<String> res =
            resTemplate.getForEntity(String.format("%s/metadata", url), String.class);
        // jsonpath allows filtering through lists with '?', where '@' represents the current
        // element
        JSONArray result =
            JsonPath.read(
                res.getBody(),
                "$.rest[?(@.mode == 'server')].security"
                    + ".extension[?(@.url == '"
                    + OAUTH_URIS
                    + "')]"
                    + ".extension[?(@.url == 'token')].valueUri");
        return result.get(0).toString();
      } catch (Exception e2) {
        logger.error("Error in Getting the TokenEndpoint for the client: {}", url, e2);
        throw e2;
      }
    }
  }

  public String getRegistrationEndpoint(String url) {
    RestTemplate resTemplate = new RestTemplate();
    try {
      ResponseEntity<String> res =
          resTemplate.getForEntity(String.format("%s/%s", url, WELL_KNOWN), String.class);
      JSONArray result = JsonPath.read(res.getBody(), "$.registration_endpoint");
      return result.get(0).toString();
    } catch (Exception ex1) {
      try {
        ResponseEntity<String> res =
            resTemplate.getForEntity(String.format("%s/metadata", url), String.class);
        // jsonpath allows filtering through lists with '?', where '@' represents the current
        // element
        JSONArray result =
            JsonPath.read(
                res.getBody(),
                "$.rest[?(@.mode == 'server')].security"
                    + ".extension[?(@.url =='"
                    + OAUTH_URIS
                    + "')]"
                    + ".extension[?(@.url == 'register')].valueUri");
        return result.get(0).toString();
      } catch (Exception ex2) {
        logger.error("Error trying to access ehr metadata endpoint");
        throw ex2;
      }
    }
  }

  public String generateJwt(String clientId, String aud) throws KeyStoreException {

    KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
    char[] passwordChar = password.toCharArray();
    try {
      InputStream store = new FileInputStream(jwksLocation);
      ks.load(store, passwordChar);
      store.close();
      Key key = ks.getKey(alias, passwordChar);

      return Jwts.builder()
          .setIssuer(clientId)
          .setSubject(clientId)
          .setAudience(aud)
          .setExpiration(
              new Date(
                  System.currentTimeMillis()
                      + TimeUnit.MINUTES.toMillis(5))) // a java.util.Date
          .setId(UUID.randomUUID().toString())
          .signWith(key)
          .compact();
    } catch (IOException
        | NoSuchAlgorithmException
        | CertificateException
        | UnrecoverableKeyException e) {
      e.printStackTrace();
    }
    return null;
  }
}
