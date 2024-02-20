package com.drajer.bsa.auth.impl;

import com.drajer.bsa.auth.AuthorizationService;
import com.drajer.bsa.model.FhirServerDetails;
import com.drajer.sof.model.Response;
import com.jayway.jsonpath.JsonPath;
import java.security.KeyStoreException;
import java.util.Objects;
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

/**
 *
 *
 * <h1>PasswordAuthorizationServiceImpl</h1>
 *
 * This class defines the implementation methods to get authorized with the EHRs.
 *
 * @author kghoreshi
 */
@Service("passwordauth")
@Transactional
public class PasswordAuthorizationServiceImpl implements AuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(PasswordAuthorizationServiceImpl.class);
  private static final String OAUTH_URIS =
      "http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris";
  private static final String WELL_KNOWN = ".well-known/smart-configuration";

  @Value("${jwks.keystore.location}")
  String jwksLocation;

  @Value("${jwks.keystore.password}")
  String password;

  /** @param fsd The processing context which contains information such as patient, encounter */
  @Override
  public JSONObject getAuthorizationToken(FhirServerDetails fsd) {
    String baseUrl = fsd.getFhirServerBaseURL();
    try {
      return connectToServer(baseUrl, fsd);
    } catch (Exception e) {
      logger.error(
          "Error in Getting the AccessToken for the client: {}", fsd.getFhirServerBaseURL(), e);
      return null;
    }
  }

  /**
   * @param url base url of ehr
   * @param fsd knowledge artifact data
   * @return the token response from the auth server
   * @throws KeyStoreException in case of invalid public/private keys
   */
  public JSONObject connectToServer(String url, FhirServerDetails fsd) throws KeyStoreException {
    RestTemplate resTemplate = new RestTemplate();
    String tokenEndpoint;

    tokenEndpoint = fsd.getTokenUrl();
    if (tokenEndpoint == null || tokenEndpoint.isEmpty()) {
      tokenEndpoint = getTokenEndpoint(url);
    }
    String clientId = fsd.getClientId();
    String clientSecret = fsd.getClientSecret();
    if(clientSecret==null){
      clientSecret="";
    }

    HttpHeaders headers = new HttpHeaders();
    headers.setBasicAuth(clientId, clientSecret);
    headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

    MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
    map.add("scope", fsd.getScopes());
    map.add("grant_type", "password");
    map.add("username", fsd.getUsername());
    map.add("password", fsd.getPassword());
    HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(map, headers);
    ResponseEntity<?> response = resTemplate.postForEntity(tokenEndpoint, request, Response.class);
    return new JSONObject(Objects.requireNonNull(response.getBody()));
  }

  /**
   * @param url base ehr url
   * @return token endpoint from the server's capability statement
   */
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
        throw e1;
      }
    }
  }
}
