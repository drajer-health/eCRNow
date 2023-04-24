package com.drajer.bsa.auth.impl;

import com.drajer.bsa.auth.AuthorizationService;
import com.drajer.bsa.model.FhirServerDetails;
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
import java.util.Date;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.nio.file.*;
import java.security.*;
import java.security.spec.*;
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
 * <h1>BackendAuthorizationServiceImpl</h1>
 *
 * This class defines the implementation methods to get authorized with the EHRs.
 *
 * @author kghoreshi
 */
@Service("backendauth")
@Transactional
public class BackendAuthorizationServiceImpl implements AuthorizationService {

  private final Logger logger = LoggerFactory.getLogger(BackendAuthorizationServiceImpl.class);
  private static final String OAUTH_URIS =
      "http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris";
  private static final String WELL_KNOWN = ".well-known/smart-configuration";

  @Value("${jwks.keystore.location}")
  String jwksLocation;

  @Value("${jwks.keystore.password}")
  String password;

  @Value("${jwks.keystore.alias}")
  String alias;

  @Value("${backendauth.privatekey.thumbprint}")
  String thumbprint;

  @Value("${backendauth.privatekey.path}")
  String path;

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
    String scopes = fsd.getScopes();
    String jwt = generateJwt(clientId, tokenEndpoint);

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

    MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
    // map.add("scope", scopes);
    map.add("grant_type", "client_credentials");

    map.add("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer");
    map.add("client_assertion", jwt);
    HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(map, headers);
    ResponseEntity<?> response = resTemplate.postForEntity(tokenEndpoint, request, Response.class);
    return new JSONObject(Objects.requireNonNull(response.getBody()));
  }

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
   * @param url base ehr url
   * @return token endpoint from the server's capability statement
   */
  public String getTokenEndpoint(String url) {
    RestTemplate resTemplate = new RestTemplate();
    try {
      ResponseEntity<String> response =
          resTemplate.getForEntity(String.format("%s/%s", url, WELL_KNOWN), String.class);
      JSONArray result = JsonPath.read(response.getBody(), "$.token_endpoint");
      return result.get(0).toString();
    } catch (Exception e1) {
      try {
        ResponseEntity<String> response =
            resTemplate.getForEntity(String.format("%s/metadata", url), String.class);
        // jsonpath allows filtering through lists with '?', where '@' represents the current
        // element
        JSONArray result =
            JsonPath.read(
                response.getBody(),
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

  /**
   * @param clientId client id of the app
   * @param aud the token endpoint of the ehr
   * @return a signed JWT
   * @throws KeyStoreException for problems with public/private keys
   */
  public String generateJwt(String clientId, String aud) throws KeyStoreException {

    try {
      PrivateKey key = getPrivateKey(path);

      return Jwts.builder()
          .setIssuer(clientId)
          .setSubject(clientId)
          .setAudience(aud)
          .setExpiration(
              new Date(
                  System.currentTimeMillis() + TimeUnit.MINUTES.toMillis(5))) // a java.util.Date
          .setId(UUID.randomUUID().toString())
          .setHeaderParam("x5t", thumbprint)
          .signWith(key)
          .compact();
    } catch (IOException
        | NoSuchAlgorithmException
        | InvalidKeySpecException e) {
      logger.error("Exception Occurred: ", e);
    }
    return null;
  }

  /**
   * @param path String representation of the path to the private key
   * @return a private key file's contents
   * @throws IOException
   * @throws InvalidKeySpecException
   * @throws NoSuchAlgorithmException
   */
  public static PrivateKey getPrivateKey(String path)
      throws IOException, InvalidKeySpecException, NoSuchAlgorithmException {
      byte[] keyBytes = Files.readAllBytes(Paths.get(path));

      PKCS8EncodedKeySpec spec = new PKCS8EncodedKeySpec(keyBytes);
      KeyFactory kf = KeyFactory.getInstance("RSA");
      return kf.generatePrivate(spec);
    }
}
