package com.drajer.bsa.auth.impl;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.ecrapp.security.AESEncryption;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.spi.json.JsonOrgJsonProvider;
import com.jayway.jsonpath.spi.json.JsonProvider;
import com.jayway.jsonpath.spi.mapper.MappingProvider;
import io.jsonwebtoken.SignatureAlgorithm;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.EnumSet;
import java.util.Set;
import org.apache.commons.lang3.tuple.Pair;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.core.env.Environment;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class BackendAuthorizationServiceImplTest {

  private static final String RSA_ALIAS = "ecrnow";
  private static final String CLIENT_ID = "test-client-id";
  private static final String AUD = "https://auth.example.com/token";
  private static final String KEY_STORE_PASSWORD = "ecrnow";
  private static final String KEYSTORE_FILE = "src/test/resources/mockKeystore.jks";

  @InjectMocks BackendAuthorizationServiceImpl backendAuthorizationService;
  @Mock private Environment environment;

  @Before
  public void setUp() {
    AESEncryption aesEncryption = new AESEncryption();
    ReflectionTestUtils.setField(aesEncryption, "secretKey", "2314");
    backendAuthorizationService.jwksLocation = KEYSTORE_FILE;
    backendAuthorizationService.password = KEY_STORE_PASSWORD;
  }

  @Test
  public void testGetAuthorizationToken_returnsNull_whenTokenUrlMissing() {
    HealthcareSetting fsd = new HealthcareSetting();
    fsd.setFhirServerBaseURL(baseUrl());
    fsd.setTokenUrl(null);
    fsd.setClientId(null);
    fsd.setScopes(null);

    JSONObject result = backendAuthorizationService.getAuthorizationToken(fsd);

    assertNull("Authorization token must be null on failure", result);
  }

  @Test
  public void testGetSignatureAlgorithm_RS256() {
    Pair<String, SignatureAlgorithm> result =
        ReflectionTestUtils.invokeMethod(
            backendAuthorizationService, "getSignatureAlgorithm", "RS256");
    assertNotNull("Result should not be null", result);
    assertEquals("RS256", result.getKey());
    assertEquals(SignatureAlgorithm.RS256, result.getValue());
  }

  @Test
  public void testGetSignatureAlgorithm_RS384() {
    Pair<String, SignatureAlgorithm> result =
        ReflectionTestUtils.invokeMethod(
            backendAuthorizationService, "getSignatureAlgorithm", "RS384");
    assertNotNull("Result should not be null", result);
    assertEquals("RS384", result.getKey());
    assertEquals(SignatureAlgorithm.RS384, result.getValue());
  }

  @Test
  public void testGetSignatureAlgorithm_ES384() {
    Pair<String, SignatureAlgorithm> result =
        ReflectionTestUtils.invokeMethod(
            backendAuthorizationService, "getSignatureAlgorithm", "ES384");
    assertNotNull("Result should not be null", result);
    assertEquals("ES384", result.getKey());
    assertEquals(SignatureAlgorithm.ES384, result.getValue());
  }

  private HealthcareSetting jwtFsd(String kid) {
    HealthcareSetting fsd = new HealthcareSetting();
    fsd.setBackendAuthKeyAlias(RSA_ALIAS);
    fsd.setBackendAuthAlg("RS256");
    fsd.setBackendAuthKid(kid);
    return fsd;
  }

  private String decodeJwtHeader(String jwt) {
    String[] parts = jwt.split("\\.");
    assertEquals("A compact JWS must have three segments", 3, parts.length);
    return new String(Base64.getUrlDecoder().decode(parts[0]), StandardCharsets.UTF_8);
  }

  @Test
  public void testGenerateJwt_kidPresent_setsKidHeader() throws Exception {
    String jwt = backendAuthorizationService.generateJwt(CLIENT_ID, AUD, jwtFsd("test-kid-123"));

    assertNotNull("JWT must be generated", jwt);
    String header = decodeJwtHeader(jwt);
    assertTrue("kid header present", header.contains("\"kid\""));
    assertTrue("kid carries configured value", header.contains("test-kid-123"));
    assertTrue("x5t thumbprint present", header.contains("\"x5t\""));
    assertTrue("alg resolved", header.contains("RS256"));
  }

  @Test
  public void testGenerateJwt_kidEmpty_omitsKidHeader() throws Exception {
    String jwt = backendAuthorizationService.generateJwt(CLIENT_ID, AUD, jwtFsd(""));

    assertNotNull("JWT must still be generated", jwt);
    assertFalse("kid header omitted", decodeJwtHeader(jwt).contains("\"kid\""));
  }

  @Test
  public void testGenerateJwt_kidNull_omitsKidHeader() throws Exception {
    String jwt = backendAuthorizationService.generateJwt(CLIENT_ID, AUD, jwtFsd(null));

    assertNotNull("JWT must still be generated", jwt);
    assertFalse("kid header omitted", decodeJwtHeader(jwt).contains("\"kid\""));
  }

  @Test
  public void testGenerateJwt_missingKeystore_returnsNull() throws Exception {
    String original = backendAuthorizationService.jwksLocation;
    try {
      backendAuthorizationService.jwksLocation = "src/test/resources/does-not-exist.jks";
      assertNull(
          "generateJwt must swallow the IOException and return null",
          backendAuthorizationService.generateJwt(CLIENT_ID, AUD, jwtFsd("kid")));
    } finally {
      backendAuthorizationService.jwksLocation = original;
    }
  }

  private static final String TOKEN_PATH = "/auth/token";
  private static final String WELL_KNOWN_PATH = "/.well-known/smart-configuration";
  private static final String METADATA_PATH = "/metadata";
  private static final String OAUTH_URIS =
      "http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris";
  private static final String TOKEN_RESPONSE =
      "{\"access_token\":\"abc123\",\"token_type\":\"Bearer\",\"expires_in\":3600}";

  private WireMockServer wireMockServer;
  private Configuration.Defaults previousDefaults;

  @Before
  public void startWireMock() {
    wireMockServer = new WireMockServer(WireMockConfiguration.options().dynamicPort());
    wireMockServer.start();
  }

  @After
  public void stopWireMock() {
    restoreJsonProvider();
    if (wireMockServer != null) {
      wireMockServer.stop();
    }
  }

  private String baseUrl() {
    return "http://localhost:" + wireMockServer.port();
  }

  private void stubTokenEndpoint() {
    wireMockServer.stubFor(
        post(urlEqualTo(TOKEN_PATH))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(TOKEN_RESPONSE)));
  }

  private HealthcareSetting restFsd(String scopes) {
    HealthcareSetting hcs = new HealthcareSetting();
    hcs.setFhirServerBaseURL(baseUrl());
    hcs.setTokenUrl(baseUrl() + TOKEN_PATH);
    hcs.setClientId(CLIENT_ID);
    hcs.setScopes(scopes);
    hcs.setBackendAuthKeyAlias(RSA_ALIAS);
    hcs.setBackendAuthAlg("RS256");
    hcs.setBackendAuthKid("test-kid");
    return hcs;
  }

  @Test
  public void testConnectToServer_withScopes_returnsParsedTokenResponse() throws Exception {
    stubTokenEndpoint();

    JSONObject result =
        backendAuthorizationService.connectToServer(baseUrl(), restFsd("system/*.read"));

    assertNotNull("A JSONObject must be built from the response body", result);
    assertEquals("access_token must be parsed", "abc123", result.getString("access_token"));
    assertEquals("expires_in must be parsed", 3600, result.getInt("expires_in"));
    assertTrue("token_type must be present", result.has("token_type"));
  }

  private void useJsonOrgProvider() {
    Configuration current = Configuration.defaultConfiguration();
    final JsonProvider prevJson = current.jsonProvider();
    final MappingProvider prevMapping = current.mappingProvider();
    final Set<Option> prevOptions = current.getOptions();

    previousDefaults =
        new Configuration.Defaults() {
          @Override
          public JsonProvider jsonProvider() {
            return prevJson;
          }

          @Override
          public MappingProvider mappingProvider() {
            return prevMapping;
          }

          @Override
          public Set<Option> options() {
            return prevOptions;
          }
        };

    Configuration.setDefaults(
        new Configuration.Defaults() {
          private final JsonProvider jsonProvider = new JsonOrgJsonProvider();

          @Override
          public JsonProvider jsonProvider() {
            return jsonProvider;
          }

          @Override
          public MappingProvider mappingProvider() {
            return prevMapping;
          }

          @Override
          public Set<Option> options() {
            return EnumSet.of(Option.ALWAYS_RETURN_LIST);
          }
        });
  }

  private void restoreJsonProvider() {
    if (previousDefaults != null) {
      Configuration.setDefaults(previousDefaults);
      previousDefaults = null;
    }
  }

  @Test
  public void testGetTokenEndpoint_wellKnown_returnsTokenEndpoint() {
    useJsonOrgProvider();
    wireMockServer.stubFor(
        get(urlEqualTo(WELL_KNOWN_PATH))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody("{\"token_endpoint\":\"https://auth.example.com/token\"}")));

    String tokenEndpoint = backendAuthorizationService.getTokenEndpoint(baseUrl());

    assertEquals(
        "token endpoint must be read from the smart-configuration document",
        "https://auth.example.com/token",
        tokenEndpoint);
    wireMockServer.verify(0, getRequestedFor(urlEqualTo(METADATA_PATH)));
  }

  @Test
  public void testGetTokenEndpoint_metadataFallback_returnsTokenUri() {
    useJsonOrgProvider();
    wireMockServer.stubFor(
        get(urlEqualTo(WELL_KNOWN_PATH)).willReturn(aResponse().withStatus(404)));

    String capabilityStatement =
        "{\"rest\":[{\"mode\":\"server\",\"security\":{\"extension\":[{\"url\":\""
            + OAUTH_URIS
            + "\",\"extension\":[{\"url\":\"token\","
            + "\"valueUri\":\"https://auth.example.com/oauth/token\"}]}]}}]}";

    wireMockServer.stubFor(
        get(urlEqualTo(METADATA_PATH))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(capabilityStatement)));

    String tokenEndpoint = backendAuthorizationService.getTokenEndpoint(baseUrl());

    assertEquals(
        "token endpoint must be read from the capability statement",
        "https://auth.example.com/oauth/token",
        tokenEndpoint);
  }
}
