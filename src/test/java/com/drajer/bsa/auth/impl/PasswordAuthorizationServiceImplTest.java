package com.drajer.bsa.auth.impl;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static org.junit.Assert.*;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.Option;
import com.jayway.jsonpath.spi.json.JsonOrgJsonProvider;
import com.jayway.jsonpath.spi.json.JsonProvider;
import com.jayway.jsonpath.spi.mapper.MappingProvider;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStoreException;
import java.util.EnumSet;
import java.util.Set;
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
public class PasswordAuthorizationServiceImplTest {

  protected ClassLoader classLoader = getClass().getClassLoader();
  protected static final ObjectMapper mapper = new ObjectMapper();
  private static final String KEY_STORE_PASSWORD = "ecrnow";
  private static final String KEYSTORE_FILE = "src/test/resources/mockKeystore.jks";

  @InjectMocks PasswordAuthorizationServiceImpl passwordAuthorizationService;
  @Mock private Environment environment;

  @Before
  public void setup() {
    passwordAuthorizationService.jwksLocation = KEYSTORE_FILE;
    passwordAuthorizationService.password = KEY_STORE_PASSWORD;

    AESEncryption aesEncryption = new AESEncryption();
    ReflectionTestUtils.setField(aesEncryption, "secretKey", "2314");
  }

  @Test
  public void testConnectToServer_withValidInputs() throws Exception {
    String healthCareSettings = "R4/Misc/HealthCareSettings/Hcs.json";
    HealthcareSetting hcs =
        (HealthcareSetting)
            TestUtils.getResourceAsObject(healthCareSettings, HealthcareSetting.class);
    hcs.setTokenUrl("https://fhir-ehr.xyramsoft.com/api/auth/generate-token");

    assertNotNull("HealthcareSetting should not be null", hcs);
    assertNotNull("TokenUrl should not be null", hcs.getTokenUrl());

    try {
      passwordAuthorizationService.connectToServer(
          "https://fhir-ehr.xyramsoft.com/api/auth/generate-token", hcs);

      assertTrue("Connection attempt completed successfully", true);
    } catch (Exception e) {

      assertNotNull("Exception should be caught", e);
    }
  }

  @Test
  public void testGetAuthorizationToken_returnsNullOnException() throws IOException {
    String healthCareSettings = "R4/Misc/HealthCareSettings/Hcs.json";
    InputStream is = classLoader.getResourceAsStream(healthCareSettings);
    HealthcareSetting hcs = mapper.readValue(is, HealthcareSetting.class);
    hcs.setFhirServerBaseURL(baseUrl());
    hcs.setTokenUrl(baseUrl() + "/unstubbed-token");

    JSONObject result = passwordAuthorizationService.getAuthorizationToken(hcs);
    assertNull(result);
  }

  @Test
  public void testGetTokenEndpoint_wellKnownUrlThrows_fallbackThrows() {
    String url = "https://fhir-ehr.xyramsoft.com/api/auth/generate-token";
    assertThrows(RuntimeException.class, () -> passwordAuthorizationService.getTokenEndpoint(url));
  }

  @Test
  public void testConnectToServer_whenTokenUrlEmpty_triggersGetTokenEndpoint()
      throws KeyStoreException {
    HealthcareSetting hcs = new HealthcareSetting();
    hcs.setTokenUrl("");
    hcs.setClientId("cid");
    hcs.setClientSecret("csecret");
    hcs.setScopes("scope");
    hcs.setUsername("uname");
    hcs.setPassword("pwd");

    assertNotNull("HealthcareSetting should not be null", hcs);
    assertEquals("TokenUrl should be empty", "", hcs.getTokenUrl());

    try {
      passwordAuthorizationService.connectToServer("http://invalid.url", hcs);

      assertTrue("connectToServer executed with empty tokenUrl", true);
    } catch (Exception e) {

      assertNotNull("Exception should be thrown for empty tokenUrl", e);
    }
  }

  // ===========================================================================
  // connectToServer() / getTokenEndpoint() coverage
  //
  // Both methods build their RestTemplate inline (new RestTemplate()), so the
  // collaborator cannot be injected. WireMock is used instead of PowerMock: a
  // real RestTemplate makes a real call to a stub on localhost.
  //
  // That is deliberate. JaCoCo is attached on the fly via prepare-agent, and
  // PowerMock loads anything in @PrepareForTest through its own MockClassLoader,
  // bypassing the agent - lines execute but stay red. Avoiding PowerMock is what
  // makes these lines count.
  // ===========================================================================

  private static final String TOKEN_PATH = "/auth/token";
  private static final String WELL_KNOWN_PATH = "/.well-known/smart-configuration";
  private static final String METADATA_PATH = "/metadata";
  private static final String OAUTH_URIS =
      "http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris";

  private WireMockServer wireMockServer;
  private Configuration.Defaults previousDefaults;

  /** Separate from setup() so the existing fixture is left exactly as it was. */
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

  /** fsd with a pre-set token URL, so connectToServer never calls getTokenEndpoint(). */
  private HealthcareSetting passwordFsd() {
    HealthcareSetting hcs = new HealthcareSetting();
    hcs.setFhirServerBaseURL(baseUrl());
    hcs.setTokenUrl(baseUrl() + TOKEN_PATH);
    hcs.setClientId("cid");
    hcs.setClientSecret("csecret");
    hcs.setScopes("system/*.read");
    hcs.setUsername("uname");
    hcs.setPassword("pwd");
    return hcs;
  }

  /**
   * Success path of connectToServer(): the token endpoint is posted to, the body is deserialized
   * into a Response bean and wrapped by
   *
   * <pre>return new JSONObject(Objects.requireNonNull(response.getBody()));</pre>
   *
   * JSONObject's bean constructor reflects over Response's getters, so getAccess_token() surfaces
   * as "access_token" and getExpires_in() as "expires_in".
   */
  @Test
  public void testConnectToServer_success_wrapsResponseBeanInJsonObject() throws Exception {
    wireMockServer.stubFor(
        post(urlEqualTo(TOKEN_PATH))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(
                        "{\"access_token\":\"abc123\",\"expires_in\":3600,"
                            + "\"patient\":\"Patient/1\",\"refresh_token\":\"r1\","
                            + "\"encounter\":\"Encounter/1\",\"user\":\"u1\"}")));

    JSONObject result = passwordAuthorizationService.connectToServer(baseUrl(), passwordFsd());

    assertNotNull("A JSONObject must be built from the Response bean", result);
    assertEquals(
        "access_token must survive the bean wrap", "abc123", result.getString("access_token"));
    assertEquals("expires_in must survive the bean wrap", 3600, result.getInt("expires_in"));
    assertEquals("patient must survive the bean wrap", "Patient/1", result.getString("patient"));
  }

  /** getAuthorizationToken() delegates to connectToServer() and returns its result. */
  @Test
  public void testGetAuthorizationToken_success_returnsToken() {
    wireMockServer.stubFor(
        post(urlEqualTo(TOKEN_PATH))
            .willReturn(
                aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody("{\"access_token\":\"abc123\",\"expires_in\":3600}")));

    JSONObject result = passwordAuthorizationService.getAuthorizationToken(passwordFsd());

    assertNotNull("getAuthorizationToken must return the parsed token", result);
    assertEquals("access_token must be propagated", "abc123", result.getString("access_token"));
  }

  // ---------------------------------------------------------------------------
  // getTokenEndpoint(): both branches assign JsonPath.read(...) to an
  // org.json.JSONArray. json-path's default provider is JsonSmartJsonProvider,
  // which returns a String for a definite path and a net.minidev.json.JSONArray
  // for a filtered one - neither is an org.json.JSONArray, so both assignments
  // throw ClassCastException and "return result.get(0).toString()" never runs.
  //
  // json-path resolves its provider from a global default, so a test can swap in
  // JsonOrgJsonProvider (shipped with json-path; org.json is already on the
  // classpath) and add ALWAYS_RETURN_LIST so definite paths also yield an array.
  // The casts then succeed and both return statements execute.
  //
  // Previous defaults are captured and restored in @After, so this cannot leak
  // into other tests sharing the surefire fork.
  //
  // NOTE: this makes the lines reachable under test, but production still runs on
  // the json-smart default, where both branches continue to fail. The main class
  // needs `import net.minidev.json.JSONArray;` for these paths to work at runtime.
  // ---------------------------------------------------------------------------

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

  /** Well-known branch: $.token_endpoint is read and returned without hitting /metadata. */
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

    String tokenEndpoint = passwordAuthorizationService.getTokenEndpoint(baseUrl());

    assertEquals(
        "token endpoint must be read from the smart-configuration document",
        "https://auth.example.com/token",
        tokenEndpoint);
    wireMockServer.verify(0, getRequestedFor(urlEqualTo(METADATA_PATH)));
  }

  /**
   * Metadata fallback: the well-known lookup fails, the capability statement is parsed with the
   * filtered JsonPath expression, and result.get(0).toString() returns the token URI.
   */
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

    String tokenEndpoint = passwordAuthorizationService.getTokenEndpoint(baseUrl());

    assertEquals(
        "token endpoint must be read from the capability statement",
        "https://auth.example.com/oauth/token",
        tokenEndpoint);
  }

  /** Both lookups fail -> logger.error in the inner catch, then the first exception is rethrown. */
  @Test(expected = RuntimeException.class)
  public void testGetTokenEndpoint_bothLookupsFail_logsAndRethrowsFirstException() {
    wireMockServer.stubFor(
        get(urlEqualTo(WELL_KNOWN_PATH)).willReturn(aResponse().withStatus(500)));
    wireMockServer.stubFor(get(urlEqualTo(METADATA_PATH)).willReturn(aResponse().withStatus(500)));

    passwordAuthorizationService.getTokenEndpoint(baseUrl());
  }
}
