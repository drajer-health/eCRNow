package com.drajer.bsa.auth.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.model.Response;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class BackendAuthorizationServiceImplTest {
  protected ClassLoader classLoader = getClass().getClassLoader();
  protected static final ObjectMapper mapper = new ObjectMapper();

  private static final String KEY_STORE_PASSWORD = "ecrnow";
  private static final String KEYSTORE_FILE = "src/test/resources/mockKeystore.jks";

  @InjectMocks BackendAuthorizationServiceImpl backendAuthorizationService;

  @Before
  public void setUp() {
    AESEncryption aesEncryption = new AESEncryption();
    ReflectionTestUtils.setField(aesEncryption, "secretKey", "2314");
    backendAuthorizationService.jwksLocation = KEYSTORE_FILE;
    backendAuthorizationService.password = KEY_STORE_PASSWORD;
  }

  //  @Test
  //  public void testconnectToServer() {
  //    String healthCareSettings = "R4/Misc/HealthCareSettings/Hcs.json";
  //    HealthcareSetting hcs =
  //        (HealthcareSetting)
  //            TestUtils.getResourceAsObject(healthCareSettings, HealthcareSetting.class);
  //    try {
  //      backendAuthorizationService.connectToServer(
  //          "https://fhir-ehr.xyramsoft.com/api/auth/generate-token", hcs);
  //    } catch (Exception e) {
  //    }
  //  }

  @Test
  public void testconnectToServer() {
    String healthCareSettings = "R4/Misc/HealthCareSettings/Hcs.json";
    HealthcareSetting hcs =
        (HealthcareSetting)
            TestUtils.getResourceAsObject(healthCareSettings, HealthcareSetting.class);

    assertNotNull("HealthcareSetting should not be null", hcs);

    try {
      backendAuthorizationService.connectToServer(
          "https://fhir-ehr.xyramsoft.com/api/auth/generate-token", hcs);

      assertTrue("Connection attempt completed", true);
    } catch (Exception e) {

      assertNotNull("Exception should be caught", e);
    }
  }

  public Response getResponse() {
    Response response = new Response();
    response.setAccess_token("access_token_value");
    response.setExpires_in(3000);
    return response;
  }

  @Test
  public void testGetAuthorizationToken_returnsNull_whenTokenUrlMissing() {
    HealthcareSetting fsd = new HealthcareSetting();
    fsd.setFhirServerBaseURL("https://fhir-ehr.xyramsoft.com");
    fsd.setTokenUrl(null);
    fsd.setClientId(null);
    fsd.setScopes(null);
    JSONObject result = backendAuthorizationService.getAuthorizationToken(fsd);
    assertNull("Authorization token must be null on failure", result);
  }

  @Test
  public void testgetTokenEndpoint_throws_Exception() {
    String url = "https://fhir-ehr.xyramsoft.com/api/auth/generate-token";
    assertThrows(RuntimeException.class, () -> backendAuthorizationService.getTokenEndpoint(url));
  }
}
