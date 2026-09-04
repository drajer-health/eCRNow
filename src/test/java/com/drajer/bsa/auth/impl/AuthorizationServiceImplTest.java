package com.drajer.bsa.auth.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

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
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@RunWith(PowerMockRunner.class)
@PrepareForTest({AuthorizationServiceImpl.class})
@PowerMockIgnore({"javax.crypto.*"})
public class AuthorizationServiceImplTest {

  protected ClassLoader classLoader = getClass().getClassLoader();
  protected static final ObjectMapper mapper = new ObjectMapper();

  @InjectMocks AuthorizationServiceImpl authorizationService;
  @Mock private Environment environment;

  @Before
  public void setUp() {
    AESEncryption aesEncryption = new AESEncryption(environment);
    ReflectionTestUtils.setField(aesEncryption, "secretKey", "2314");
  }

  @Test
  public void testgetAuthorizationToken() throws Exception {
    String healthCareSettings = "R4/Misc/HealthCareSettings/Hcs.json";
    HealthcareSetting hcs =
        (HealthcareSetting)
            TestUtils.getResourceAsObject(healthCareSettings, HealthcareSetting.class);

    RestTemplate mockRestemplate = Mockito.mock(RestTemplate.class);

    PowerMockito.whenNew(RestTemplate.class).withNoArguments().thenReturn(mockRestemplate);
    when(mockRestemplate.exchange(
            anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(Response.class)))
        .thenReturn(new ResponseEntity<>(getResponse(), HttpStatus.OK));
    JSONObject result = authorizationService.getAuthorizationToken(hcs);
    assertNotNull(result);
    assertEquals("access_token_value", result.getString("access_token"));
    assertEquals(3000, result.getInt("expires_in"));
  }

  public Response getResponse() {
    Response response = new Response();
    response.setAccess_token("access_token_value");
    response.setExpires_in(3000);
    return response;
  }

  @Test
  public void testgetAuthorizationTokenWithException() {
    String healthCareSettings = "R4/Misc/HealthCareSettings/HealthcareSettingsNoAuth.json";
    HealthcareSetting hcs =
        (HealthcareSetting)
            TestUtils.getResourceAsObject(healthCareSettings, HealthcareSetting.class);
    JSONObject result = authorizationService.getAuthorizationToken(hcs);
    assertNull(result);
  }

  @Test
  public void testGetAuthorizationToken_wrongAuthType_returnsNull() {
    HealthcareSetting hcs = new HealthcareSetting();
    hcs.setFhirServerBaseURL("https://fhir.example.com");
    hcs.setClientId("client-789");
    hcs.setClientSecret("secret-789");
    hcs.setTokenUrl("https://auth.example.com/token");
    hcs.setAuthType("INVALID_AUTH_TYPE");

    JSONObject result = authorizationService.getAuthorizationToken(hcs);
    assertNull("Token with wrong authType should be null", result);
  }

  @Test
  public void testGetAuthorizationToken_nullAuthType_returnsNull() {
    HealthcareSetting hcs = new HealthcareSetting();
    hcs.setFhirServerBaseURL("https://fhir.example.com");
    hcs.setClientId("client-000");
    hcs.setClientSecret("secret-000");
    hcs.setTokenUrl("https://auth.example.com/token");
    hcs.setAuthType(null);

    JSONObject result = authorizationService.getAuthorizationToken(hcs);
    assertNull("Token with null authType should be null", result);
  }
}
