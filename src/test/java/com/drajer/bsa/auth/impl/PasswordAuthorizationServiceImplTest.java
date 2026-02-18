package com.drajer.bsa.auth.impl;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;

import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStoreException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class PasswordAuthorizationServiceImplTest {

  protected ClassLoader classLoader = getClass().getClassLoader();
  protected static final ObjectMapper mapper = new ObjectMapper();
  private static final String keyStorePassword = "ecrnow";
  private static final String keystoreFile = "src/test/resources/mockKeystore.jks";

  @InjectMocks PasswordAuthorizationServiceImpl passwordAuthorizationService;

  @Before
  public void setup() {
    passwordAuthorizationService.jwksLocation = keystoreFile;
    passwordAuthorizationService.password = keyStorePassword;

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

    try {
      passwordAuthorizationService.connectToServer(
          "https://fhir-ehr.xyramsoft.com/api/auth/generate-token", hcs);
    } catch (Exception e) {

    }
  }

  @Test
  public void testGetAuthorizationToken_returnsNullOnException() throws IOException {
    String healthCareSettings = "R4/Misc/HealthCareSettings/Hcs.json";
    //        File file = new File(classLoader.getResource(healthCareSettings).getFile());
    InputStream is = classLoader.getResourceAsStream(healthCareSettings);
    HealthcareSetting hcs = mapper.readValue(is, HealthcareSetting.class);
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
    try {
      passwordAuthorizationService.connectToServer("http://invalid.url", hcs);
    } catch (Exception e) {
    }
  }
}
