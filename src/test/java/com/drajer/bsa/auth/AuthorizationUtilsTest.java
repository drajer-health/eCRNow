package com.drajer.bsa.auth;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import com.drajer.bsa.model.FhirServerDetails;
import com.drajer.bsa.model.HealthcareSetting;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class AuthorizationUtilsTest {

  @Mock AuthorizationService backendAuthorizationService;

  @Mock AuthorizationService ehrAuthorizationService;

  @Mock AuthorizationService passwordAuthorizationService;

  @InjectMocks AuthorizationUtils authorizationUtils;

  @Test
  public void testGetToken_UserNamePwd() throws JSONException {
    FhirServerDetails fhirServerDetails = new HealthcareSetting();
    fhirServerDetails.setAuthType("UserNamePwd");
    JSONObject expectedToken = new JSONObject();
    expectedToken.put("access_token", "test-token");
    expectedToken.put("expires_in", 3600);
    when(passwordAuthorizationService.getAuthorizationToken(fhirServerDetails))
        .thenReturn(expectedToken);
    JSONObject result = authorizationUtils.getToken(fhirServerDetails);
    assertEquals("test-token", result.getString("access_token"));

    verify(passwordAuthorizationService).getAuthorizationToken(fhirServerDetails);
  }

  @Test
  public void testGetToken_SofProvider() throws JSONException {
    FhirServerDetails fhirServerDetails = new HealthcareSetting();
    fhirServerDetails.setAuthType("SofProvider");

    JSONObject expectedToken = new JSONObject();
    expectedToken.put("access_token", "backend-token");
    expectedToken.put("expires_in", 3600);

    when(backendAuthorizationService.getAuthorizationToken(fhirServerDetails))
        .thenReturn(expectedToken);

    JSONObject result = authorizationUtils.getToken(fhirServerDetails);
    assertEquals("backend-token", result.getString("access_token"));

    verify(backendAuthorizationService).getAuthorizationToken(fhirServerDetails);
  }

  @Test
  public void testGetToken_MultiTenantSystemLaunch() throws JSONException {
    FhirServerDetails fhirServerDetails = new HealthcareSetting();
    fhirServerDetails.setAuthType("MultiTenantSystemLaunch");

    JSONObject expectedToken = new JSONObject();
    expectedToken.put("access_token", "MultiTenantSystemLaunch-token");
    expectedToken.put("expires_in", 3600);

    when(ehrAuthorizationService.getAuthorizationToken(fhirServerDetails))
        .thenReturn(expectedToken);

    JSONObject result = authorizationUtils.getToken(fhirServerDetails);
    assertEquals("MultiTenantSystemLaunch-token", result.getString("access_token"));

    verify(ehrAuthorizationService).getAuthorizationToken(fhirServerDetails);
  }

  @Test
  public void testGetToken_DefaultOrUnknownAuthType() throws JSONException {
    FhirServerDetails fhirServerDetails = new HealthcareSetting();
    fhirServerDetails.setAuthType("INVALID_AUTH_TYPE");

    JSONObject result = authorizationUtils.getToken(fhirServerDetails);

    assertNotNull(result);
    assertEquals("", result.getString("access_token"));
    assertEquals(60 * 60 * 24, result.getInt("expires_in"));

    verifyZeroInteractions(
        passwordAuthorizationService, backendAuthorizationService, ehrAuthorizationService);
  }
}
