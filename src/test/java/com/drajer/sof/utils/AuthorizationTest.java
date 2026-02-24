package com.drajer.sof.utils;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.Response;
import com.drajer.sof.service.ClientDetailsService;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.web.client.RestTemplate;

@RunWith(PowerMockRunner.class)
@PrepareForTest({RestTemplate.class})
public class AuthorizationTest {

  @InjectMocks private Authorization authorization;

  @Mock private ClientDetailsService clientDetailsService;

  @Mock private RestTemplate restTemplate;

  @Before
  public void setUp() throws Exception {
    PowerMockito.whenNew(RestTemplate.class).withNoArguments().thenReturn(restTemplate);
  }

  @Test
  public void testGetMetadata_exception() {
    String url = "http://error/metadata";

    when(restTemplate.exchange(
            eq(url), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
        .thenThrow(new RuntimeException("Connection error"));

    JSONObject result = authorization.getMetadata(url);

    assertNull(result);
  }

  @Test
  public void testCreateAuthUrl() {
    LaunchDetails launch = mock(LaunchDetails.class);
    ClientDetails client = mock(ClientDetails.class);

    when(launch.getAuthUrl()).thenReturn("http://auth");
    when(launch.getRedirectURI()).thenReturn("http://redirect");
    when(launch.getLaunchId()).thenReturn("launch123");
    when(launch.getEhrServerURL()).thenReturn("http://ehr");
    when(client.getClientId()).thenReturn("client1");
    when(client.getScopes()).thenReturn("openid fhirUser");

    String result = authorization.createAuthUrl(launch, client, 123);

    assertNotNull(result);
    assertTrue(result.startsWith("http://auth?"));
    assertTrue(result.contains("response_type=code"));
    assertTrue(result.contains("client_id=client1"));
    assertTrue(result.contains("redirect_uri=http://redirect"));
    assertTrue(result.contains("launch=launch123"));
    assertTrue(result.contains("state=123"));
    assertTrue(result.contains("scope=openid fhirUser"));
    assertTrue(result.contains("aud=http://ehr"));
  }

  @Test
  public void testGetAccessToken_exception() {
    LaunchDetails launch = mock(LaunchDetails.class);

    when(launch.getClientId()).thenReturn("client1");
    when(launch.getTokenUrl()).thenReturn("http://token");

    when(restTemplate.exchange(
            eq("http://token"), eq(HttpMethod.POST), any(HttpEntity.class), eq(Response.class)))
        .thenThrow(new RuntimeException("Token error"));

    JSONObject result = authorization.getAccessToken(launch);

    assertNull(result);
  }
}
