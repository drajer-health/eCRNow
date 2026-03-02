package com.drajer.sof.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.rest.client.api.IHttpRequest;
import com.drajer.sof.model.LaunchDetails;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class EcrOAuthBearerTokenInterceptorTest {

  @Mock private LaunchDetails launchDetails;

  @Mock private IHttpRequest httpRequest;

  private EcrOAuthBearerTokenInterceptor interceptor;

  private Map<String, String> headers;

  @Before
  public void setUp() {
    interceptor = new EcrOAuthBearerTokenInterceptor(launchDetails);

    headers = new HashMap<>();

    doAnswer(
            invocation -> {
              headers.put(invocation.getArgument(0), invocation.getArgument(1));
              return null;
            })
        .when(httpRequest)
        .addHeader(anyString(), anyString());
  }

  @Test
  public void testInterceptRequest_setsAuthorizationHeader() {

    String accessToken = "test-access-token-123";
    when(launchDetails.getAccessToken()).thenReturn(accessToken);

    interceptor.interceptRequest(httpRequest);

    assertNotNull(headers);
    assertEquals(1, headers.size());
    assertEquals("Bearer " + accessToken, headers.get("Authorization"));
  }
}
