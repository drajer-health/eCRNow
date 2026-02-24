package com.drajer.sof.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.rest.client.api.IHttpRequest;
import ca.uhn.fhir.rest.client.api.IHttpResponse;
import java.io.IOException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

public class HttpHeaderInterceptorTest {

  private HttpHeaderInterceptor interceptor;
  private IHttpRequest httpRequest;
  private IHttpResponse httpResponse;

  @Before
  public void setUp() {
    interceptor = new HttpHeaderInterceptor("X-Test-Header", "initial-value");
    httpRequest = Mockito.mock(IHttpRequest.class);
    httpResponse = Mockito.mock(IHttpResponse.class);
  }

  // ----------------------------------------------------
  // Constructor + Getter
  // ----------------------------------------------------

  @Test
  public void testConstructorAndGetValue() {
    assertNotNull(interceptor);
    assertEquals("initial-value", interceptor.getValue());
  }

  // ----------------------------------------------------
  // Setter
  // ----------------------------------------------------

  @Test
  public void testSetValue() {
    interceptor.setValue("updated-value");

    assertEquals("updated-value", interceptor.getValue());
  }

  // ----------------------------------------------------
  // interceptRequest
  // ----------------------------------------------------

  @Test
  public void testInterceptRequest_AddsHeader() {
    interceptor.interceptRequest(httpRequest);

    verify(httpRequest, times(1)).addHeader("X-Test-Header", "initial-value");
  }

  // ----------------------------------------------------
  // interceptResponse (empty method but must be covered)
  // ----------------------------------------------------

  @Test
  public void testInterceptResponse_DoesNothing() throws IOException {
    interceptor.interceptResponse(httpResponse);

    // No exception expected
    verifyNoInteractions(httpResponse);
  }
}
