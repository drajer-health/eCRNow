package com.drajer.bsa.ehr.customizations;

import static org.junit.Assert.*;

import ca.uhn.fhir.rest.client.interceptor.AdditionalRequestHeadersInterceptor;
import java.lang.reflect.Method;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

public class SampleEhrHeaderInterceptorImplTest {

  private SampleEhrHeaderInterceptorImpl interceptorImpl;

  @Before
  public void setUp() {
    interceptorImpl = new SampleEhrHeaderInterceptorImpl();
  }

  @Test
  public void testGetHeaderInterceptor_WithValidJson() {
    String ehrContext = "{\"Authorization\":\"Bearer token123\", \"Custom-Header\":\"value1\"}";

    AdditionalRequestHeadersInterceptor result = interceptorImpl.getHeaderInterceptor(ehrContext);

    assertNotNull("Interceptor should not be null for valid input", result);
  }

  @Test
  public void testGetHeaderInterceptor_WithValidJson1() throws Exception {
    String ehrContext = "{\"Authorization\":\"Bearer token123\", \"Custom-Header\":\"value1\"}";

    AdditionalRequestHeadersInterceptor result = interceptorImpl.getHeaderInterceptor(ehrContext);

    assertNotNull("Interceptor should not be null for valid input", result);

    Method getHeaderValuesMethod =
        AdditionalRequestHeadersInterceptor.class.getDeclaredMethod(
            "getHeaderValues", String.class);
    getHeaderValuesMethod.setAccessible(true);

    List<String> authorizationHeaders =
        (List<String>) getHeaderValuesMethod.invoke(result, "Authorization");
    List<String> customHeaders =
        (List<String>) getHeaderValuesMethod.invoke(result, "Custom-Header");

    assertNotNull("Authorization header should not be null", authorizationHeaders);
    assertEquals("Authorization header should contain one value", 1, authorizationHeaders.size());
    assertEquals(
        "Authorization header value should match", "Bearer token123", authorizationHeaders.get(0));

    assertNotNull("Custom header should not be null", customHeaders);
    assertEquals("Custom header should contain one value", 1, customHeaders.size());
    assertEquals("Custom header value should match", "value1", customHeaders.get(0));
  }

  @Test
  public void testGetHeaderInterceptor_WithInvalidJson() {
    String ehrContext =
        "{\"Authorization\":\"Bearer token123\", \"Custom-Header\":}"; // malformed JSON

    AdditionalRequestHeadersInterceptor result = interceptorImpl.getHeaderInterceptor(ehrContext);

    assertNull("Interceptor should be null for invalid JSON", result);
  }

  @Test
  public void testGetHeaderInterceptor_WithEmptyJson() {
    String ehrContext = "{}";

    AdditionalRequestHeadersInterceptor result = interceptorImpl.getHeaderInterceptor(ehrContext);

    assertNull("Interceptor should be null for empty JSON object", result);
  }

  @Test
  public void testGetHeaderInterceptor_WithNullInput() {
    AdditionalRequestHeadersInterceptor result = interceptorImpl.getHeaderInterceptor(null);

    assertNull("Interceptor should be null for null input", result);
  }

  @Test
  public void testGetHeaderInterceptor_WithEmptyString() {
    AdditionalRequestHeadersInterceptor result = interceptorImpl.getHeaderInterceptor("");

    assertNull("Interceptor should be null for empty string input", result);
  }
}
