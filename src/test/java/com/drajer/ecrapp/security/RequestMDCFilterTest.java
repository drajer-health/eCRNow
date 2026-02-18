package com.drajer.ecrapp.security;

import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.*;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.MDC;

@RunWith(MockitoJUnitRunner.class)
public class RequestMDCFilterTest {

  @InjectMocks private RequestMDCFilter filter;

  @Mock private HttpServletRequest request;

  @Mock private HttpServletResponse response;

  @Mock private FilterChain filterChain;

  @Before
  public void setUp() {
    MDC.clear();
  }

  @After
  public void tearDown() {
    MDC.clear();
  }

  @Test
  public void testDoFilter_WithAllHeaders() throws IOException, ServletException {
    when(request.getRequestURI()).thenReturn("/api/test");
    when(request.getHeader(RequestMDCFilter.X_REQUEST_ID_HEADER)).thenReturn("req-123");
    when(request.getHeader(RequestMDCFilter.X_CORRELATION_ID_HEADER)).thenReturn("corr-456");
    when(request.getHeader(RequestMDCFilter.X_DOMAIN_LOGICALDOMAIN_ID)).thenReturn("domain-789");
    filter.doFilter(request, response, filterChain);
    verify(filterChain).doFilter(request, response);
    assertNull(MDC.get(RequestMDCFilter.MDC_REQUEST_ID_KEY));
    assertNull(MDC.get(RequestMDCFilter.MDC_CORRELATION_ID_KEY));
    assertNull(MDC.get(RequestMDCFilter.MDC_DOMAIN_LOGICAL_DOMAIN_ID));
  }

  @Test
  public void testDoFilter_WithoutHeaders_GeneratesRequestId()
      throws IOException, ServletException {

    when(request.getRequestURI()).thenReturn("/api/test");
    when(request.getHeader(anyString())).thenReturn(null);
    filter.doFilter(request, response, filterChain);
    verify(filterChain).doFilter(request, response);
    assertNull(MDC.get(RequestMDCFilter.MDC_REQUEST_ID_KEY));
  }

  @Test
  public void testDoFilter_HealthCheckEndpoint() throws IOException, ServletException {

    when(request.getRequestURI()).thenReturn("/actuator/health");
    filter.doFilter(request, response, filterChain);
    verify(filterChain).doFilter(request, response);
    assertNull(MDC.get(RequestMDCFilter.MDC_REQUEST_ID_KEY));
  }

  @Test
  public void testDoFilter_BlankHeaders() throws IOException, ServletException {

    when(request.getRequestURI()).thenReturn("/api/test");
    when(request.getHeader(RequestMDCFilter.X_REQUEST_ID_HEADER)).thenReturn("");
    when(request.getHeader(RequestMDCFilter.X_CORRELATION_ID_HEADER)).thenReturn(" ");
    when(request.getHeader(RequestMDCFilter.X_DOMAIN_LOGICALDOMAIN_ID)).thenReturn("");

    filter.doFilter(request, response, filterChain);
    verify(filterChain).doFilter(request, response);
    assertNull(MDC.get(RequestMDCFilter.MDC_REQUEST_ID_KEY));
  }
}
