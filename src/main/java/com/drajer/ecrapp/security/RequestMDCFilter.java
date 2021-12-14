package com.drajer.ecrapp.security;

import java.io.IOException;
import java.util.UUID;
import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Component
@Order(1)
public class RequestMDCFilter implements Filter {
  private static final Logger logger = LoggerFactory.getLogger(RequestMDCFilter.class);

  public static final String X_REQUEST_ID_HEADER = "X-Request-ID";
  public static final String MDC_REQUEST_ID_KEY = "requestId";

  public static final String X_CORRELATION_ID_HEADER = "X-Correlation-ID";
  public static final String MDC_CORRELATION_ID_KEY = "correlationId";

  public void doFilter(
      final ServletRequest request, final ServletResponse response, final FilterChain chain)
      throws IOException, ServletException {
    HttpServletRequest req = (HttpServletRequest) request;
    if (logger.isDebugEnabled()) {
      logger.debug("Start to handle request: {}", req.getRequestURI());
    }

    String requestId = req.getHeader(X_REQUEST_ID_HEADER);
    final String correlationId = req.getHeader(X_CORRELATION_ID_HEADER);

    try {

      // Create a requestId if one does not exist.
      if (StringUtils.isBlank(requestId)) {
        requestId = UUID.randomUUID().toString();

        if (logger.isInfoEnabled()) {
          logger.info("No X-Request-ID header found; creating new requestId: {}", requestId);
        }
      } else {
        if (logger.isInfoEnabled()) {
          logger.info("Using given X-Request-ID header for requestId: {}", requestId);
        }
      }

      if (StringUtils.isBlank(correlationId)) {
        if (logger.isInfoEnabled()) {
          logger.info("No X-Correlation-ID header found.");
        }
      } else {
        if (logger.isInfoEnabled()) {
          logger.info("Using given X-Correlation-ID header for correlationId: {}", correlationId);
        }
      }

      if (logger.isInfoEnabled()) {
        logger.info(
            "Request {} being handled for requestId {} associated to correlationId {}",
            req.getRequestURI(),
            requestId,
            correlationId);
      }

      if (MDC.get(MDC_REQUEST_ID_KEY) == null) {
        MDC.put(MDC_REQUEST_ID_KEY, requestId);
      }
      if (MDC.get(MDC_CORRELATION_ID_KEY) == null) {
        MDC.put(MDC_CORRELATION_ID_KEY, correlationId);
      }

      chain.doFilter(request, response);
    } finally {
      if (logger.isInfoEnabled()) {
        logger.info(
            "Request {} completed for requestId {} associated to correlationId {}",
            req.getRequestURI(),
            requestId,
            correlationId);
      }

      MDC.remove(MDC_REQUEST_ID_KEY);
      MDC.remove(MDC_CORRELATION_ID_KEY);
    }

    if (logger.isDebugEnabled()) {
      logger.debug("Completed handling request: {}", req.getRequestURI());
    }
  }

  public void init(final FilterConfig filterConfig) throws ServletException {}

  public void destroy() {}
}
