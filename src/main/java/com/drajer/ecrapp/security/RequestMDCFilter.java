package com.drajer.ecrapp.security;

import java.io.IOException;
import java.util.UUID;
import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
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

  public static final String X_DOMAIN_LOGICALDOMAIN_ID = "X-Domain-LogicalDomain-ID";
  public static final String MDC_DOMAIN_LOGICAL_DOMAIN_ID = "domain-logicalDomainId";

  @Override
  public void doFilter(
      final ServletRequest request, final ServletResponse response, final FilterChain chain)
      throws IOException, ServletException {

    HttpServletRequest req = (HttpServletRequest) request;
    String uri = req.getRequestURI();

    boolean isHealthCheck = uri.contains("/health") || uri.contains("/actuator/health");

    if (!isHealthCheck) {
      logger.debug("Start to handle request: {}", StringEscapeUtils.escapeJava(uri));
    }

    String requestId = req.getHeader(X_REQUEST_ID_HEADER);
    final String correlationId = req.getHeader(X_CORRELATION_ID_HEADER);
    final String domainLogicalDomainId = req.getHeader(X_DOMAIN_LOGICALDOMAIN_ID);

    try {
      if (StringUtils.isBlank(requestId)) {
        requestId = UUID.randomUUID().toString();
        logger.debug("No X-Request-ID header found; creating new requestId: {}", requestId);
      } else {
        logger.debug("Using given X-Request-ID header for requestId: {}", requestId);
      }

      if (StringUtils.isBlank(correlationId)) {
        logger.debug("No X-Correlation-ID header found.");
      } else {
        logger.debug("Using given X-Correlation-ID header for correlationId: {}", correlationId);
      }

      if (StringUtils.isBlank(domainLogicalDomainId)) {
        logger.debug("No X-Domain-LogicalDomain-ID header found.");
      } else {
        logger.debug(
            "Using given X-Domain-LogicalDomain-ID from header: {}", domainLogicalDomainId);
      }

      MDC.put(MDC_REQUEST_ID_KEY, requestId);
      MDC.put(MDC_CORRELATION_ID_KEY, correlationId);
      MDC.put(MDC_DOMAIN_LOGICAL_DOMAIN_ID, domainLogicalDomainId);

      chain.doFilter(request, response);

    } finally {
      if (!isHealthCheck) {
        logger.info(
            "Request {} completed for requestId {} associated to correlationId {} for domain {}",
            StringEscapeUtils.escapeJava(uri),
            StringEscapeUtils.escapeJava(requestId),
            StringEscapeUtils.escapeJava(correlationId),
            StringEscapeUtils.escapeJava(domainLogicalDomainId));
      }

      MDC.remove(MDC_REQUEST_ID_KEY);
      MDC.remove(MDC_CORRELATION_ID_KEY);
      MDC.remove(MDC_DOMAIN_LOGICAL_DOMAIN_ID);

      if (!isHealthCheck) {
        logger.debug("Completed handling request: {}", StringEscapeUtils.escapeJava(uri));
      }
    }
  }

  @Override
  public void init(final FilterConfig filterConfig) {
    // No initialization needed
  }

  @Override
  public void destroy() {
    // No cleanup needed
  }
}
