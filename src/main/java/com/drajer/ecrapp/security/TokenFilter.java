package com.drajer.ecrapp.security;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;
import java.io.IOException;
import java.util.stream.Collectors;
import javax.servlet.FilterChain;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;
import org.springframework.web.filter.OncePerRequestFilter;

// @Component
public class TokenFilter extends OncePerRequestFilter {

  private final Logger logger = LoggerFactory.getLogger(TokenFilter.class);

  ClientDetailsService clientDetailsService;

  @Override
  protected void doFilterInternal(
      HttpServletRequest request, HttpServletResponse response, FilterChain chain)
      throws ServletException, IOException {

    if (clientDetailsService == null) {
      ServletContext servletContext = request.getServletContext();
      WebApplicationContext webApplicationContext =
          WebApplicationContextUtils.getWebApplicationContext(servletContext);
      clientDetailsService = webApplicationContext.getBean(ClientDetailsService.class);
    }
    logger.info("Received Authorization Header========> {}", request.getHeader("Authorization"));

    // Read the Request body from the Request
    String requestBody =
        request.getReader().lines().collect(Collectors.joining(System.lineSeparator()));
    requestBody = requestBody.replace("\n", "").replace("\r", "");
    logger.info("RequestBody===========> {}", requestBody);

    // Get the Client Details using fhirServerURL received in request body
    JSONObject requestBodyObj = new JSONObject(requestBody);

    logger.info(requestBodyObj.getString("fhirServerURL"));

    ClientDetails clientDetails =
        clientDetailsService.getClientDetailsByUrl(requestBodyObj.getString("fhirServerURL"));

    // Read the Token Instrospection URL, Client Id and Client Secret from Client Details
    // String tokenIntrospectionURL = clientDetails.getTokenIntrospectionURL();
    String clientId = clientDetails.getClientId();
    String clientSecret = clientDetails.getClientSecret();

    if (validateAccessToken(clientId, clientSecret)) {
      chain.doFilter(request, response);
    }
  }

  // Validate the AccessToken Using Token Introspection URL
  private boolean validateAccessToken(String clientId, String clientSecret) {

    // Enable the Below code when the Introspection URL is ready to test
    /**
     * String auth = clientId + clientSecret; RestTemplate restTemplate = new RestTemplate();
     * HttpHeaders headers = new HttpHeaders(); headers.add("Content-Type",
     * MediaType.APPLICATION_JSON_VALUE); headers.add("Authorization", "Basic " +
     * Base64.getEncoder().encodeToString(auth.getBytes())); HttpEntity<String> request = new
     * HttpEntity<String>(headers);
     *
     * <p>logger.info("Sending Token Intropsection request to Endpoint::::: {}",
     * tokenIntrospectionURL); ResponseEntity<String> response =
     * restTemplate.exchange(tokenIntrospectionURL, HttpMethod.POST, request, String.class); *
     */
    return true;
  }
}
