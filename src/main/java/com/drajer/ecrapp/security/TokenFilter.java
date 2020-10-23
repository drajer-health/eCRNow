package com.drajer.ecrapp.security;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;
import java.io.IOException;
import java.util.Base64;
import java.util.stream.Collectors;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.io.IOUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.filter.OncePerRequestFilter;

public class TokenFilter extends OncePerRequestFilter {

  private final Logger logger = LoggerFactory.getLogger(TokenFilter.class);

  @Autowired ClientDetailsService clientDetailsService;

  @Override
  protected void doFilterInternal(
      HttpServletRequest request, HttpServletResponse response, FilterChain chain)
      throws ServletException, IOException {
    logger.info("Received Authorization Header========>"+request.getHeader("Authorization"));
    
    // Read the Request body from the Request
    String requestBody = request.getReader().lines().collect(Collectors.joining(System.lineSeparator()));
    
    // Get the Client Details using fhirServerURL received in request body
    JSONObject requestBodyObj = new JSONObject(requestBody);
    
    ClientDetails clientDetails =
        clientDetailsService.getClientDetailsByUrl(requestBodyObj.getString("fhirServerURL"));

    // Read the Token Instrospection URL, Client Id and Client Secret from Client Details
    String tokenIntrospectionURL = clientDetails.getTokenIntrospectionURL();
    String clientId = clientDetails.getClientId();
    String clientSecret = clientDetails.getClientSecret();

    if (validateAccessToken(tokenIntrospectionURL, clientId, clientSecret)) {
      chain.doFilter(request, response);
    }
  }

  // Validate the AccessToken Using Token Introspection URL 
  private Boolean validateAccessToken(
      String tokenIntrospectionURL, String clientId, String clientSecret) {
    String auth = clientId + clientSecret;
    RestTemplate restTemplate = new RestTemplate();
    HttpHeaders headers = new HttpHeaders();
    headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
    headers.add("Authorization", "Basic " + Base64.getEncoder().encodeToString(auth.getBytes()));
    HttpEntity<String> request = new HttpEntity<String>(headers);

    logger.info("Sending Token Intropsection request to Endpoint::::: {}", tokenIntrospectionURL);
    ResponseEntity<String> response =
        restTemplate.exchange(tokenIntrospectionURL, HttpMethod.POST, request, String.class);
    
    
    return true;
  }
}
