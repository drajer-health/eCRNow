package com.drajer.ecrapp.security;

import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.service.ClientDetailsService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.stream.Collectors;
import org.apache.commons.text.StringEscapeUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

@Component
@ConditionalOnProperty(
    name = "token.validator.class",
    havingValue = "com.drajer.ecrapp.security.TokenFilter")
public class TokenFilter extends OncePerRequestFilter {

  private final Logger log = LoggerFactory.getLogger(TokenFilter.class);

  @Autowired private KeyCloakTokenValidationClient cloakTokenValidationClient;

  @Override
  protected void doFilterInternal(
      HttpServletRequest request, HttpServletResponse response, FilterChain chain)
      throws ServletException, IOException {

    String authorizationHeader = request.getHeader("Authorization");
    if (authorizationHeader != null) {
      log.info(
          "Received Authorization Header: {}", StringEscapeUtils.escapeJava(authorizationHeader));
    }

    if (validateAccessToken(request)) {

      UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken =
          new UsernamePasswordAuthenticationToken(
              "user", null, Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER")));

      SecurityContextHolder.getContext().setAuthentication(usernamePasswordAuthenticationToken);

      chain.doFilter(request, response);
    } else {
      chain.doFilter(request, response);
    }
  }

  // Validate the access token using the Token Introspection URL
  private boolean validateAccessToken(HttpServletRequest request) {
    return cloakTokenValidationClient.validateToken(request);
  }
}
