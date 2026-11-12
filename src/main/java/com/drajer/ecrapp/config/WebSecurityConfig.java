package com.drajer.ecrapp.config;

import jakarta.servlet.Filter;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityCustomizer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

@EnableWebSecurity
@Configuration
public class WebSecurityConfig {

  private static final Logger logger = LoggerFactory.getLogger(WebSecurityConfig.class);

  @Autowired private ApplicationContext context;

  @Value("${token.validator.class:}")
  private String tokenFilterClassName;

  @Value("#{'${security.whitelist-endpoints}'.split(',')}")
  private List<String> whitelistEndpoints;

  @Value("#{'${security.cors.allowed-origins:*}'.split(',')}")
  private List<String> corsAllowedOrigins;

  // java:S5122: CORS is governed by the security.cors.allowed-origins property (comma separated)
  // instead of per-controller @CrossOrigin annotations. The default keeps the behavior the
  // application has always had; production deployments should set the property (or the
  // SECURITY_CORS_ALLOWED_ORIGINS environment variable) to the origins of their UI, or to an
  // empty value to turn cross-origin browser access off entirely.
  private CorsConfiguration buildCorsConfiguration() {
    CorsConfiguration config = new CorsConfiguration();
    List<String> origins =
        corsAllowedOrigins.stream().map(String::trim).filter(o -> !o.isEmpty()).toList();
    config.setAllowedOrigins(origins);
    config.setAllowedMethods(List.of("GET", "POST", "PUT", "DELETE", "OPTIONS"));
    config.setAllowedHeaders(List.of("*"));
    return config;
  }

  @Bean
  public CorsFilter corsFilter() {
    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
    source.registerCorsConfiguration("/**", buildCorsConfiguration());
    return new CorsFilter(source);
  }

  @Bean
  public CorsConfigurationSource corsConfigurationSource() {
    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
    source.registerCorsConfiguration("/**", buildCorsConfiguration());
    return source;
  }

  @Bean
  public WebSecurityCustomizer webSecurityCustomizer() {
    return web -> web.ignoring().requestMatchers(whitelistEndpoints.toArray(new String[0]));
  }

  @Bean
  @SuppressWarnings(
      "java:S4502") // CSRF disabled intentionally - this is a stateless REST API using token-based
  // authentication, not cookie-based auth
  public SecurityFilterChain configure(HttpSecurity http) throws Exception {

    logger.info("*******************************************************************");
    logger.info("Security Configuration {}", tokenFilterClassName);
    logger.info("*******************************************************************");

    if (tokenFilterClassName != null && !tokenFilterClassName.isBlank()) {

      logger.info("Token Filter class Name is not empty");

      Class<?> classInstance = Class.forName(tokenFilterClassName);

      Filter customFilter =
          (Filter) context.getAutowireCapableBeanFactory().autowire(classInstance, 1, true);

      http.cors(cors -> cors.configurationSource(corsConfigurationSource()))
          .csrf(csrf -> csrf.disable())
          .sessionManagement(
              session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
          .authorizeHttpRequests(
              authorize ->
                  authorize
                      .requestMatchers(whitelistEndpoints.toArray(new String[0]))
                      .permitAll()
                      .anyRequest()
                      .authenticated())
          .addFilterAfter(customFilter, UsernamePasswordAuthenticationFilter.class);

    } else {

      logger.info("Token Filter class Name is empty");

      http.cors(cors -> cors.configurationSource(corsConfigurationSource()))
          .csrf(csrf -> csrf.disable())
          .sessionManagement(
              session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
          .authorizeHttpRequests(authorize -> authorize.anyRequest().permitAll());
    }

    return http.build();
  }
}
