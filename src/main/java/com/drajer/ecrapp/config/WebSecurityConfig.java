package com.drajer.ecrapp.config;

import jakarta.servlet.Filter;
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
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@EnableWebSecurity
@Configuration
public class WebSecurityConfig {

  private final Logger logger = LoggerFactory.getLogger(WebSecurityConfig.class);

  @Autowired private ApplicationContext context;

  @Value("${token.validator.class}")
  private String tokenFilterClassName;

  private static final String[] AUTH_WHITELIST = {
    "/v2/api-docs",
    "/v3/api-docs",
    "/v3/api-docs/**",
    "/swagger-resources",
    "/swagger-resources/**",
    "/configuration/ui",
    "/configuration/security",
    "/swagger-ui/**",
    "/webjars/**",
    "/swagger-ui.html",
    "/meta/**",
    "/actuator/**",
    "/api/auth/refresh-token",
    "/api/receiveEicr",
    "/api/auth/generate-token",
    "/api/auth/generateAuthToken"
  };

  @Bean
  public WebSecurityCustomizer webSecurityCustomizer() {
    return (web) -> web.ignoring().requestMatchers(AUTH_WHITELIST);
  }

  @Bean
  protected SecurityFilterChain configure(HttpSecurity http) throws Exception {
    logger.info("*******************************************************************");
    logger.info("Security Configuration {}", tokenFilterClassName);
    logger.info("*******************************************************************");
    if (tokenFilterClassName != null && !tokenFilterClassName.isEmpty()) {
      logger.info("Token Filter class Name is not empty");
      Class<?> classInstance = Class.forName(tokenFilterClassName);
      logger.info(classInstance.getDeclaredMethods()[0].getName());

      Filter customFilter =
          (Filter) context.getAutowireCapableBeanFactory().autowire(classInstance, 1, true);

      http.csrf(csrf -> csrf.disable())
          .authorizeHttpRequests(
              (authorize) ->
                  authorize
                      .requestMatchers(AUTH_WHITELIST)
                      .permitAll()
                      .anyRequest()
                      .authenticated())
          .addFilterAfter(customFilter, UsernamePasswordAuthenticationFilter.class);

    } else {
      logger.info("Token Filter class Name is empty");
      http.csrf(csrf -> csrf.disable()).authorizeRequests().anyRequest().permitAll();
    }

    return http.build();
  }
}
