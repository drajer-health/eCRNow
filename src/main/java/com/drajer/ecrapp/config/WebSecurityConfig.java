package com.drajer.ecrapp.config;

import javax.servlet.Filter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@EnableWebSecurity
@Configuration
public class WebSecurityConfig extends WebSecurityConfigurerAdapter {

  private final Logger logger = LoggerFactory.getLogger(WebSecurityConfig.class);

  @Autowired private ApplicationContext context;

  @Value("${token.validator.class}")
  private String tokenFilterClassName;

  @Override
  public void configure(WebSecurity web) throws Exception {
    web.ignoring()
        .antMatchers(
            "/meta/**",
            "/actuator/**",
            "/swagger-ui/**",
            "/v3/api-docs/**",
            "/api/receiveEicr",
            "/api/auth/generate-token",
            "/api/auth/generateAuthToken",
            "/api/auth/refresh_token");
  }

  @Override
  protected void configure(HttpSecurity http) throws Exception {
    logger.info("*******************************************************************");
    logger.info("Security Configuration {}", tokenFilterClassName);
    logger.info("*******************************************************************");
    if (tokenFilterClassName != null && !tokenFilterClassName.isEmpty()) {
      logger.info("Token Filter class Name is not empty");
      Class<?> classInstance = Class.forName(tokenFilterClassName);
      logger.info(classInstance.getDeclaredMethods()[0].getName());

      Filter customFilter =
          (Filter) context.getAutowireCapableBeanFactory().autowire(classInstance, 1, true);

      http.csrf()
          .disable()
          .addFilterAfter(customFilter, UsernamePasswordAuthenticationFilter.class)
          .authorizeRequests()
          .antMatchers("/api/**")
          .authenticated();
    } else {
      logger.info("Token Filter class Name is empty");
      http.csrf().disable().authorizeRequests().anyRequest().permitAll();
    }
  }
}
