package com.drajer.ecrapp.config;

import javax.servlet.Filter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@EnableWebSecurity
@Configuration
public class WebSecurityConfig extends WebSecurityConfigurerAdapter {

  private final Logger logger = LoggerFactory.getLogger(WebSecurityConfig.class);

  @Value("${token.validator.class}")
  private String tokenFilterClassName;

  protected void configure(HttpSecurity http) throws Exception {
    logger.info("*******************************************************************");
    logger.info("Security Configuration" + tokenFilterClassName);
    logger.info("*******************************************************************");
    if (!tokenFilterClassName.isEmpty() && tokenFilterClassName != null) {
      logger.info("Token Filter class Name is not empty");
      Class classInstance = Class.forName(tokenFilterClassName);
      logger.info(classInstance.getDeclaredMethods()[0].getName());
      http.csrf()
          .disable()
          .addFilterAfter(
              (Filter) classInstance.newInstance(), UsernamePasswordAuthenticationFilter.class)
          .authorizeRequests()
          .antMatchers(HttpMethod.POST, "/api/**")
          .permitAll()
          .anyRequest()
          .authenticated();
    } else {
      logger.info("Token Filter class Name is empty");
      http.csrf().disable().authorizeRequests().anyRequest().permitAll();
    }
  }
}
