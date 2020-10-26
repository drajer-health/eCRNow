package com.drajer.ecrapp.config;

import com.drajer.ecrapp.security.TokenFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

  @Override
  protected void configure(HttpSecurity http) throws Exception {
    logger.info("*******************************************************************");
    logger.info("Security Configuration");
    logger.info("*******************************************************************");
    http.csrf()
        .disable()
        .addFilterAfter(new TokenFilter(), UsernamePasswordAuthenticationFilter.class)
        .authorizeRequests()
        .antMatchers(HttpMethod.POST, "/api/**")
        .permitAll()
        .anyRequest()
        .authenticated();
  }
}
