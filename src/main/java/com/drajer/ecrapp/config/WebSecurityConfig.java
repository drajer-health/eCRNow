package com.drajer.ecrapp.config;

import java.util.Arrays;
import javax.servlet.Filter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

@EnableWebSecurity
@Configuration
public class WebSecurityConfig extends WebSecurityConfigurerAdapter {

  private final Logger logger = LoggerFactory.getLogger(WebSecurityConfig.class);

  @Autowired private ApplicationContext context;

  @Value("${token.validator.class}")
  private String tokenFilterClassName;

  @Value("${security.whitelist-endpoints}")
  private String[] whitelistEndpoints;

  @Override
  public void configure(WebSecurity web) throws Exception {

    if (whitelistEndpoints != null && whitelistEndpoints.length > 0) {

      web.ignoring().antMatchers(whitelistEndpoints);
    } else {
      web.ignoring()
          .antMatchers(
              "/meta/**",
              "/actuator/**",
              "/swagger-ui/**",
              "/v3/api-docs/**",
              "/api/receiveEicr",
              "/api/auth/generate-token",
              "/api/auth/generateAuthToken",
              "/api/auth/refresh-token");
    }
  }

  @Bean
  public CorsFilter corsFilter() {
    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
    CorsConfiguration config = new CorsConfiguration();
    config.setAllowedOrigins(Arrays.asList("*"));
    config.setAllowedMethods(Arrays.asList("GET", "POST", "PUT", "DELETE", "OPTIONS"));
    config.setAllowedHeaders(Arrays.asList("*"));
    //   config.setAllowCredentials(true);
    source.registerCorsConfiguration("/**", config);
    return new CorsFilter(source);
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
      http.cors()
          .and()
          .csrf().ignoringAntMatchers("/api/auth/**") // Exempt public auth endpoints from CSRF
          .and()
          .addFilterAfter(customFilter, UsernamePasswordAuthenticationFilter.class)
          .authorizeRequests()
          .antMatchers("/api/auth/**").permitAll() // Publicly accessible endpoints
          .authenticated();
    } else {
      logger.info("Token Filter class Name is empty");
      http.cors()
          .and()
          .csrf().ignoringAntMatchers("/api/auth/**") // Exempt auth endpoints
          .and()
          .authorizeRequests()
          .anyRequest()
          .permitAll();
    }
  }
}
