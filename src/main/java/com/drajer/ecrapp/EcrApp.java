package com.drajer.ecrapp;

import com.drajer.ecrapp.security.AuthorizationService;
import com.drajer.ecrapp.security.RequestMDCFilter;
import com.drajer.ecrapp.security.SampleAuthorizationServiceImpl;
import java.util.TimeZone;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.client.RestTemplate;

@SpringBootApplication
@EnableTransactionManagement
@ServletComponentScan
public class EcrApp extends SpringBootServletInitializer {

  @Value("${rest.template.connection.timeout}")
  private Integer connectionTimeOut;

  @Value("${rest.template.read.timeout}")
  private Integer readTimeout;

  @Override
  protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
    return application.sources(EcrApp.class);
  }

  public static void main(String[] args) {
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"));
    SpringApplication.run(EcrApp.class, args);
  }

  @Bean
  public RestTemplate restTemplate() {
    SimpleClientHttpRequestFactory clientHttpRequestFactory = new SimpleClientHttpRequestFactory();
    clientHttpRequestFactory.setConnectTimeout(connectionTimeOut);
    clientHttpRequestFactory.setReadTimeout(readTimeout);
    RestTemplate restTemplate = new RestTemplate(clientHttpRequestFactory);
    return restTemplate;
  }

  @Bean
  public AuthorizationService authorizationService() {
    return new SampleAuthorizationServiceImpl();
  }

  @Bean
  public FilterRegistrationBean<RequestMDCFilter> loggingFilter() {
    FilterRegistrationBean<RequestMDCFilter> registrationBean = new FilterRegistrationBean<>();

    registrationBean.setFilter(new RequestMDCFilter());
    registrationBean.addUrlPatterns("/api/*");

    return registrationBean;
  }
}
