package com.drajer.ecrapp.config;

import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springdoc.core.GroupedOpenApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

  @Bean
  public GroupedOpenApi publicApi() {
    return GroupedOpenApi.builder().group("public").pathsToMatch("/**").build();
  }

  @Bean
  public io.swagger.v3.oas.models.OpenAPI customOpenAPI() {
    return new io.swagger.v3.oas.models.OpenAPI()
        .info(
            new Info()
                .title("Spring Boot API")
                .version("v1.0")
                .description("API documentation")
                .termsOfService("https://example.com/terms")
                .license(new License().name("Apache 2.0").url("https://springdoc.org")));
  }
}
