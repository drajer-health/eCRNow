package com.drajer.sof.utils;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

@Configuration
public class WebConfiguration extends WebMvcConfigurerAdapter {

  private static final String FORWARD_SLASH = "forward:/";

  @Override
  public void addViewControllers(ViewControllerRegistry registry) {
    registry.addViewController("/{spring:\\w+}").setViewName(FORWARD_SLASH);
    registry.addViewController("/**/{spring:\\w+}").setViewName(FORWARD_SLASH);
    registry
        .addViewController("/{spring:\\w+}/**{spring:?!(\\.js|\\.css)$}")
        .setViewName(FORWARD_SLASH);
  }
}
