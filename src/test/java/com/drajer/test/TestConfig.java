package com.drajer.test;

import com.drajer.bsa.kar.action.BsaActionStatus;
import jakarta.inject.Singleton;
import java.util.HashMap;
import java.util.Map;
import org.hl7.fhir.r4.model.Bundle;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;

@TestConfiguration
public class TestConfig {
  @Bean
  public Map<String, BsaActionStatus> actions() {
    return new HashMap<String, BsaActionStatus>();
  }

  @Bean
  @Singleton
  public Map<String, Bundle> eicrBundles() {
    return new HashMap<String, Bundle>();
  }
}
