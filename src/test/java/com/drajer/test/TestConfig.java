package com.drajer.test;

import com.drajer.bsa.model.BsaTypes;
import java.util.HashMap;
import java.util.Map;
import javax.inject.Singleton;
import org.hl7.fhir.r4.model.Bundle;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;

@TestConfiguration
public class TestConfig {
  @Bean
  public Map<String, BsaTypes.BsaActionStatusType> actions() {
    return new HashMap<String, BsaTypes.BsaActionStatusType>();
  }

  @Bean
  @Singleton
  public Map<String, Bundle> eicrBundles() {
    return new HashMap<String, Bundle>();
  }
}
