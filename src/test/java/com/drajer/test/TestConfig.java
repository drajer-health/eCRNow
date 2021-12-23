package com.drajer.test;

import com.drajer.bsa.model.BsaTypes;
import java.util.HashMap;
import java.util.Map;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;

@TestConfiguration
public class TestConfig {
  @Bean
  public Map<String, BsaTypes.BsaActionStatusType> actions() {
    return new HashMap<String, BsaTypes.BsaActionStatusType>();
  }
}
