package com.drajer.ecrapp.config;

import java.util.HashMap;
import java.util.Map;
import javax.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.Environment;
import org.springframework.core.env.PropertySource;
import org.springframework.stereotype.Component;

@Component
public class AppConfig {

  @Autowired Environment environment;

  @Value("${longencounter.enableSuspend:false}")
  private boolean enableSuspend;

  private static Map<String, String> props = new HashMap<>();

  @Value("${longencounter.suspendThreshold:45}")
  private int suspendThreshold;

  public boolean isEnableSuspend() {
    return enableSuspend;
  }

  public void setEnableSuspend(final boolean enableSuspend) {
    this.enableSuspend = enableSuspend;
  }

  public int getSuspendThreshold() {
    return suspendThreshold;
  }

  public void setSuspendThreshold(final int suspendThreshold) {
    this.suspendThreshold = suspendThreshold;
  }

  public Environment getEnvironment() {
    return environment;
  }

  @PostConstruct
  public Map<String, String> readAllProperties() {

    if (environment instanceof ConfigurableEnvironment) {
      ConfigurableEnvironment configEnv = (ConfigurableEnvironment) environment;

      for (PropertySource<?> propertySource : configEnv.getPropertySources()) {
        if (propertySource.getSource() instanceof Map) {
          @SuppressWarnings("unchecked")
          Map<String, Object> source = (Map<String, Object>) propertySource.getSource();
          for (Map.Entry<String, Object> entry : source.entrySet()) {
            if (!props.containsKey(entry.getKey())) {
              String rawValue = String.valueOf(entry.getValue());

              // Resolve nested placeholders like ${ehr.product.version}
              String resolvedValue = environment.resolvePlaceholders(rawValue);

              props.put(entry.getKey(), resolvedValue);
            }
          }
        }
      }
    }

    return props;
  }

  public static Map<String, String> getAllProperties() {
    return props;
  }
}
