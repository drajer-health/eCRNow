package com.drajer.ecrapp.fhir.utils;

import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "ecrfhirretrytemplate")
@Getter
@Setter
public class FHIRRetryTemplateConfig {

  private int maxRetries;
  private long retryWaitTimeInMillis;
  private List<Integer> retryStatusCodes;
  private Map<String, HttpMethodType> httpMethodTypeMap;

  public int getMaxRetries() {
    return (maxRetries > 0 ? maxRetries : 0);
  }

  public long getRetryWaitTimeInMillis() {
    return (retryWaitTimeInMillis > 0 ? retryWaitTimeInMillis : 1000);
  }

  @Getter
  @Setter
  public static class HttpMethodType {
    private int maxRetries;

    private long retryWaitTimeInMillis;
    private List<Integer> retryStatusCodes;

    public int getMaxRetries() {
      return (maxRetries > 0 ? maxRetries : 0);
    }

    public long getRetryWaitTimeInMillis() {
      return (retryWaitTimeInMillis > 0 ? retryWaitTimeInMillis : 1000);
    }
  }
}
