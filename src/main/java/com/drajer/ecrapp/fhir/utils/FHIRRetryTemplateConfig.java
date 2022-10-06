package com.drajer.ecrapp.fhir.utils;

import java.util.List;
import java.util.Map;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "retrytemplate")
public class FHIRRetryTemplateConfig {

  private int maxRetries;
  private long retryWaitTime;
  private List<Integer> retryStatusCodes;
  private Map<String, HttpMethodType> httpMethodTypeMap;

  public int getMaxRetries() {
    return maxRetries;
  }

  public void setMaxRetries(int maxRetries) {
    this.maxRetries = maxRetries;
  }

  public long getRetryWaitTime() {
    return retryWaitTime;
  }

  public void setRetryWaitTime(long retryWaitTime) {
    this.retryWaitTime = retryWaitTime;
  }

  public List<Integer> getRetryStatusCodes() {
    return retryStatusCodes;
  }

  public void setRetryStatusCodes(List<Integer> retryStatusCodes) {
    this.retryStatusCodes = retryStatusCodes;
  }

  public Map<String, HttpMethodType> getHttpMethodTypeMap() {
    return httpMethodTypeMap;
  }

  public void setHttpMethodTypeMap(Map<String, HttpMethodType> httpMethodTypeMap) {
    this.httpMethodTypeMap = httpMethodTypeMap;
  }

  public Boolean verifyStatusCode(int httpstatusCode) {
    for (HttpMethodType map : httpMethodTypeMap.values()) {
      for (int statusCode : map.retryStatusCodes) {
        if (statusCode == httpstatusCode) return true;
      }
    }
    return false;
  }

  public static class HttpMethodType {
    private int maxRetries;
    private long retryWaitTime;
    private List<Integer> retryStatusCodes;

    public int getMaxRetries() {
      return maxRetries;
    }

    public void setMaxRetries(int maxRetries) {
      this.maxRetries = maxRetries;
    }

    public long getRetryWaitTime() {
      return retryWaitTime;
    }

    public void setRetryWaitTime(long retryWaitTime) {
      this.retryWaitTime = retryWaitTime;
    }

    public List<Integer> getRetryStatusCodes() {
      return retryStatusCodes;
    }

    public void setRetryStatusCodes(List<Integer> retryStatusCodes) {
      this.retryStatusCodes = retryStatusCodes;
    }
  }
}
