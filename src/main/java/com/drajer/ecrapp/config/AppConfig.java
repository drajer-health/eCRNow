package com.drajer.ecrapp.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class AppConfig {

  @Value("${longencounter.enableSuspend:false}")
  private boolean enableSuspend;

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
}
