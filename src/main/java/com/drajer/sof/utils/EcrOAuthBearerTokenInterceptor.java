package com.drajer.sof.utils;

import ca.uhn.fhir.rest.client.api.IHttpRequest;
import ca.uhn.fhir.rest.client.interceptor.BearerTokenAuthInterceptor;
import com.drajer.sof.model.LaunchDetails;

public class EcrOAuthBearerTokenInterceptor extends BearerTokenAuthInterceptor {

  private LaunchDetails launchDetails;

  public EcrOAuthBearerTokenInterceptor(LaunchDetails launchDetails) {
    this.launchDetails = launchDetails;
  }

  @Override
  public void interceptRequest(IHttpRequest theRequest) {
    super.setToken(launchDetails.getAccessToken());
    super.interceptRequest(theRequest);
  }
}
