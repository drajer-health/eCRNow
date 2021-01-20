package com.drajer.ecrapp.security;

import com.drajer.sof.model.LaunchDetails;

public interface SampleAuthorizationService {

  public String getAuthorizationHeader(LaunchDetails launchDetails);
}
