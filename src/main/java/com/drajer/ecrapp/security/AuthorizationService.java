package com.drajer.ecrapp.security;

import com.drajer.sof.model.LaunchDetails;

public interface AuthorizationService {

  public String getAuthorizationHeader(LaunchDetails launchDetails);
}
