package com.drajer.ecrapp.security;

import com.drajer.sof.model.LaunchDetails;
import org.springframework.http.HttpHeaders;

public interface AuthorizationService {

  public HttpHeaders getAuthorizationHeader(LaunchDetails launchDetails);
}
