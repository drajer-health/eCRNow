package com.drajer.bsa.model;

import java.util.Date;

public interface FhirServerDetails {

  Integer getId();

  void setId(Integer id);

  String getClientId();

  void setClientId(String clientId);

  String getClientSecret();

  void setClientSecret(String clientSecret);

  String getUsername();

  void setUsername(String username);

  String getPassword();

  void setPassword(String password);

  String getFhirServerBaseURL();

  void setFhirServerBaseURL(String fhirServerBaseURL);

  String getScopes();

  void setScopes(String scopes);

  Date getLastUpdated();

  void setLastUpdated(Date lastUpdated);

  String getTokenUrl();

  void setTokenUrl(String tokenUrl);

  String getFhirVersion();

  void setFhirVersion(String fhirVersion);

  Boolean getRequireAud();

  void setRequireAud(Boolean requireAud);

  String getAuthType();

  void setAuthType(String authType);

  String getBackendAuthKeyAlias();

  void setBackendAuthKeyAlias(String alias);
}
