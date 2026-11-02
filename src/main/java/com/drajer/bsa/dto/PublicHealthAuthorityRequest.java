package com.drajer.bsa.dto;

import com.drajer.bsa.model.PublicHealthAuthority;
import java.util.Date;

/**
 * Request body for creating or updating a {@link PublicHealthAuthority}. It accepts the same JSON
 * properties as the entity, so the API contract is unchanged, and is converted to the entity with
 * {@link #toEntity()} so the persistence class is never bound directly from the request.
 */
public class PublicHealthAuthorityRequest {

  private Integer id;
  private String clientId;
  private String clientSecret;
  private String username;
  private String password;
  private String fhirServerBaseURL;
  private String scopes;
  private Date lastUpdated;
  private String tokenUrl;
  private String fhirVersion;
  private Boolean requireAud = false;
  private String authType;
  private String backendAuthKeyAlias;
  private String backendAuthAlg;
  private String backendAuthKid;

  /** Creates a new PublicHealthAuthority carrying every value of this request. */
  public PublicHealthAuthority toEntity() {
    PublicHealthAuthority entity = new PublicHealthAuthority();
    entity.setId(id);
    entity.setClientId(clientId);
    entity.setClientSecret(clientSecret);
    entity.setUsername(username);
    entity.setPassword(password);
    entity.setFhirServerBaseURL(fhirServerBaseURL);
    entity.setScopes(scopes);
    entity.setLastUpdated(lastUpdated);
    entity.setTokenUrl(tokenUrl);
    entity.setFhirVersion(fhirVersion);
    entity.setRequireAud(requireAud);
    entity.setAuthType(authType);
    entity.setBackendAuthKeyAlias(backendAuthKeyAlias);
    entity.setBackendAuthAlg(backendAuthAlg);
    entity.setBackendAuthKid(backendAuthKid);
    return entity;
  }

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public String getClientId() {
    return clientId;
  }

  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  public String getClientSecret() {
    return clientSecret;
  }

  public void setClientSecret(String clientSecret) {
    this.clientSecret = clientSecret;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public String getFhirServerBaseURL() {
    return fhirServerBaseURL;
  }

  public void setFhirServerBaseURL(String fhirServerBaseURL) {
    this.fhirServerBaseURL = fhirServerBaseURL;
  }

  public String getScopes() {
    return scopes;
  }

  public void setScopes(String scopes) {
    this.scopes = scopes;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public String getTokenUrl() {
    return tokenUrl;
  }

  public void setTokenUrl(String tokenUrl) {
    this.tokenUrl = tokenUrl;
  }

  public String getFhirVersion() {
    return fhirVersion;
  }

  public void setFhirVersion(String fhirVersion) {
    this.fhirVersion = fhirVersion;
  }

  public Boolean getRequireAud() {
    return requireAud;
  }

  public void setRequireAud(Boolean requireAud) {
    this.requireAud = requireAud;
  }

  public String getAuthType() {
    return authType;
  }

  public void setAuthType(String authType) {
    this.authType = authType;
  }

  public String getBackendAuthKeyAlias() {
    return backendAuthKeyAlias;
  }

  public void setBackendAuthKeyAlias(String backendAuthKeyAlias) {
    this.backendAuthKeyAlias = backendAuthKeyAlias;
  }

  public String getBackendAuthAlg() {
    return backendAuthAlg;
  }

  public void setBackendAuthAlg(String backendAuthAlg) {
    this.backendAuthAlg = backendAuthAlg;
  }

  public String getBackendAuthKid() {
    return backendAuthKid;
  }

  public void setBackendAuthKid(String backendAuthKid) {
    this.backendAuthKid = backendAuthKid;
  }
}
