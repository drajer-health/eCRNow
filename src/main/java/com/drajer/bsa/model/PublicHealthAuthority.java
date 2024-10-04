package com.drajer.bsa.model;

import java.util.Date;
import javax.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>PublicHealthAuthority</h1>
 */
@Entity
@Table(name = "public_health_authority")
@DynamicUpdate
public class PublicHealthAuthority implements FhirServerDetails {

  @Transient private final Logger logger = LoggerFactory.getLogger(PublicHealthAuthority.class);

  /** The attribute represents the primary key for the table and is auto incremented. */
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  /** The attribute represents the client id that is to be used for SMART on FHIR Authorization. */
  @Column(name = "clientId", nullable = false, columnDefinition = "TEXT")
  private String clientId;

  /**
   * The attribute represents the client secret that is to be used for SMART on FHIR Authorization.
   */
  @Column(name = "clientSecret", columnDefinition = "TEXT")
  private String clientSecret;

  /** The attribute represents a username that may be used with password-based Authorization. */
  @Column(name = "username", columnDefinition = "TEXT")
  private String username;

  /**
   * The attribute represents a password associated with the username for password-based
   * authorization.
   */
  @Column(name = "password", columnDefinition = "TEXT")
  private String password;

  /**
   * The attribute represents the FHIR Server URL for the PublicHealthAuthority. This is unique for
   * the entire table.
   */
  @Column(name = "fhir_server_base_url", nullable = false, unique = true)
  private String fhirServerBaseURL;

  /** The attribute represents the FHIR Server version of the specification supported. */
  @Column(name = "fhir_version")
  private String fhirVersion;

  /**
   * The attribute represents the Token URL for requesting access tokens as part of SMART on FHIR
   * Authorization. This is provided to override what is present in the CapabilityStatement.
   */
  @Column(name = "token_url", columnDefinition = "TEXT")
  private String tokenUrl;

  /**
   * The attribute represents the scopes for which permission is requested during the SMART on FHIR
   * Authorization. This is provided to override what is present in the CapabilityStatement.
   */
  @Column(name = "scopes", nullable = false, columnDefinition = "TEXT")
  private String scopes;

  @Column(name = "require_aud", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean requireAud = false;

  /**
   * This attribute represents the type of authentication to be used by the public health authority.
   */
  @Column(name = "auth_type", nullable = false, columnDefinition = "TEXT")
  private String authType;

  /** The attribute represents the Key Alias for the Private Key to be used for signing. */
  @Column(name = "backend_auth_key_alias", columnDefinition = "TEXT")
  private String backendAuthKeyAlias;

  /** This attribute represents the last time when the object was updated. */
  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

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

  @Override
  public Boolean getRequireAud() {
    return requireAud;
  }

  @Override
  public void setRequireAud(Boolean requireAud) {
    this.requireAud = requireAud;
  }

  @Override
  public String getAuthType() {
    return authType;
  }

  @Override
  public void setAuthType(String authType) {
    this.authType = authType;
  }

  public String getBackendAuthKeyAlias() {
    return backendAuthKeyAlias;
  }

  public void setBackendAuthKeyAlias(String backendAuthKeyAlias) {
    this.backendAuthKeyAlias = backendAuthKeyAlias;
  }

  public void log() {

    logger.info(" **** Printing PublicHealthAuthority Details **** ");

    logger.info(" Id : {}", id);
    logger.info(" Client Id : {}", clientId);
    logger.info(" FHIR Server URL : {}", fhirServerBaseURL);
    logger.info(" Token URL : {}", tokenUrl);

    logger.info(" **** End Printing PublicHealthAuthority Details **** ");
  }
}
