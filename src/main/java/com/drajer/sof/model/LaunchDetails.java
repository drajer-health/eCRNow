package com.drajer.sof.model;

import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.utils.RefreshTokenScheduler;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.Type;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Entity
@Table(
    name = "launch_details",
    uniqueConstraints =
        @UniqueConstraint(columnNames = {"ehr_server_url", "launch_patient_id", "encounter_id"}))
@DynamicUpdate
// @TypeDefs({ @TypeDef(name = "StringJsonObject", typeClass = JSONObjectUserType.class) })
public class LaunchDetails {

  @Transient private final Logger logger = LoggerFactory.getLogger(LaunchDetails.class);

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  @Column(name = "client_id", nullable = true)
  private String clientId;

  @Column(name = "client_secret", nullable = true)
  private String clientSecret;

  @Column(name = "ehr_server_url", nullable = true)
  private String ehrServerURL;

  @Column(name = "auth_url", nullable = true, columnDefinition = "TEXT")
  private String authUrl;

  @Column(name = "token_url", nullable = true, columnDefinition = "TEXT")
  private String tokenUrl;

  @Column(name = "access_token", nullable = true, columnDefinition = "TEXT")
  private String accessToken;

  @Column(name = "user_id", nullable = true)
  private String userId;

  @Column(name = "expiry", nullable = true)
  private int expiry;

  @Column(name = "scope", nullable = true, columnDefinition = "TEXT")
  private String scope;

  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  @Column(name = "start_date", nullable = true)
  @Temporal(TemporalType.TIMESTAMP)
  private Date startDate;

  @Column(name = "end_date", nullable = true)
  @Temporal(TemporalType.TIMESTAMP)
  private Date endDate;

  @Column(name = "token_expiry_date", nullable = true)
  @Temporal(TemporalType.TIMESTAMP)
  private Date tokenExpiryDateTime;

  @Column(name = "refresh_token", nullable = true, columnDefinition = "TEXT")
  private String refreshToken;

  @Column(name = "launch_patient_id", nullable = true)
  private String launchPatientId;

  @Column(name = "fhir_version", nullable = true)
  private String fhirVersion;

  @Column(name = "encounter_id", nullable = true)
  private String encounterId;

  @Column(
      name = "status",
      nullable = true,
      columnDefinition = "TEXT") // Status can be active or completed.
  private String status;

  @Column(name = "aa_id", nullable = true) // AA ID for creating CDA.
  private String assigningAuthorityId;

  @Column(name = "set_id", nullable = true)
  private String setId;

  @Column(name = "ver_number", nullable = true)
  private int versionNumber;

  @Column(name = "direct_host", nullable = true) // Direct Host for sending SMTP messages
  private String directHost;

  @Column(name = "direct_user", nullable = true) // SMTP User NAme
  private String directUser;

  @Column(name = "direct_pwd", nullable = true) // SMTP Pwd
  private String directPwd;

  @Column(name = "smtp_port", nullable = true)
  private String smtpPort;

  @Column(name = "imap_port", nullable = true)
  private String imapPort;

  @Column(name = "direct_recipient", nullable = true) // AIMS receiver address.
  private String directRecipient;

  @Column(name = "rest_api_url", nullable = true) // RESTful API for integration
  private String restAPIURL;

  @Column(name = "is_covid19", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isCovid = true;

  @Column(name = "launch_id", nullable = true)
  private String launchId;

  @Column(name = "launch_state", nullable = true)
  private int launchState;

  @Column(name = "redirect_uri", nullable = true)
  private String redirectURI;

  @Column(name = "auth_code", nullable = true)
  private String authorizationCode;

  @Column(name = "is_system_launch", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isSystem = false;

  @Column(name = "debug_fhir_query_and_eicr", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean debugFhirQueryAndEicr = false;

  @Column(name = "require_aud", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean requireAud = false;

  @Column(name = "x_request_id", nullable = true)
  private String xRequestId;

  @Column(name = "validation_mode", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean validationMode = false;

  @Column(name = "launch_type", nullable = true)
  private String launchType;

  public Boolean getIsCovid() {
    return isCovid;
  }

  public void setIsCovid(Boolean isCovid) {
    this.isCovid = isCovid;
  }

  public String getDirectHost() {
    return directHost;
  }

  public void setDirectHost(String directHost) {
    this.directHost = directHost;
  }

  public String getDirectRecipient() {
    return directRecipient;
  }

  public void setDirectRecipient(String directRecipient) {
    this.directRecipient = directRecipient;
  }

  public String getAssigningAuthorityId() {
    return assigningAuthorityId;
  }

  public void setAssigningAuthorityId(String assigningAuthorityId) {
    this.assigningAuthorityId = assigningAuthorityId;
  }

  public String getSetId() {
    return setId;
  }

  public void setSetId(String setId) {
    this.setId = setId;
  }

  public int getVersionNumber() {
    return versionNumber;
  }

  public void setVersionNumber(int versionNumber) {
    this.versionNumber = versionNumber;
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
    if (clientSecret != null) {
      return AESEncryption.decrypt(clientSecret);
    } else {
      return null;
    }
  }

  public void setClientSecret(String clientSecret) {
    if (clientSecret != null) this.clientSecret = AESEncryption.encrypt(clientSecret);
  }

  public String getEhrServerURL() {
    return ehrServerURL;
  }

  public void setEhrServerURL(String ehrServerURL) {
    this.ehrServerURL = ehrServerURL;
  }

  public String getAuthUrl() {
    return authUrl;
  }

  public void setAuthUrl(String authUrl) {
    this.authUrl = authUrl;
  }

  public String getTokenUrl() {
    return tokenUrl;
  }

  public void setTokenUrl(String tokenUrl) {
    this.tokenUrl = tokenUrl;
  }

  public String getAccessToken() {
    if (this.getTokenExpiryDateTime() != null) {
      Date currentDate = new Date();
      Date tokenExpirtyDate = this.getTokenExpiryDateTime();
      int value = currentDate.compareTo(tokenExpirtyDate);
      if (value > 0) {
        logger.info("AccessToken is Expired. Getting new AccessToken");
        JSONObject accessTokenObj = new RefreshTokenScheduler().getAccessToken(this);
        return accessTokenObj.getString("access_token");
      } else {
        logger.info("AccessToken is Valid. No need to get new AccessToken");
        return accessToken;
      }
    } else {
      return accessToken;
    }
  }

  public void setAccessToken(String accessToken) {
    this.accessToken = accessToken;
  }

  public String getUserId() {
    return userId;
  }

  public String getStatus() {
    return status;
  }

  public void setUserId(String userId) {
    this.userId = userId;
  }

  public int getExpiry() {
    return expiry;
  }

  public void setExpiry(int expiry) {
    this.expiry = expiry;
  }

  public String getScope() {
    return scope;
  }

  public void setScope(String scope) {
    this.scope = scope;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public Date getStartDate() {
    return startDate;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public Date getEndDate() {
    return endDate;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public Date getTokenExpiryDateTime() {
    return tokenExpiryDateTime;
  }

  public void setTokenExpiryDateTime(Date tokenExpiryDateTime) {
    this.tokenExpiryDateTime = tokenExpiryDateTime;
  }

  public String getRefreshToken() {
    return refreshToken;
  }

  public void setRefreshToken(String refreshToken) {
    this.refreshToken = refreshToken;
  }

  public String getLaunchPatientId() {
    return launchPatientId;
  }

  public void setLaunchPatientId(String launchPatientId) {
    this.launchPatientId = launchPatientId;
  }

  public String getFhirVersion() {
    return fhirVersion;
  }

  public void setFhirVersion(String fhirVersion) {
    this.fhirVersion = fhirVersion;
  }

  public String getEncounterId() {
    return encounterId;
  }

  public void setEncounterId(String encounterId) {
    this.encounterId = encounterId;
  }

  public void setStatus(String stat) {
    this.status = stat;
  }

  public String getDirectUser() {
    return directUser;
  }

  public void setDirectUser(String directUser) {
    this.directUser = directUser;
  }

  public String getDirectPwd() {
    if (directPwd != null) {
      return AESEncryption.decrypt(directPwd);
    } else {
      return null;
    }
  }

  public void setDirectPwd(String directPwd) {
    if (directPwd != null) this.directPwd = AESEncryption.encrypt(directPwd);
  }

  public String getRestAPIURL() {
    return restAPIURL;
  }

  public void setRestAPIURL(String restAPIURL) {
    this.restAPIURL = restAPIURL;
  }

  public String getLaunchId() {
    return launchId;
  }

  public void setLaunchId(String launchId) {
    this.launchId = launchId;
  }

  public int getLaunchState() {
    return launchState;
  }

  public void setLaunchState(int launchState) {
    this.launchState = launchState;
  }

  public String getRedirectURI() {
    return redirectURI;
  }

  public void setRedirectURI(String redirectURI) {
    this.redirectURI = redirectURI;
  }

  public String getAuthorizationCode() {
    return authorizationCode;
  }

  public void setAuthorizationCode(String authorizationCode) {
    this.authorizationCode = authorizationCode;
  }

  public Boolean getIsSystem() {
    return isSystem;
  }

  public void setIsSystem(Boolean isSystem) {
    this.isSystem = isSystem;
  }

  public String getSmtpPort() {
    return smtpPort;
  }

  public void setSmtpPort(String smtpPort) {
    this.smtpPort = smtpPort;
  }

  public String getImapPort() {
    return imapPort;
  }

  public void setImapPort(String imapPort) {
    this.imapPort = imapPort;
  }

  public Boolean getDebugFhirQueryAndEicr() {
    return debugFhirQueryAndEicr;
  }

  public void setDebugFhirQueryAndEicr(Boolean debugFhirQueryAndEicr) {
    this.debugFhirQueryAndEicr = debugFhirQueryAndEicr;
  }

  public Boolean getRequireAud() {
    return requireAud;
  }

  public void setRequireAud(Boolean requireAud) {
    this.requireAud = requireAud;
  }

  public String getxRequestId() {
    return xRequestId;
  }

  public void setxRequestId(String xRequestId) {
    this.xRequestId = xRequestId;
  }

  public Boolean getValidationMode() {
    return validationMode;
  }

  public void setValidationMode(Boolean validationMode) {
    this.validationMode = validationMode;
  }

  public String getLaunchType() {
    return launchType;
  }

  public void setLaunchType(String launchType) {
    this.launchType = launchType;
  }
}
