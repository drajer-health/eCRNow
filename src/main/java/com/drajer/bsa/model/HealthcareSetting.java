package com.drajer.bsa.model;

import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.ecrapp.security.AESEncryption;
import java.util.Date;
import java.util.Set;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 * <h1>HealthareSettings</h1>
 *
 * The Entity represents a Practice location such as a doctor's office, or a hospital or a network
 * of providers. HealthcareSettings employ EHRs and each EHR is expected to have a FHIR Server. A
 * set of practice locations may use a single EHR instance hosted in a cloud or in a data center. A
 * HealthcareSetting is expected to be created by the administrator for each EHR instance. This EHR
 * instance can be dedicated to a single health care setting or can be shared by multiple health
 * care settings.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
@Entity
@Table(name = "healthcare_setting")
@DynamicUpdate
public class HealthcareSetting implements FhirServerDetails {

  @Transient private final Logger logger = LoggerFactory.getLogger(HealthcareSetting.class);

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
  @Column(name = "clientSecret", nullable = true, columnDefinition = "TEXT")
  private String clientSecret;

  /**
   * The attribute represents the FHIR Server URL for the HealthcareSetting. This is unique for the
   * entire table.
   */
  @Column(name = "fhir_server_base_url", nullable = false, unique = true)
  private String fhirServerBaseURL;

  /** The attribute represents the FHIR Server version of the specification supported. */
  @Column(name = "fhir_version", nullable = true, unique = false)
  private String fhirVersion;

  /**
   * The attribute represents the Token URL for requesting access tokens as part of SMART on FHIR
   * Authorization. This is provided to override what is present in the CapabilityStatement.
   */
  @Column(name = "token_url", nullable = true, columnDefinition = "TEXT")
  private String tokenUrl;

  /**
   * The attribute represents the scopes for which permission is requested during the SMART on FHIR
   * Authorization. This is provided to override what is present in the CapabilityStatement.
   */
  @Column(name = "scopes", nullable = false, columnDefinition = "TEXT")
  private String scopes;

  /**
   * The attribute represents the Default Provider ID that has to be used for sending or receiving
   * messages and posting to EHR by some EHR vendors.
   */
  @Column(name = "default_provider_id", nullable = true, columnDefinition = "TEXT")
  private String defaultProviderId;

  /*
   * The flag that indicates that Direct Transport has to be used.
   */
  @Column(name = "is_direct", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isDirect;

  /*
   * The flag that indicates that XDR Transport has to be used.
   */
  @Column(name = "is_xdr", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isXdr = false;

  /*
   * The flag that indicates that RESTful API has to be used.
   */
  @Column(name = "is_restapi", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isRestAPI;

  /*
   * The flag that indicates that FHIR API has to be used.
   */
  @Column(name = "is_fhir", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean fhirAPI;

  /*
   * The domain of the direct server, (for example abc.direct.org) that will be used as the HISP to send the direct messages.
   */
  @Column(name = "direct_host", nullable = true, columnDefinition = "TEXT")
  private String directHost;

  /*
   * The user name (mail Id) (for example user1@abc.direct.org) that will be used as the
   * sending address for the message.
   */
  @Column(name = "direct_user", nullable = true)
  private String directUser;

  /*
   * The password for the above user that will be used to connect to the direct server
   * using the directUser to be able send via an SMTP protocol.
   */
  @Column(name = "direct_pwd", nullable = true)
  private String directPwd;

  /*
   * The port for the SMTP connection. Default would be port 25, but many HISPs use 465
   */
  @Column(name = "smtp_port", nullable = true)
  private String smtpPort;

  /*
   * The Url of the SMTP URL (for e.g smtp.abc.direct.org) that will be used to send messages via the HISP.
   */
  @Column(name = "smtp_url", nullable = true)
  private String smtpUrl;

  /*
   * The port for the IMAP connection. Default would be port 143, but many HISPs use 993
   */
  @Column(name = "imap_port", nullable = true)
  private String imapPort;

  /*
   * The Url of the IMAP URL (for e.g impa.abc.direct.org) that will be used to receive messages from the HISP.
   */
  @Column(name = "imap_url", nullable = true)
  private String imapUrl;

  /*
   * The port for the POP connection.
   */
  @Column(name = "pop_port", nullable = true)
  private String popPort;

  /*
   * The Url of the POP URL (for e.g pop.abc.direct.org) that will be used to receive messages from the HISP.
   */
  @Column(name = "pop_url", nullable = true)
  private String popUrl;

  /*
   * The address to which the Direct messages have to be sent.
   */
  @Column(name = "direct_recipient_address", nullable = true)
  private String directRecipientAddress;

  /*
   * The TLS version to be used for Direct SMTP related protocols
   */
  @Column(name = "smtp_tls_version", nullable = true)
  private String directTlsVersion;

  /*
   * The address to which the XDR payload has to be sent.
   */
  @Column(name = "xdr_recipient_address", nullable = true)
  private String xdrRecipientAddress;

  /**
   * This attribute represents the intermediary URL where the messages have to be sent when
   * populated. IF this is not populated the Recipients Address from the Knowledge Artifact will be
   * used.
   */
  @Column(name = "rest_api_url", nullable = true)
  private String restApiUrl;

  /**
   * This attribute represents the value for creating a document reference in the EHR, when a
   * response is received.
   */
  @Column(name = "create_doc_ref_response", nullable = true)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean createDocRefForResponse;

  /**
   * This attribute represents the Mime Type to be used to persist an XML document in the Document
   * Reference object.
   */
  @Column(name = "doc_ref_mime_type", nullable = true)
  private String docRefMimeType;

  /**
   * This attribute represents the intermediary URL where the responses have to be sent when this is
   * populated. IF this is not populated then the response is either persisted as a
   * DocumentReference or it will be persisted in the ecrNow DB.
   */
  @Column(name = "response_rest_api_url", nullable = true)
  private String handOffResponseToRestApi;

  /*
   * The Assigning Authority ID (OID) that would be used to create the CDA documents.
   */
  @Column(name = "aa_id", nullable = true)
  private String assigningAuthorityId;

  /**
   * This attribute represents the lower threshold time to be used for retrieving FHIR Resources
   * based on notifications. For example, when this is set to 2, the application can look at all
   * FHIR Resources updated within time (T) - 2 hours. Where T is the time of the subscription
   * notification.
   */
  @Column(name = "encounter_start_time", nullable = true)
  private String encounterStartThreshold;

  /**
   * This attribute represents the higher threshold time to be used for retrieving FHIR Resources
   * based on notifications. For example, when this is set to 3, the application can look at all
   * FHIR Resources updated within time (T) + 3 hours. Where T is the time of the subscription
   * notification.
   */
  @Column(name = "encounter_end_time", nullable = true)
  private String encounterEndThreshold;

  /**
   * This attribute represents the list of Knowledge Artifacts that are currently active for the
   * HealthcareSetting. This is populated using the HealthcareSettingOperationalKnowledgeArtifacts
   * class which is converted to a JSON string and stored.
   */
  @Column(name = "kars_active", nullable = true, columnDefinition = "TEXT")
  private String karsActive;

  /** This attribute represents the type of authentication to be used by the healthcare setting. */
  @Column(name = "auth_type", nullable = false, columnDefinition = "TEXT")
  private String authType;

  /**
   * The access token after authorization to the EHR, this is used with EHRs who are cloud based and
   * a single access token is used across all healthcare settings. Access token calls are expensive
   * and not needed in these cases and hence it will be stored in HealthcareSetting based on
   * AuthType.
   */
  @Column(name = "ehr_access_token", nullable = true, columnDefinition = "TEXT")
  private String ehrAccessToken;

  /** The expiry duration in seconds for the EHR Access Token */
  @Column(name = "ehr_access_token_expiry_duration")
  private int ehrAccessTokenExpiryDuration;

  /** The expiration time for EHR Access Token. */
  @Column(name = "token_expiry_date", nullable = true)
  @Temporal(TemporalType.TIMESTAMP)
  private Date ehrAccessTokenExpirationTime;

  @Column(name = "require_aud", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean requireAud = false;

  @Column(name = "ehr_supports_subscriptions", nullable = true)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean ehrSupportsSubscriptions = false;

  /**
   * This attribute defines the trusted third party end point that reports should be submitted to.
   */
  @Column(name = "trusted_third_party", nullable = true, columnDefinition = "TEXT")
  private String trustedThirdParty;

  /** This attribute defines the PHA end point that reports should be submitted to. */
  @Column(name = "pha_url", nullable = true, columnDefinition = "TEXT")
  private String phaUrl;

  /** The attribute represents the default name of the organization. */
  @Column(name = "org_name", nullable = true, columnDefinition = "TEXT")
  private String orgName;

  /**
   * The attribute represents the default namespace for the "orgId" identifier value. For CDA based
   * reporting, the Assigning Authority Id will be used. For FHIR based systems, if a URL is
   * provided, it can be stored in this attribute.
   */
  @Column(name = "org_id_system", nullable = true, columnDefinition = "TEXT")
  private String orgIdSystem;

  /** The attribute represents a unique identifier for the organization */
  @Column(name = "org_id", nullable = true, columnDefinition = "TEXT")
  private String orgId;

  /**
   * The attribute represents the time when the offhours timers can be scheduled for the healthcare
   * setting. This is the hours component in 0 to 24 hour format.
   */
  @Column(name = "off_hours_start", nullable = true, columnDefinition = "int")
  private Integer offHoursStart;

  /** The attribute represents the minutes component for the off hours start hour component. */
  @Column(name = "off_hours_start_min", nullable = true, columnDefinition = "int")
  private Integer offHoursStartMin;

  /**
   * The attribute represents the time when the offhours timers should end for the healthcare
   * setting. This is the hours component in 0 to 24 hour format.
   */
  @Column(name = "off_hours_end", nullable = true, columnDefinition = "int")
  private Integer offHoursEnd;

  /** The attribute represents the minutes component for the off hours end hour component. */
  @Column(name = "off_hours_end_min", nullable = true, columnDefinition = "int")
  private Integer offHoursEndMin;

  /** The attribute represents the timezone for the offhours. Currently only UTC is supported. */
  @Column(name = "off_hours_timezone", nullable = true, columnDefinition = "TEXT")
  private String offHoursTimezone;

  @Column(name = "off_hours_enabled", nullable = true)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean offhoursEnabled = true;

  /** The attribute represents a username that may be used with password-based Authorization. */
  @Column(name = "username", columnDefinition = "TEXT")
  private String username;

  /**
   * The attribute represents a password associated with the username for password-based
   * authorization.
   */
  @Column(name = "password", columnDefinition = "TEXT")
  private String password;

  /** The attribute represents the Key Alias for the Private Key to be used for signing. */
  @Column(name = "backend_auth_key_alias", columnDefinition = "TEXT")
  private String backendAuthKeyAlias;

  @Column(name = "debug_enabled", nullable = true)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean debugEnabled = true;

  /** This attribute represents the last time when the object was updated. */
  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  /**
   * This is an object that is used for convenience to exchange the information as objects. The data
   * itself gets stored in the karsActive attribute in the database.
   */
  @Transient private HealthcareSettingOperationalKnowledgeArtifacts kars;

  public Boolean getDebugEnabled() {
    return debugEnabled;
  }

  public void setDebugEnabled(Boolean debugEnabled) {
    this.debugEnabled = debugEnabled;
  }

  public String getKarsActive() {
    return karsActive;
  }

  public void setKarsActive(String karsActive) {
    this.karsActive = karsActive;
  }

  public HealthcareSettingOperationalKnowledgeArtifacts getKars() {
    return kars;
  }

  public void setKars(HealthcareSettingOperationalKnowledgeArtifacts kars) {
    this.kars = kars;
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

  public String getFhirServerBaseURL() {
    return fhirServerBaseURL;
  }

  public void setFhirServerBaseURL(String fhirServerBaseURL) {
    this.fhirServerBaseURL = fhirServerBaseURL;
  }

  public String getTokenUrl() {
    return tokenUrl;
  }

  public void setTokenUrl(String tokenUrl) {
    this.tokenUrl = tokenUrl;
  }

  public String getScopes() {
    return scopes;
  }

  public void setScopes(String scopes) {
    this.scopes = scopes;
  }

  public Boolean getIsDirect() {
    return isDirect;
  }

  public void setIsDirect(Boolean isDirect) {
    this.isDirect = isDirect;
  }

  public Boolean getIsXdr() {
    return isXdr;
  }

  public void setIsXdr(Boolean isXdr) {
    this.isXdr = isXdr;
  }

  public Boolean getIsRestAPI() {
    return isRestAPI;
  }

  public void setIsRestAPI(Boolean isRestAPI) {
    this.isRestAPI = isRestAPI;
  }

  public String getDirectHost() {
    return directHost;
  }

  public void setDirectHost(String directHost) {
    this.directHost = directHost;
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

  public String getDirectRecipientAddress() {
    return directRecipientAddress;
  }

  public void setDirectRecipientAddress(String directRecipientAddress) {
    this.directRecipientAddress = directRecipientAddress;
  }

  public String getXdrRecipientAddress() {
    return xdrRecipientAddress;
  }

  public void setXdrRecipientAddress(String xdrRecipientAddress) {
    this.xdrRecipientAddress = xdrRecipientAddress;
  }

  public String getRestApiUrl() {
    return restApiUrl;
  }

  public void setRestApiUrl(String rul) {
    this.restApiUrl = rul;
  }

  public String getAssigningAuthorityId() {
    return assigningAuthorityId;
  }

  public void setAssigningAuthorityId(String assigningAuthorityId) {
    this.assigningAuthorityId = assigningAuthorityId;
  }

  public String getEncounterStartThreshold() {
    return encounterStartThreshold;
  }

  public void setEncounterStartThreshold(String encounterStartThreshold) {
    this.encounterStartThreshold = encounterStartThreshold;
  }

  public String getEncounterEndThreshold() {
    return encounterEndThreshold;
  }

  public void setEncounterEndThreshold(String encounterEndThreshold) {
    this.encounterEndThreshold = encounterEndThreshold;
  }

  public String getAuthType() {
    return authType;
  }

  public void setAuthType(String authType) {
    this.authType = authType;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public Boolean getRequireAud() {
    return requireAud;
  }

  public void setRequireAud(Boolean requireAud) {
    this.requireAud = requireAud;
  }

  public String getFhirVersion() {
    return fhirVersion;
  }

  public void setFhirVersion(String fhirVersion) {
    this.fhirVersion = fhirVersion;
  }

  public Boolean getFhirAPI() {
    return fhirAPI;
  }

  public void setFhirAPI(Boolean fhirAPI) {
    this.fhirAPI = fhirAPI;
  }

  public String getSmtpUrl() {
    return smtpUrl;
  }

  public void setSmtpUrl(String smtpUrl) {
    this.smtpUrl = smtpUrl;
  }

  public String getImapUrl() {
    return imapUrl;
  }

  public void setImapUrl(String imapUrl) {
    this.imapUrl = imapUrl;
  }

  public String getPopPort() {
    return popPort;
  }

  public void setPopPort(String popPort) {
    this.popPort = popPort;
  }

  public String getPopUrl() {
    return popUrl;
  }

  public void setPopUrl(String popUrl) {
    this.popUrl = popUrl;
  }

  public String getDefaultProviderId() {
    return defaultProviderId;
  }

  public void setDefaultProviderId(String defaultProviderId) {
    this.defaultProviderId = defaultProviderId;
  }

  public String getDocRefMimeType() {
    return docRefMimeType;
  }

  public void setDocRefMimeType(String docRefMimeType) {
    this.docRefMimeType = docRefMimeType;
  }

  public Boolean getCreateDocRefForResponse() {
    return createDocRefForResponse;
  }

  public void setCreateDocRefForResponse(Boolean createDocRefForResponse) {
    this.createDocRefForResponse = createDocRefForResponse;
  }

  public String getHandOffResponseToRestApi() {
    return handOffResponseToRestApi;
  }

  public void setHandOffResponseToRestApi(String handOffResponseToRestApi) {
    this.handOffResponseToRestApi = handOffResponseToRestApi;
  }

  public int getEhrAccessTokenExpiryDuration() {
    return ehrAccessTokenExpiryDuration;
  }

  public void setEhrAccessTokenExpiryDuration(int ehrAccessTokenExpiryDuration) {
    this.ehrAccessTokenExpiryDuration = ehrAccessTokenExpiryDuration;
  }

  public String getEhrAccessToken() {
    return ehrAccessToken;
  }

  public void setEhrAccessToken(String ehrAccessToken) {
    this.ehrAccessToken = ehrAccessToken;
  }

  public Date getEhrAccessTokenExpirationTime() {
    return ehrAccessTokenExpirationTime;
  }

  public void setEhrAccessTokenExpirationTime(Date ehrAccessTokenExpirationTime) {
    this.ehrAccessTokenExpirationTime = ehrAccessTokenExpirationTime;
  }

  public String getTrustedThirdParty() {
    return trustedThirdParty;
  }

  public void setTrustedThirdParty(String trustedThirdParty) {
    this.trustedThirdParty = trustedThirdParty;
  }

  public String getOrgName() {
    return orgName;
  }

  public void setOrgName(String orgName) {
    this.orgName = orgName;
  }

  public String getOrgIdSystem() {
    return orgIdSystem;
  }

  public void setOrgIdSystem(String orgIdSystem) {
    this.orgIdSystem = orgIdSystem;
  }

  public String getOrgId() {
    return orgId;
  }

  public void setOrgId(String orgId) {
    this.orgId = orgId;
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

  public String getBackendAuthKeyAlias() {
    return backendAuthKeyAlias;
  }

  public void setBackendAuthKeyAlias(String backendAuthKeyAlias) {
    this.backendAuthKeyAlias = backendAuthKeyAlias;
  }

  public Boolean getEhrSupportsSubscriptions() {
    return ehrSupportsSubscriptions;
  }

  public void setEhrSupportsSubscriptions(Boolean ehrSupportsSubscriptions) {
    this.ehrSupportsSubscriptions = ehrSupportsSubscriptions;
  }

  public String getPhaUrl() {
    return phaUrl;
  }

  public void setPhaUrl(String phaUrl) {
    this.phaUrl = phaUrl;
  }

  public String getOffHoursTimezone() {
    return offHoursTimezone;
  }

  public void setOffHoursTimezone(String offHoursTimezone) {
    this.offHoursTimezone = offHoursTimezone;
  }

  public Boolean getOffhoursEnabled() {
    return offhoursEnabled;
  }

  public void setOffhoursEnabled(Boolean offhoursEnabled) {
    this.offhoursEnabled = offhoursEnabled;
  }

  public Integer getOffHoursStartMin() {
    return offHoursStartMin;
  }

  public void setOffHoursStartMin(Integer offHoursStartMin) {
    this.offHoursStartMin = offHoursStartMin;
  }

  public Integer getOffHoursEnd() {
    return offHoursEnd;
  }

  public void setOffHoursEnd(Integer offHoursEnd) {
    this.offHoursEnd = offHoursEnd;
  }

  public Integer getOffHoursEndMin() {
    return offHoursEndMin;
  }

  public void setOffHoursEndMin(Integer offHoursEndMin) {
    this.offHoursEndMin = offHoursEndMin;
  }

  public Integer getOffHoursStart() {
    return offHoursStart;
  }

  public void setOffHoursStart(Integer offHoursStart) {
    this.offHoursStart = offHoursStart;
  }

  public String getDirectTlsVersion() {
    return directTlsVersion;
  }

  public void setDirectTlsVersion(String directTlsVersion) {
    this.directTlsVersion = directTlsVersion;
  }

  public KnowledgeArtifactStatus getArtifactStatus(String uniqueUrl) {

    HealthcareSettingOperationalKnowledgeArtifacts arts = getKars();

    if (arts != null) {

      Set<KnowledgeArtifactStatus> stats = arts.getArtifactStatus();

      if (stats != null) {

        for (KnowledgeArtifactStatus s : stats) {

          if (s.getVersionUniqueKarId().equals(uniqueUrl)) {
            logger.debug(" Found the version specific Kar");
            return s;
          }
        }
      } else {
        logger.error(" Unable to retrieve Knowlege Artifact Status, hence cannot proceed.");
      }
    } else {
      logger.error(" Unable to retrieve Operational Knowledge Artifacts, hence cannot proceed.");
    }

    return null;
  }

  public void log() {

    logger.info(" **** Printing HealthcareSetting Details **** ");

    logger.info(" Id : {}", id);
    logger.info(" Client Id : {}", clientId);
    logger.info(" FHIR Server URL : {}", fhirServerBaseURL);
    logger.info(" Token URL : {}", tokenUrl);
    logger.info(" Rest API URL : {}", restApiUrl);
    logger.info(" Encounter Start Threshold : {}", encounterStartThreshold);
    logger.info(" Encounter End Threshold : {}", encounterEndThreshold);
    logger.info(" KnowledgArtifacts Active : {}", karsActive);
    logger.info(" Healthcare Setting Direct Protocol : {}", directTlsVersion);

    if (kars != null) kars.log();

    logger.info(" **** End Printing HealthcareSetting Details **** ");
  }
}
