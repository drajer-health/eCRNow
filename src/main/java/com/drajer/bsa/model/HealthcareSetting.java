package com.drajer.bsa.model;

import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.ecrapp.security.AESEncryption;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
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
public class HealthcareSetting {

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

  @Column(name = "is_direct", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isDirect;

  @Column(name = "is_xdr", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isXdr;

  @Column(name = "is_restapi", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean isRestAPI;

  @Column(name = "direct_host", nullable = true, columnDefinition = "TEXT")
  private String directHost;

  @Column(name = "direct_user", nullable = true)
  private String directUser;

  @Column(name = "direct_pwd", nullable = true)
  private String directPwd;

  @Column(name = "smtp_port", nullable = true)
  private String smtpPort;

  @Column(name = "imap_port", nullable = true)
  private String imapPort;

  @Column(name = "direct_recipient_address", nullable = true)
  private String directRecipientAddress;

  @Column(name = "xdr_recipient_address", nullable = true)
  private String xdrRecipientAddress;

  /**
   * This attribute represents the intermediary URL where the messages have to be sent when
   * populated. IF this is not populated the Recipients Address from the Knowledge Artifact will be
   * used.
   */
  @Column(name = "rest_api_url", nullable = true)
  private String restApiUrl;

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

  @Column(name = "require_aud", nullable = false)
  @Type(type = "org.hibernate.type.NumericBooleanType")
  private Boolean requireAud = false;

  /** This attribute represents the last time when the object was updated. */
  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  /** The attribute represents the name of the organization. */
  @Column(name = "orgName", nullable = true, columnDefinition = "TEXT")
  private String orgName;

  /** The attribute represents the namespace for the "orgId" identifier value . */
  @Column(name = "orgIdSystem", nullable = false, columnDefinition = "TEXT")
  private String orgIdSystem;

  /** The attribute represents a unique identifier for the organization */
  @Column(name = "orgId", nullable = false, columnDefinition = "TEXT")
  private String orgId;

  /**
   * This is an object that is used for convenience to exchange the information as objects. The data
   * itself gets stored in the karsActive attribute in the database.
   */
  @Transient private HealthcareSettingOperationalKnowledgeArtifacts kars;

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

    if (kars != null) kars.log();

    logger.info(" **** End Printing HealthcareSetting Details **** ");
  }
}
