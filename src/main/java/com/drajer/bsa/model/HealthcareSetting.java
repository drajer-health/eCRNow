package com.drajer.bsa.model;

import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
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

  /**
   * The attribute represents the Token URL for requesting access tokens as part of SMART on FHIR
   * Authorization. This is provided to override what is present in the CapabilityStatement.
   */
  @Column(name = "token_url", nullable = true, columnDefinition = "TEXT")
  private String tokenURL;

  /**
   * The attribute represents the scopes for which permission is requested during the SMART on FHIR
   * Authorization. This is provided to override what is present in the CapabilityStatement.
   */
  @Column(name = "scopes", nullable = false, columnDefinition = "TEXT")
  private String scopes;

  /**
   * This attribute represents the intermediary URL where the messages have to be sent when
   * populated. IF this is not populated the Recipients Address from the Knowledge Artifact will be
   * used.
   */
  @Column(name = "rest_api_url", nullable = true)
  private String restApiUrl;

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

  /** This attribute represents the last time when the object was updated. */
  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

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
    return clientSecret;
  }

  public void setClientSecret(String clientSecret) {
    this.clientSecret = clientSecret;
  }

  public String getFhirServerBaseURL() {
    return fhirServerBaseURL;
  }

  public void setFhirServerBaseURL(String fhirServerBaseURL) {
    this.fhirServerBaseURL = fhirServerBaseURL;
  }

  public String getTokenURL() {
    return tokenURL;
  }

  public void setTokenURL(String tokenURL) {
    this.tokenURL = tokenURL;
  }

  public String getScopes() {
    return scopes;
  }

  public void setScopes(String scopes) {
    this.scopes = scopes;
  }

  public String getRestApiUrl() {
    return restApiUrl;
  }

  public void setRestApiUrl(String rul) {
    this.restApiUrl = rul;
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

  public BsaTypes.AuthenticationType getAuthType() {
    return authType;
  }

  public void setAuthType(BsaTypes.AuthenticationType authType) {
    this.authType = authType;
  }

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public void log() {

    logger.info(" **** Printing HealthcareSetting Details **** ");

    logger.info(" Id : {}", id);
    logger.info(" Client Id : {}", clientId);
    logger.info(" FHIR Server URL : {}", fhirServerBaseURL);
    logger.info(" Token URL : {}", tokenURL);
    logger.info(" Rest API URL : {}", restApiUrl);
    logger.info(" Encounter Start Threshold : {}", encounterStartThreshold);
    logger.info(" Encounter End Threshold : {}", encounterEndThreshold);
    logger.info(" KnowledgArtifacts Active : {}", karsActive);

    if (kars != null) kars.log();

    logger.info(" **** End Printing HealthcareSetting Details **** ");
  }
}
