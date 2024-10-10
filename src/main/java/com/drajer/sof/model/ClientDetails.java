package com.drajer.sof.model;

import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.sof.utils.RefreshTokenScheduler;
import java.time.Instant;
import java.util.Date;
import jakarta.persistence.*;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@Entity
@Table(name = "client_details")
@DynamicUpdate
@JsonIgnoreProperties(ignoreUnknown = true)
public class ClientDetails {

  @Transient private final Logger logger = LoggerFactory.getLogger(ClientDetails.class);

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Integer id;

  @Column(name = "is_provider_launch", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isProvider;

  @Column(name = "is_system_launch", nullable = false, columnDefinition = "int default 1")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isSystem;

  @Column(
      name = "is_multi_tenant_system_launch",
      nullable = true,
      columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isMultiTenantSystemLaunch;

  @Column(name = "is_user_account_launch", nullable = true)
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isUserAccountLaunch;

  @Column(name = "clientId", nullable = false, columnDefinition = "TEXT")
  private String clientId;

  @Column(name = "clientSecret", nullable = true, columnDefinition = "TEXT")
  private String clientSecret;

  @Column(name = "fhir_server_base_url", nullable = false, unique = true)
  private String fhirServerBaseURL;

  @Column(name = "token_url", nullable = true, columnDefinition = "TEXT")
  private String tokenURL;

  @Column(name = "scopes", nullable = false, columnDefinition = "TEXT")
  private String scopes;

  @Column(name = "access_token", nullable = true, columnDefinition = "TEXT")
  private String accessToken;

  @Column(name = "token_expiry", nullable = true, columnDefinition = "int default 0")
  private Integer tokenExpiry;

  @Column(name = "token_expiry_date", nullable = true)
  @Temporal(TemporalType.TIMESTAMP)
  private Date tokenExpiryDateTime;

  @Column(name = "is_direct", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isDirect;

  @Column(name = "is_xdr", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isXdr;

  @Column(name = "is_restapi", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isRestAPI;

  @Column(name = "direct_host", nullable = true, columnDefinition = "TEXT")
  private String directHost;

  @Column(name = "direct_user", nullable = true)
  private String directUser;

  @Column(name = "direct_pwd", nullable = true)
  private String directPwd;

  @Column(name = "smtp_url", nullable = true)
  private String smtpUrl;

  @Column(name = "smtp_port", nullable = true)
  private String smtpPort;

  @Column(name = "imap_url", nullable = true)
  private String imapUrl;

  @Column(name = "imap_port", nullable = true)
  private String imapPort;

  @Column(name = "direct_recipient_address", nullable = true)
  private String directRecipientAddress;

  @Column(name = "xdr_recipient_address", nullable = true)
  private String xdrRecipientAddress;

  @Column(name = "rest_api_url", nullable = true)
  private String restAPIURL;

  @Column(name = "aa_id", nullable = true)
  private String assigningAuthorityId;

  @Column(name = "encounter_start_time", nullable = true)
  private String encounterStartThreshold;

  @Column(name = "encounter_end_time", nullable = true)
  private String encounterEndThreshold;

  @Column(name = "is_covid19", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isCovid;

  @Column(name = "is_full_ecr", nullable = false, columnDefinition = "int default 1")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isFullEcr;

  @Column(
      name = "is_emergent_reporting_enabled",
      nullable = false,
      columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isEmergentReportingEnabled;

  @Column(name = "rrprocessing_createdocRef", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isCreateDocRef;

  @Column(name = "rrprocessing_invokerestapi", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isInvokeRestAPI;

  @Column(name = "rrprocessing_both", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean isBoth;

  @Column(name = "rr_rest_api_url", nullable = true)
  private String rrRestAPIUrl;

  @Column(name = "rr_doc_ref_mime_type", nullable = true)
  private String rrDocRefMimeType;

  @Column(name = "debug_fhir_query_and_eicr", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean debugFhirQueryAndEicr;

  @Column(name = "require_aud", nullable = false, columnDefinition = "int default 0")
  @Convert(converter = org.hibernate.type.NumericBooleanConverter.class)
  private Boolean requireAud = false;

  @Column(name = "last_updated_ts", nullable = false)
  @CreationTimestamp
  private Date lastUpdated;

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public Boolean getIsProvider() {
    return isProvider;
  }

  public void setIsProvider(Boolean isProvider) {
    this.isProvider = isProvider;
  }

  public Boolean getIsSystem() {
    return isSystem;
  }

  public void setIsSystem(Boolean isSystem) {
    this.isSystem = isSystem;
  }

  public Boolean getIsUserAccountLaunch() {
    return isUserAccountLaunch;
  }

  public void setIsUserAccountLaunch(Boolean isUserAccountLaunch) {
    this.isUserAccountLaunch = isUserAccountLaunch;
  }

  public Boolean getIsMultiTenantSystemLaunch() {
    return isMultiTenantSystemLaunch;
  }

  public void setIsMultiTenantSystemLaunch(Boolean isMultiTenantSystemLaunch) {
    this.isMultiTenantSystemLaunch = isMultiTenantSystemLaunch;
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

  public String getAccessToken() {
    if (this.getTokenExpiryDateTime() != null && this.getIsMultiTenantSystemLaunch()) {
      // Retrieve Access token 3 minutes before it expires.
      // 3 minutes is enough buffer for Trigger or Loading query to complete.
      Instant currentInstant = new Date().toInstant().plusSeconds(180);
      Date currentDate = Date.from(currentInstant);
      Date tokenExpiryTime = this.getTokenExpiryDateTime();
      int value = currentDate.compareTo(tokenExpiryTime);
      if (value > 0) {
        logger.info("AccessToken is Expired. Getting new AccessToken");
        JSONObject accessTokenObj =
            new RefreshTokenScheduler().getAccessTokenUsingClientDetails(this);
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

  public Integer getTokenExpiry() {
    return tokenExpiry;
  }

  public void setTokenExpiry(Integer tokenExpiry) {
    this.tokenExpiry = tokenExpiry;
  }

  public Date getTokenExpiryDateTime() {
    return tokenExpiryDateTime;
  }

  public void setTokenExpiryDateTime(Date tokenExpiryDateTime) {
    this.tokenExpiryDateTime = tokenExpiryDateTime;
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

  public Boolean getIsRestAPI() {
    return isRestAPI;
  }

  public void setIsRestAPI(Boolean isRestAPI) {
    this.isRestAPI = isRestAPI;
  }

  public String getRestAPIURL() {
    return restAPIURL;
  }

  public void setRestAPIURL(String restAPIURL) {
    this.restAPIURL = restAPIURL;
  }

  public void setIsXdr(Boolean isXdr) {
    this.isXdr = isXdr;
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
    if (directPwd != null && !StringUtils.isEmpty(directPwd)) {
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

  public Boolean getIsCovid() {
    return isCovid;
  }

  public void setIsCovid(Boolean isCovid) {
    this.isCovid = isCovid;
  }

  public Boolean getIsFullEcr() {
    return isFullEcr;
  }

  public void setIsFullEcr(Boolean isFullEcr) {
    this.isFullEcr = isFullEcr;
  }

  public Boolean getIsCreateDocRef() {
    return isCreateDocRef;
  }

  public void setIsCreateDocRef(Boolean isCreateDocRef) {
    this.isCreateDocRef = isCreateDocRef;
  }

  public Boolean getIsInvokeRestAPI() {
    return isInvokeRestAPI;
  }

  public void setIsInvokeRestAPI(Boolean isInvokeRestAPI) {
    this.isInvokeRestAPI = isInvokeRestAPI;
  }

  public Boolean getIsBoth() {
    return isBoth;
  }

  public void setIsBoth(Boolean isBoth) {
    this.isBoth = isBoth;
  }

  public String getRrRestAPIUrl() {
    return rrRestAPIUrl;
  }

  public void setRrRestAPIUrl(String rrRestAPIUrl) {
    this.rrRestAPIUrl = rrRestAPIUrl;
  }

  public String getRrDocRefMimeType() {
    return rrDocRefMimeType;
  }

  public void setRrDocRefMimeType(String rrDocRefMimeType) {
    this.rrDocRefMimeType = rrDocRefMimeType;
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

  public Date getLastUpdated() {
    return lastUpdated;
  }

  public void setLastUpdated(Date lastUpdated) {
    this.lastUpdated = lastUpdated;
  }

  public Boolean getIsEmergentReportingEnabled() {
    return isEmergentReportingEnabled;
  }

  public void setIsEmergentReportingEnabled(Boolean isEmergentReportingEnabled) {
    this.isEmergentReportingEnabled = isEmergentReportingEnabled;
  }

  public void print() {

    logger.info(" **** Printing Client Details **** ");

    logger.info(" Id = {}", id);
    logger.info(" Provider Launch = {}", isProvider);
    logger.info(" System Launch = {}", isSystem);
    logger.info(" Client Id = {}", clientId);
    logger.info(" FHIR Server URL = {}", fhirServerBaseURL);
    logger.info(" Token URL = {}", tokenURL);
    logger.info(" Scopes = {}", scopes);
    logger.info(" Is Direct ? = {}", isDirect);
    logger.info(" Is XDR = {}", isXdr);
    logger.info(" Is Rest API = {}", isRestAPI);
    logger.info(" Direct Host = {}", directHost);
    logger.info(" Direct Recipient Address = {}", directRecipientAddress);
    logger.info(" XDR Recipient Address = {}", xdrRecipientAddress);
    logger.info(" Rest API URL = {}", restAPIURL);
    logger.info(" Assigning Authority Id = {}", assigningAuthorityId);
    logger.info(" Encounter Start Threshold = {}", encounterStartThreshold);
    logger.info(" Encounter End Threshold = {}", encounterEndThreshold);
    logger.info(" Is Covid = {}", isCovid);
    logger.info(" Is Full ECR = {}", isFullEcr);
    logger.info(" Is Aud required = {}", requireAud);
    logger.info(" Debug Fhir Query And Eicr {}", debugFhirQueryAndEicr);

    logger.info(" **** End Printing Client Details **** ");
  }
}
