package com.drajer.sof.model;

import com.drajer.ecrapp.security.AESEncryption;
import java.util.Date;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ClientDetailsDTO {

  private final Logger logger = LoggerFactory.getLogger(ClientDetailsDTO.class);

  private Integer id;

  private Boolean isProvider;

  private Boolean isSystem;

  private Boolean isMultiTenantSystemLaunch;

  private Boolean isUserAccountLaunch;

  private String clientId;

  private String clientSecret;

  private String fhirServerBaseURL;

  private String tokenURL;

  private String scopes;

  private String accessToken;

  private int tokenExpiry;

  private Date tokenExpiryDateTime;

  private Boolean isDirect;

  private Boolean isXdr;

  private Boolean isRestAPI;

  private String directHost;

  private String directUser;

  private String directPwd;

  private String smtpUrl;

  private String smtpPort;

  private String imapUrl;

  private String imapPort;

  private String directRecipientAddress;

  private String xdrRecipientAddress;

  private String restAPIURL;

  private String assigningAuthorityId;

  private String encounterStartThreshold;

  private String encounterEndThreshold;

  private Boolean isCovid;

  private Boolean isEmergentReportingEnabled;

  private Boolean isFullEcr;

  private Boolean isCreateDocRef;

  private Boolean isInvokeRestAPI;

  private Boolean isBoth;

  private String rrRestAPIUrl;

  private String rrDocRefMimeType;

  private Boolean debugFhirQueryAndEicr;

  private Boolean requireAud = false;

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

  public Boolean getIsUserAccountLaunch() {
    return isUserAccountLaunch;
  }

  public void setIsUserAccountLaunch(Boolean isUserAccountLaunch) {
    this.isUserAccountLaunch = isUserAccountLaunch;
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
    return accessToken;
  }

  public void setAccessToken(String accessToken) {
    this.accessToken = accessToken;
  }

  public int getTokenExpiry() {
    return tokenExpiry;
  }

  public void setTokenExpiry(int tokenExpiry) {
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
