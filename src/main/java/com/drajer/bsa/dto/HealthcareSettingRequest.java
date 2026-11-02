package com.drajer.bsa.dto;

import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.model.HealthcareSetting;
import java.util.Date;

/**
 * Request body for creating or updating a {@link HealthcareSetting}. It accepts the same JSON
 * properties as the entity, so the API contract is unchanged, and is converted to the entity with
 * {@link #toEntity()} so the persistence class is never bound directly from the request.
 */
public class HealthcareSettingRequest {

  private Boolean debugEnabled = true;
  private String karsActive;
  private HealthcareSettingOperationalKnowledgeArtifacts kars;
  private Integer id;
  private String clientId;
  private String clientSecret;
  private String fhirServerBaseURL;
  private String tokenUrl;
  private String scopes;
  private Boolean isDirect;
  private Boolean isXdr = false;
  private Boolean isRestAPI;
  private String directHost;
  private String directUser;
  private String directPwd;
  private String smtpPort;
  private String imapPort;
  private String directRecipientAddress;
  private String xdrRecipientAddress;
  private String restApiUrl;
  private String assigningAuthorityId;
  private String encounterStartThreshold;
  private String encounterEndThreshold;
  private String authType;
  private Date lastUpdated;
  private Boolean requireAud = false;
  private String fhirVersion;
  private Boolean fhirAPI;
  private String smtpUrl;
  private String imapUrl;
  private String popPort;
  private String popUrl;
  private String defaultProviderId;
  private String docRefMimeType;
  private Boolean createDocRefForResponse;
  private String handOffResponseToRestApi;
  private int ehrAccessTokenExpiryDuration;
  private String ehrAccessToken;
  private Date ehrAccessTokenExpirationTime;
  private String trustedThirdParty;
  private String orgName;
  private String orgIdSystem;
  private String orgId;
  private String username;
  private String password;
  private String backendAuthKeyAlias;
  private Boolean ehrSupportsSubscriptions = false;
  private String phaUrl;
  private String offHoursTimezone;
  private Boolean offhoursEnabled = true;
  private Integer offHoursStartMin;
  private Integer offHoursEnd;
  private Integer offHoursEndMin;
  private Integer offHoursStart;
  private String directTlsVersion;
  private String backendAuthAlg;
  private String backendAuthKid;
  private String directEndpointCertificateNameOrAlias;
  private Boolean smtpAuthEnabled = false;
  private Boolean smtpSslEnabled = false;

  /** Creates a new HealthcareSetting carrying every value of this request. */
  public HealthcareSetting toEntity() {
    HealthcareSetting entity = new HealthcareSetting();
    entity.setDebugEnabled(debugEnabled);
    entity.setKarsActive(karsActive);
    entity.setKars(kars);
    entity.setId(id);
    entity.setClientId(clientId);
    entity.setClientSecret(clientSecret);
    entity.setFhirServerBaseURL(fhirServerBaseURL);
    entity.setTokenUrl(tokenUrl);
    entity.setScopes(scopes);
    entity.setIsDirect(isDirect);
    entity.setIsXdr(isXdr);
    entity.setIsRestAPI(isRestAPI);
    entity.setDirectHost(directHost);
    entity.setDirectUser(directUser);
    entity.setDirectPwd(directPwd);
    entity.setSmtpPort(smtpPort);
    entity.setImapPort(imapPort);
    entity.setDirectRecipientAddress(directRecipientAddress);
    entity.setXdrRecipientAddress(xdrRecipientAddress);
    entity.setRestApiUrl(restApiUrl);
    entity.setAssigningAuthorityId(assigningAuthorityId);
    entity.setEncounterStartThreshold(encounterStartThreshold);
    entity.setEncounterEndThreshold(encounterEndThreshold);
    entity.setAuthType(authType);
    entity.setLastUpdated(lastUpdated);
    entity.setRequireAud(requireAud);
    entity.setFhirVersion(fhirVersion);
    entity.setFhirAPI(fhirAPI);
    entity.setSmtpUrl(smtpUrl);
    entity.setImapUrl(imapUrl);
    entity.setPopPort(popPort);
    entity.setPopUrl(popUrl);
    entity.setDefaultProviderId(defaultProviderId);
    entity.setDocRefMimeType(docRefMimeType);
    entity.setCreateDocRefForResponse(createDocRefForResponse);
    entity.setHandOffResponseToRestApi(handOffResponseToRestApi);
    entity.setEhrAccessTokenExpiryDuration(ehrAccessTokenExpiryDuration);
    entity.setEhrAccessToken(ehrAccessToken);
    entity.setEhrAccessTokenExpirationTime(ehrAccessTokenExpirationTime);
    entity.setTrustedThirdParty(trustedThirdParty);
    entity.setOrgName(orgName);
    entity.setOrgIdSystem(orgIdSystem);
    entity.setOrgId(orgId);
    entity.setUsername(username);
    entity.setPassword(password);
    entity.setBackendAuthKeyAlias(backendAuthKeyAlias);
    entity.setEhrSupportsSubscriptions(ehrSupportsSubscriptions);
    entity.setPhaUrl(phaUrl);
    entity.setOffHoursTimezone(offHoursTimezone);
    entity.setOffhoursEnabled(offhoursEnabled);
    entity.setOffHoursStartMin(offHoursStartMin);
    entity.setOffHoursEnd(offHoursEnd);
    entity.setOffHoursEndMin(offHoursEndMin);
    entity.setOffHoursStart(offHoursStart);
    entity.setDirectTlsVersion(directTlsVersion);
    entity.setBackendAuthAlg(backendAuthAlg);
    entity.setBackendAuthKid(backendAuthKid);
    entity.setDirectEndpointCertificateNameOrAlias(directEndpointCertificateNameOrAlias);
    entity.setSmtpAuthEnabled(smtpAuthEnabled);
    entity.setSmtpSslEnabled(smtpSslEnabled);
    return entity;
  }

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
    return directPwd;
  }

  public void setDirectPwd(String directPwd) {
    this.directPwd = directPwd;
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

  public void setRestApiUrl(String restApiUrl) {
    this.restApiUrl = restApiUrl;
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

  public String getDirectEndpointCertificateNameOrAlias() {
    return directEndpointCertificateNameOrAlias;
  }

  public void setDirectEndpointCertificateNameOrAlias(String directEndpointCertificateNameOrAlias) {
    this.directEndpointCertificateNameOrAlias = directEndpointCertificateNameOrAlias;
  }

  public Boolean getSmtpAuthEnabled() {
    return smtpAuthEnabled;
  }

  public void setSmtpAuthEnabled(Boolean smtpAuthEnabled) {
    this.smtpAuthEnabled = smtpAuthEnabled;
  }

  public Boolean getSmtpSslEnabled() {
    return smtpSslEnabled;
  }

  public void setSmtpSslEnabled(Boolean smtpSslEnabled) {
    this.smtpSslEnabled = smtpSslEnabled;
  }
}
