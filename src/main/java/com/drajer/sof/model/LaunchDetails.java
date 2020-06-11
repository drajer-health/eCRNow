package com.drajer.sof.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.TypeDefs;
import org.hibernate.annotations.UpdateTimestamp;

import com.drajer.ecrapp.config.JSONObjectUserType;
import com.drajer.ecrapp.encryptor.ColumnEncryptor;

@Entity
@Table(name = "launch_details")
@DynamicUpdate
@TypeDefs({ @TypeDef(name = "StringJsonObject", typeClass = JSONObjectUserType.class) })
public class LaunchDetails {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Integer id;

	@Column(name = "client_id", nullable = true)
	private String clientId;

	@Column(name = "ehr_server_url", nullable = true, columnDefinition = "TEXT")
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

	@Column(name = "refresh_token", nullable = true, columnDefinition = "TEXT")
	private String refreshToken;

	@Column(name = "launch_patient_id", nullable = true)
	private String launchPatientId;

	@Column(name = "fhir_version", nullable = true)
	private String fhirVersion;

	@Column(name = "encounter_id", nullable = true)
	private String encounterId;

	@Column(name = "status", nullable = true) // Status can be active or completed.
	@Type(type = "StringJsonObject")
	private String status;

	@Column(name = "aa_id", nullable = true) // Status can be active or completed.
	private String assigningAuthorityId;

	@Column(name = "set_id", nullable = true) // Status can be active or completed.
	private String setId;

	@Column(name = "ver_number", nullable = true) // Status can be active or completed.
	private String versionNumber;
	
	@Column(name = "direct_host", nullable = true) // Status can be active or completed.
	private String directHost;

	@Column(name = "direct_user", nullable = true) // Status can be active or completed.
	private String directUser;

	@Column(name = "direct_pwd", nullable = true) // Status can be active or completed.
	@Convert(converter = ColumnEncryptor.class)
	private String directPwd;
	
	@Column(name = "direct_recipient", nullable = true) // Status can be active or completed.
	private String directRecipient;
	
	@Column(name = "is_covid19", columnDefinition = "boolean default true", nullable = false)
	private Boolean isCovid = true;
	
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

	public String getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(String versionNumber) {
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
		return accessToken;
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
		return directPwd;
	}

	public void setDirectPwd(String directPwd) {
		this.directPwd = directPwd;
	}

}
