package com.drajer.sof.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.DynamicUpdate;

import org.hibernate.annotations.Type;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jline.internal.Log;


@Entity
@Table(name = "client_details")
@DynamicUpdate
public class ClientDetails {

	@Transient
	private final Logger logger = LoggerFactory.getLogger(ClientDetails.class);
	
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Integer id;
	
	@Column(name = "is_provider_launch", nullable = false)
	@Type(type = "org.hibernate.type.NumericBooleanType")
	private Boolean isProvider = false;
	
	@Column(name = "is_system_launch", nullable = false)
	@Type(type = "org.hibernate.type.NumericBooleanType")
	private Boolean isSystem = false;
	
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

	@Column(name = "is_direct", nullable = false)
	@Type(type = "org.hibernate.type.NumericBooleanType")
	private Boolean isDirect = false;
	
	@Column(name = "is_xdr", nullable = false)
	@Type(type = "org.hibernate.type.NumericBooleanType")
	private Boolean isXdr = false;
	
	@Column(name = "direct_host", nullable = true, columnDefinition = "TEXT") 
	private String directHost;
	
	@Column(name = "direct_user", nullable = true) 
	private String directUser;

	@Column(name = "direct_pwd", nullable = true)
	private String directPwd;
	
	@Column(name = "direct_recipient_address", nullable = true)
	private String directRecipientAddress;
	
	@Column(name = "xdr_recipient_address", nullable = true, columnDefinition = "TEXT")
	private String xdrRecipientAddress;
	
	@Column(name = "aa_id", nullable = true) 
	private String assigningAuthorityId;
	
	@Column(name = "encounter_start_time", nullable = true) 
	private String encounterStartThreshold;
	
	@Column(name = "encounter_end_time", nullable = true) 
	private String encounterEndThreshold;
	
	@Column(name = "is_covid19", nullable = false)
	@Type(type = "org.hibernate.type.NumericBooleanType")
	private Boolean isCovid = false;
	
	@Column(name = "is_full_ecr", nullable = false)
	@Type(type = "org.hibernate.type.NumericBooleanType")
	private Boolean isFullEcr = false;

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
	
	public void print() {

		logger.info(" **** Printing Client Details **** ");
		
		Log.info(" Id = " + id );
		Log.info(" Provider Launch = " + isProvider );
		Log.info(" System Launch = " + isSystem );
		Log.info(" Client Id = " + clientId );
		Log.info(" Client Secret = " + clientSecret );
		Log.info(" FHIR Server URL = " + fhirServerBaseURL );
		Log.info(" Token URL = " + tokenURL );
		Log.info(" Scopes = " + scopes );
		Log.info(" Is Direct ? = " + isDirect );
		Log.info(" Is XDR = " + isXdr );
		Log.info(" Direct Host = " + directHost );
		Log.info(" Direct pwd = " + directPwd );
		Log.info(" Direct Recipient Address = " + directRecipientAddress );
		Log.info(" XDR Recipient Address = " + xdrRecipientAddress );
		Log.info(" Assigning Authority Id = " + assigningAuthorityId);
		Log.info(" Encounter Start Threshold = " + encounterStartThreshold);
		Log.info(" Encounter End Threshold = " + encounterEndThreshold);
		Log.info(" Is Covid = " + isCovid);
		Log.info(" Is Full ECR = " + isFullEcr);
		
		logger.info(" **** End Printing Client Details **** ");
	}
}
