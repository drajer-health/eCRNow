package com.drajer.sof.utils;

import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.Response;
import com.drajer.sof.service.LaunchService;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.api.ServerValidationModeEnum;

@Component
public class RefreshTokenScheduler {

	@Autowired
	RestTemplate restTemplate;

	@Autowired
	LaunchService authDetailsService;

	@Autowired
	ThreadPoolTaskScheduler taskScheduler;

	@Autowired
	ResourceData resourceData;

	private final Logger logger = LoggerFactory.getLogger(RefreshTokenScheduler.class);

	public void scheduleJob(LaunchDetails authDetails) {
		logger.info("Scheduling Job to Get AccessToken");
		long minutes = TimeUnit.SECONDS.toMinutes(authDetails.getExpiry());
		// String cronExpression = "* "+"0/"+minutes+" * * * ?";
		String cronExpression = "0/60 * * * * ?";
		CronTrigger cronTrigger = new CronTrigger(cronExpression);
		taskScheduler.schedule(new RunnableTask(authDetails), cronTrigger);
		logger.info("Job Scheduled to get AccessToken for every " + minutes + " minutes for Client: "
				+ authDetails.getClientId());
	}

	class RunnableTask implements Runnable {

		public LaunchDetails authDetails;

		public RunnableTask(LaunchDetails authDetails) {
			this.authDetails = authDetails;
		}

		@Override
		public void run() {
			try {
				getAccessToken(this.authDetails);
				Thread.currentThread().interrupt();
			} catch (Exception e) {
				logger.info("Error in InvokeExtractions" + e.getMessage());
			}
		}
	}

	private JSONObject getAccessToken(LaunchDetails authDetails) {
		JSONObject tokenResponse = null;
		logger.info("Getting AccessToken for Client: " + authDetails.getClientId());
		try {
			RestTemplate restTemplate = new RestTemplate();
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
			MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
			map.add("grant_type", "refresh_token");
			map.add("refresh_token", authDetails.getRefreshToken());
			HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
			ResponseEntity<?> response = restTemplate.exchange(authDetails.getTokenUrl(), HttpMethod.POST, entity,
					Response.class);
			tokenResponse = new JSONObject(response.getBody());
			logger.info("Received AccessToken for Client: " + authDetails.getClientId());
			logger.info("Received AccessToken: " + tokenResponse);
			updateAccessToken(authDetails, tokenResponse);

		} catch (Exception e) {
			logger.error("Error in Getting the AccessToken for the client: " + authDetails.getClientId());
		}
		return tokenResponse;
	}

	private void updateAccessToken(LaunchDetails authDetails, JSONObject tokenResponse) {
		LaunchDetails existingAuthDetails = new LaunchDetails();
		try {
			logger.info("Updating the AccessToken value in database");
			existingAuthDetails = authDetailsService.getAuthDetailsById(authDetails.getId());
			existingAuthDetails.setAccessToken(tokenResponse.getString("access_token"));
			existingAuthDetails.setExpiry(tokenResponse.getInt("expires_in"));
			existingAuthDetails.setLastUpdated(new Date());
			authDetailsService.saveOrUpdate(existingAuthDetails);
			logger.info("Successfully updated AccessToken value in database");
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in Updating the AccessToken value into database: " + e.getMessage());
		}

		getResourcesData(existingAuthDetails);

	}

	private void getResourcesData(LaunchDetails authDetails) {
		FhirContext context = resourceData.getFhirContext(authDetails.getFhirVersion());
		context.getRestfulClientFactory().setServerValidationMode(ServerValidationModeEnum.NEVER);
		IGenericClient genericClient = resourceData.createClient(context, authDetails.getEhrServerURL(),
				authDetails.getAccessToken());
		try {
			resourceData.getResouceById(authDetails, genericClient, context, "Patient", authDetails.getLaunchPatientId());
		} catch (Exception e) {
			logger.error("Error in getting Patient details");
		}

		try {
			resourceData.getResouceById(authDetails, genericClient, context, "Encounter", authDetails.getEncounterId());
			// resourceData.getEncounterData(client, genericClient, ctx);
		} catch (Exception e) {
			logger.error("Error in getting Encounter details");
		}

		try {
			resourceData.getResourceByPatientId(authDetails, genericClient, context, "Observation");
			// resourceData.getObservationData(client, genericClient, ctx);
		} catch (Exception e) {
			logger.error("Error in getting Observation details");
		}

		try {
			resourceData.getResourceByPatientId(authDetails, genericClient, context, "Condition");
			// resourceData.getConditionData(client, genericClient, ctx);
		} catch (Exception e) {
			logger.error("Error in getting Condition details");
		}

		if (authDetails.getFhirVersion().equalsIgnoreCase("DSTU2")) {
			try {
				resourceData.getResourceByPatientId(authDetails, genericClient, context, "MedicationAdministration");
				// resourceData.getMedicationAdministrationData(client, genericClient, ctx);
			} catch (Exception e) {
				logger.error("Error in getting MedicationAdministration details");
			}

			try {
				resourceData.getResourceByPatientId(authDetails, genericClient, context, "MedicationOrder");
				// resourceData.getMedicationOrderData(client, genericClient, ctx);
			} catch (Exception e) {
				logger.error("Error in getting MedicationOrder details");
			}

			try {
				resourceData.getResourceByPatientId(authDetails, genericClient, context, "MedicationStatement");
				// resourceData.getMedicationStatementData(client, genericClient, ctx);
			} catch (Exception e) {
				logger.error("Error in getting MedicationStatement details");
			}
		} else if (authDetails.getFhirVersion().equalsIgnoreCase("R4")) {
			try {
				resourceData.getResourceByPatientId(authDetails, genericClient, context, "MedicationRequest");
				// resourceData.getMedicationAdministrationData(client, genericClient, ctx);
			} catch (Exception e) {
				logger.error("Error in getting MedicationRequest details");
			}
		}
	}
}
