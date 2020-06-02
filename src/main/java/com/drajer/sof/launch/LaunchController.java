package com.drajer.sof.launch;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.r4.model.Period;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.SystemLuanch;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.RefreshTokenScheduler;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.rest.client.api.IGenericClient;

@RestController
public class LaunchController {

	private final Logger logger = LoggerFactory.getLogger(LaunchController.class);

	@Autowired
	LaunchService authDetailsService;

	@Autowired
	RefreshTokenScheduler tokenScheduler;

	@Autowired
	Authorization authorization;

	@Autowired
	TriggerQueryService triggerQueryService;

	@Autowired
	LoadingQueryService loadingQueryService;

	@Autowired
	WorkflowService workflowService;

	@Autowired
	ClientDetailsService clientDetailsService;

	@Autowired
	FhirContextInitializer fhirContextInitializer;

	public static Map<Integer, JSONObject> authDetailsSet = new HashMap<Integer, JSONObject>();

	@CrossOrigin
	@RequestMapping("/api/launchDetails/{tokenId}")
	public LaunchDetails getLaunchDetailsById(@PathVariable("tokenId") Integer tokenId) {
		return authDetailsService.getAuthDetailsById(tokenId);
	}

	// POST method to create a Client
	@CrossOrigin
	@RequestMapping(value = "/api/launchDetails", method = RequestMethod.POST)
	public LaunchDetails saveLaunchDetails(@RequestBody LaunchDetails launchDetails) {

		logger.info(" Saving Launch Context");
		authDetailsService.saveOrUpdate(launchDetails);

		logger.info("Scheduling refresh token job ");
		tokenScheduler.scheduleJob(launchDetails);

		// Kick off the Launch Event Processing
		logger.info("Invoking SOF Launch workflow event handler ");
		workflowService.handleWorkflowEvent(WorkflowEvent.SOF_LAUNCH, launchDetails);

		return launchDetails;
	}

	@CrossOrigin
	@RequestMapping("/api/triggerQueryService/{tokenId}")
	public String triggerDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
		LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
		SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd");
		try {
			Date start = ft.parse("2012-02-19");
			triggerQueryService.getData(launchDetails, start, new Date());
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return "Success";
	}

	@CrossOrigin
	@RequestMapping("/api/loadingQueryService/{tokenId}")
	public String loadingDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
		LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
		SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd");
		try {
			Date start = ft.parse("2012-02-19");
			loadingQueryService.getData(launchDetails, start, new Date());
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return "Success";
	}

	@CrossOrigin
	@RequestMapping(value = "/api/systemLaunch", method = RequestMethod.POST)
	public String invokeSystemLaunch(@RequestBody SystemLuanch systemLaunch) {
		try {
			ClientDetails clientDetails = clientDetailsService.getClientDetailsByUrl(systemLaunch.getFhirServerURL());
			tokenScheduler.getSystemAccessToken(clientDetails);
		} catch (Exception e) {
			logger.info("Error in Invoking System Launch");
		}

		return "App is launched successfully";
	}

	@CrossOrigin
	@RequestMapping(value = "/api/launch")
	public void launchApp(@RequestParam String launch, @RequestParam String iss, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		if (launch != null && iss != null) {
			logger.info("Received Launch Paramter:::::" + launch);
			logger.info("Received FHIR Server Base URL:::::" + iss);
			String uri = request.getScheme() + "://" + request.getServerName()
					+ ("http".equals(request.getScheme()) && request.getServerPort() == 80
							|| "https".equals(request.getScheme()) && request.getServerPort() == 443 ? ""
									: ":" + request.getServerPort())
					+ request.getContextPath();
			Random random = new Random();
			Integer state = random.nextInt();
			JSONObject authDetailsObject = new JSONObject();
			authDetailsObject.put("redirectUrl", uri + "/api/redirect");
			authDetailsObject.put("iss", iss);
			authDetailsObject.put("launch", launch);
			try {
				JSONObject object = authorization.getMetadata(iss + "/metadata");
				if (object != null) {
					logger.info("Reading Metadata information");
					JSONObject security = (JSONObject) object.getJSONArray("rest").get(0);
					JSONObject sec = security.getJSONObject("security");
					JSONObject extension = (JSONObject) sec.getJSONArray("extension").get(0);
					JSONArray innerExtension = extension.getJSONArray("extension");
					if (object.getString("fhirVersion").equals("1.0.2")) {
						authDetailsObject.put("fhirVersion", FhirVersionEnum.DSTU2.toString());
					}
					if (object.getString("fhirVersion").equals("4.0.0")) {
						authDetailsObject.put("fhirVersion", FhirVersionEnum.R4.toString());
					}
					for (int i = 0; i < innerExtension.length(); i++) {
						JSONObject urlExtension = innerExtension.getJSONObject(i);
						if (urlExtension.getString("url").equals("authorize")) {
							logger.info("Authorize URL:::::" + urlExtension.getString("valueUri"));
							authDetailsObject.put("authorizeUrl", urlExtension.getString("valueUri"));
						}
						if (urlExtension.getString("url").equals("token")) {
							logger.info("Token URL:::::" + urlExtension.getString("valueUri"));
							authDetailsObject.put("tokenUrl", urlExtension.getString("valueUri"));
						}
					}
					ClientDetails clientDetails = clientDetailsService
							.getClientDetailsByUrl(authDetailsObject.getString("iss"));
					authDetailsObject.put("client_id", clientDetails.getClientId());
					authDetailsObject.put("scope", clientDetails.getScopes());
					String constructedAuthUrl = authorization.createAuthUrl(authDetailsObject, clientDetails, state);
					logger.info("Constructed Authorization URL:::::" + constructedAuthUrl);
					logger.info(
							"Storing the oAuth Details to a HashMap using randomly generated state value as Key:::::"
									+ authDetailsObject.toString());
					authDetailsSet.put(state, authDetailsObject);
					response.sendRedirect(constructedAuthUrl);
				}
			} catch (IOException e) {
				logger.error("Error in getting Authorization with Server");
			}
		} else {
			throw new Exception("Launch or Issuer URL is missing");
		}
	}

	@CrossOrigin
	@RequestMapping(value = "/api/redirect")
	public void redirectEndPoint(@RequestParam String code, @RequestParam String state, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		if (code != null && state != null) {
			logger.info("Received Code Parameter:::::" + code);
			logger.info("Received State Parameter:::::" + state);
			logger.info("Reading the oAuth Details stored in HashMap using state value");
			JSONObject currentStateDetails = authDetailsSet.get(Integer.parseInt(state));
			if (currentStateDetails != null) {
				currentStateDetails.put("code", code);
				JSONObject accessTokenObject = authorization.getAccessToken(currentStateDetails);
				if (accessTokenObject != null) {
					logger.info("Received Access Token:::::" + accessTokenObject.toString());

					ClientDetails clientDetails = clientDetailsService
							.getClientDetailsByUrl(currentStateDetails.getString("iss"));
					
					LaunchDetails launchDetails = setLaunchDetails(currentStateDetails, accessTokenObject,
							clientDetails);
					
					saveLaunchDetails(launchDetails);
					authDetailsSet.remove(Integer.parseInt(state));
				} else {
					throw new Exception("Error in getting AccessToken from Token Endpoint");
				}
			} else {
				throw new Exception(
						"Error in getting the oAuth Details from HashMap using State Parameter:::::" + state);
			}
		} else {
			throw new Exception("Code or State Parmater is Missing");
		}
		
	}

	public LaunchDetails setLaunchDetails(JSONObject currentStateDetails, JSONObject accessTokenObject,
			ClientDetails clientDetails) {

		LaunchDetails launchDetails = new LaunchDetails();
		launchDetails.setClientId(currentStateDetails.getString("client_id"));
		launchDetails.setEhrServerURL(currentStateDetails.getString("iss"));
		launchDetails.setAuthUrl(currentStateDetails.getString("authorizeUrl"));
		launchDetails.setTokenUrl(currentStateDetails.getString("tokenUrl"));
		launchDetails.setAccessToken(accessTokenObject.getString("access_token"));
		launchDetails.setRefreshToken(accessTokenObject.getString("refresh_token"));
		launchDetails.setUserId(accessTokenObject.getString("user"));
		launchDetails.setExpiry(accessTokenObject.getInt("expires_in"));
		launchDetails.setScope(currentStateDetails.getString("scope"));
		launchDetails.setLaunchPatientId(accessTokenObject.getString("patient"));
		launchDetails.setFhirVersion(currentStateDetails.getString("fhirVersion"));
		launchDetails.setEncounterId(accessTokenObject.getString("encounter"));
		launchDetails.setAssigningAuthorityId(clientDetails.getAssigningAuthorityId());
		launchDetails.setSetId(accessTokenObject.getString("patient") + "+" + accessTokenObject.getString("encounter"));
		launchDetails.setVersionNumber("1");
		launchDetails.setDirectUser(clientDetails.getDirectUser());
		launchDetails.setDirectHost(clientDetails.getDirectHost());
		launchDetails.setDirectPwd(clientDetails.getDirectPwd());
		launchDetails.setDirectRecipient(clientDetails.getDirectRecipientAddress());

		setStartAndEndDates(launchDetails,clientDetails, currentStateDetails);

		return launchDetails;
	}
	
	public void setStartAndEndDates(LaunchDetails launchDetails, ClientDetails clientDetails, JSONObject currentStateDetails) {
		FhirContext context = fhirContextInitializer.getFhirContext(launchDetails.getFhirVersion());

		IGenericClient client = fhirContextInitializer.createClient(context, launchDetails.getEhrServerURL(),
				launchDetails.getAccessToken());
		
		if (currentStateDetails.getString("fhirVersion").equals(FhirVersionEnum.DSTU2.toString())) {
			Encounter encounter = (Encounter) fhirContextInitializer.getResouceById(launchDetails, client, context,
					"Encounter", launchDetails.getEncounterId());
			if (encounter.getPeriod() != null) {
				PeriodDt period = encounter.getPeriod();
				if (period.getStart() != null) {
					launchDetails.setStartDate(period.getStart());
				} else {
					Date startDate = DateUtils.addHours(new Date(),
							Integer.parseInt(clientDetails.getEncounterStartThreshold()));
					launchDetails.setStartDate(startDate);
				}
				if (period.getEnd() != null) {
					launchDetails.setEndDate(period.getEnd());
				} else {
					Date endDate = DateUtils.addHours(new Date(),
							Integer.parseInt(clientDetails.getEncounterEndThreshold()));
					launchDetails.setEndDate(endDate);
				}
			} 
		}
		
		if (currentStateDetails.getString("fhirVersion").equals(FhirVersionEnum.R4.toString())) {
			org.hl7.fhir.r4.model.Encounter r4Encounter = (org.hl7.fhir.r4.model.Encounter) fhirContextInitializer.getResouceById(launchDetails, client, context,
					"Encounter", launchDetails.getEncounterId());
			if (r4Encounter.getPeriod() != null) {
				Period period = r4Encounter.getPeriod();
				if (period.getStart() != null) {
					launchDetails.setStartDate(period.getStart());
				} else {
					Date startDate = DateUtils.addHours(new Date(),
							Integer.parseInt(clientDetails.getEncounterStartThreshold()));
					launchDetails.setStartDate(startDate);
				}
				if (period.getEnd() != null) {
					launchDetails.setEndDate(period.getEnd());
				} else {
					Date endDate = DateUtils.addHours(new Date(),
							Integer.parseInt(clientDetails.getEncounterEndThreshold()));
					launchDetails.setEndDate(endDate);
				}
			} 
		}
	}
}
