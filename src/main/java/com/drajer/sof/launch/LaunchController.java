package com.drajer.sof.launch;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.server.exceptions.ResourceNotFoundException;
import com.drajer.eca.model.EventTypes;
import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.eca.model.PatientExecutionState;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.routing.RestApiSender;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.LaunchDetails.ProcessingStatus;
import com.drajer.sof.model.SystemLaunch;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.RefreshTokenScheduler;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.security.SecureRandom;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r4.model.Period;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;

@RestController
public class LaunchController {

  private final Logger logger = LoggerFactory.getLogger(LaunchController.class);

  private static final String FHIR_VERSION = "fhirVersion";
  private static final String ACCESS_TOKEN = "access_token";
  private static final String EXPIRES_IN = "expires_in";
  private static final String VALUE_URI = "valueUri";
  private static final String PATIENT = "patient";
  private static final String ENCOUNTER = "encounter";
  private static final String EXTENSION = "extension";
  private static final String PROVIDER_UUID = "uuid";

  @Autowired LaunchService authDetailsService;

  @Autowired RefreshTokenScheduler tokenScheduler;

  @Autowired Authorization authorization;

  @Autowired TriggerQueryService triggerQueryService;

  @Autowired LoadingQueryService loadingQueryService;

  @Autowired WorkflowService workflowService;

  @Autowired ClientDetailsService clientDetailsService;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired RestApiSender xmlSender;

  @Autowired ObjectMapper mapper;

  private final SecureRandom random = new SecureRandom();

  @CrossOrigin
  @GetMapping("/api/launchDetails/{tokenId}")
  public LaunchDetails getLaunchDetailsById(@PathVariable("tokenId") Integer tokenId) {
    return authDetailsService.getAuthDetailsById(tokenId);
  }

  // POST method to create a Client
  @CrossOrigin
  @PostMapping(value = "/api/launchDetails")
  public LaunchDetails saveLaunchDetails(@RequestBody LaunchDetails launchDetails) {

    logger.info(" Saving Launch Context: {}", launchDetails);
    authDetailsService.saveOrUpdate(launchDetails);

    if (Boolean.FALSE.equals(launchDetails.getIsMultiTenantSystemLaunch())) {
      logger.info("Scheduling refresh token job ");
      tokenScheduler.scheduleJob(launchDetails);
    }

    String taskInstanceId = "";
    // Kick off the Launch Event Processing
    scheduleJob(launchDetails, taskInstanceId);

    return launchDetails;
  }

  private void scheduleJob(LaunchDetails launchDetails, String taskInstanceId) {

    try {
      if (launchDetails.getEncounterId() != null && launchDetails.getStartDate() != null) {
        logger.info("Scheduling the job based on Encounter period.start time:::::");
        // Setup Execution State.
        PatientExecutionState state =
            new PatientExecutionState(
                launchDetails.getLaunchPatientId(), launchDetails.getEncounterId());

        launchDetails.setStatus(mapper.writeValueAsString(state));
        authDetailsService.saveOrUpdate(launchDetails);
        Instant t = launchDetails.getStartDate().toInstant();
        workflowService.invokeScheduler(
            launchDetails.getId(), EcrActionTypes.MATCH_TRIGGER, t, taskInstanceId);
      } else {
        logger.info("Invoking SOF Launch workflow event handler ");
        workflowService.handleWorkflowEvent(WorkflowEvent.SOF_LAUNCH, launchDetails);
      }
    } catch (JsonProcessingException e) {
      logger.error("Error in Scheduling the Job", e);
    }
  }

  @CrossOrigin
  @GetMapping("/api/triggerQueryService/{tokenId}")
  public String triggerDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
    LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
    SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd");
    try {
      Date start = ft.parse("2012-02-19");
      triggerQueryService.getData(launchDetails, start, new Date());
    } catch (ParseException e) {
      logger.error("Error while triggering data from EHR", e);
    }

    return "Success";
  }

  @CrossOrigin
  @GetMapping("/api/loadingQueryService/{tokenId}")
  public String loadingDataFromEHR(@PathVariable("tokenId") Integer tokenId) {
    LaunchDetails launchDetails = authDetailsService.getAuthDetailsById(tokenId);
    SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd");
    try {
      Date start = ft.parse("2012-02-19");
      loadingQueryService.getData(launchDetails, start, new Date());
    } catch (ParseException e) {
      logger.error("Error while loading data from EHR", e);
    }

    return "Success";
  }

  @CrossOrigin
  @PostMapping(value = "/api/systemLaunch")
  public String invokeSystemLaunch(
      @RequestBody SystemLaunch systemLaunch,
      HttpServletRequest request,
      HttpServletResponse response)
      throws IOException {

    logger.info(
        "System launch request received for patientId: {} and encounterId: {}",
        StringEscapeUtils.escapeJava(systemLaunch.getPatientId()),
        StringEscapeUtils.escapeJava(systemLaunch.getEncounterId()));

    ClientDetails clientDetails =
        clientDetailsService.getClientDetailsByUrl(systemLaunch.getFhirServerURL());
    String requestIdHeadervalue = request.getHeader("X-Request-ID");
    if (clientDetails != null) {

      String fhirVersion = "";
      String tokenEndpoint = "";
      JSONObject object = authorization.getMetadata(systemLaunch.getFhirServerURL() + "/metadata");
      if (object != null) {
        logger.info("Reading Metadata information");
        JSONObject security = (JSONObject) object.getJSONArray("rest").get(0);
        JSONObject sec = security.getJSONObject("security");
        JSONObject extension = (JSONObject) sec.getJSONArray(EXTENSION).get(0);
        JSONArray innerExtension = extension.getJSONArray(EXTENSION);
        if (object.getString(FHIR_VERSION).startsWith("1.")) {
          fhirVersion = FhirVersionEnum.DSTU2.toString();
        }
        if (object.getString(FHIR_VERSION).startsWith("4.")) {
          fhirVersion = FhirVersionEnum.R4.toString();
        }

        for (int i = 0; i < innerExtension.length(); i++) {
          JSONObject urlExtension = innerExtension.getJSONObject(i);
          if (urlExtension.getString("url").equals("token")) {
            tokenEndpoint = urlExtension.getString(VALUE_URI);
            if (clientDetails.getTokenURL() == null || clientDetails.getTokenURL().isEmpty()) {
              logger.info(
                  "Token URL not found in ClientDetails. So reading the Token URL from Metadata::::: {}",
                  urlExtension.getString(VALUE_URI));
              clientDetails.setTokenURL(tokenEndpoint);
            }
          }
        }
      }

      JSONObject tokenResponse = null;
      if (Boolean.TRUE.equals(clientDetails.getIsMultiTenantSystemLaunch())
          && clientDetails.getTokenExpiryDateTime() != null
          && clientDetails.getAccessToken() != null) {
        // Retrieve Access token 3 minutes before it expires.
        Instant currentInstant = new Date().toInstant().plusSeconds(180);
        Date currentDate = Date.from(currentInstant);
        Date tokenExpiryTime = clientDetails.getTokenExpiryDateTime();
        int value = currentDate.compareTo(tokenExpiryTime);
        if (value > 0) {
          logger.info("AccessToken is Expired. Getting new AccessToken");
          tokenResponse = tokenScheduler.getAccessTokenUsingClientDetails(clientDetails);
          if (tokenResponse != null) {
            clientDetails.setTokenExpiryDateTime(
                getTokenExpirationDateTime(tokenResponse.getInt(EXPIRES_IN)));
          }
        } else {
          logger.info("AccessToken is Valid. No need to get new AccessToken");
          tokenResponse = new JSONObject();
          tokenResponse.put(ACCESS_TOKEN, clientDetails.getAccessToken());
          tokenResponse.put(EXPIRES_IN, clientDetails.getTokenExpiry());
        }
      } else {
        tokenResponse = tokenScheduler.getAccessTokenUsingClientDetails(clientDetails);
        if (tokenResponse != null
            && Boolean.TRUE.equals(clientDetails.getIsMultiTenantSystemLaunch())) {
          clientDetails.setTokenExpiryDateTime(
              getTokenExpirationDateTime(tokenResponse.getInt(EXPIRES_IN)));
        }
      }

      if (tokenResponse != null) {
        if (systemLaunch.getPatientId() != null) {
          if (Boolean.FALSE.equals(
              checkWithExistingPatientAndEncounter(
                  systemLaunch.getPatientId(),
                  systemLaunch.getEncounterId(),
                  systemLaunch.getFhirServerURL()))) {

            LaunchDetails launchDetails = new LaunchDetails();
            launchDetails.setAccessToken(tokenResponse.getString(ACCESS_TOKEN));
            launchDetails.setExpiry(tokenResponse.getInt(EXPIRES_IN));
            launchDetails.setAssigningAuthorityId(clientDetails.getAssigningAuthorityId());
            launchDetails.setClientId(clientDetails.getClientId());
            launchDetails.setClientSecret(clientDetails.getClientSecret());
            launchDetails.setScope(clientDetails.getScopes());
            launchDetails.setDirectHost(clientDetails.getDirectHost());
            launchDetails.setDirectPwd(clientDetails.getDirectPwd());
            launchDetails.setSmtpUrl(clientDetails.getSmtpUrl());
            launchDetails.setSmtpPort(clientDetails.getSmtpPort());
            launchDetails.setImapUrl(clientDetails.getImapUrl());
            launchDetails.setImapPort(clientDetails.getImapPort());
            launchDetails.setDirectRecipient(clientDetails.getDirectRecipientAddress());
            launchDetails.setDirectUser(clientDetails.getDirectUser());
            launchDetails.setEhrServerURL(clientDetails.getFhirServerBaseURL());
            launchDetails.setEncounterId(systemLaunch.getEncounterId());
            if (tokenResponse.has(PROVIDER_UUID)) {
              launchDetails.setProviderUUID(tokenResponse.getString(PROVIDER_UUID));
            }
            launchDetails.setFhirVersion(fhirVersion);
            launchDetails.setIsCovid(clientDetails.getIsCovid());
            launchDetails.setIsFullEcr(clientDetails.getIsFullEcr());
            launchDetails.setIsEmergentReportingEnabled(
                clientDetails.getIsEmergentReportingEnabled());
            launchDetails.setLaunchPatientId(systemLaunch.getPatientId());
            launchDetails.setTokenUrl(clientDetails.getTokenURL());
            launchDetails.setSetId(
                systemLaunch.getPatientId() + "|" + systemLaunch.getEncounterId());
            launchDetails.setVersionNumber(1);
            launchDetails.setIsSystem(clientDetails.getIsSystem());
            launchDetails.setIsMultiTenantSystemLaunch(
                clientDetails.getIsMultiTenantSystemLaunch());
            launchDetails.setIsUserAccountLaunch(clientDetails.getIsUserAccountLaunch());
            launchDetails.setDebugFhirQueryAndEicr(clientDetails.getDebugFhirQueryAndEicr());
            launchDetails.setRequireAud(clientDetails.getRequireAud());
            launchDetails.setRestAPIURL(clientDetails.getRestAPIURL());
            launchDetails.setIsCreateDocRef(clientDetails.getIsCreateDocRef());
            launchDetails.setIsInvokeRestAPI(clientDetails.getIsInvokeRestAPI());
            launchDetails.setIsBoth(clientDetails.getIsBoth());
            launchDetails.setRrRestAPIUrl(clientDetails.getRrRestAPIUrl());
            launchDetails.setRrDocRefMimeType(clientDetails.getRrDocRefMimeType());
            launchDetails.setxRequestId(requestIdHeadervalue);
            launchDetails.setProcessingState(LaunchDetails.getString(ProcessingStatus.In_Progress));
            if (systemLaunch.getValidationMode() != null) {
              launchDetails.setValidationMode(systemLaunch.getValidationMode());
            }
            if (Boolean.TRUE.equals(clientDetails.getIsMultiTenantSystemLaunch())) {}

            if (tokenResponse.get(EXPIRES_IN) != null) {
              if (Boolean.TRUE.equals(clientDetails.getIsMultiTenantSystemLaunch())) {
                clientDetails.setAccessToken(tokenResponse.getString(ACCESS_TOKEN));
                clientDetails.setTokenExpiry(tokenResponse.getInt(EXPIRES_IN));
                launchDetails.setTokenExpiryDateTime(clientDetails.getTokenExpiryDateTime());
              } else {
                launchDetails.setTokenExpiryDateTime(
                    getTokenExpirationDateTime(tokenResponse.getInt(EXPIRES_IN)));
              }
            }
            launchDetails.setLaunchType("SystemLaunch");

            IBaseResource encounter = getEncounterById(launchDetails);
            setStartAndEndDates(clientDetails, launchDetails, encounter);

            clientDetailsService.saveOrUpdate(clientDetails);

            saveLaunchDetails(launchDetails);

            response.setStatus(HttpServletResponse.SC_ACCEPTED);

            logger.info(
                "System launch was successful for patientId: {} and encounterId: {} with launchId: {}",
                StringEscapeUtils.escapeJava(launchDetails.getLaunchPatientId()),
                StringEscapeUtils.escapeJava(launchDetails.getEncounterId()),
                launchDetails.getId());
          } else {
            throw new ResponseStatusException(
                HttpStatus.CONFLICT,
                "Launch Context is already present for Patient:::::" + systemLaunch.getPatientId());
          }
        } else {
          logger.error("Please provide Patient Id and Encounter Id");
          throw new ResponseStatusException(
              HttpStatus.BAD_REQUEST, "Please provide Patient Id and Encounter Id");
        }
      } else {
        response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Error in Launching the App");
      }
    } else {
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unrecognized client");
    }

    return "App is launched successfully";
  }

  @CrossOrigin
  @PostMapping(value = "/api/launch")
  public void launchApp(
      @RequestParam String launch,
      @RequestParam String iss,
      HttpServletRequest request,
      HttpServletResponse response)
      throws Exception {
    if (launch != null && iss != null) {
      logger.info("Received Launch Parameter::::: {}", StringEscapeUtils.escapeJava(launch));
      logger.info("Received FHIR Server Base URL::::: {}", StringEscapeUtils.escapeJava(iss));
      String uri =
          "https"
              + "://"
              + request.getServerName()
              + ("http".equals(request.getScheme()) && request.getServerPort() == 80
                      || "https".equals(request.getScheme()) && request.getServerPort() == 443
                  ? ""
                  : ":" + request.getServerPort())
              + request.getContextPath();
      String requestIdHeadervalue = request.getHeader("X-Request-ID");
      Integer state = random.nextInt();
      logger.info("Random State Value==========> {}", state);
      LaunchDetails launchDetails = new LaunchDetails();
      launchDetails.setRedirectURI(uri + "/api/redirect");
      launchDetails.setEhrServerURL(iss);
      launchDetails.setLaunchId(launch);
      try {
        JSONObject object = authorization.getMetadata(iss + "/metadata");
        if (object != null) {
          logger.info("Reading Metadata information");
          JSONObject security = (JSONObject) object.getJSONArray("rest").get(0);
          JSONObject sec = security.getJSONObject("security");
          JSONObject extension = (JSONObject) sec.getJSONArray(EXTENSION).get(0);
          JSONArray innerExtension = extension.getJSONArray(EXTENSION);
          if (object.getString(FHIR_VERSION).equals("1.(.*).(.*)")) {
            launchDetails.setFhirVersion(FhirVersionEnum.DSTU2.toString());
          }
          if (object.getString(FHIR_VERSION).equals("4.(.*).(.*)")) {
            launchDetails.setFhirVersion(FhirVersionEnum.R4.toString());
          }
          for (int i = 0; i < innerExtension.length(); i++) {
            JSONObject urlExtension = innerExtension.getJSONObject(i);
            if (urlExtension.getString("url").equals("authorize")) {
              logger.info("Authorize URL::::: {}", urlExtension.getString(VALUE_URI));
              launchDetails.setAuthUrl(urlExtension.getString(VALUE_URI));
            }
            if (urlExtension.getString("url").equals("token")) {
              logger.info("Token URL::::: {}", urlExtension.getString(VALUE_URI));
              launchDetails.setTokenUrl(urlExtension.getString(VALUE_URI));
            }
          }
          ClientDetails clientDetails =
              clientDetailsService.getClientDetailsByUrl(launchDetails.getEhrServerURL());
          launchDetails.setClientId(clientDetails.getClientId());
          launchDetails.setScope(clientDetails.getScopes());
          launchDetails.setLaunchState(state);
          launchDetails.setxRequestId(requestIdHeadervalue);
          launchDetails.setRequireAud(clientDetails.getRequireAud());
          launchDetails.setLaunchType("SoF");
          String constructedAuthUrl =
              authorization.createAuthUrl(launchDetails, clientDetails, state);
          logger.info(
              "Constructed Authorization URL::::: {}",
              StringEscapeUtils.escapeJava(constructedAuthUrl));
          authDetailsService.saveOrUpdate(launchDetails);
          response.sendRedirect(constructedAuthUrl);
        }
      } catch (Exception e) {
        logger.error("Error in getting Authorization with Server", e);
      }
    } else {
      throw new Exception("Launch or Issuer URL is missing");
    }
  }

  @CrossOrigin
  @PostMapping(value = "/api/redirect")
  public void redirectEndPoint(
      @RequestParam String code,
      @RequestParam String state,
      HttpServletRequest request,
      HttpServletResponse response)
      throws Exception {
    if (code != null && state != null) {
      logger.info("Received Code Parameter::::: {}", StringEscapeUtils.escapeJava(code));
      logger.info("Received State Parameter::::: {}", StringEscapeUtils.escapeJava(state));
      logger.info("Reading the oAuth Details stored in HashMap using state value");
      LaunchDetails currentLaunchDetails =
          authDetailsService.getLaunchDetailsByState(Integer.parseInt(state));
      boolean isPatientLaunched = false;
      if (currentLaunchDetails != null) {
        currentLaunchDetails.setAuthorizationCode(code);
        JSONObject accessTokenObject = authorization.getAccessToken(currentLaunchDetails);
        if (accessTokenObject != null) {
          logger.debug(
              "Received Access Token::::: {}",
              StringEscapeUtils.escapeJava(accessTokenObject.getString(ACCESS_TOKEN)));
          if (accessTokenObject.get(PATIENT) != null && accessTokenObject.get(ENCOUNTER) != null) {
            isPatientLaunched =
                checkWithExistingPatientAndEncounter(
                    accessTokenObject.getString(PATIENT),
                    accessTokenObject.getString(ENCOUNTER),
                    currentLaunchDetails.getEhrServerURL());
          }
          if (!isPatientLaunched) {
            ClientDetails clientDetails =
                clientDetailsService.getClientDetailsByUrl(currentLaunchDetails.getEhrServerURL());

            currentLaunchDetails =
                setLaunchDetails(currentLaunchDetails, accessTokenObject, clientDetails);

            saveLaunchDetails(currentLaunchDetails);
          } else {
            logger.error(
                "Launch Context is already present for Patient::::: {}",
                accessTokenObject.getString(PATIENT));
            response.sendError(
                HttpServletResponse.SC_BAD_REQUEST,
                "Launch Context is already present for Patient:::::"
                    + accessTokenObject.getString(PATIENT));
          }
        } else {
          throw new Exception("Error in getting AccessToken from Token Endpoint");
        }
      } else {
        throw new Exception(
            "Error in getting the oAuth Details from HashMap using State Parameter:::::" + state);
      }
    } else {
      throw new Exception("Code or State Parameter is Missing");
    }
  }

  private Boolean checkWithExistingPatientAndEncounter(
      String patient, String encounter, String fhirServerUrl) {
    LaunchDetails launchDetails =
        authDetailsService.getLaunchDetailsByPatientAndEncounter(patient, encounter, fhirServerUrl);
    if (launchDetails != null) {
      logger.info(
          "Launch context found with Patient:::: {}, Encounter::::: {}, From EHR::::: {}",
          StringEscapeUtils.escapeJava(patient),
          StringEscapeUtils.escapeJava(encounter),
          StringEscapeUtils.escapeJava(fhirServerUrl));
      return true;
    } else {
      logger.info("Launch context not found");
      return false;
    }
  }

  public LaunchDetails setLaunchDetails(
      LaunchDetails currentStateDetails,
      JSONObject accessTokenObject,
      ClientDetails clientDetails) {

    currentStateDetails.setAccessToken(accessTokenObject.getString(ACCESS_TOKEN));
    currentStateDetails.setRefreshToken(accessTokenObject.getString("refresh_token"));
    currentStateDetails.setUserId(accessTokenObject.getString("user"));
    currentStateDetails.setExpiry(accessTokenObject.getInt(EXPIRES_IN));
    currentStateDetails.setLaunchPatientId(accessTokenObject.getString(PATIENT));
    currentStateDetails.setEncounterId(accessTokenObject.getString(ENCOUNTER));
    currentStateDetails.setAssigningAuthorityId(clientDetails.getAssigningAuthorityId());
    currentStateDetails.setSetId(
        accessTokenObject.getString("PATIENT") + "|" + accessTokenObject.getString("ENCOUNTER"));
    currentStateDetails.setVersionNumber(1);
    currentStateDetails.setDirectUser(clientDetails.getDirectUser());
    currentStateDetails.setDirectHost(clientDetails.getDirectHost());
    currentStateDetails.setDirectPwd(clientDetails.getDirectPwd());
    currentStateDetails.setDirectRecipient(clientDetails.getDirectRecipientAddress());
    currentStateDetails.setRestAPIURL(clientDetails.getRestAPIURL());
    currentStateDetails.setIsCovid(clientDetails.getIsCovid());
    currentStateDetails.setDebugFhirQueryAndEicr(clientDetails.getDebugFhirQueryAndEicr());

    IBaseResource encounterResource = getEncounterById(currentStateDetails);
    setStartAndEndDates(clientDetails, currentStateDetails, encounterResource);

    return currentStateDetails;
  }

  public IBaseResource getEncounterById(LaunchDetails launchDetails) {

    String fhirVersion = launchDetails.getFhirVersion();
    String encounterId = launchDetails.getEncounterId();
    IBaseResource encounterResource = null;
    String encounterError = "Error in getting Encounter resource by Id: " + encounterId;

    logger.info("Getting Encounter data by ID {}", StringEscapeUtils.escapeJava(encounterId));

    try {
      FhirContext fhirContext = fhirContextInitializer.getFhirContext(fhirVersion);

      IGenericClient fhirClient =
          fhirContextInitializer.createClient(
              fhirContext, launchDetails, EventTypes.QueryType.NONE);

      if (!StringUtils.isEmpty(encounterId)) {
        return fhirClient.read().resource("Encounter").withId(encounterId).execute();
      }

      IBaseBundle bundle =
          fhirContextInitializer.getResourceByPatientId(
              launchDetails, fhirClient, fhirContext, "Encounter");

      if (bundle != null && !bundle.isEmpty()) {

        if (fhirVersion.equalsIgnoreCase(FhirVersionEnum.R4.toString())) {
          Bundle r4Bundle = (Bundle) bundle;
          Map<org.hl7.fhir.r4.model.Encounter, Date> encounterMap = new HashMap<>();
          for (BundleEntryComponent entry : r4Bundle.getEntry()) {
            org.hl7.fhir.r4.model.Encounter encounterEntry =
                (org.hl7.fhir.r4.model.Encounter) entry.getResource();
            encounterId = encounterEntry.getIdElement().getIdPart();
            logger.info("Received Encounter Id ========> {}", encounterId);
            encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
          }
          encounterResource =
              Collections.max(encounterMap.entrySet(), Map.Entry.comparingByValue()).getKey();
          launchDetails.setEncounterId(encounterResource.getIdElement().getIdPart());
          return encounterResource;
        }

        if (fhirVersion.equalsIgnoreCase(FhirVersionEnum.DSTU2.toString())) {
          ca.uhn.fhir.model.dstu2.resource.Bundle dstu2Bundle =
              (ca.uhn.fhir.model.dstu2.resource.Bundle) bundle;
          Map<Encounter, Date> encounterMap = new HashMap<>();
          for (Entry entry : dstu2Bundle.getEntry()) {
            Encounter encounterEntry = (Encounter) entry.getResource();
            encounterId = encounterEntry.getIdElement().getIdPart();
            logger.info("Received Encounter Id ========> {}", encounterId);
            encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
          }
          encounterResource =
              Collections.max(encounterMap.entrySet(), Map.Entry.comparingByValue()).getKey();
          launchDetails.setEncounterId(encounterResource.getIdElement().getIdPart());
          return encounterResource;
        }
      }

      logger.error(encounterError);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, encounterError);

    } catch (ResourceNotFoundException notFoundException) {

      logger.error(encounterError, notFoundException);
      throw new ResponseStatusException(HttpStatus.NOT_FOUND, encounterError, notFoundException);

    } catch (Exception e) {

      logger.error(encounterError, e);
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, encounterError, e);
    }
  }

  public void setStartAndEndDates(
      ClientDetails clientDetails, LaunchDetails launchDetails, IBaseResource encounter) {

    String fhirVersion = launchDetails.getFhirVersion();

    // This is explicitly set to null so that when we don't have the encounter
    // period present, we can default it to launch immediately.
    launchDetails.setStartDate(null);
    launchDetails.setEndDate(null);

    if (fhirVersion.equalsIgnoreCase(FhirVersionEnum.R4.toString())) {
      org.hl7.fhir.r4.model.Encounter r4Encounter = (org.hl7.fhir.r4.model.Encounter) encounter;
      if (r4Encounter != null) {
        Period r4Period = r4Encounter.getPeriod();
        if (r4Period != null) {
          if (r4Period.getStart() != null) {
            launchDetails.setStartDate(r4Period.getStart());
          } else {
            launchDetails.setStartDate(getDate(clientDetails.getEncounterStartThreshold()));
          }
          if (r4Period.getEnd() != null) {
            launchDetails.setEndDate(r4Period.getEnd());
          } else {
            launchDetails.setEndDate(getDate(clientDetails.getEncounterEndThreshold()));
          }
        }
      }
      return;
    }

    if (fhirVersion.equalsIgnoreCase(FhirVersionEnum.DSTU2.toString())) {
      Encounter dstu2Encounter = (Encounter) encounter;
      if (dstu2Encounter != null) {
        PeriodDt dstu2Period = dstu2Encounter.getPeriod();
        if (dstu2Period != null) {
          if (dstu2Period.getStart() != null) {
            launchDetails.setStartDate(dstu2Period.getStart());
          } else {
            launchDetails.setStartDate(getDate(clientDetails.getEncounterStartThreshold()));
          }
          if (dstu2Period.getEnd() != null) {
            launchDetails.setEndDate(dstu2Period.getEnd());
          } else {
            launchDetails.setEndDate(getDate(clientDetails.getEncounterEndThreshold()));
          }
        }
      }
    }
  }

  private static Date getDate(String thresholdValue) {
    return DateUtils.addHours(new Date(), Integer.parseInt(thresholdValue));
  }

  private Date getTokenExpirationDateTime(Integer expiresIn) {
    Instant expireInstantTime = new Date().toInstant().plusSeconds(new Long(expiresIn));
    return new Date().from(expireInstantTime);
  }

  @CrossOrigin
  @DeleteMapping(value = "/api/launchDetails")
  public String deleteLaunchDetails(
      @RequestBody SystemLaunch systemLaunch,
      HttpServletRequest request,
      HttpServletResponse response)
      throws IOException {
    LaunchDetails launchDetails =
        authDetailsService.getLaunchDetailsByPatientAndEncounter(
            systemLaunch.getPatientId(),
            systemLaunch.getEncounterId(),
            systemLaunch.getFhirServerURL());
    if (launchDetails != null) {
      authDetailsService.delete(launchDetails);
      WorkflowService.cancelAllScheduledTasksForLaunch(launchDetails, false);
      return "LaunchDetails deleted successfully.";
    }
    response.sendError(HttpServletResponse.SC_NOT_FOUND, "Launch Details Not found");
    return "Launch Details Not found";
  }
}
