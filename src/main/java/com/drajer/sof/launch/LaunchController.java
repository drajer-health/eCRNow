package com.drajer.sof.launch;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.model.dstu2.composite.PeriodDt;
import ca.uhn.fhir.model.dstu2.resource.Bundle.Entry;
import ca.uhn.fhir.model.dstu2.resource.Encounter;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.service.WorkflowService;
import com.drajer.routing.RestApiSender;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.SystemLaunch;
import com.drajer.sof.service.ClientDetailsService;
import com.drajer.sof.service.LaunchService;
import com.drajer.sof.service.LoadingQueryService;
import com.drajer.sof.service.TriggerQueryService;
import com.drajer.sof.utils.Authorization;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.sof.utils.RefreshTokenScheduler;
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
import org.apache.commons.lang3.time.DateUtils;
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

  @Autowired LaunchService authDetailsService;

  @Autowired RefreshTokenScheduler tokenScheduler;

  @Autowired Authorization authorization;

  @Autowired TriggerQueryService triggerQueryService;

  @Autowired LoadingQueryService loadingQueryService;

  @Autowired WorkflowService workflowService;

  @Autowired ClientDetailsService clientDetailsService;

  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired RestApiSender xmlSender;

  private final SecureRandom random = new SecureRandom();

  @CrossOrigin
  @RequestMapping("/api/launchDetails/{tokenId}")
  public LaunchDetails getLaunchDetailsById(@PathVariable("tokenId") Integer tokenId) {
    return authDetailsService.getAuthDetailsById(tokenId);
  }

  // POST method to create a Client
  @CrossOrigin
  @RequestMapping(value = "/api/launchDetails", method = RequestMethod.POST)
  public LaunchDetails saveLaunchDetails(@RequestBody LaunchDetails launchDetails) {

    logger.info(" Saving Launch Context", launchDetails);
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
      logger.error("Error while triggering data from EHR", e);
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
      logger.error("Error while loading data from EHR", e);
    }

    return "Success";
  }

  @CrossOrigin
  @RequestMapping(value = "/api/systemLaunch", method = RequestMethod.POST)
  public String invokeSystemLaunch(
      @RequestBody SystemLaunch systemLaunch,
      HttpServletRequest request,
      HttpServletResponse response)
      throws IOException {
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
        if (object.getString(FHIR_VERSION).equals("1.(.*).(.*)")) {
          fhirVersion = FhirVersionEnum.DSTU2.toString();
        }
        if (object.getString(FHIR_VERSION).matches("4.(.*).(.*)")) {
          fhirVersion = FhirVersionEnum.R4.toString();
        }

        for (int i = 0; i < innerExtension.length(); i++) {
          JSONObject urlExtension = innerExtension.getJSONObject(i);
          if (urlExtension.getString("url").equals("token")) {
            logger.info("Token URL::::: {}", urlExtension.getString(VALUE_URI));
            tokenEndpoint = urlExtension.getString(VALUE_URI);
            clientDetails.setTokenURL(tokenEndpoint);
          }
        }
      }

      JSONObject tokenResponse = tokenScheduler.getSystemAccessToken(clientDetails);
      if (tokenResponse != null) {
        if (systemLaunch.getPatientId() != null) {
          if (!checkWithExistingPatientAndEncounter(
              systemLaunch.getPatientId(),
              systemLaunch.getEncounterId(),
              systemLaunch.getFhirServerURL())) {

            LaunchDetails launchDetails = new LaunchDetails();
            launchDetails.setAccessToken(tokenResponse.getString(ACCESS_TOKEN));
            launchDetails.setAssigningAuthorityId(clientDetails.getAssigningAuthorityId());
            launchDetails.setClientId(clientDetails.getClientId());
            launchDetails.setClientSecret(clientDetails.getClientSecret());
            launchDetails.setScope(clientDetails.getScopes());
            launchDetails.setDirectHost(clientDetails.getDirectHost());
            launchDetails.setDirectPwd(clientDetails.getDirectPwd());
            launchDetails.setSmtpPort(clientDetails.getSmtpPort());
            launchDetails.setImapPort(clientDetails.getImapPort());
            launchDetails.setDirectRecipient(clientDetails.getDirectRecipientAddress());
            launchDetails.setDirectUser(clientDetails.getDirectUser());
            launchDetails.setEhrServerURL(clientDetails.getFhirServerBaseURL());
            launchDetails.setEncounterId(systemLaunch.getEncounterId());
            launchDetails.setExpiry(tokenResponse.getInt("expires_in"));
            launchDetails.setFhirVersion(fhirVersion);
            launchDetails.setIsCovid(clientDetails.getIsCovid());
            launchDetails.setLaunchPatientId(systemLaunch.getPatientId());
            launchDetails.setTokenUrl(clientDetails.getTokenURL());
            launchDetails.setSetId(
                systemLaunch.getPatientId() + "|" + systemLaunch.getEncounterId());
            launchDetails.setVersionNumber(1);
            launchDetails.setIsSystem(clientDetails.getIsSystem());
            launchDetails.setDebugFhirQueryAndEicr(clientDetails.getDebugFhirQueryAndEicr());
            launchDetails.setRequireAud(clientDetails.getRequireAud());
            launchDetails.setRestAPIURL(clientDetails.getRestAPIURL());
            launchDetails.setxRequestId(requestIdHeadervalue);
            if (systemLaunch.getValidationMode() != null) {
              launchDetails.setValidationMode(systemLaunch.getValidationMode());
            }
            if (tokenResponse.get(EXPIRES_IN) != null) {
              Integer expiresInSec = (Integer) tokenResponse.get(EXPIRES_IN);
              Instant expireInstantTime =
                  new Date().toInstant().plusSeconds(new Long(expiresInSec));
              launchDetails.setTokenExpiryDateTime(new Date().from(expireInstantTime));
            }
            launchDetails.setLaunchType("SystemLaunch");

            setStartAndEndDates(clientDetails, launchDetails);

            clientDetailsService.saveOrUpdate(clientDetails);

            saveLaunchDetails(launchDetails);

            response.setStatus(HttpServletResponse.SC_ACCEPTED);
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
  @RequestMapping(value = "/api/launch")
  public void launchApp(
      @RequestParam String launch,
      @RequestParam String iss,
      HttpServletRequest request,
      HttpServletResponse response)
      throws Exception {
    if (launch != null && iss != null) {
      logger.info("Received Launch Parameter::::: {}", launch);
      logger.info("Received FHIR Server Base URL::::: {}", iss);
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
          logger.info("Constructed Authorization URL::::: {}", constructedAuthUrl);
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
  @RequestMapping(value = "/api/redirect")
  public void redirectEndPoint(
      @RequestParam String code,
      @RequestParam String state,
      HttpServletRequest request,
      HttpServletResponse response)
      throws Exception {
    if (code != null && state != null) {
      logger.info("Received Code Parameter::::: {}", code);
      logger.info("Received State Parameter::::: {}", state);
      logger.info("Reading the oAuth Details stored in HashMap using state value");
      LaunchDetails currentLaunchDetails =
          authDetailsService.getLaunchDetailsByState(Integer.parseInt(state));
      boolean isPatientLaunched = false;
      if (currentLaunchDetails != null) {
        currentLaunchDetails.setAuthorizationCode(code);
        JSONObject accessTokenObject = authorization.getAccessToken(currentLaunchDetails);
        if (accessTokenObject != null) {
          logger.info("Received Access Token::::: {}", accessTokenObject.getString(ACCESS_TOKEN));
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
          patient,
          encounter,
          fhirServerUrl);
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
    currentStateDetails.setExpiry(accessTokenObject.getInt("expires_in"));
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

    setStartAndEndDates(clientDetails, currentStateDetails);

    return currentStateDetails;
  }

  public void setStartAndEndDates(ClientDetails clientDetails, LaunchDetails currentStateDetails) {
    FhirContext context =
        fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion());

    IGenericClient client =
        fhirContextInitializer.createClient(
            context, currentStateDetails.getEhrServerURL(), currentStateDetails.getAccessToken());
    PeriodDt dstu2Period = null;
    Period r4Period = null;

    if (currentStateDetails.getFhirVersion().equals(FhirVersionEnum.DSTU2.toString())
        && currentStateDetails.getEncounterId() != null) {
      logger.info("DSTU2");
      Encounter encounter =
          (Encounter)
              fhirContextInitializer.getResouceById(
                  currentStateDetails,
                  client,
                  context,
                  "Encounter",
                  currentStateDetails.getEncounterId());
      if (encounter.getPeriod() != null) {
        dstu2Period = encounter.getPeriod();
      }
    } else if (currentStateDetails.getFhirVersion().equals(FhirVersionEnum.DSTU2.toString())) {
      Encounter encounter = new Encounter();
      // If Encounter Id is not Present in Launch Details Get Encounters by Patient Id
      // and Find the latest Encounter
      ca.uhn.fhir.model.dstu2.resource.Bundle bundle =
          (ca.uhn.fhir.model.dstu2.resource.Bundle)
              fhirContextInitializer.getResourceByPatientId(
                  currentStateDetails, client, context, "Encounter");
      if (!bundle.getEntry().isEmpty()) {
        Map<Encounter, Date> encounterMap = new HashMap<>();
        for (Entry entry : bundle.getEntry()) {
          Encounter encounterEntry = (Encounter) entry.getResource();
          String encounterId = encounterEntry.getIdElement().getIdPart();
          logger.info("Received Encounter Id========> {}", encounterId);
          encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
        }
        encounter = Collections.max(encounterMap.entrySet(), Map.Entry.comparingByValue()).getKey();
        if (encounter != null) {
          currentStateDetails.setEncounterId(encounter.getIdElement().getIdPart());
          if (encounter.getPeriod() != null) {
            dstu2Period = encounter.getPeriod();
          }
        }
      }
    }

    if (currentStateDetails.getFhirVersion().equals(FhirVersionEnum.R4.toString())
        && currentStateDetails.getEncounterId() != null) {
      org.hl7.fhir.r4.model.Encounter r4Encounter =
          (org.hl7.fhir.r4.model.Encounter)
              fhirContextInitializer.getResouceById(
                  currentStateDetails,
                  client,
                  context,
                  "Encounter",
                  currentStateDetails.getEncounterId());
      if (r4Encounter != null && r4Encounter.getPeriod() != null) {
        r4Period = r4Encounter.getPeriod();
      }
    } else if (currentStateDetails.getFhirVersion().equals(FhirVersionEnum.R4.toString())) {
      org.hl7.fhir.r4.model.Encounter r4Encounter;
      // If Encounter Id is not Present in Launch Details Get Encounters by Patient Id
      // and Find the latest Encounter
      Bundle bundle =
          (Bundle)
              fhirContextInitializer.getResourceByPatientId(
                  currentStateDetails, client, context, "Encounter");

      if (!bundle.getEntry().isEmpty()) {
        Map<org.hl7.fhir.r4.model.Encounter, Date> encounterMap = new HashMap<>();
        for (BundleEntryComponent entry : bundle.getEntry()) {
          org.hl7.fhir.r4.model.Encounter encounterEntry =
              (org.hl7.fhir.r4.model.Encounter) entry.getResource();
          String encounterId = encounterEntry.getIdElement().getIdPart();
          logger.info("Received Encounter Id========> {}", encounterId);
          encounterMap.put(encounterEntry, encounterEntry.getMeta().getLastUpdated());
        }
        r4Encounter =
            Collections.max(encounterMap.entrySet(), Map.Entry.comparingByValue()).getKey();
        if (r4Encounter != null) {
          currentStateDetails.setEncounterId(r4Encounter.getIdElement().getIdPart());
          if (r4Encounter.getPeriod() != null) {
            r4Period = r4Encounter.getPeriod();
          }
        }
      }
    }

    if (dstu2Period != null) {
      if (dstu2Period.getStart() != null) {
        currentStateDetails.setStartDate(dstu2Period.getStart());
      } else {
        currentStateDetails.setStartDate(getDate(clientDetails.getEncounterStartThreshold()));
      }
      if (dstu2Period.getEnd() != null) {
        currentStateDetails.setEndDate(dstu2Period.getEnd());
      } else {
        currentStateDetails.setEndDate(getDate(clientDetails.getEncounterEndThreshold()));
      }
    } else if (r4Period != null) {
      if (r4Period.getStart() != null) {
        currentStateDetails.setStartDate(r4Period.getStart());
      } else {
        currentStateDetails.setStartDate(getDate(clientDetails.getEncounterStartThreshold()));
      }
      if (r4Period.getEnd() != null) {
        currentStateDetails.setEndDate(r4Period.getEnd());
      } else {
        currentStateDetails.setEndDate(getDate(clientDetails.getEncounterEndThreshold()));
      }
    } else {
      currentStateDetails.setStartDate(null);
      currentStateDetails.setEndDate(null);
    }
  }

  private static Date getDate(String thresholdValue) {
    return DateUtils.addHours(new Date(), Integer.parseInt(thresholdValue));
  }

  @CrossOrigin
  @RequestMapping(value = "/api/launchDetails", method = RequestMethod.DELETE)
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
      return "LaunchDetails deleted successfully.";
    }
    response.sendError(HttpServletResponse.SC_NOT_FOUND, "Launch Details Not found");
    return "Launch Details Not found";
  }
}
