package com.drajer.bsa.kar.action;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.api.ServerValidationModeEnum;
import ca.uhn.fhir.rest.gclient.IOperationProcessMsgMode;
import ca.uhn.fhir.rest.server.exceptions.InvalidRequestException;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.BsaTypes.OutputContentType;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarExecutionState;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import com.drajer.bsa.routing.impl.RestfulTransportImpl;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.utils.FhirContextInitializer;
import io.micrometer.core.instrument.util.StringUtils;
import java.io.InputStream;
import java.time.Instant;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.UriType;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.stereotype.Service;

@Service
public class SubmitReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(SubmitReport.class);

  private String submissionEndpoint;

  private static final int RR_CHECK_TIME = 60;
  private static final String RR_CHECK_TIME_UNITS = "s";

  private static final FhirContext context = FhirContext.forR4();

  private BsaServiceUtils bsaServiceUtils;

  private PublicHealthMessagesDao phDao;

  private DirectTransportImpl directSender;

  private RestfulTransportImpl restSubmitter;

  private AuthorizationUtils authorizationUtils;

  private PublicHealthAuthorityService publicHealthAuthorityService;

  private FhirContextInitializer fhirContextInitializer;

  private String checkResponseActionId;

  public String getCheckResponseActionId() {
    return checkResponseActionId;
  }

  public void setCheckResponseActionId(String checkResponseActionId) {
    this.checkResponseActionId = checkResponseActionId;
  }

  public BsaServiceUtils getBsaServiceUtils() {
    return bsaServiceUtils;
  }

  public void setBsaServiceUtils(BsaServiceUtils bsaServiceUtils) {
    this.bsaServiceUtils = bsaServiceUtils;
  }

  public DirectTransportImpl getDirectSender() {
    return directSender;
  }

  public void setDirectSender(DirectTransportImpl directSender) {
    this.directSender = directSender;
  }

  public PublicHealthMessagesDao getPhDao() {
    return phDao;
  }

  public void setPhDao(PublicHealthMessagesDao phDao) {
    this.phDao = phDao;
  }

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    logger.info(" Executing the submission of the Report to : {}", submissionEndpoint);

    BsaActionStatus actStatus = new SubmitReportStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    if (status != BsaActionStatusType.SCHEDULED || Boolean.TRUE.equals(getIgnoreTimers())) {

      logger.info("Action is not timed going through to submission");

      // Get the Kar.
      KnowledgeArtifact art = data.getKar();
      KnowledgeArtifactStatus artStatus =
          data.getHealthcareSetting().getArtifactStatus(art.getVersionUniqueId());

      if (artStatus != null
          && (artStatus.getOutputFormat() == OutputContentType.CDA_R11
              || artStatus.getOutputFormat() == OutputContentType.CDA_R30)) {

        logger.info(" Submitting CDA Output ");
        submitCdaOutput(data, actStatus, data.getHealthcareSetting());
      } else if (artStatus != null && artStatus.getOutputFormat() == OutputContentType.FHIR) {

        logger.info(" Submitting FHIR Output ");
        // by default it is FHIR Payload and validate accordingly.
        submitFhirOutput(data, actStatus, ehrService);
      } else if (artStatus != null && artStatus.getOutputFormat() == OutputContentType.BOTH) {

        logger.info(" Submitting Both CDA and FHIR Output ");
        submitCdaOutput(data, actStatus, data.getHealthcareSetting());
        submitFhirOutput(data, actStatus, ehrService);
      }

      if (Boolean.TRUE.equals(conditionsMet(data, ehrService))) {
        // Execute sub Actions
        executeSubActions(data, ehrService);
        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }

      actStatus.setActionStatus(BsaActionStatusType.COMPLETED);

    } else {
      logger.info(
          " Action may execute in future or Conditions not met, can't process further. Setting Action Status : {}",
          status);
      actStatus.setActionStatus(status);
    }

    data.addActionStatus(data.getExecutionSequenceId(), actStatus);
    return actStatus;
  }

  public boolean submitCdaOutput(
      KarProcessingData data, BsaActionStatus actStatus, HealthcareSetting hs) {
    logger.info("Bsa Action Status:{}", actStatus);

    data.getSubmittedCdaData();

    // Handle Direct.
    if (Boolean.TRUE.equals(hs.getIsDirect())) {

      logger.info(" Sending payload via Direct Transport ");

      directSender.sendEicrDataUsingDirect(data);

      logger.info(" Finished Sending payload via Direct Transport ");

      // Setup a rrCheck schedule job after a delay of 300 seconds.
      if (Boolean.FALSE.equals(ignoreTimers)) {

        logger.info(" Setting up Timer to check for Reportability Responses after 5 minutes ");

        KarExecutionState st =
            data.getKarExecutionStateService().saveOrUpdate(data.getKarExecutionState());

        Duration d = new Duration();
        d.setCode(RR_CHECK_TIME_UNITS);
        d.setUnit(RR_CHECK_TIME_UNITS);
        d.setValue(RR_CHECK_TIME);
        Instant t = ApplicationUtils.convertDurationToInstant(d);

        scheduler.scheduleJob(
            st.getId(),
            getCheckResponseActionId(),
            BsaTypes.ActionType.CHECK_RESPONSE,
            t,
            data.getxRequestId(),
            data.getJobType(),
            MDC.getCopyOfContextMap());

        logger.info(" Finished scheduling timer for checking RR ");

      } else {

        logger.info(
            " Reportability Response will not be ready, so no need to check when timers are ignored ");
      }

    } else if (Boolean.TRUE.equals(hs.getIsRestAPI())
        && restSubmitter != null
        && !StringUtils.isEmpty(data.getSubmittedCdaData())) {

      logger.info(" Submitting to restful endpoint ");
      restSubmitter.sendEicrDataUsingRestfulApi(data);

    } else if (Boolean.TRUE.equals(hs.getIsXdr())) {
      logger.info(" Submitting to XDR endpoint :TO DO");

    } else {

      logger.error(" Unknown Transport Option, cannot submit CDA data ");
    }

    // IF there are no exceptions then no need to set status.
    // Validator errors will be put into the log file.
    // There should not be any errors in production.

    return true;
  }

  public void submitFhirOutput(
      KarProcessingData data, BsaActionStatus actStatus, EhrQueryService ehrService) {

    List<DataRequirement> input = getInputData();
    Set<Resource> resourcesToSubmit = new HashSet<>();

    if (input != null) {

      for (DataRequirement dr : input) {
        Set<Resource> resources =
            data.getDataForId(dr.getId(), this.getInputDataIdToRelatedDataIdMap());

        if (resources != null) resourcesToSubmit.addAll(resources);
      }

    } else {
      logger.info("Input is null");
    }

    if (resourcesToSubmit.isEmpty()) {
      logger.info(" No resources to submit");
    } else {
      logger.info(" {} resource(s) to submit", resourcesToSubmit.size());

      if (data.getHealthcareSetting().getTrustedThirdParty() != null
          && !data.getHealthcareSetting().getTrustedThirdParty().isEmpty()) {
        logger.info(
            "Sending to trusted thrid party {}",
            data.getHealthcareSetting().getTrustedThirdParty());
        submitResources(
            resourcesToSubmit,
            data,
            ehrService,
            data.getHealthcareSetting().getTrustedThirdParty());
      } else if (submissionEndpoint != null && !submissionEndpoint.isEmpty()) {
        logger.info("Sending to submissionEndpoint {}", submissionEndpoint);
        submitResources(resourcesToSubmit, data, ehrService, submissionEndpoint);
      } else {
        Set<UriType> endpoints = data.getKar().getReceiverAddresses();
        logger.info("Sending data to endpoints {} ", endpoints);
        if (!endpoints.isEmpty()) {
          for (UriType uri : endpoints) {
            logger.info("Submitting resources to {}", uri);
            try {
              submitResources(resourcesToSubmit, data, ehrService, uri.getValueAsString());
            } catch (Exception th) {
              logger.error("Error sending data to {} ", uri, th);
            }
          }
        }
      }
      actStatus.setActionStatus(BsaActionStatusType.COMPLETED);
    }
  }

  private void submitResources(
      Set<Resource> resourcesToSubmit,
      KarProcessingData data,
      EhrQueryService ehrService,
      String submissionEndpoint) {
    logger.info("Ehr Query Service:{}", ehrService);

    logger.info("SubmitResources called: sending data to {}", submissionEndpoint);
    PublicHealthAuthority pha;

    if (submissionEndpoint.endsWith("/")) {
      submissionEndpoint = submissionEndpoint.substring(0, submissionEndpoint.length() - 1);
    }

    pha = publicHealthAuthorityService.getPublicHealthAuthorityByUrl(submissionEndpoint);

    if (pha == null) {
      logger.info("PHA is NULL");
      if (!submissionEndpoint.endsWith("$process-message")) {
        pha =
            publicHealthAuthorityService.getPublicHealthAuthorityByUrl(
                String.format("%s/$process-message", submissionEndpoint));
      }
    }

    String token = "";
    if (pha != null) {
      logger.info(
          "Attempting to retrieve TOKEN from PHA {} or {}", pha.getTokenUrl(), pha.getTokenUrl());

      JSONObject obj = authorizationUtils.getToken(pha);

      if (obj != null) {
        token = obj.getString("access_token");
        logger.debug(" Successfully retrieve token {}", token);
      } else {
        logger.error(" Unable to retrieve access token for PHA: {}", pha.getFhirServerBaseURL());
      }

    } else {
      logger.warn("No PHA was found with submission endpoint {}", submissionEndpoint);
      logger.warn("Continuing without auth token");
    }

    Properties headers = new Properties();
    try (InputStream inputStream =
        SubmitReport.class.getClassLoader().getResourceAsStream("report-headers.properties")) {
      headers.load(inputStream);
    } catch (Exception ex) {
      logger.error("Error while loading report headers from Properties File ");
    }

    // for all resources to be submitted
    logger.info("{} Resources to submit ", resourcesToSubmit.size());
    for (Resource r : resourcesToSubmit) {

      IGenericClient client =
          fhirContextInitializer.createClient(
              context, submissionEndpoint, token, data.getxRequestId());

      context.getRestfulClientFactory().setSocketTimeout(120 * 1000);
      context.getRestfulClientFactory().setServerValidationMode(ServerValidationModeEnum.NEVER);

      // All submissions are expected to be bundles
      Bundle bundleToSubmit = (Bundle) r;

      IOperationProcessMsgMode<IBaseResource> operation =
          client.operation().processMessage().setMessageBundle(bundleToSubmit);

      headers.forEach((key, value) -> operation.withAdditionalHeader((String) key, (String) value));

      Bundle responseBundle;
      Object response = null;
      try {

        logger.info(" Trying to invoke $process-message to: {}", submissionEndpoint);
        response = operation.encodedJson().execute();
        logger.info(" Response Received from process message ");
        responseBundle = (Bundle) response;
      } catch (InvalidRequestException ex) {
        String myResp = ex.getResponseBody();
        logger.error(" ResponseBody : ", myResp);
        return;
      } catch (RuntimeException re) {
        logger.error("Error calling $process-message endpoint", re);
        logger.info("Response Object was {}", re);
        return;
      }

      logger.debug("Response is {}", responseBundle);
      if (responseBundle != null) {

        data.addActionOutput(actionId, responseBundle);

        logger.info(" Adding Response Bundle to output using id {}", responseBundle.getId());

        data.addActionOutputById(responseBundle.getId(), responseBundle);
      } else {
        logger.error("Response BUNDLE IS NULL");
      }
    }
  }

  protected String getSubmissionOperation() {
    String endpoint = this.getSubmissionEndpoint();

    // Default operation for message bundles in the case that
    // one isn't specified in the configuration
    if (endpoint == null || !endpoint.contains("$")) {
      return "process-message";
    }

    // Otherwise, parse the operation name
    return endpoint.substring(endpoint.indexOf("$") + 1);
  }

  public String getSubmissionEndpoint() {
    return submissionEndpoint;
  }

  public void setSubmissionEndpoint(String submissionEndpoint) {
    this.submissionEndpoint = submissionEndpoint;
  }

  public RestfulTransportImpl getRestSubmitter() {
    return restSubmitter;
  }

  public void setRestSubmitter(RestfulTransportImpl restSubmitter) {
    this.restSubmitter = restSubmitter;
  }

  public AuthorizationUtils getAuthorizationUtils() {
    return authorizationUtils;
  }

  public void setAuthorizationUtils(AuthorizationUtils authorizationUtils) {
    this.authorizationUtils = authorizationUtils;
  }

  public PublicHealthAuthorityService getPublicHealthAuthorityService() {
    return publicHealthAuthorityService;
  }

  public void setPublicHealthAuthorityService(
      PublicHealthAuthorityService publicHealthAuthorityService) {
    this.publicHealthAuthorityService = publicHealthAuthorityService;
  }

  public FhirContextInitializer getFhirContextInitializer() {
    return fhirContextInitializer;
  }

  public void setFhirContextInitializer(FhirContextInitializer fhirContextInitializer) {
    this.fhirContextInitializer = fhirContextInitializer;
  }

  @Override
  public void printSummary() {

    logger.info(" **** START Printing Derived Action **** ({})", actionId);

    logger.info(" checkResponseActionId : {}", checkResponseActionId);

    logger.info(" Submission End point : {}", submissionEndpoint);

    logger.info(" **** START Printing Derived Action **** ({})", actionId);
  }

  @Override
  public void log() {

    logger.info(" **** START Printing Derived Action **** {}", actionId);

    logger.info(" checkResponseActionId : {}", checkResponseActionId);

    logger.info(" Submission End point : {}", submissionEndpoint);

    logger.info(" **** END Printing Derived Action **** {}", actionId);
  }
}
