package com.drajer.bsa.kar.action;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.impl.GenericClient;
import ca.uhn.fhir.rest.gclient.IOperationProcessMsgMode;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.PublicHealthAuthority;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.sof.utils.FhirContextInitializer;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SubmitReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(SubmitReport.class);

  private String submissionEndpoint;

  private static final FhirContext context = FhirContext.forR4();

  /** The FHIR Context Initializer necessary to retrieve FHIR resources */
  @Autowired FhirContextInitializer fhirContextInitializer;

  @Autowired PublicHealthAuthorityService publicHealthAuthorityService;

  @Autowired BsaServiceUtils bsaServiceUtils;
  @Autowired AuthorizationUtils authorizationUtils;

  PublicHealthMessagesDao phDao;

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

    if (status != BsaActionStatusType.Scheduled || getIgnoreTimers()) {
      logger.info("Action is not timed going through to submission");
      List<DataRequirement> input = getInputData();

      Set<Resource> resourcesToSubmit = new HashSet<>();

      if (input != null) {
        for (DataRequirement dr : input) {
          Set<Resource> resources = data.getOutputDataById(dr.getId());
          resourcesToSubmit.addAll(resources);
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
          if (endpoints.size() > 0) {
            for (UriType uri : endpoints) {
              logger.info("Submitting resources to {}", uri);
              submitResources(resourcesToSubmit, data, ehrService, uri.getValueAsString());
            }
          }
        }
        actStatus.setActionStatus(BsaActionStatusType.Completed);
      }

    } else {

      logger.info(
          " Action may be executed in the future or Conditions have not been met, so cannot proceed any further. ");
      logger.info(" Setting Action Status : {}", status);
      actStatus.setActionStatus(status);
    }

    return actStatus;
  }

  private void submitResources(
      Set<Resource> resourcesToSubmit,
      KarProcessingData data,
      EhrQueryService ehrService,
      String submissionEndpoint) {

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
          "Attempting to retrieve TOKEN from PHA {} or {}", pha.getTokenUrl(), pha.getTokenURL());
      token = authorizationUtils.getToken(pha).getString("access_token");
    } else {
      logger.warn("No PHA was found with submission endpoint {}", submissionEndpoint);
      logger.warn("Continuing without auth token");
    }
    Properties headers = new Properties();
    try (InputStream inputStream =
        SubmitReport.class.getClassLoader().getResourceAsStream("report-headers.properties")) {
      headers.load(inputStream);
    } catch (IOException ex) {
      logger.error("Error while loading Action Classes from Proporties File ");
    }
    // for all resources to be submitted
    logger.info("{} Resources to submit ", resourcesToSubmit.size());
    for (Resource r : resourcesToSubmit) {

      IGenericClient client =
          fhirContextInitializer.createClient(
              context,
              submissionEndpoint,
              token,
              data.getNotificationContext().getNotificationResourceId());

      ((GenericClient) client).setDontValidateConformance(true);
      context.getRestfulClientFactory().setSocketTimeout(30 * 1000);

      // All submissions are expected to be bundles
      Bundle bundleToSubmit = (Bundle) r;

      IOperationProcessMsgMode<IBaseResource> operation =
          client.operation().processMessage().setMessageBundle(bundleToSubmit);
      headers.forEach((key, value) -> operation.withAdditionalHeader((String) key, (String) value));
      logger.info("Calling $processMessage operation");
      Bundle responseBundle;
      Object response = null;
      try {
        response = operation.encodedJson().execute();
        responseBundle = (Bundle) response;
      } catch (RuntimeException re) {

        logger.error("Error calling $process-message endpoint", re);
        logger.info("Response Object was {}", response);
        return;
      }
      logger.info("Response is {}", responseBundle);
      if (responseBundle != null) {
        //          logger.info(
        //              "Response Bundle:::::{}",
        //              context.newJsonParser().encodeResourceToString(responseBundle));

        data.addActionOutput(actionId, responseBundle);

        logger.info(" Adding Response Bundle to output using id {}", responseBundle.getId());

        data.addActionOutputById(responseBundle.getId(), responseBundle);
      } else {
        logger.info("Response BUNDLE IS NULL");
      }

      if (conditionsMet(data)) {

        // Execute sub Actions
        executeSubActions(data, ehrService);

        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }
      bsaServiceUtils.saveEicrState(bundleToSubmit.getIdElement().getIdPart(), bundleToSubmit);
    }
  }

  public String getSubmissionEndpoint() {
    return submissionEndpoint;
  }

  public void setSubmissionEndpoint(String submissionEndpoint) {
    this.submissionEndpoint = submissionEndpoint;
  }
}
