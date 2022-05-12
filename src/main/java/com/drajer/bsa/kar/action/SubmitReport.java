package com.drajer.bsa.kar.action;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.client.api.IGenericClient;
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
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import com.drajer.bsa.routing.impl.RestfulTransportImpl;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.ecrapp.util.ApplicationUtils;
import java.time.Instant;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class SubmitReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(SubmitReport.class);

  private String submissionEndpoint;

  private static final int rrCheckTime = 60;
  private static final String rrCheckTimeUnits = "s";

  private static final FhirContext context = FhirContext.forR4();

  private BsaServiceUtils bsaServiceUtils;

  private PublicHealthMessagesDao phDao;

  private DirectTransportImpl directSender;

  private RestfulTransportImpl restfulSubmitter;

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

    if (status != BsaActionStatusType.Scheduled || getIgnoreTimers()) {

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
        submitFhirOutput(data, actStatus);
      } else if (artStatus != null && artStatus.getOutputFormat() == OutputContentType.Both) {

        logger.info(" Submitting Both CDA and FHIR Output ");
        submitCdaOutput(data, actStatus, data.getHealthcareSetting());
        submitFhirOutput(data, actStatus);
      }

      if (conditionsMet(data)) {

        // Execute sub Actions
        executeSubActions(data, ehrService);

        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }

      actStatus.setActionStatus(BsaActionStatusType.Completed);

    } else {

      logger.info(
          " Action may be executed in the future or Conditions have not been met, so cannot proceed any further. ");
      logger.info(" Setting Action Status : {}", status);
      actStatus.setActionStatus(status);
    }

    data.addActionStatus(data.getExecutionSequenceId(), actStatus);

    return actStatus;
  }

  public boolean submitCdaOutput(
      KarProcessingData data, BsaActionStatus actStatus, HealthcareSetting hs) {

    String cda = data.getSubmittedCdaData();

    if (hs.getIsDirect()) {

      logger.info(" Sending payload via Direct Transport ");

      directSender.sendEicrDataUsingDirect(data);

      logger.info(" Finished Sending payload via Direct Transport ");

      // Setup a rrCheck schedule job after a delay of 300 seconds.
      if (!ignoreTimers) {

        logger.info(" Setting up Timer to check for Reportability Responses after 5 minutes ");

        KarExecutionState st =
            data.getKarExecutionStateService().saveOrUpdate(data.getKarExecutionState());

        Duration d = new Duration();
        d.setCode(rrCheckTimeUnits);
        d.setUnit(rrCheckTimeUnits);
        d.setValue(rrCheckTime);
        Instant t = ApplicationUtils.convertDurationToInstant(d);

        scheduler.scheduleJob(
            st.getId(), getCheckResponseActionId(), BsaTypes.ActionType.CheckResponse, t);

        logger.info(" Finished scheduling timer for checking RR ");

      } else {

        logger.info(
            " Reportability Response will not be ready, so no need to check when timers are ignored ");
      }

    } else if (hs.getIsRestAPI()) {

    } else if (hs.getIsXdr()) {

    } else {

      logger.error(" Unknown Transport Option, cannot submit CDA data ");
    }

    // IF there are no exceptions then no need to set status.
    // Validator errors will be put into the log file.
    // There should not be any errors in production.

    return true;
  }

  public void submitFhirOutput(KarProcessingData data, BsaActionStatus actStatus) {

    List<DataRequirement> input = getInputData();

    if (input != null) {

      for (DataRequirement dr : input) {
        Set<Resource> resources = data.getOutputDataById(dr.getId());
        if (resources != null && !resources.isEmpty()) {

          // get a restful client and the submission endpoint
          context.getRestfulClientFactory().setSocketTimeout(30 * 1000);
          IGenericClient client = context.newRestfulGenericClient(submissionEndpoint);

          for (Resource r : resources) {

            // All submissions are expected to be bundles
            Bundle bundleToSubmit = (Bundle) r;
            if (logger.isInfoEnabled()) {
              logger.info(
                  " Submit Bundle:::::{}",
                  context.newJsonParser().encodeResourceToString(bundleToSubmit));
            }

            Bundle responseBundle =
                client
                    .operation()
                    .onServer()
                    .named(this.getSubmissionOperation())
                    .withParameter(Parameters.class, "content", bundleToSubmit)
                    .returnResourceType(Bundle.class)
                    .execute();

            if (responseBundle != null) {
              logger.debug(
                  "Response Bundle:::::{}",
                  context.newJsonParser().encodeResourceToString(responseBundle));

              data.addActionOutput(actionId, responseBundle);

              logger.info(" Adding Response Bundle to output using id {}", responseBundle.getId());

              data.addActionOutputById(responseBundle.getId(), responseBundle);
            }
          } // for
        } else {
          logger.error(" No resources to submit based on input data requirement ");
        }
      }
    } else {

      logger.error(" No input data requirement identifying the data that has to be submitted ");
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

  public void printSummary() {

    logger.info(" **** START Printing Derived Action **** ({})", actionId);

    logger.info(" checkResponseActionId : {}", checkResponseActionId);

    logger.info(" **** START Printing Derived Action **** ({})", actionId);
  }

  public void log() {

    logger.info(" **** START Printing Derived Action **** {}", actionId);

    logger.info(" checkResponseActionId : {}", checkResponseActionId);

    logger.info(" **** END Printing Derived Action **** {}", actionId);
  }
}
