package com.drajer.bsa.kar.action;

import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.BsaTypes.OutputContentType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.cda.utils.CdaValidatorUtil;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.OperationOutcome;
import org.hl7.fhir.r4.model.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public class ValidateReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(ValidateReport.class);

  private String validatorEndpoint;

  PublicHealthMessagesDao phDao;

  public PublicHealthMessagesDao getPhDao() {
    return phDao;
  }

  public void setPhDao(PublicHealthMessagesDao phDao) {
    this.phDao = phDao;
  }

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    logger.info(" Executing the Validation of the Report");

    BsaActionStatus actStatus = new ValidateReportStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.Scheduled || getIgnoreTimers()) {

      // Get the Kar.
      KnowledgeArtifact art = data.getKar();
      KnowledgeArtifactStatus artStatus =
          data.getHealthcareSetting().getArtifactStatus(art.getVersionUniqueId());

      if (artStatus != null
          && (artStatus.getOutputFormat() == OutputContentType.CDA_R11
              || artStatus.getOutputFormat() == OutputContentType.CDA_R30)) {

        logger.info(" Validating CDA Output ");
        validateCdaOutput(data, actStatus);
      } else if (artStatus != null && artStatus.getOutputFormat() == OutputContentType.FHIR) {

        logger.info(" Validating FHIR Output ");
        // by default it is FHIR Payload and validate accordingly.
        validateFhirOutput(data, actStatus);
      } else if (artStatus != null && artStatus.getOutputFormat() == OutputContentType.Both) {

        logger.info(" Validating Both CDA and FHIR Output ");
        validateCdaOutput(data, actStatus);
        validateFhirOutput(data, actStatus);
      }

      // Execute Sub and related actions
      if (conditionsMet(data)) {

        // Execute sub Actions
        executeSubActions(data, ehrService);

        // Execute Related Actions.
        executeRelatedActions(data, ehrService);
      }

      actStatus.setActionStatus(BsaActionStatusType.Completed);

    } // Action to be executed
    else {

      logger.info(
          " Action may be executed in the future or Conditions have not been met, so cannot proceed any further. ");
      logger.info(" Setting Action Status : {}", status);
      actStatus.setActionStatus(status);
    }

    data.addActionStatus(data.getExecutionSequenceId(), actStatus);

    return actStatus;
  }

  public boolean validateCdaOutput(KarProcessingData data, BsaActionStatus actStatus) {

    String cda = data.getSubmittedCdaData();

    boolean validatedResult = CdaValidatorUtil.validateEicrXMLData(cda);

    // IF there are no exceptions then no need to set status.
    // Validator errors will be put into the log file.
    // There should not be any errors in production.

    return validatedResult;
  }

  public void validateFhirOutput(KarProcessingData data, BsaActionStatus actStatus) {

    OperationOutcome outcome = new OperationOutcome();

    try {

      List<DataRequirement> input = getInputData();

      Set<Resource> resourcesToValidate = new HashSet<Resource>();

      if (input != null) {

        for (DataRequirement dr : input) {

          Set<Resource> resources = data.getOutputDataById(dr.getId());
          resourcesToValidate.addAll(resources);
        }
      }

      for (Resource r : resourcesToValidate) {

        String request = jsonParser.encodeResourceToString(r);

        logger.debug(" Data to be validated : {}", request);

        if (validatorEndpoint != null && !validatorEndpoint.isEmpty()) {
          ResponseEntity<String> response =
              restTemplate.postForEntity(validatorEndpoint, request, String.class);
          logger.debug(response.getBody());
          outcome = (OperationOutcome) jsonParser.parseResource(response.getBody());
        } else {
          logger.warn("No validation endpoint set. Skipping validation");
        }

        if (ActionUtils.operationOutcomeHasErrors(outcome)) {

          logger.error(
              " Total # of issues found in the Operation Outcome {}", outcome.getIssue().size());

          // For now, go ahead and add the output as being valid.
          addValidatedOutputById(data, r);

        } else {

          logger.info(" No errors found, so add the resource as needed to the outputs ");

          // For now, go ahead and add the output as being valid.
          addValidatedOutputById(data, r);
        }
      } // for

    } catch (Exception e) {

      actStatus.setActionStatus(BsaActionStatusType.Failed);

      outcome
          .addIssue()
          .setSeverity(org.hl7.fhir.r4.model.OperationOutcome.IssueSeverity.ERROR)
          .setDiagnostics(
              "Failed to parse request body as JSON resource. Error was: {}" + e.getMessage());
    }
  }

  public void addValidatedOutputById(KarProcessingData data, Resource res) {

    // If the validation passed, add the Resources to Validated List
    List<DataRequirement> output = getOutputData();

    if (output != null && !output.isEmpty()) {

      for (DataRequirement dr : output) {

        logger.info(" Adding output by Id ");
        data.addActionOutputById(dr.getId(), res);
      }
    } else {
      logger.error(" Plan Definiton Validate Report Action does not have an output.");
    }
  }

  public String getValidatorEndpoint() {
    return validatorEndpoint;
  }

  public void setValidatorEndpoint(String validatorEndpoint) {
    this.validatorEndpoint = validatorEndpoint;
  }
}
