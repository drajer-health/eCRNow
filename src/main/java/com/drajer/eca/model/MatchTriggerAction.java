package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.Dstu2FhirData;
import com.drajer.sof.model.FhirData;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MatchTriggerAction extends AbstractAction {

  private final Logger logger = LoggerFactory.getLogger(MatchTriggerAction.class);

  @Override
  public void execute(Object obj, WorkflowEvent launchType) {

    logger.info(" Executing Match Trigger Action ");

    if (obj instanceof LaunchDetails) {

      LaunchDetails details = (LaunchDetails) obj;

      PatientExecutionState state = null;

      state = ApplicationUtils.getDetailStatus(details);
      state.getMatchTriggerStatus().setActionId(getActionId());

      // Execute the Match Trigger Action, even if it completed, because it could be invoked
      // multiple times from
      // other EICR Actions.
      logger.info(
          " Executing Match Trigger Action , Prior Execution State : = {}", details.getStatus());

      // Call the Trigger Queries.
      if (ActionRepo.getInstance().getTriggerQueryService() != null) {

        logger.info(" Getting necessary data from Trigger Queries ");
        FhirData data =
            ActionRepo.getInstance()
                .getTriggerQueryService()
                .getData(details, details.getStartDate(), details.getEndDate());

        if (data != null && data instanceof Dstu2FhirData) {

          Dstu2FhirData dstu2Data = (Dstu2FhirData) data;

          // For Match Trigger Action, we expect the following
          // No preConditions
          // No relatedActions
          // No timingData

          // We only expect to match codes. So get the paths and match the codes.
          if (getTriggerData() != null && !getTriggerData().isEmpty()) {

            // we have triggers to match against COVID Value Sets for now.
            // In the future we will use the specific paths provided by the ersd spec to match.

            List<ActionData> codePaths = getTriggerData();

            EcaUtils.matchTriggerCodesForDSTU2(codePaths, dstu2Data, state, details);

          } else {

            String msg = "No Trigger Data to match trigger Codes.";
            logger.error(msg);

            throw new RuntimeException(msg);
          }

          // Job is completed, even if it did not match.
          // The next job has to check the Match Status to see if something needs to be reported, it
          // may elect to run the matching again
          // because data may be entered late even though the app was launched.
          state.getMatchTriggerStatus().setJobStatus(JobStatus.COMPLETED);
          EcaUtils.updateDetailStatus(details, state);

        } else if (data != null && data instanceof R4FhirData) {

          R4FhirData r4Data = (R4FhirData) data;

          // For Match Trigger Action, we expect the following
          // No preConditions
          // No relatedActions
          // No timingData

          // We only expect to match codes. So get the paths and match the codes.
          if (getTriggerData() != null && !getTriggerData().isEmpty()) {

            // we have triggers to match against COVID Value Sets for now.
            // In the future we will use the specific paths provided by the ersd spec to match.

            List<ActionData> codePaths = getTriggerData();

            EcaUtils.matchTriggerCodesForR4(codePaths, r4Data, state, details);

          } else {

            String msg = "No Trigger Data to match trigger Codes.";
            logger.error(msg);

            throw new RuntimeException(msg);
          }

          // Job is completed, even if it did not match.
          // The next job has to check the Match Status to see if something needs to be reported, it
          // may elect to run the matching again
          // because data may be entered late even though the app was launched.
          state.getMatchTriggerStatus().setJobStatus(JobStatus.COMPLETED);
          EcaUtils.updateDetailStatus(details, state);

        } else {

          String msg = "No Fhir Data retrieved to match trigger Codes.";
          logger.error(msg);

          throw new RuntimeException(msg);
        }
      } else {

        String msg =
            "System Startup Issue, Spring Injection not functioning properly, trigger service is null.";
        logger.error(msg);

        throw new RuntimeException(msg);
      }

    } else {

      String msg =
          "Invalid Object passed to Execute method, Launch Details expected, found : "
              + obj.getClass().getName();
      logger.error(msg);

      throw new RuntimeException(msg);
    }
  }

  @Override
  public void print() {

    logger.info(" **** Printing MatchTriggerAction **** ");
    printBase();
    logger.info(" **** End Printing MatchTriggerAction **** ");
  }
}
