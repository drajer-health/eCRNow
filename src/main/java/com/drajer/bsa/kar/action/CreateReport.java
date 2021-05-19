package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.ecrapp.util.ApplicationUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Set;
import org.hl7.fhir.r4.model.CanonicalType;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CreateReport extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(CreateReport.class);

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {

    BsaActionStatus actStatus = new CreateReportStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.Scheduled) {

      logger.info(
          " Action {} can proceed as it does not have timing information ", this.getActionId());

      // Get the Resources that need to be retrieved.
      HashMap<String, ResourceType> resourceTypes = getInputResourceTypes();

      // Get necessary data to process.
      HashMap<ResourceType, Set<Resource>> res = ehrService.getFilteredData(data, resourceTypes);

      // Get the Output Data Requirement to determine the type of bundle to create.
      for (DataRequirement dr : outputData) {

        if (dr.hasProfile()) {

          List<CanonicalType> profiles = dr.getProfile();

          for (CanonicalType ct : profiles) {

            logger.info("Getting Report Creator for  {}", ct.asStringValue());
            ReportCreator rc = ReportCreator.getReportCreator(ct.asStringValue());

            if (rc != null) {

              logger.info("Start creating report");
              Resource output = rc.createReport(data, ehrService, dr.getId(), ct.asStringValue());
              logger.info("Finished creating report");

              if (output != null) {

                logger.info(" Adding Report to output generated {}", output.getId());
                data.addActionOutput(actionId, output);
               
              }
            }
          }
        }
      }

      if (conditionsMet(data)) {

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

    return actStatus;
  }
}
