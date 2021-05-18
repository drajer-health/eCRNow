package com.drajer.bsa.kar.action;

import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The class provides implementation for the InitiateReporting Workflow.
 *
 * @author nbashyam
 */
public class InitiateReporting extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(InitiateReporting.class);

  @Override
  public BsaActionStatus process(KarProcessingData data) {

    BsaActionStatus actStatus = new InitiateReportingStatus();
    actStatus.setActionId(this.getActionId());

    // Check Timing constraints and handle them before we evaluate conditions.
    BsaActionStatusType status = processTimingData(data);

    // Ensure the activity is In-Progress and the Conditions are met.
    if (status != BsaActionStatusType.Scheduled && conditionsMet(data)) {

      logger.info(" All conditions in the Actions have been met for {}", this.getActionId());

      // Execute Related Actions.
      executeRelatedActions(data);

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
