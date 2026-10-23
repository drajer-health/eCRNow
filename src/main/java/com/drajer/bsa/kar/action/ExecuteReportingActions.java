package com.drajer.bsa.kar.action;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.profiler.Profiler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExecuteReportingActions extends BsaAction {

  private final Logger logger = LoggerFactory.getLogger(ExecuteReportingActions.class);

  @Override
  public BsaActionStatus process(KarProcessingData data, EhrQueryService ehrService) {
    BsaActionStatus actStatus = new ExecuteReportingActionsStatus();
    actStatus.setActionId(this.getActionId());

    Profiler profiler = Profiler.get();
    try (Profiler.Step total = profiler.step("Total Execute Reporting Actions Processing")) {

      // Check Timing constraints and handle them before we evaluate conditions.
      BsaActionStatusType status = processTimingData(data);

      // Get the Resources that need to be retrieved.
      try (Profiler.Step input = profiler.step("Input Loading")) {
        ehrService.getFilteredData(data, getInputData());
      }

      // Ensure the activity is In-Progress and the Conditions are met.
      boolean conditionsSatisfied = false;
      if (status != BsaActionStatusType.SCHEDULED) {
        try (Profiler.Step cond = profiler.step("Condition Evaluation")) {
          conditionsSatisfied = Boolean.TRUE.equals(conditionsMet(data, ehrService));
        }
      }

      if (conditionsSatisfied) {

        logger.info(" All conditions in the Actions have been met for {}", this.getActionId());

        // Execute sub Actions
        executeSubActions(data, ehrService);

        // Execute Related Actions.
        executeRelatedActions(data, ehrService);

        actStatus.setActionStatus(BsaActionStatusType.COMPLETED);

      } else {

        logger.info(
            " Action may be executed in the future or Conditions have not been met, so cannot proceed any further. ");
        logger.info(" Setting Action Status : {}", status);
        actStatus.setActionStatus(status);
      }

      data.addActionStatus(data.getExecutionSequenceId(), actStatus);
    }

    return actStatus;
  }
}
