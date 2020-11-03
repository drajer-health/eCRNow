package com.drajer.eca.model;

import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.sof.model.LaunchDetails;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractAction {

  public abstract void execute(Object obj, WorkflowEvent launchType);

  private String actionId;

  private List<AbstractCondition> preConditions;
  private List<RelatedAction> relatedActions;
  private List<TimingSchedule> timingData;
  private List<ActionData> triggerData;

  private final Logger logger = LoggerFactory.getLogger(AbstractAction.class);

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public List<AbstractCondition> getPreConditions() {
    return preConditions;
  }

  public void setPreConditions(List<AbstractCondition> preConditions) {
    this.preConditions = preConditions;
  }

  public List<RelatedAction> getRelatedActions() {
    return relatedActions;
  }

  public void setRelatedActions(List<RelatedAction> relatedActions) {
    this.relatedActions = relatedActions;
  }

  public List<TimingSchedule> getTimingData() {
    return timingData;
  }

  public void setTimingData(List<TimingSchedule> timingData) {
    this.timingData = timingData;
  }

  public List<ActionData> getTriggerData() {
    return triggerData;
  }

  public void setTriggerData(List<ActionData> triggerData) {
    this.triggerData = triggerData;
  }

  public void addActionData(ActionData act) {

    if (triggerData == null) {
      triggerData = new ArrayList<>();
    }

    triggerData.add(act);
  }

  public void addCondition(AbstractCondition cond) {

    if (preConditions == null) {
      preConditions = new ArrayList<>();
    }

    preConditions.add(cond);
  }

  public void addRelatedAction(RelatedAction ra) {

    if (relatedActions == null) {
      relatedActions = new ArrayList<>();
    }

    relatedActions.add(ra);
  }

  public void addTimingData(TimingSchedule ts) {

    if (timingData == null) {
      timingData = new ArrayList<>();
    }

    timingData.add(ts);
  }

  public abstract void print();

  public void printBase() {

    logger.info(" Action Id = {}", actionId);

    if (preConditions != null) {

      for (AbstractCondition c : preConditions) {
        c.print();
      }
    }

    if (relatedActions != null) {

      for (RelatedAction ra : relatedActions) {
        ra.print();
      }
    }

    if (timingData != null) {

      for (TimingSchedule ts : timingData) {
        ts.print();
      }
    }

    if (triggerData != null) {

      for (ActionData ad : triggerData) {
        ad.print();
      }
    }
  }

  public boolean matchCondition(LaunchDetails details) {
    boolean conditionsMet = true;
    if (getPreConditions() != null && !getPreConditions().isEmpty()) {

      logger.info(" Evaluating PreConditions ");
      List<AbstractCondition> conds = getPreConditions();

      for (AbstractCondition cond : conds) {

        if (!cond.evaluate(details)) {
          if (logger.isInfoEnabled()) {
            logger.info(" Condition Not met {}", cond.getConditionType().toString());
          }
          conditionsMet = false;
        }
      }
    }
    return conditionsMet;
  }
}
