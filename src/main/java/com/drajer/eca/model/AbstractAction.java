package com.drajer.eca.model;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.drajer.eca.model.EventTypes.EcrActionTypes;
import com.drajer.eca.model.EventTypes.JobStatus;
import com.drajer.eca.model.EventTypes.WorkflowEvent;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.util.ApplicationUtils;
import com.drajer.sof.model.LaunchDetails;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.JsonProcessingException;

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

		logger.info(" Action Id = {}" , actionId);

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

	public void handleException(Exception e1, Logger logger, String msg) {
		logger.error(msg,e1);
		throw new RuntimeException(msg,e1);
	}

	public boolean matchCondition(LaunchDetails details, Boolean conditionsMet) {
		if (getPreConditions() != null && getPreConditions().size() > 0) {

			logger.info(" Evaluating PreConditions ");
			List<AbstractCondition> conds = getPreConditions();

			for (AbstractCondition cond : conds) {

				if (!cond.evaluate(details)) {
					logger.info(" Condition Not met {}" , cond.getConditionType().toString());
					conditionsMet = false;
				}
			}
		}
		return conditionsMet;
	}
	

	public PatientExecutionState recheckTriggerCodes(LaunchDetails details, WorkflowEvent launchType) {

		Set<AbstractAction> acts = ActionRepo.getInstance().getActions().get(EcrActionTypes.MATCH_TRIGGER);
		for (AbstractAction act : acts) {
			act.execute(details, launchType);
			ActionRepo.getInstance().getLaunchService().saveOrUpdate(details);
		}

		ObjectMapper mapper = new ObjectMapper();
		PatientExecutionState newState = null;

		try {
			newState = mapper.readValue(details.getStatus(), PatientExecutionState.class);
			logger.info(" Successfully set the State value ");
		}  catch (JsonProcessingException e1) {
			String msg = "Unable to read/write execution state";
			handleException(e1, logger, msg);
		}

		return newState;
	}
	

}
