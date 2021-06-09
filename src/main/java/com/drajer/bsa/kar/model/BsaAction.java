package com.drajer.bsa.kar.model;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.eca.model.TimingSchedule;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.hl7.fhir.r4.model.ResourceType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is used to represent the PlanDefinition Action. The model has been simplified for
 * processing as compared to the FHIR Resource for the Action to avoid all the nestings.
 *
 * @author nbashyam
 */
public abstract class BsaAction {

  private final Logger logger = LoggerFactory.getLogger(KnowledgeArtifact.class);

  /** The unique Id for the action. */
  protected String actionId;

  /** The list of named events upon which this action will be triggered, this may be empty. */
  protected Set<String> namedEventTriggers;

  /** The list of input data requirements required for processing of the action. */
  protected List<DataRequirement> inputData;

  /** The list of Resource Types summarized from input Data */
  protected HashMap<String, ResourceType> inputResourceTypes;

  /** The list of output data the action is supposed to create. */
  protected List<DataRequirement> outputData;

  /**
   * The conditions when present and evaluated to true, the action will be executed. If the
   * conditions are not evaluated to true, then the rest of the actions are not processed. If there
   * are more than one conditions in the List, then all of them have to be true.
   */
  private List<BsaCondition> conditions;

  /**
   * The list of related actions that have to be executed once this action is complete. The
   * ActionRelationshipType that is currently handled is only "before-start" per MedMorph. In the
   * future others may be handled.
   */
  private HashMap<ActionRelationshipType, Set<BsaRelatedAction>> relatedActions;

  /**
   * The timing data related to this action, which essentially provides a time offset for executing
   * this action once all the conditions are met.
   */
  private List<TimingSchedule> timingData;

  /**
   * The list of sub actions that need to be executed as part of the parent action, when the
   * conditions are met.
   */
  private List<BsaAction> subActions;

  /** */
  public abstract BsaActionStatus process(KarProcessingData data, EhrQueryService ehrservice);

  public Boolean conditionsMet(KarProcessingData kd) {

    Boolean retVal = true;

    for (BsaCondition bc : conditions) {

      // If any of the conditions evaluate to be false, then the method returns false.
      if (!bc.getConditionProcessor().evaluateExpression(bc, this, kd)) {
        logger.info(" Condition Processing evaluated to false for action {}", this.getActionId());
        retVal = false;
      }
    }

    return retVal;
  }

  public void executeSubActions(KarProcessingData kd, EhrQueryService ehrService) {

    logger.info(" Start Executing Sub Actions for action {}", this.getActionId());

    for (BsaAction act : subActions) {

      logger.info(" Executing Action {}", act.getActionId());
      act.process(kd, ehrService);
    }

    logger.info(" Finished Executing Sub Actions for action {}", this.getActionId());
  }

  public void executeRelatedActions(KarProcessingData kd, EhrQueryService ehrService) {

    logger.info(" Start Executing Related Action for action {}", this.getActionId());

    for (Map.Entry<ActionRelationshipType, Set<BsaRelatedAction>> entry :
        relatedActions.entrySet()) {

      if (entry.getKey() == ActionRelationshipType.BEFORESTART) {

        Set<BsaRelatedAction> actions = entry.getValue();

        for (BsaRelatedAction ract : actions) {

          logger.info(" **** Start Executing Related Action : {} **** ", ract.getRelatedActionId());
          if (ract.getAction() != null) {
            logger.info(" Found the Related Action, hence execuing it ");
            ract.getAction().process(kd, ehrService);
            logger.info(" **** Finished execuing the Related Action. **** ");
          } else {
            logger.info(
                " Related Action not found, so skipping executing of action {} ",
                ract.getRelatedActionId());
          }
        }

      } else {

        logger.info(
            " Not executing Related Action because relationship type is {}", entry.getKey());
      }
    }

    logger.info(" Finished Executing Related Action for action {}", this.getActionId());
  }

  public BsaActionStatusType processTimingData(KarProcessingData kd) {

    if (timingData != null && timingData.size() > 0) {

      // Check and setup future timers.

      return BsaActionStatusType.Scheduled;
    } else {

      logger.info(" No timing data, so continue with the execution of the action ");
      return BsaActionStatusType.InProgress;
    }
  }

  public BsaAction() {

    actionId = "";
    namedEventTriggers = new HashSet<>();
    inputData = new ArrayList<DataRequirement>();
    inputResourceTypes = new HashMap<>();
    outputData = new ArrayList<DataRequirement>();
    conditions = new ArrayList<BsaCondition>();
    relatedActions = new HashMap<>();
    timingData = new ArrayList<TimingSchedule>();
    subActions = new ArrayList<>();
  }

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public Set<String> getNamedEventTriggers() {
    return namedEventTriggers;
  }

  public void setNamedEventTriggers(Set<String> namedEventTriggers) {
    this.namedEventTriggers = namedEventTriggers;
  }

  public List<DataRequirement> getInputData() {
    return inputData;
  }

  public void setInputData(List<DataRequirement> inputData) {
    this.inputData = inputData;
  }

  public List<DataRequirement> getOutputData() {
    return outputData;
  }

  public void setOutputData(List<DataRequirement> outputData) {
    this.outputData = outputData;
  }

  public List<BsaCondition> getConditions() {
    return conditions;
  }

  public void setConditions(List<BsaCondition> conditions) {
    this.conditions = conditions;
  }

  public HashMap<ActionRelationshipType, Set<BsaRelatedAction>> getRelatedActions() {
    return relatedActions;
  }

  public void setRelatedActions(
      HashMap<ActionRelationshipType, Set<BsaRelatedAction>> relatedActions) {
    this.relatedActions = relatedActions;
  }

  public List<TimingSchedule> getTimingData() {
    return timingData;
  }

  public void setTimingData(List<TimingSchedule> timingData) {
    this.timingData = timingData;
  }

  public HashMap<String, ResourceType> getInputResourceTypes() {
    return inputResourceTypes;
  }

  public void setInputResourceTypes(HashMap<String, ResourceType> inputResourceTypes) {
    this.inputResourceTypes = inputResourceTypes;
  }

  public List<BsaAction> getSubActions() {
    return subActions;
  }

  public void setSubActions(List<BsaAction> subActions) {
    this.subActions = subActions;
  }

  public void addInputResourceType(String id, ResourceType rt) {

    if (!inputResourceTypes.containsKey(id)) {
      inputResourceTypes.put(id, rt);
    } else {

      //  nothing to do , it is already present.
    }
  }

  public void addAction(BsaAction action) {
    subActions.add(action);
  }

  public void addCondition(BsaCondition cond) {
    conditions.add(cond);
  }

  public void addRelatedAction(BsaRelatedAction ract) {

    if (relatedActions.containsKey(ract.getRelationship())) {
      relatedActions.get(ract.getRelationship()).add(ract);
    } else {
      Set<BsaRelatedAction> racts = new HashSet<BsaRelatedAction>();
      racts.add(ract);
      relatedActions.put(ract.getRelationship(), racts);
    }
  }

  public void log() {

    logger.info(" **** START Printing Action **** ");

    logger.info(" Action Id : {}", actionId);

    namedEventTriggers.forEach(ne -> logger.info(" Named Event : {}", ne));

    for (DataRequirement inp : inputData) {

      logger.info(" Input Data Req Id : {}", inp.getId());
      logger.info(" Input Data Type : {}", inp.getType());

      if (inp.getProfile() != null && inp.getProfile().size() >= 1) {
        logger.info(" Input Data Profile : {}", inp.getProfile().get(0).asStringValue());
      }

      if (inp.hasCodeFilter()) {

        if (inp.getCodeFilterFirstRep().hasPath()) {
          logger.info(" Code Filter Path : {}", inp.getCodeFilterFirstRep().getPath());
        }

        if (inp.getCodeFilterFirstRep().hasValueSet()) {
          logger.info(" Code Filter Value Set : {}", inp.getCodeFilterFirstRep().getValueSet());
        }
      }

      if (inp.hasCodeFilter()
          && inp.getCodeFilterFirstRep().hasCode()
          && inp.getCodeFilterFirstRep().getCodeFirstRep() != null) {
        logger.info(
            " Input Code Filter Resource : {} ",
            inp.getCodeFilterFirstRep().getCodeFirstRep().getCode());
      }
    }

    for (DataRequirement output : outputData) {

      logger.info(" Output Data Req Id : {}", output.getId());
      logger.info(" Output Data Type : {}", output.getType());

      if (output.getProfile() != null && output.getProfile().size() >= 1) {
        logger.info(" Output Data Profile : {}", output.getProfile().get(0).asStringValue());
      }
    }

    conditions.forEach(con -> con.log());

    if (relatedActions != null)
      relatedActions.forEach((key, value) -> value.forEach(act -> act.log()));

    timingData.forEach(td -> td.print());

    logger.info(" Start Printing Sub Actions ");
    subActions.forEach(act -> act.log());
    logger.info(" Finished Printing Sub Actions ");

    logger.info(" **** END Printing Action **** ");
  }
}
