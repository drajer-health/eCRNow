package com.drajer.bsa.kar.model;

import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.action.BsaActionStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.ActionType;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.BsaTypes.BsaJobType;
import com.drajer.bsa.model.KarExecutionState;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.scheduler.BsaScheduler;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.ecrapp.util.ApplicationUtils;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Parameters;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r5.model.Enumerations.QuantityComparator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.web.client.RestTemplate;

/**
 * This class is used to represent the PlanDefinition Action. The model has been simplified for
 * processing as compared to the FHIR Resource for the Action to avoid all the nestings.
 *
 * @author nbashyam
 */
public abstract class BsaAction {

  private final Logger logger = LoggerFactory.getLogger(BsaAction.class);

  /** The unique Id for the action. */
  protected String actionId;

  /** The type of action */
  protected BsaTypes.ActionType type;

  /** The list of named events upon which this action will be triggered, this may be empty. */
  protected Set<String> namedEventTriggers;

  /** The list of input data requirements required for processing of the action. */
  protected List<DataRequirement> inputData;

  /** The list of Resource Types summarized from input Data */
  protected HashMap<String, ResourceType> inputResourceTypes;

  /**
   * The list of Default Queries from the KAR for the specific Input DataRequirement which are part
   * of the action. Any queries associated with Input DataRequirements of the sub actions will be
   * part of the sub-action itself. However when the list of queries are returned, all queries
   * including the ones from the sub-actions will be returned.
   *
   * <p>The Map below contains the Id of the DataRequirement as the key which is unique across the
   * plan definition. The Value is the QueryFilter which contains the default query or the
   * customized queries from a config file.
   */
  protected HashMap<String, FhirQueryFilter> inputDataRequirementQueries;

  /**
   * The attribute contains a mapping of input data requirements to other related data items in the
   * same plan definition that may have already been extracted with queries.
   *
   * <p>for e.g if a Patient is already known, the same patient can be used everywhere and there is
   * no need to query the EHR again. the Related Data would identify the patient already retrieved.
   *
   * <p>The Map below contains the Id of the DataRequirement as the Key which is unique across the
   * plan definition. The Value points to another Id of the DataRequirement which may already have
   * been retrieved from the EHR and can be used for the purposes of executing this action.
   */
  protected HashMap<String, String> inputDataIdToRelatedDataIdMap;

  /** The list of output data the action is supposed to create. */
  protected List<DataRequirement> outputData;

  /**
   * The conditions when present and evaluated to true, the action will be executed. If the
   * conditions are not evaluated to true, then the rest of the actions are not processed. If there
   * are more than one conditions in the List, then all of them have to be true.
   */
  protected List<BsaCondition> conditions;

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

  /** The attribute that holds a definition of a measure in case of measure evaluation */
  private String measureUri;

  /** RestTemplate used to invoke other APIs */
  protected RestTemplate restTemplate;

  /** JSON Parser to deal with FHIR Encode and Decode */
  protected IParser jsonParser;

  /** XML Parser to deal with FHIR Encode and Decode */
  protected IParser xmlParser;

  /** The scheduler that is required to be used to schedule jobs. */
  protected BsaScheduler scheduler;

  /** Attribute that can be set to ignore timers for testing through the same code paths */
  protected Boolean ignoreTimers;

  /** The directory for the output to be written */
  protected String logDirectory;

  /** The method that all actions have to implement to process data. */
  public abstract BsaActionStatus process(KarProcessingData data, EhrQueryService ehrservice);

  public Boolean conditionsMet(KarProcessingData kd, EhrQueryService ehrService) {

    Boolean retVal = true;

    for (BsaCondition bc : conditions) {

      // If any of the conditions evaluate to be false, then the method returns false.
      if (Boolean.FALSE.equals(
          bc.getConditionProcessor().evaluateExpression(bc, this, kd, ehrService))) {
        logger.info(" Condition Processing evaluated to false for action {}", this.getActionId());
        retVal = false;
      } else {
        logger.info(" Condition Processing evaluated to true for action {}", this.getActionId());
      }
    }

    return retVal;
  }

  public void executeSubActions(KarProcessingData kd, EhrQueryService ehrService) {

    logger.info(" *** Start Executing Sub Actions for action {}", this.getActionId());

    for (BsaAction act : subActions) {

      logger.info(" **** Executing Action {}", act.getActionId());
      act.process(kd, ehrService);
    }

    logger.info(" *** Finished Executing Sub Actions for action {}", this.getActionId());
  }

  public void executeRelatedActions(KarProcessingData kd, EhrQueryService ehrService) {

    logger.info(" *** Start Executing Related Action for action {}", this.getActionId());

    for (Map.Entry<ActionRelationshipType, Set<BsaRelatedAction>> entry :
        relatedActions.entrySet()) {

      if (entry.getKey() == ActionRelationshipType.BEFORESTART) {

        Set<BsaRelatedAction> actions = entry.getValue();

        for (BsaRelatedAction ract : actions) {

          if (ract.getDuration() == null && ract.getAction() != null) {

            logger.info(
                " **** Start Executing Related Action: {} **** ", ract.getRelatedActionId());
            ract.getAction().process(kd, ehrService);
            logger.info("**** Finished execuing the Related Action. **** ");

          } else if (ract.getDuration() != null && ract.getAction() != null) {

            logger.info(
                " Found the Related Action, with a duration so need to setup a timer to execute later ");

            // Check if offhours is enabled.
            if (Boolean.TRUE.equals(
                    kd.getHealthcareSetting().getOffhoursEnabled()
                        && ract.getDuration().hasComparator())
                && Objects.equals(
                    ract.getDuration().getComparator().toString(),
                    QuantityComparator.LESS_OR_EQUAL.toString())) {

              logger.info(" Off hours is enabled, so the timers have to be shifted ");

              Instant t =
                  ApplicationUtils.getInstantForOffHours(
                      ract.getDuration(),
                      kd.getHealthcareSetting().getOffHoursStart(),
                      kd.getHealthcareSetting().getOffHoursStartMin(),
                      kd.getHealthcareSetting().getOffHoursEnd(),
                      kd.getHealthcareSetting().getOffHoursEndMin(),
                      kd.getHealthcareSetting().getOffHoursTimezone());

              if (t != null && !ignoreTimers) {

                logger.info("Setting up timer to expire at: {}", t);
                setupTimer(kd, t, ract);
              } else {
                t = ApplicationUtils.convertDurationToInstant(ract.getDuration());

                if (t != null && !ignoreTimers) {

                  logger.info(" Setting up timer to expire at: {}", t);
                  setupTimer(kd, t, ract);
                } else {
                  logger.info(
                      " **** Start Executing Related Action : {} **** ", ract.getRelatedActionId());
                  ract.getAction().process(kd, ehrService);
                  logger.info(" **** Finished execuing the Related Action. **** ");
                }
              }

            } else {

              logger.info(
                  " Does not qualify for off hour timers, so the timers will be set as in the Knowledge Artifact. ");

              Instant t = ApplicationUtils.convertDurationToInstant(ract.getDuration());

              if (t != null && !ignoreTimers) {

                logger.info(" Setting up timer to expire at: {}", t);
                setupTimer(kd, t, ract);
              } else {
                logger.info(
                    " **** Start Executing Related Action : {} **** ", ract.getRelatedActionId());
                ract.getAction().process(kd, ehrService);
                logger.info(" **** Finished execuing the Related Action. **** ");
              }
            }

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

    logger.info(" *** Finished Executing Related Action for action {}", this.getActionId());
  }

  public void setupTimer(KarProcessingData kd, Instant t, BsaRelatedAction ract) {

    logger.info(" Setting timer for Instant {}", t);

    KarExecutionState st = kd.getKarExecutionStateService().saveOrUpdate(kd.getKarExecutionState());

    scheduler.scheduleJob(
        st.getId(),
        ract.getAction().getActionId(),
        ract.getAction().getType(),
        t,
        kd.getxRequestId(),
        kd.getJobType(),
        MDC.getCopyOfContextMap());
  }

  public BsaActionStatusType processTimingData(KarProcessingData kd) {
    logger.info("KarProcessingData:{}", kd);

    if (timingData != null && !timingData.isEmpty() && Boolean.FALSE.equals(ignoreTimers)) {

      // Check and setup future timers.

      return BsaActionStatusType.SCHEDULED;
    } else {

      logger.info(" No timing data, so continue with the execution of the action ");
      return BsaActionStatusType.IN_PROGRESS;
    }
  }

  public void populateParamsForConditionEvaluation(KarProcessingData data) {

    Parameters params = new Parameters();
    for (DataRequirement dr : inputData) {

      Set<Resource> resources = data.getDataForId(dr.getId(), this.getRelatedDataId(dr.getId()));

      BsaServiceUtils.convertDataToParameters(
          dr.getId(),
          dr.getType(),
          (dr.hasLimit() ? Integer.toString(dr.getLimit()) : "*"),
          resources,
          params);
    }

    data.addParameters(actionId, params);
  }

  public void scheduleJob(
      UUID karExecutionStateId,
      String actionId,
      ActionType actType,
      Instant t,
      String xRequestId,
      BsaJobType jobtype,
      Map<String, String> contextMap) {

    scheduler.scheduleJob(
        karExecutionStateId, actionId, actType, t, xRequestId, jobtype, contextMap);
  }

  protected BsaAction() {

    actionId = "";
    namedEventTriggers = new HashSet<>();
    inputData = new ArrayList<>();
    inputResourceTypes = new HashMap<>();
    outputData = new ArrayList<>();
    conditions = new ArrayList<>();
    relatedActions = new HashMap<>();
    timingData = new ArrayList<>();
    subActions = new ArrayList<>();
    measureUri = "";
    inputDataRequirementQueries = new HashMap<>();
    inputDataIdToRelatedDataIdMap = new HashMap<>();
  }

  public String getActionId() {
    return actionId;
  }

  public void setActionId(String actionId, String planDefinitionContext) {
    this.actionId = String.format("%s-PlanDefinition/%s", actionId, planDefinitionContext);
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

  public BsaTypes.ActionType getType() {
    return type;
  }

  public void setType(BsaTypes.ActionType type) {
    this.type = type;
  }

  public String getMeasureUri() {
    return measureUri;
  }

  public void setMeasureUri(String measureUri) {
    this.measureUri = measureUri;
  }

  public BsaScheduler getScheduler() {
    return scheduler;
  }

  public void setScheduler(BsaScheduler scheduler) {
    this.scheduler = scheduler;
  }

  public Boolean getIgnoreTimers() {
    return ignoreTimers;
  }

  public void setIgnoreTimers(Boolean ignoreTimers) {
    this.ignoreTimers = ignoreTimers;
  }

  public RestTemplate getRestTemplate() {
    return restTemplate;
  }

  public void setRestTemplate(RestTemplate restTemplate) {
    this.restTemplate = restTemplate;
  }

  public IParser getJsonParser() {
    return jsonParser;
  }

  public void setJsonParser(IParser jsonParser) {
    this.jsonParser = jsonParser;
  }

  public IParser getXmlParser() {
    return xmlParser;
  }

  public void setXmlParser(IParser xmlParser) {
    this.xmlParser = xmlParser;
  }

  public String getLogDirectory() {
    return logDirectory;
  }

  public void setLogDirectory(String logDirectory) {
    this.logDirectory = logDirectory;
  }

  public void setActionId(String actionId) {
    this.actionId = actionId;
  }

  public HashMap<String, FhirQueryFilter> getInputDataRequirementQueries() {
    return inputDataRequirementQueries;
  }

  public void setInputDataRequirementQueries(
      HashMap<String, FhirQueryFilter> inputDataRequirementQueries) {
    this.inputDataRequirementQueries = inputDataRequirementQueries;
  }

  public HashMap<String, String> getInputDataIdToRelatedDataIdMap() {
    return inputDataIdToRelatedDataIdMap;
  }

  public void setInputDataIdToRelatedDataIdMap(
      HashMap<String, String> inputDataIdToRelatedDataIdMap) {
    this.inputDataIdToRelatedDataIdMap = inputDataIdToRelatedDataIdMap;
  }

  public void addRelatedAction(BsaRelatedAction ract) {

    if (relatedActions.containsKey(ract.getRelationship())) {
      relatedActions.get(ract.getRelationship()).add(ract);
    } else {
      Set<BsaRelatedAction> racts = new HashSet<>();
      racts.add(ract);
      relatedActions.put(ract.getRelationship(), racts);
    }
  }

  public void addQueryFilter(String drId, FhirQueryFilter queryFilter) {

    if (inputDataRequirementQueries.containsKey(drId)) {

      logger.error(" Duplicate Id {} encountered, so this should be flagged  as an error", drId);

    } else {
      logger.debug(" Adding query filter {} to DR Id {}", queryFilter.getQueryString(), drId);
      inputDataRequirementQueries.put(drId, queryFilter);
    }
  }

  public void addRelatedDataId(String drId, String relatedId) {

    if (inputDataIdToRelatedDataIdMap.containsKey(drId)) {

      logger.error(" Duplicate Id {} encountered, so this should be flagged  as an error", drId);

    } else {
      logger.debug(" Adding Related Data Id {} to DR Id {}", relatedId, drId);
      inputDataIdToRelatedDataIdMap.put(drId, relatedId);
    }
  }

  public String getRelatedDataId(String id) {

    if (inputDataIdToRelatedDataIdMap.containsKey(id)) {

      return inputDataIdToRelatedDataIdMap.get(id);

    } else {
      return null;
    }
  }

  public void printSummary() {

    logger.info(" **** START Printing Action **** ({})", actionId);

    logger.info(" Action Type : {}", type);

    namedEventTriggers.forEach(ne -> logger.info(" Named Event : ({})", ne));

    conditions.forEach(con -> con.log());

    if (inputDataRequirementQueries != null)
      inputDataRequirementQueries.forEach(
          (key, value) -> {
            logger.info(" DR ID: {}", key);
            value.log();
          });

    if (inputDataIdToRelatedDataIdMap != null)
      inputDataIdToRelatedDataIdMap.forEach(
          (key, value) -> logger.info(" DR Id : {}, Related Data Id {}", key, value));

    if (relatedActions != null && relatedActions.size() > 0) {

      logger.info(" ****** Number of Related Actions : ({}) ****** ", relatedActions.size());

      for (Map.Entry<ActionRelationshipType, Set<BsaRelatedAction>> entry :
          relatedActions.entrySet()) {

        logger.info(" ****** RelationshipType : ({}) ****** ", entry.getKey());
        Set<BsaRelatedAction> racts = entry.getValue();

        for (BsaRelatedAction ract : racts) {

          logger.info(" ******** Related Action Id : ({}) ******** ", ract.getRelatedActionId());
        }
      }
    }

    if (!subActions.isEmpty()) {

      logger.info(" ****** Number of SubActions : ({}) ****** ", subActions.size());
      for (BsaAction subAct : subActions) {

        logger.info(" ******** Sub Action Id : ({}) ******** ", subAct.getActionId());

        if (subAct.getRelatedActions() != null && subAct.getRelatedActions().size() > 0) {

          for (Map.Entry<ActionRelationshipType, Set<BsaRelatedAction>> entry :
              subAct.getRelatedActions().entrySet()) {

            logger.info(" ********** RelationshipType : ({}) ********** ", entry.getKey());
            Set<BsaRelatedAction> racts = entry.getValue();

            for (BsaRelatedAction ract : racts) {

              logger.info(
                  " ************ Related Action Id : ({}) ************ ",
                  ract.getRelatedActionId());
            }
          }
        } else {

          logger.info(
              " ********** No Related Actions for sub Action : ({}) ********** ",
              subAct.getActionId());
        }

        logger.info(" START Printing Sub Actions Data Requirements ");

        if (subAct.getInputDataRequirementQueries() != null)
          subAct
              .getInputDataRequirementQueries()
              .forEach(
                  (key, value) -> {
                    logger.info(" DR Id: {}", key);
                    value.log();
                  });

        if (subAct.getInputDataIdToRelatedDataIdMap() != null)
          subAct
              .getInputDataIdToRelatedDataIdMap()
              .forEach((key, value) -> logger.info(" DR Id : {}, RelatedData Id {}", key, value));

        logger.info(" END Printing Sub Actions Data Requirements ");
      }

    } else {
      logger.info(" ******** No Sub Actions for : ({}) ******** ", actionId);
    }

    logger.info(" **** END Printing Action **** {}", actionId);
  }

  public void log() {

    logger.info(" **** START Printing Action **** {}", actionId);

    logger.info(" Action Type : {}", type);
    namedEventTriggers.forEach(ne -> logger.info(" Named Event : {}", ne));

    for (DataRequirement inp : inputData) {

      logger.info(" Input Data Req Id : {}", inp.getId());
      logger.info(" Input Data Type : {}", inp.getType());

      if (inp.getProfile() != null && !inp.getProfile().isEmpty()) {
        String inputDataProfile = inp.getProfile().get(0).asStringValue();
        logger.info(" Input Data Profile : {}", inputDataProfile);
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

      if (output.getProfile() != null && !output.getProfile().isEmpty()) {
        String outputDataProfile = output.getProfile().get(0).asStringValue();
        logger.info(" Output Data Profile : {}", outputDataProfile);
      }
    }

    conditions.forEach(BsaCondition::log);

    if (relatedActions != null)
      relatedActions.forEach((key, value) -> value.forEach(BsaRelatedAction::log));

    if (inputDataRequirementQueries != null)
      inputDataRequirementQueries.forEach(
          (key, value) -> {
            logger.info(" DR Id: {}", key);
            value.log();
          });

    if (inputDataIdToRelatedDataIdMap != null)
      inputDataIdToRelatedDataIdMap.forEach(
          (key, value) -> logger.info(" DR Id : {}, RelatedData Id {}", key, value));

    timingData.forEach(TimingSchedule::print);

    logger.info(" Start Printing Sub Actions ");
    subActions.forEach(BsaAction::log);
    logger.info(" Finished Printing Sub Actions ");

    logger.info(" **** END Printing Action **** {}", actionId);
  }
}
