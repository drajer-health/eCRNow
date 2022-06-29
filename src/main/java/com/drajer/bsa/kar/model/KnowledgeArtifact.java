package com.drajer.bsa.kar.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.UriType;
import org.hl7.fhir.r4.model.ValueSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This entity is a simplified representation of the Knowledge Artifact. While we can use the FHIR
 * objects themselves, this is more of a simplified version for storing and processing. This
 * decision can be re-evaluated based on implementation experience. The originalKarBundle contains
 * the complete KnowledgeArtifact per the MedMorph FHIR IG.
 *
 * @author nbashyam
 * @since 2021-04-15
 */
public class KnowledgeArtifact {

  private final Logger logger = LoggerFactory.getLogger(KnowledgeArtifact.class);

  /**
   * The original Bundle containing all the components of a knowledge artifact. This is flattened
   * into what is required for processing in the other attributes.
   */
  private Bundle originalKarBundle;

  /** The path to the actual KAR that is used to load it into memory. */
  private String karPath;

  /**
   * The unique id for the KnowledgeArtifact, this along with the version will make the
   * KnowledgeArtifact unique.
   */
  private String karId;

  /**
   * The version of the KnowledgeArtifact, this along with the karId will make the KnowledgeArtifact
   * unique.
   */
  private String karVersion;

  /** The human friendly name of the KnowledgeArtifact */
  private String karName;

  /** The publisher name of the KnowledgeArtifact */
  private String karPublisher;

  /**
   * The Map of actions present in the KnowledgeArtifact. The string(Key) is the Action id present
   * in the Knowledge Artifact.
   */
  private HashMap<String, BsaAction> actionMap;

  /**
   * The Map of Named Event Triggers that are mapped to Actions. The string (Key) is the Named Event
   * published in the US PH Named Event value set.
   */
  private HashMap<String, Set<BsaAction>> triggerEventActionMap;

  /**
   * This attribute represents all the default queries that are present in the Knowledge Artifact.
   */
  private HashMap<String, FhirQueryFilter> defaultQueries;

  /**
   * This attribute stores different types of resources that are required for the processing of the
   * Knowledge Artifact. These include the following currently 1. ValueSet Instances that are
   * required for processing. 2. Group Instances that may be required for processing. 3. Library
   * Instances that are required for processing.
   *
   * <p>The Inner HashMap stores the Resource by its full URL so that it can be easily accessed.
   */
  private HashMap<ResourceType, HashMap<String, Resource>> dependencies;

  /** This attribute represents the receivers of the Report created by the BSA. */
  private Set<UriType> receiverAddresses;

  /** This attribute represents the first level actions of the KAR. */
  private List<BsaAction> firstLevelActions;

  public BsaAction getAction(String actionId) {

    if (actionMap != null && actionMap.containsKey(actionId)) return actionMap.get(actionId);
    else return null;
  }

  public Resource getDependentResource(ResourceType rt, String url) {

    if (dependencies.containsKey(rt) && dependencies.get(rt).containsKey(url)) {

      return dependencies.get(rt).get(url);
    }

    return null;
  }

  public void addDependentValueSet(ValueSet res) {

    if (dependencies.containsKey(res.getResourceType())) {

      // Dependency to be added.
      HashMap<String, Resource> resources = dependencies.get(res.getResourceType());

      if (!resources.isEmpty() && resources.containsKey(res.getUrl())) {
        logger.info(" Value Set already present ");
      } else if (res.getUrl() != null) {
        resources.put(res.getUrl(), res);
        dependencies.put(res.getResourceType(), resources);
      }
    } else if (res.getUrl() != null) {
      logger.info("Resource Type does not exist, so add to map ");
      HashMap<String, Resource> resources = new HashMap<>();
      resources.put(res.getUrl(), res);
      dependencies.put(res.getResourceType(), resources);
    }
  }

  public void addDependentResource(Resource res) {
    // format id for local reference
    String id = String.format("%s/%s", res.getResourceType(), res.getIdElement().getIdPart());
    if (dependencies.containsKey(res.getResourceType())) {

      // Dependency to be added.
      HashMap<String, Resource> resources = dependencies.get(res.getResourceType());

      if (resources != null && resources.containsKey(id)) {
        logger.info("Resource already present");
      } else if (resources != null) {
        resources.put(id, res);
        dependencies.put(res.getResourceType(), resources);
      }
    } else {
      logger.info("Resource Type does not exist, so add to map ");
      HashMap<String, Resource> resources = new HashMap<>();
      resources.put(id, res);
      dependencies.put(res.getResourceType(), resources);
    }
  }

  public void initializeRelatedActions() {

    for (Map.Entry<String, BsaAction> entry : actionMap.entrySet()) {

      BsaAction act = entry.getValue();

      if (act.getRelatedActions() != null) {

        for (Map.Entry<ActionRelationshipType, Set<BsaRelatedAction>> ent :
            act.getRelatedActions().entrySet()) {

          Set<BsaRelatedAction> racts = ent.getValue();

          for (BsaRelatedAction ract : racts) {

            ract.setAction(actionMap.get(ract.getRelatedActionId()));
          }
        }
      }
    }
  }

  public void addTriggerEvent(BsaAction act) {

    Set<String> triggers = act.getNamedEventTriggers();

    for (String s : triggers) {

      addTriggerEvent(s, act);
    }

    // Recursively add actions and trigger events
    for (BsaAction action : act.getSubActions()) {

      addTriggerEvent(action);
    }
  }

  public void addTriggerEvent(String event, BsaAction act) {
    if (!triggerEventActionMap.containsKey(event)) {
      Set<BsaAction> acts = new HashSet<>();
      acts.add(act);
      triggerEventActionMap.put(event, acts);
    } else {
      triggerEventActionMap.get(event).add(act);
    }
  }

  /**
   * The method returns the list of variable ids that are used across actions from the Output Data
   * Requirement element of an action for a given Resource Type. For e.g, get the variable Id of the
   * MeasureReport output produced by an action.
   */
  public List<String> getOuputVariableIdsForResourceType(ResourceType type) {

    List<String> retVal = new ArrayList<>();

    for (Map.Entry<String, BsaAction> entry : actionMap.entrySet()) {

      for (DataRequirement dr : entry.getValue().getOutputData()) {

        if (dr.hasType() && dr.getType().contentEquals(type.toString())) {

          retVal.add(dr.getId());
        }
      }
    }

    return retVal;
  }

  public void addReceiverAddress(UriType t) {
    receiverAddresses.add(t);
  }

  public KnowledgeArtifact() {
    originalKarBundle = null;
    karPath = "";
    karId = "";
    karVersion = "";
    actionMap = new HashMap<>();
    triggerEventActionMap = new HashMap<>();
    dependencies = new HashMap<>();
    receiverAddresses = new HashSet<>();
    firstLevelActions = new ArrayList<>();
  }

  public String getKarId() {
    return karId;
  }

  public void setKarId(String karId) {
    this.karId = karId;
  }

  public Bundle getOriginalKarBundle() {
    return originalKarBundle;
  }

  public void setOriginalKarBundle(Bundle originalKarBundle) {
    this.originalKarBundle = originalKarBundle;
  }

  public String getKarVersion() {
    return karVersion;
  }

  public void setKarVersion(String karVersion) {
    this.karVersion = karVersion;
  }

  public Map<String, BsaAction> getActionMap() {
    return actionMap;
  }

  public void setActionMap(HashMap<String, BsaAction> actionMap) {
    this.actionMap = actionMap;
  }

  public Map<String, Set<BsaAction>> getTriggerEventActionMap() {
    return triggerEventActionMap;
  }

  public void setTriggerEventActionMap(HashMap<String, Set<BsaAction>> triggerEventActionMap) {
    this.triggerEventActionMap = triggerEventActionMap;
  }

  public Map<ResourceType, HashMap<String, Resource>> getDependencies() {
    return dependencies;
  }

  public void setDependencies(HashMap<ResourceType, HashMap<String, Resource>> dependencies) {
    this.dependencies = dependencies;
  }

  public String getVersionUniqueId() {
    return this.karId + "|" + this.getKarVersion();
  }

  public Set<BsaAction> getActionsForTriggerEvent(String event) {

    if (triggerEventActionMap != null && triggerEventActionMap.containsKey(event))
      return triggerEventActionMap.get(event);
    else return new HashSet<>();
  }

  public void addAction(BsaAction act) {
    if (!actionMap.containsKey(act.getActionId())) {
      actionMap.put(act.getActionId(), act);

      List<BsaAction> subActions = act.getSubActions();

      if (subActions != null) {

        for (BsaAction subAct : subActions) {

          addAction(subAct);
        }
      }
    }
  }

  public List<BsaAction> getFirstLevelActions() {
    return firstLevelActions;
  }

  public void setFirstLevelActions(List<BsaAction> firstLevelActions) {
    this.firstLevelActions = firstLevelActions;
  }

  public void addFirstLevelAction(BsaAction act) {

    firstLevelActions.add(act);
  }

  public Set<UriType> getReceiverAddresses() {
    return receiverAddresses;
  }

  public void setReceiverAddresses(Set<UriType> receiverAddresses) {
    this.receiverAddresses = receiverAddresses;
  }

  public String getKarPath() {
    return karPath;
  }

  public void setKarPath(String karPath) {
    this.karPath = karPath;
  }

  public String getKarName() {
    return karName;
  }

  public void setKarName(String karName) {
    this.karName = karName;
  }

  public String getKarPublisher() {
    return karPublisher;
  }

  public void setKarPublisher(String karPublisher) {
    this.karPublisher = karPublisher;
  }

  public HashMap<String, FhirQueryFilter> getDefaultQueries() {
    return defaultQueries;
  }

  public void setDefaultQueries(HashMap<String, FhirQueryFilter> defaultQueries) {
    this.defaultQueries = defaultQueries;
  }

  public void printKarSummary() {

    logger.info(" **** START Printing KnowledgeArtifactSummary **** ");

    logger.info(" KAR Id : {}", karId);
    logger.info(" Kar Version : {} ", karVersion);

    firstLevelActions.forEach(act -> act.printSummary());
    defaultQueries.forEach(
        (key, value) -> logger.info(" Data Req Id : {}, Query String: {}", key, value));

    logger.info(" **** END Printing KnowledgeArtifactSummary **** ");
  }

  public void log() {

    logger.info(" **** START Printing Knowledge Artifact **** ");

    logger.info(" KAR Path : {}", karPath);
    logger.info(" KAR Id : {}", karId);
    logger.info(" Kar Version : {} ", karVersion);

    if (actionMap != null) actionMap.forEach((key, value) -> value.log());

    if (dependencies != null) {
      for (Map.Entry<ResourceType, HashMap<String, Resource>> ent : dependencies.entrySet()) {

        logger.info(" Dependency on Resource Type {}", ent.getKey());

        for (Map.Entry<String, Resource> res : ent.getValue().entrySet()) {

          logger.info(" Resource URL : {}", res.getKey());
        }
      }
    }

    logger.info(" **** END Printing Knowledge Artifacts **** ");
  }

  public void populateDefaultQueries(BsaAction action) {

    logger.info(" Populating Default Queries for action {}", action.getActionId());
    if (defaultQueries == null) {
      defaultQueries = new HashMap<>();
      action
          .getInputDataRequirementQueries()
          .forEach((key, value) -> defaultQueries.put(key, value));

    } else {
      action
          .getInputDataRequirementQueries()
          .forEach((key, value) -> defaultQueries.put(key, value));
    }

    logger.info(" Default Queries size = {}", defaultQueries.size());
  }

  public String getQueryForDataRequirement(String dataReqId, String relatedDataId) {

    String queryToExecute = "";
    if (defaultQueries != null && defaultQueries.containsKey(dataReqId)) {
      logger.info(" Found Default Query in KAR for {}", dataReqId);
      queryToExecute = defaultQueries.get(dataReqId).getQueryString();
    }

    if (queryToExecute.isEmpty()
        && relatedDataId != null
        && !relatedDataId.isEmpty()
        && defaultQueries != null
        && defaultQueries.containsKey(relatedDataId)) {
      queryToExecute = defaultQueries.get(relatedDataId).getQueryString();
    }

    return queryToExecute;
  }

  public FhirQueryFilter getQueryFilter(String dataReqId, String relatedDataId) {

    FhirQueryFilter filter = null;
    if (defaultQueries != null && defaultQueries.containsKey(dataReqId)) {
      logger.info(" Found Default Query in KAR for {}", dataReqId);
      filter = defaultQueries.get(dataReqId);
    }

    if (filter == null
        && relatedDataId != null
        && !relatedDataId.isEmpty()
        && defaultQueries != null
        && defaultQueries.containsKey(relatedDataId)) {
      filter = defaultQueries.get(relatedDataId);
    }

    return filter;
  }
}
