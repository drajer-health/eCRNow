package com.drajer.bsa.kar.model;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
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
  Bundle originalKarBundle;

  /**
   * The unique id for the KnowledgeArtifact, this along with the version will make the
   * KnowledgeArtifact unique.
   */
  String karId;

  /**
   * The version of the KnowledgeArtifact, this along with the karId will make the KnowledgeArtifact
   * unique.
   */
  String karVersion;

  /**
   * The Map of actions present in the KnowledgeArtifact. The string(Key) is the Action id published
   * in the US PH Action Value Set.
   */
  HashMap<String, BsaAction> actionMap;

  /**
   * The Map of Named Event Triggers that are mapped to Actions. The string (Key) is the Named Event
   * published in the US PH Named Event value set.
   */
  HashMap<String, Set<BsaAction>> triggerEventActionMap;

  /**
   * This attribute stores different types of resources that are required for the processing of the
   * Knowledge Artifact. These include the following currently 1. ValueSet Instances that are
   * required for processing. 2. Group Instances that may be required for processing. 3. Library
   * Instances that are required for processing.
   *
   * <p>The Inner HashMap stores the Resource by its full URL so that it can be easily accessed.
   */
  HashMap<ResourceType, HashMap<String, Resource>> dependencies;

  public KnowledgeArtifact() {
    originalKarBundle = null;
    karId = "";
    karVersion = "";
    actionMap = new HashMap<String, BsaAction>();
    triggerEventActionMap = new HashMap<String, Set<BsaAction>>();
    dependencies = new HashMap<ResourceType, HashMap<String, Resource>>();
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

  public HashMap<String, BsaAction> getActionMap() {
    return actionMap;
  }

  public void setActionMap(HashMap<String, BsaAction> actionMap) {
    this.actionMap = actionMap;
  }

  public HashMap<String, Set<BsaAction>> getTriggerEventActionMap() {
    return triggerEventActionMap;
  }

  public void setTriggerEventActionMap(HashMap<String, Set<BsaAction>> triggerEventActionMap) {
    this.triggerEventActionMap = triggerEventActionMap;
  }

  public HashMap<ResourceType, HashMap<String, Resource>> getDependencies() {
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
    else return new HashSet<BsaAction>();
  }

  public void addAction(BsaAction act) {
    if (!actionMap.containsKey(act.getActionId())) {
      actionMap.put(act.getActionId(), act);
    }
  }

  public void addTriggerEvent(BsaAction act) {

    Set<String> triggers = act.getNamedEventTriggers();

    for (String s : triggers) {

      addTriggerEvent(s, act);
    }
  }

  public void addTriggerEvent(String event, BsaAction act) {
    if (!triggerEventActionMap.containsKey(event)) {
      Set<BsaAction> acts = new HashSet<BsaAction>();
      acts.add(act);
      triggerEventActionMap.put(event, acts);
    } else {
      triggerEventActionMap.get(event).add(act);
    }
  }

  public void log() {

    logger.info(" **** START Printing Knowledge Artifact **** ");

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
}
