package com.drajer.ecrapp.config;

import com.drajer.ecrapp.util.ApplicationUtils;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.ValueSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValueSetSingleton {

  private static ValueSetSingleton instance;

  private final Logger logger = LoggerFactory.getLogger(ValueSetSingleton.class);

  private Set<ValueSet> emergentValueSets;
  private Set<String> emergentValueSetsAsString;
  private Set<ValueSet> valueSets;
  private Set<ValueSet> grouperValueSets;

  private Map<String, Set<ValueSet>> triggerPathToValueSetsMap;
  private Map<String, Set<ValueSet>> triggerPathToGrouperMap;
  private Map<String, Set<ValueSet>> grouperToValueSetMap;
  private Map<String, Set<ValueSet>> grouperToEmergentValueSetMap;

  public void print() {

    if (logger.isInfoEnabled()) {
      logger.info(" *** Start Printing Value Set Singleton *** ");

      logger.info(" *** Start Printing Grouper To Value Sets **** {}", grouperToValueSetMap.size());

      for (Map.Entry<String, Set<ValueSet>> entry : grouperToValueSetMap.entrySet()) {

        logger.info(" Key = " + entry.getKey());

        Set<ValueSet> vs = entry.getValue();

        if (vs != null) {
          for (ValueSet v : vs) {

            logger.info(" Value Set = {} {}", v.getId(), v.getUrl());
          }
        }
      }

      logger.info(" *** End Printing Grouper To Value Sets **** ");

      if (grouperToEmergentValueSetMap != null) {

        logger.info(
            " *** Start Printing Grouper To Emergent Value Sets **** {}",
            grouperToEmergentValueSetMap.size());

        for (Map.Entry<String, Set<ValueSet>> coventry : grouperToEmergentValueSetMap.entrySet()) {

          logger.info(" Key = {}", coventry.getKey());

          Set<ValueSet> vs = coventry.getValue();

          if (vs != null && !vs.isEmpty()) {

            for (ValueSet v : vs) {

              logger.info(" Value Set = {} {}", v.getId(), v.getUrl());
            }
          }
        }
      }

      logger.info(" *** Start Printing Grouper To Emergent Value Sets **** ");

      logger.info(" *** End Printing Value Set Singleton *** ");
    }
  }

  public Map<String, Set<ValueSet>> getGrouperToValueSetMap() {
    return grouperToValueSetMap;
  }

  public void setGrouperToValueSetMap(Map<String, Set<ValueSet>> grouperToValueSetMap) {
    this.grouperToValueSetMap = grouperToValueSetMap;
  }

  public Map<String, Set<ValueSet>> getGrouperToEmergentValueSetMap() {
    return grouperToEmergentValueSetMap;
  }

  public void setGrouperToEmergentValueSetMap(
      Map<String, Set<ValueSet>> grouperToEmergentValueSetMap) {
    this.grouperToEmergentValueSetMap = grouperToEmergentValueSetMap;
  }

  public Set<String> getEmergentValueSetsAsString() {
    return emergentValueSetsAsString;
  }

  public void setEmergentValueSetsAsString(Set<String> emergentValueSetsAsString) {
    this.emergentValueSetsAsString = emergentValueSetsAsString;
  }

  public static void setInstance(ValueSetSingleton instance) {
    ValueSetSingleton.instance = instance;
  }

  public static ValueSetSingleton getInstance() {
    if (instance == null) {
      instance = new ValueSetSingleton();
    }
    return instance;
  }

  public Set<ValueSet> getEmergentValueSets() {
    if (emergentValueSets == null) {
      emergentValueSets = new HashSet<>();
    }
    return emergentValueSets;
  }

  public void setEmergentValueSets(Set<ValueSet> emergentValueSets) {
    this.emergentValueSets = emergentValueSets;
    this.emergentValueSetsAsString = ApplicationUtils.convertValueSetsToString(emergentValueSets);
  }

  public Set<ValueSet> getValueSets() {
    if (valueSets == null) {
      valueSets = new HashSet<>();
    }
    return valueSets;
  }

  public void setValueSets(Set<ValueSet> valueSets) {
    this.valueSets = valueSets;
  }

  public Set<ValueSet> getGrouperValueSets() {
    return grouperValueSets;
  }

  public void setGrouperValueSets(Set<ValueSet> grouperValueSets) {
    this.grouperValueSets = grouperValueSets;
  }

  public Map<String, Set<ValueSet>> getTriggerPathToValueSetsMap() {
    if (triggerPathToValueSetsMap == null) {
      triggerPathToValueSetsMap = new HashMap<>();
    }
    return triggerPathToValueSetsMap;
  }

  public void setTriggerPathToValueSetsMap(Map<String, Set<ValueSet>> triggerPathToValueSetsMap) {
    this.triggerPathToValueSetsMap = triggerPathToValueSetsMap;
  }

  public Map<String, Set<ValueSet>> getTriggerPathToGrouperMap() {
    if (triggerPathToGrouperMap == null) {
      triggerPathToGrouperMap = new HashMap<>();
    }
    return triggerPathToGrouperMap;
  }

  public void setTriggerPathToGrouperMap(Map<String, Set<ValueSet>> triggerPathToGrouperMap) {
    this.triggerPathToGrouperMap = triggerPathToGrouperMap;
  }

  public void addGrouperToValueSetMap(String grouper, Set<ValueSet> vs) {

    if (grouperToValueSetMap != null) {

      if (grouperToValueSetMap.containsKey(grouper)) {

        logger.info("Value sets for Grouper {} Size = {} is already added", grouper, vs.size());
        // grouperToValueSetMap.get(grouper).addAll(vs);

      } else {

        logger.info(
            " Creating new entry and adding value sets for Grouper {} Size = {}",
            grouper,
            vs.size());

        grouperToValueSetMap.put(grouper, vs);
      }

    } else {

      logger.info(
          " Creating new entry and adding value sets for Grouper {} Size = {}", grouper, vs.size());

      grouperToValueSetMap = new HashMap<>();

      grouperToValueSetMap.put(grouper, vs);
    }
  }

  public void addGrouperToEmergentValueSetMap(String grouper, Set<ValueSet> vs) {

    if (grouperToEmergentValueSetMap != null) {

      if (grouperToEmergentValueSetMap.containsKey(grouper)) {
        logger.info(
            "Emergent Value sets for Grouper that exists {} Size = {} is already added",
            grouper,
            vs.size());
        // grouperToEmergentValueSetMap.get(grouper).addAll(vs);

      } else {

        logger.info(
            " Creating new entry and adding Emergent value sets for Grouper that does not exist {} Size = {}",
            grouper,
            vs.size());

        grouperToEmergentValueSetMap.put(grouper, vs);
      }

    } else {

      logger.info(
          " Creating new entry and adding Emergent Value sets for Grouper  {} Size = {}",
          grouper,
          vs.size());
      grouperToEmergentValueSetMap = new HashMap<>();

      grouperToEmergentValueSetMap.put(grouper, vs);
    }
  }

  public Set<String> getValueSetsAsStringForGrouper(String path) {

    String grouperId = null;
    Set<String> retVal = new HashSet<>();
    Set<ValueSet> grouperValueSet = getTriggerPathToGrouperMap().get(path);
    if (grouperValueSet != null) {
      logger.debug("Found the grouper value set for {} with size {}", path, grouperValueSet.size());
      for (ValueSet g : grouperValueSet) {
        grouperId = g.getId();
        if (grouperId != null && getGrouperToValueSetMap() != null) {
          Set<ValueSet> valueSets = getGrouperToValueSetMap().get(grouperId);
          if (valueSets != null && !valueSets.isEmpty()) {
            logger.debug("Found {} Value Sets for grouper {}", valueSets.size(), grouperId);
            retVal.addAll(ApplicationUtils.convertValueSetsToString(valueSets));
          } else {
            logger.debug("Didn't find value sets for grouper {}", grouperId);
          }
        } else {
          logger.debug("Didn't find the value sets for the grouper for path {}", path);
        }
      }
    }
    logger.debug("Found {} Value Sets for path {}", retVal.size(), path);
    return retVal;
  }

  public Set<String> getEmergentValueSetsAsStringForGrouper(String path) {

    String grouperId = null;
    Set<String> retVal = new HashSet<>();

    Set<ValueSet> grouperValueSet = getTriggerPathToGrouperMap().get(path);

    if (grouperValueSet != null) {
      logger.debug("Found the grouper value set for {} with size {}", path, grouperValueSet.size());
      for (ValueSet g : grouperValueSet) {
        grouperId = g.getId();
        if (grouperId != null && getGrouperToEmergentValueSetMap() != null) {
          Set<ValueSet> valueSet = getGrouperToEmergentValueSetMap().get(grouperId);
          if (valueSet != null && !valueSet.isEmpty()) {
            logger.debug("Found {} Value Sets for grouper {}", valueSet.size(), grouperId);
            retVal.addAll(ApplicationUtils.convertValueSetsToString(valueSet));
          } else {
            logger.debug("Didn't find value sets for grouper {}", grouperId);
          }

        } else {
          logger.debug("Didn't find the value sets for the grouper for path {}", path);
        }
      }
    } else {

      logger.debug("Grouper not found for path {}", path);
    }
    logger.info("Found {} Value Sets for path {}", retVal.size(), path);
    return retVal;
  }

  private ValueSetSingleton() {}
}
