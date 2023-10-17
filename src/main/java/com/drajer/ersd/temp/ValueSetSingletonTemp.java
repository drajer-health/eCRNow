package com.drajer.ersd.temp;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.hl7.fhir.r4.model.ValueSet;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionComponent;
import org.hl7.fhir.r4.model.ValueSet.ValueSetExpansionContainsComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ValueSetSingletonTemp {

  private static ValueSetSingletonTemp instance;

  private final Logger logger = LoggerFactory.getLogger(ValueSetSingletonTemp.class);

  private Set<ValueSet> emergentValueSets;
  private Set<ValueSet> valueSets;
  private Set<ValueSet> grouperValueSets;

  private Map<String, Set<ValueSet>> triggerPathToValueSetsMap;
  private Map<String, Set<ValueSet>> triggerPathToGrouperMap;
  private Map<String, Set<ValueSet>> grouperToValueSetMap;
  private Map<String, Set<ValueSet>> grouperToEmergentValueSetMap;

  private ValueSetSingletonTemp() {}

  public static ValueSetSingletonTemp getInstance() {
    if (instance == null) {
      instance = new ValueSetSingletonTemp();
    }
    return instance;
  }

  public void addGrouperToValueSetMap(String grouper, Set<ValueSet> vs) {

    if (grouperToValueSetMap != null) {

      if (grouperToValueSetMap.containsKey(grouper)) {

        logger.debug("Value sets for Grouper {} Size = {} is already added", grouper, vs.size());
      } else {

        logger.debug(
            " Creating new entry and adding value sets for Grouper {} Size = {}",
            grouper,
            vs.size());

        grouperToValueSetMap.put(grouper, vs);
      }

    } else {

      logger.debug(
          " Creating new entry and adding value sets for Grouper {} Size = {}", grouper, vs.size());

      grouperToValueSetMap = new HashMap<>();

      grouperToValueSetMap.put(grouper, vs);
    }
  }

  public void addGrouperToEmergentValueSetMap(String grouper, Set<ValueSet> vs) {

    if (grouperToEmergentValueSetMap != null) {

      if (grouperToEmergentValueSetMap.containsKey(grouper)) {
        logger.debug(
            "Emergent Value sets for Grouper that exists {} Size = {} is already added",
            grouper,
            vs.size());

      } else {

        logger.debug(
            " Creating new entry and adding Emergent value sets for Grouper that does not exist {} Size = {}",
            grouper,
            vs.size());

        grouperToEmergentValueSetMap.put(grouper, vs);
      }

    } else {

      logger.debug(
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
          Set<ValueSet> valueSetsTemp = getGrouperToValueSetMap().get(grouperId);
          if (valueSetsTemp != null && !valueSetsTemp.isEmpty()) {
            logger.debug("Found {} Value Sets for grouper {}", valueSetsTemp.size(), grouperId);
            retVal.addAll(convertValueSetsToString(valueSetsTemp));
          } else {
            logger.debug("Didn't find value sets for grouper {}", grouperId);
          }
        } else {
          logger.debug("Didn't find the value sets for the grouper for path {}", path);
        }
      }
    }

    logger.info("Found {} Value Sets for path {}", retVal.size(), path);
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
            retVal.addAll(convertValueSetsToString(valueSet));
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

  private Set<String> convertValueSetsToString(Set<ValueSet> valuesets) {

    Set<String> retVal = new HashSet<>();
    ValueSetExpansionComponent valueSetExpansionComponent;
    List<ValueSetExpansionContainsComponent> valueSetExpansionContainsComponentList;

    if (valuesets != null && !valuesets.isEmpty()) {
      for (ValueSet vs : valuesets) {
        logger.debug("Value Set Id = {}", vs.getId());
        valueSetExpansionComponent = vs.getExpansion();
        valueSetExpansionContainsComponentList = valueSetExpansionComponent.getContains();
        for (ValueSetExpansionContainsComponent vscomp : valueSetExpansionContainsComponentList) {
          if (vscomp.getSystem() != null && vscomp.getCode() != null) {
            retVal.add(vscomp.getSystem() + "|" + vscomp.getCode());
          }
        }
      }
    }

    return retVal;
  }

  public Map<String, Set<ValueSet>> getGrouperToValueSetMap() {
    return grouperToValueSetMap;
  }

  public Map<String, Set<ValueSet>> getGrouperToEmergentValueSetMap() {
    return grouperToEmergentValueSetMap;
  }

  public Set<ValueSet> getEmergentValueSets() {
    if (emergentValueSets == null) {
      emergentValueSets = new HashSet<>();
    }
    return emergentValueSets;
  }

  public void setEmergentValueSets(Set<ValueSet> emergentValueSets) {
    this.emergentValueSets = emergentValueSets;
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

  public Map<String, Set<ValueSet>> getTriggerPathToGrouperMap() {
    if (triggerPathToGrouperMap == null) {
      triggerPathToGrouperMap = new HashMap<>();
    }
    return triggerPathToGrouperMap;
  }
}
