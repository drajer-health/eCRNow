package com.drajer.eca.model;

import java.util.HashSet;
import java.util.Set;
import org.apache.commons.collections4.SetUtils;

public class MatchedTriggerCodes {

  private Set<String> matchedCodes;
  private Set<String> matchedValues;
  private String valueSet;
  private String valueSetVersion;
  private String matchedPath;

  public MatchedTriggerCodes() {
    matchedCodes = new HashSet<>();
  }

  public Set<String> getMatchedCodes() {
    return matchedCodes;
  }

  public void setMatchedCodes(Set<String> matchedCodes) {
    this.matchedCodes = matchedCodes;
  }

  public String getValueSet() {
    return valueSet;
  }

  public void setValueSet(String valueSet) {
    this.valueSet = valueSet;
  }

  public String getValueSetVersion() {
    return valueSetVersion;
  }

  public void setValueSetVersion(String valueSetVersion) {
    this.valueSetVersion = valueSetVersion;
  }

  public String getMatchedPath() {
    return matchedPath;
  }

  public void setMatchedPath(String matchedPath) {
    this.matchedPath = matchedPath;
  }

  public Set<String> getMatchedValues() {
    return matchedValues;
  }

  public void setMatchedValues(Set<String> matchedValues) {
    this.matchedValues = matchedValues;
  }

  public Boolean hasMatchedTriggerCodes(String type) {

    return matchedCodes != null
        && !matchedCodes.isEmpty()
        && matchedPath != null
        && matchedPath.contains(type);
  }

  public Boolean hasMatchedTriggerValue(String type) {

    return matchedValues != null
        && !matchedValues.isEmpty()
        && matchedPath != null
        && matchedPath.contains(type);
  }

  public void addCodes(Set<String> codes) {

    if (matchedCodes == null) matchedCodes = new HashSet<>();

    if (codes != null) {
      Set<String> union = SetUtils.union(matchedCodes, codes);

      matchedCodes = union;
    }
  }

  public void addValues(Set<String> values) {

    if (matchedValues == null) matchedValues = new HashSet<>();

    if (values != null) {
      Set<String> union = SetUtils.union(matchedValues, values);

      matchedValues = union;
    }
  }
}
