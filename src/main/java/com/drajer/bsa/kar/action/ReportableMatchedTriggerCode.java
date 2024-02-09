package com.drajer.bsa.kar.action;

import java.util.HashSet;
import java.util.Set;

public class ReportableMatchedTriggerCode {

  private String valueSetVersion;
  private String valueSet;
  private String valueSetOid;
  private String codeSystem;
  private String code;
  private Set<String> allMatches;

  public ReportableMatchedTriggerCode() {
    allMatches = new HashSet<>();
  }

  public Set<String> getAllMatches() {
    return allMatches;
  }

  public void setAllMatches(Set<String> allMatches) {
    this.allMatches = allMatches;
  }

  public String getValueSetVersion() {
    return valueSetVersion;
  }

  public void setValueSetVersion(String valueSetVersion) {
    this.valueSetVersion = valueSetVersion;
  }

  public String getValueSet() {
    return valueSet;
  }

  public void setValueSet(String valueSet) {
    this.valueSet = valueSet;
  }

  public String getCodeSystem() {
    return codeSystem;
  }

  public void setCodeSystem(String codeSystem) {
    this.codeSystem = codeSystem;
  }

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getValueSetOid() {
    return valueSetOid;
  }

  public void setValueSetOid(String valueSetOid) {
    this.valueSetOid = valueSetOid;
  }
}
