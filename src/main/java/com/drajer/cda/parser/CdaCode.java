package com.drajer.cda.parser;

import java.util.ArrayList;

public class CdaCode extends CdaDataElement {

  private String code;
  private String codeSystem;
  private String codeSystemName;
  private String displayName;
  private String valueSetOid;
  private ArrayList<CdaCode> translations;
  private String nullFlavor;

  public CdaCode() {
    translations = new ArrayList<CdaCode>();
  }

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getCodeSystem() {
    return codeSystem;
  }

  public void setCodeSystem(String codeSystem) {
    this.codeSystem = codeSystem;
  }

  public String getCodeSystemName() {
    return codeSystemName;
  }

  public void setCodeSystemName(String codeSystemName) {
    this.codeSystemName = codeSystemName;
  }

  public String getDisplayName() {
    return displayName;
  }

  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  public String getValueSetOid() {
    return valueSetOid;
  }

  public void setValueSetOid(String valueSetOid) {
    this.valueSetOid = valueSetOid;
  }

  public ArrayList<CdaCode> getTranslations() {
    return translations;
  }

  public void setTranslations(ArrayList<CdaCode> translations) {
    this.translations = translations;
  }

  public String getNullFlavor() {
    return nullFlavor;
  }

  public void setNullFlavor(String nullFlavor) {
    this.nullFlavor = nullFlavor;
  }
}
