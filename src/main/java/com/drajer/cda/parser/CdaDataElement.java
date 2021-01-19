package com.drajer.cda.parser;

public class CdaDataElement {

  private String value;
  private Integer lineNumber;
  private String xpath;
  private String use;

  public void removeSpecialCharacters(String regex, String repl) {
    value = value.replaceAll(regex, repl);
  }

  public String getUse() {
    return use;
  }

  public void setUse(String use) {
    this.use = use;
  }

  public CdaDataElement(String value) {
    this.value = value;
  }

  public CdaDataElement() {}

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public Integer getLineNumber() {
    return lineNumber;
  }

  public void setLineNumber(Integer lineNumber) {
    this.lineNumber = lineNumber;
  }

  public String getXpath() {
    return xpath;
  }

  public void setXpath(String xpath) {
    this.xpath = xpath;
  }
}
