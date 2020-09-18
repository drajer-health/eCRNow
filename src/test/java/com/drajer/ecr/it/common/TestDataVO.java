package com.drajer.ecr.it.common;

import java.util.Map;

public class TestDataVO {
  private String testName;
  private String testId;
  private Map<String, ?> dataMap;

  public String getTestName() {
    return testName;
  }

  public void setTestName(String testName) {
    this.testName = testName;
  }

  public String getTestId() {
    return testId;
  }

  public void setTestId(String testId) {
    this.testId = testId;
  }

  public Map<String, ?> getDataMap() {
    return dataMap;
  }

  public void setDataMap(Map<String, ?> dataMap) {
    this.dataMap = dataMap;
  }
}
