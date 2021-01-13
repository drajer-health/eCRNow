package com.drajer.test.model;

import java.util.Map;

public class TestDataVO {
  private String testName;

  private Map<String, TestCase> testCase;

  public TestDataVO() {
    super();
  }

  public TestDataVO(String testName, Map<String, TestCase> testCase) {
    super();
    this.testName = testName;
    this.testCase = testCase;
  }

  public String getTestName() {
    return testName;
  }

  public void setTestName(String testName) {
    this.testName = testName;
  }

  public Map<String, TestCase> getTestCase() {
    return testCase;
  }

  public void setTestCase(Map<String, TestCase> testCase) {
    this.testCase = testCase;
  }
}
