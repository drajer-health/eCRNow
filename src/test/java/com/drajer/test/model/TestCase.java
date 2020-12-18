package com.drajer.test.model;

import java.util.List;
import java.util.Map;

public class TestCase {
  private Map<String, String> testData;
  private Map<String, ?> resourceMappingData;
  private Map<String, String> otherMappingData;
  private List<Map<String, String>> validate;

  public TestCase(
      Map<String, String> fileData,
      Map<String, ?> resourceMappingData,
      Map<String, String> otherMappingData,
      List<Map<String, String>> validate) {
    super();
    this.testData = fileData;
    this.resourceMappingData = resourceMappingData;
    this.otherMappingData = otherMappingData;
    this.validate = validate;
  }

  public TestCase() {
    super();
  }

  public Map<String, String> getTestData() {
    return testData;
  }

  public void setTestData(Map<String, String> testData) {
    this.testData = testData;
  }

  public Map<String, ?> getResourceMappingData() {
    return resourceMappingData;
  }

  public void setResourceMappingData(Map<String, ?> resourceMappingData) {
    this.resourceMappingData = resourceMappingData;
  }

  public Map<String, String> getOtherMappingData() {
    return otherMappingData;
  }

  public void setOtherMappingData(Map<String, String> otherMappingData) {
    this.otherMappingData = otherMappingData;
  }

  public List<Map<String, String>> getvalidate() {
    return validate;
  }

  public void setvalidate(List<Map<String, String>> validate) {
    this.validate = validate;
  }
}
