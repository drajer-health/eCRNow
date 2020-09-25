package com.drajer.ecr.it.common;

import java.util.Map;

public class TestCase {
  private Map<String, String> fileData;
  private Map<String, ?> resourceMappingData;
  private Map<String, String> otherMappingData;

  public TestCase(
      Map<String, String> fileData,
      Map<String, ?> resourceMappingData,
      Map<String, String> otherMappingData) {
    super();
    this.fileData = fileData;
    this.resourceMappingData = resourceMappingData;
    this.otherMappingData = otherMappingData;
  }

  public TestCase() {
    super();
  }

  public Map<String, String> getFileData() {
    return fileData;
  }

  public void setFileData(Map<String, String> fileData) {
    this.fileData = fileData;
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
}
