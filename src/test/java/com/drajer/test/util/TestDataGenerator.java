package com.drajer.test.util;

import com.drajer.ecr.it.common.TestDataVO;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Set;

public class TestDataGenerator {
  String testYamlFileName;

  ObjectMapper mapper = new ObjectMapper();

  public TestDataGenerator(String testYamlFileName) {
    super();
    this.testYamlFileName = testYamlFileName;
  }

  ClassLoader classLoader = this.getClass().getClassLoader();

  static ObjectMapper ymlMapper = new ObjectMapper(new YAMLFactory());

  private TestDataVO getTestDataVO() {
    TestDataVO testData = null;
    try {
      InputStream is = classLoader.getResourceAsStream(this.testYamlFileName);
      testData = ymlMapper.readValue(is, TestDataVO.class);

    } catch (IOException e) {
      System.out.println("Error in reading file:" + testYamlFileName);
    }
    return testData;
  }

  public String getTestFile(String testCaseId, String fileName) {

    return (String) getTestDataVO().getTestCase().get(testCaseId).getFileData().get(fileName);
  }

  public Map<String, ?> getResourceMappings(String testCaseId) {

    return getTestDataVO().getTestCase().get(testCaseId).getResourceMappingData();
  }

  public Map<String, ?> getOtherMappings(String testCaseId) {

    return getTestDataVO().getTestCase().get(testCaseId).getOtherMappingData();
  }

  public Set<String> getAllTestCases() {

    return getTestDataVO().getTestCase().keySet();
  }
}
