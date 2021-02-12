package com.drajer.test.util;

import com.drajer.test.model.TestCase;
import com.drajer.test.model.TestDataVO;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class TestDataGenerator {
  String testYamlFileName;

  ObjectMapper mapper = new ObjectMapper();
  private TestDataVO testData = null;

  public TestDataGenerator(String testYamlFileName) {
    super();
    this.testYamlFileName = testYamlFileName;
  }

  ClassLoader classLoader = this.getClass().getClassLoader();

  static ObjectMapper ymlMapper = new ObjectMapper(new YAMLFactory());

  private TestDataVO getTestDataVO() {

    if (testData == null) {
      try {
        InputStream is = classLoader.getResourceAsStream(this.testYamlFileName);
        testData = ymlMapper.readValue(is, TestDataVO.class);

      } catch (IOException e) {
        System.out.println("Error in reading file:" + testYamlFileName);
      }
    }
    return testData;
  }

  public String getTestData(String testCaseId, String testDataElement) {

    return (String)
        getTestDataVO().getTestCase().get(testCaseId).getTestData().get(testDataElement);
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

  public Set<String> getAllResourceNames(String testCaseId) {
    return getResourceMappings(testCaseId).keySet();
  }

  public Map<String, List<String>> getResourceFiles(String testCaseId) {
    Map<String, List<String>> resourceMap = new LinkedHashMap<>();
    Set<String> resources = getAllResourceNames(testCaseId);
    for (String resource : resources) {
      List<LinkedHashMap> stubVOs =
          (List<LinkedHashMap>) getResourceMappings(testCaseId).get(resource);
      List<String> resourceList = new ArrayList<>();
      for (LinkedHashMap map : stubVOs) {
        String filePath = (String) map.get("responseFilePath");
        resourceList.add(filePath);
      }

      resourceMap.put(resource, resourceList);
    }
    return resourceMap;
  }

  public List<Map<String, String>> getValidate(String testCaseId) {
    return getTestDataVO().getTestCase().get(testCaseId).getvalidate();
  }

  public TestCase getTestCaseByID(String testCaseId) {
    return getTestDataVO().getTestCase().get(testCaseId);
  }
}
