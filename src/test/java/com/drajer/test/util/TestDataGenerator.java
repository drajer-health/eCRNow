package com.drajer.test.util;

import com.drajer.ecr.it.common.TestDataVO;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import java.io.IOException;

public class TestDataGenerator {
  String testYamlFileName;

  ObjectMapper mapper = new ObjectMapper();

  public TestDataGenerator(String testYamlFileName) {
    super();
    this.testYamlFileName = testYamlFileName;
  }

  ClassLoader classLoader = getClass().getClassLoader();

  static ObjectMapper ymlMapper = new ObjectMapper(new YAMLFactory());

  public String getTestData(String resource)
      throws JsonParseException, JsonMappingException, IOException {

    TestDataVO testData =
        ymlMapper.readValue(
            classLoader.getResourceAsStream(this.testYamlFileName), TestDataVO.class);
    return (String) testData.getDataMap().get(resource);
  }
}
