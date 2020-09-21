package com.drajer.test.util;

import com.drajer.ecr.it.common.StubVO;
import com.drajer.ecr.it.common.TestCase;
import com.drajer.ecr.it.common.TestDataVO;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import org.apache.commons.collections4.MapUtils;

public class CreateTestYaml {

  ClassLoader classLoader = getClass().getClassLoader();

  static ObjectMapper ymlMapper = new ObjectMapper(new YAMLFactory());
  static ObjectMapper jsonMapper = new ObjectMapper();

  static Map<String, String> filePath = new LinkedHashMap<>();
  static Map<String, ? super Object> resourceStubMap = new LinkedHashMap<>();
  static Map<String, String> otherStubMap = new LinkedHashMap<>();

  public static void main(String[] args) throws IOException {
    createData();
  }

  public static Map<String, Object> createData() throws JsonProcessingException {
    String outputFile = "LaunchTestData.yaml"; // Later Use OutputStream to write to this file

    filePath.put("ClientDataToBeSaved", "R4/Misc/ClientDetails/ClientDataEntry1.json");
    filePath.put("SystemLaunchPayload", "R4/Misc/SystemLaunchPayload/systemLaunchRequest.json");
    filePath.put("LaunchDataToBeSaved", "R4/Misc/LaunchDetails/LaunchDataEntry1.json");
    filePath.put("ExpectedEICRFile", "R4/Misc/ExpectedEICR/EICR_Expected.xml");

    StubVO vo1 = new StubVO("12742571", "R4/Patient/Patient_12742571.json");
    StubVO vo2_1 = new StubVO("97953900", "R4/Encounter/Encounter_97953900.json");
    StubVO vo2_2 =
        new StubVO(
            MapUtils.putAll(
                new LinkedHashMap<String, String>(), new Object[] {"patient", "12742571"}),
            "R4/Encounter/EncounterBundle_97953900.json");

    StubVO vo3_1 = new StubVO("11817978", "R4/Practitioner/Practitioner_11817978.json");
    StubVO vo3_2 = new StubVO("4122622", "R4/Practitioner/Practitioner_4122622.json");
    StubVO vo3_3 = new StubVO("11938004", "R4/Practitioner/Practitioner_11938004.json");

    StubVO vo4 =
        new StubVO(
            MapUtils.putAll(
                new LinkedHashMap<String, String>(), new Object[] {"patient", "12742571"}),
            "R4/Condition/ConditionBundle_d2572364249.json");

    StubVO vo5_1 =
        new StubVO(
            MapUtils.putAll(
                new LinkedHashMap<String, String>(),
                new Object[] {"patient", "12742571", "category", "laboratory"}),
            "R4/Observation/ObservationBundle_1.json");
    StubVO vo5_2 =
        new StubVO(
            MapUtils.putAll(
                new LinkedHashMap<String, String>(),
                new Object[] {"patient", "12742571", "code", "http://loinc.org|90767-5"}),
            "R4/Observation/ObservationBundle_2.json");
    StubVO vo5_3 =
        new StubVO(
            MapUtils.putAll(
                new LinkedHashMap<String, String>(),
                new Object[] {"patient", "12742571", "code", "http://loinc.org|929762-2"}),
            "R4/Observation/ObservationBundle_3.json");

    resourceStubMap.put("Patient", Arrays.asList(vo1));
    resourceStubMap.put("Encounter", Arrays.asList(vo2_1, vo2_2));
    resourceStubMap.put("Practitioner", Arrays.asList(vo3_1, vo3_2, vo3_3));
    resourceStubMap.put("Condition", Arrays.asList(vo4));
    resourceStubMap.put("Observation", Arrays.asList(vo5_1, vo5_2, vo5_3));

    otherStubMap.put("metadata", "R4/Misc/MetaData_r4.json");
    otherStubMap.put("token", "R4/Misc/AccessToken.json");
    otherStubMap.put("default", "R4/Misc/NoDataFound_Default.json");

    // Till here

    // Do not touch code below this
    TestDataVO testData = new TestDataVO();
    testData.setTestName("TestSystemLaunch");
    testData.setTestCase(
        MapUtils.putAll(
            new LinkedHashMap<String, TestCase>(),
            new Object[] {
              "TestCase001",
              new TestCase(filePath, resourceStubMap, otherStubMap),
              "TestCase002",
              new TestCase(filePath, resourceStubMap, otherStubMap)
            }));
    String ymlContent = ymlMapper.writeValueAsString(testData);

    System.out.println(ymlContent);
    return resourceStubMap;
  }
}
