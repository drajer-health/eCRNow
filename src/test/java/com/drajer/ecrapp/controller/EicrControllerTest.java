package com.drajer.ecrapp.controller;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doThrow;

import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.service.EicrRRService;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.json.JSONObject;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@RunWith(MockitoJUnitRunner.class)
public class EicrControllerTest {

  @InjectMocks private EicrController eicrController;

  @Mock private EicrRRService eicrRRService;

  @Mock private HttpServletRequest httpServletRequest;

  @Mock private HttpServletResponse httpServletResponse;

  @Test
  public void getEicrData() throws Exception {

    JSONObject eicrData = new JSONObject();
    eicrData.put("eicrId", "67735647");
    eicrData.put("eicrDocId", "877");
    eicrData.put("setId", "390");
    eicrData.put("patientId", "293204");
    eicrData.put("encounterId", "98323");
    eicrData.put("version", "1.0");
    eicrData.put("fhirServerUrl", "http://ecrunitest/ecr/dao");
    eicrData.put("xRequestId", "5678");

    List<JSONObject> eicrDataList = new ArrayList<>();
    eicrDataList.add(eicrData);

    ObjectMapper objectMapper = new ObjectMapper();
    Mockito.lenient()
        .doReturn(eicrDataList)
        .when(eicrRRService)
        .getEicrData(objectMapper.readValue(eicrData.toString(), Map.class));

    assertEquals(
        eicrDataList.toString(),
        eicrController
            .getEicrData(
                "67735647",
                "877",
                "390",
                "293204",
                "98323",
                "1.0",
                "http://ecrunitest/ecr/dao",
                "5678",
                null,
                null)
            .getBody());
  }

  @Test
  public void getEicrDataByException() {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class)
                  .when(
                      eicrController
                          .getEicrData(
                              "67735647",
                              "877",
                              "390",
                              "293204",
                              "98323",
                              "1.0",
                              "http://ecrunitest/ecr/dao",
                              "5678",
                              null,
                              null)
                          .getBody());
            });
  }

  @Test
  public void getRRData() throws Exception {

    JSONObject eicrData = new JSONObject();
    eicrData.put("responseDocId", "57735647");
    eicrData.put("eicrDocId", "877");
    eicrData.put("setId", "390");
    eicrData.put("fhirServerUrl", "http://ecrunitest/ecr/dao");
    eicrData.put("patientId", "293204");
    eicrData.put("encounterId", "98323");
    eicrData.put("version", "1.0");

    List<JSONObject> eicrDataList = new ArrayList<>();
    eicrDataList.add(eicrData);

    ObjectMapper objectMapper = new ObjectMapper();
    Mockito.lenient()
        .doReturn(eicrDataList)
        .when(eicrRRService)
        .getRRData(objectMapper.readValue(eicrData.toString(), Map.class));

    assertEquals(
        eicrDataList.toString(),
        eicrController
            .getRRData(
                "57735647", "877", "390", "http://ecrunitest/ecr/dao", "293204", "98323", "1.0")
            .getBody());
  }

  @Test
  public void getRRDataByException() {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class)
                  .when(eicrController.getRRData("", "", "", "", "", "", "").getStatusCode());
            });
  }

  @Test
  public void getEicrAndRRByRequestId() {

    JSONObject eicrData = new JSONObject();
    eicrData.put("xRequestId", "ecrunittest_id");

    List<JSONObject> eicrDataList = new ArrayList<>();
    eicrDataList.add(eicrData);

    Mockito.lenient()
        .doReturn(eicrDataList)
        .when(eicrRRService)
        .getEicrAndRRByXRequestId("ecrunittest_id");

    assertEquals(
        eicrDataList.toString(),
        eicrController
            .getEicrAndRRByRequestId("ecrunittest_id", "ecrunittest_id", "ecrUnitTestCorrelationID")
            .getBody());
  }

  @Test
  public void getEicrAndRRByRequestIdByException() {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class)
                  .when(eicrController.getEicrAndRRByRequestId("", "", "").getStatusCode());
            });
  }

  @Test
  public void getEicrByEicrDocID() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    Eicr eicr =
        objectMapper.readValue(new File("src/test/resources/R4/Misc/eicr.json"), Eicr.class);
    Mockito.lenient().doReturn(eicr).when(eicrRRService).getEicrByDocId("123");

    assertEquals(
        eicr,
        eicrController
            .getEicrByEicrDocID("123", "ecrunittest_id", "ecrUnitTestCorrelationID")
            .getBody());
  }

  @Test
  public void getEicrByEicrDocIDByException() {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class)
                  .when(eicrController.getEicrByEicrDocID("123", "", "").getStatusCode());
            });
  }

  @Test
  public void deleteEicrByEicrDocID() throws Exception {

    ObjectMapper objectMapper = new ObjectMapper();
    Eicr eicr =
        objectMapper.readValue(new File("src/test/resources/R4/Misc/eicr.json"), Eicr.class);
    Mockito.lenient().doReturn(eicr).when(eicrRRService).getEicrByDocId("123");

    assertEquals(
        "Eicr deleted successfully",
        eicrController
            .deleteEicrByEicrDocID(
                "123",
                "ecrunittest_id",
                "ecrUnitTestCorrelationID",
                httpServletRequest,
                httpServletResponse)
            .getBody());
  }

  @Test
  public void deleteEicrByEicrDocIDByException() {
    assertThatExceptionOfType(RuntimeException.class)
        .isThrownBy(
            () -> {
              doThrow(RuntimeException.class)
                  .when(
                      eicrController
                          .deleteEicrByEicrDocID(
                              "123", "", "", httpServletRequest, httpServletResponse)
                          .getStatusCode());
            });
  }

  @Test
  public void test2() throws Exception {
    assertEquals("Hello", eicrController.test2(5));
  }
}
