package com.drajer.ecrapp.dao.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = SpringConfiguration.class)
@AutoConfigureTestDatabase
@SpringBootTest
@ActiveProfiles("test")
@Transactional
public class EicrDaoTest {

  @Autowired private EicrDaoImpl eicrDaoImpl;

  private static final ObjectMapper mapper = new ObjectMapper();
  private static String inputEicr;
  private static final Logger logger = LoggerFactory.getLogger(EicrDaoTest.class);

  private Eicr expectedEicr;

  @BeforeClass
  public static void setUp() {
    inputEicr = TestUtils.getFileContentAsString("R4/Misc/eicr.json");
  }

  @Before
  public void initializeData() {
    try {
      expectedEicr = mapper.readValue(inputEicr, Eicr.class);
    } catch (JsonProcessingException e) {
      logger.error("Exception in parsing input data: ", e);
      fail("This exception is not expected, fix the test");
    }
  }

  @Test
  public void saveOrUpdateEicr() {

    Eicr actualEicr = eicrDaoImpl.saveOrUpdate(expectedEicr);

    assertNotNull(actualEicr);
    assertEicr(expectedEicr, actualEicr);
  }

  @Test
  public void getEicrById() {

    Eicr savedEicr = eicrDaoImpl.saveOrUpdate(expectedEicr);
    Eicr actualEicr = eicrDaoImpl.getEicrById(savedEicr.getId());

    assertNotNull(actualEicr);
    assertEicr(expectedEicr, actualEicr);
  }

  @Test
  public void getMaxVersionId() throws JsonProcessingException {

    // First Row with docverison 1.0
    eicrDaoImpl.saveOrUpdate(expectedEicr);

    // second Row with docverison 2.0
    JSONObject inputJson = new JSONObject(inputEicr);
    inputJson.put("docVersion", 2.0);
    Eicr secondEicr = mapper.readValue(inputJson.toString(), Eicr.class);
    eicrDaoImpl.saveOrUpdate(secondEicr);

    Integer actualDocVersion = eicrDaoImpl.getMaxVersionId(expectedEicr);
    assertEquals(secondEicr.getDocVersion(), actualDocVersion);
  }

  @Test
  public void getEicrByCoorrelationId() {
    eicrDaoImpl.saveOrUpdate(expectedEicr);
    Eicr actualEicr = eicrDaoImpl.getEicrByCoorrelationId(expectedEicr.getxCoorrelationId());

    assertNotNull(actualEicr);
    assertEicr(expectedEicr, actualEicr);
  }

  public void assertEicr(Eicr expectedEicr, Eicr actualEicr) {
    assertEquals(expectedEicr.getxRequestId(), actualEicr.getxRequestId());
    assertEquals(expectedEicr.getxCoorrelationId(), actualEicr.getxCoorrelationId());
    assertEquals(expectedEicr.getEicrDocId(), actualEicr.getEicrDocId());
    assertEquals(expectedEicr.getSetId(), actualEicr.getSetId());
    assertEquals(expectedEicr.getDocVersion(), actualEicr.getDocVersion());
    assertEquals(expectedEicr.getEicrData(), actualEicr.getEicrData());
    assertEquals(expectedEicr.getInitiatingAction(), actualEicr.getInitiatingAction());
    assertEquals(expectedEicr.getResponseType(), actualEicr.getResponseType());
    assertEquals(expectedEicr.getResponseXRequestId(), actualEicr.getResponseXRequestId());
    assertEquals(expectedEicr.getResponseDocId(), actualEicr.getResponseDocId());
    assertEquals(expectedEicr.getResponseData(), actualEicr.getResponseData());
    assertEquals(expectedEicr.getFhirServerUrl(), actualEicr.getFhirServerUrl());
    assertEquals(expectedEicr.getLaunchPatientId(), actualEicr.getLaunchPatientId());
    assertEquals(expectedEicr.getEncounterId(), actualEicr.getEncounterId());
  }
}
