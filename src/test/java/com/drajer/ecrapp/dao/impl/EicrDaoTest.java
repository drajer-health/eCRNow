package com.drajer.ecrapp.dao.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.ecrapp.model.ReportabilityResponse;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
  private static Eicr expectedEicr;
  private static ReportabilityResponse expectedRR;
  private static final Logger logger = LoggerFactory.getLogger(EicrDaoTest.class);

  @BeforeClass
  public static void setUp() {
    try {
      expectedEicr =
          mapper.readValue(TestUtils.getFileContentAsString("R4/Misc/eicr.json"), Eicr.class);
      expectedRR =
          mapper.readValue(
              TestUtils.getFileContentAsString("R4/Misc/reportabilityResponse.json"),
              ReportabilityResponse.class);
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
  public void saveOrUpdateReportabilityResponse() {
    ReportabilityResponse actualRR = eicrDaoImpl.saveOrUpdate(expectedRR);

    assertNotNull(actualRR);
    assertRR(expectedRR, actualRR);
  }

  @Test
  public void getReportabilityResponseById() {
    ReportabilityResponse savedRR = eicrDaoImpl.saveOrUpdate(expectedRR);
    ReportabilityResponse actualRR = eicrDaoImpl.getRRById(savedRR.getId());

    assertNotNull(actualRR);
    assertRR(expectedRR, actualRR);
  }

  public void assertEicr(Eicr expectedEicr, Eicr actualEicr) {
    assertEquals(expectedEicr.getxRequestId(), actualEicr.getxRequestId());
    assertEquals(expectedEicr.getEicrData(), actualEicr.getEicrData());
    assertEquals(expectedEicr.getEncounterId(), actualEicr.getEncounterId());
    assertEquals(expectedEicr.getFhirServerUrl(), actualEicr.getFhirServerUrl());
    assertEquals(expectedEicr.getLaunchPatientId(), actualEicr.getLaunchPatientId());
    assertEquals(expectedEicr.getResponseData(), actualEicr.getResponseData());
    assertEquals(expectedEicr.getResponseType(), actualEicr.getResponseType());
    assertEquals(expectedEicr.getSetId(), actualEicr.getSetId());
    assertEquals(expectedEicr.getResponseId(), actualEicr.getResponseId());
  }

  public void assertRR(ReportabilityResponse expectedRR, ReportabilityResponse actualRR) {
    assertEquals(expectedRR.getxRequestId(), actualRR.getxRequestId());
    assertEquals(expectedRR.getRrData(), actualRR.getRrData());
    assertEquals(expectedRR.getRrType(), actualRR.getRrType());
  }
}
