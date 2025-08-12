package com.drajer.ecrapp.dao.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.*;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.dao.EicrDao;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hibernate.Criteria;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.SimpleExpression;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
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

  @Autowired private EicrDao eicrDao;

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
  public void deleteEicr() {

    Eicr eicr = eicrDaoImpl.saveOrUpdate(expectedEicr);
    assertNotNull(eicr);
    String eicrDocId = eicr.getEicrDocId();

    Eicr eicrSaved = eicrDaoImpl.getEicrByDocId(eicrDocId);
    assertNotNull(eicrSaved);

    eicrDaoImpl.deleteEicr(eicrSaved);
    eicrSaved = eicrDaoImpl.getEicrByDocId(eicrDocId);
    assertNull(eicrSaved);
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
  public void getEicrByDocId() {

    Eicr savedEicr = eicrDaoImpl.saveOrUpdate(expectedEicr);
    Eicr actualEicr = eicrDaoImpl.getEicrByDocId(savedEicr.getEicrDocId());

    assertNotNull(actualEicr);
    assertEicr(expectedEicr, actualEicr);
  }

  @Test
  public void getEicrAndRRDataByXRequestId() {

    Eicr savedEicr = eicrDaoImpl.saveOrUpdate(expectedEicr);
    List<Eicr> actualEicr = eicrDaoImpl.getEicrAndRRByXRequestId(savedEicr.getxRequestId());

    assertEquals(1, actualEicr.size());
    assertEquals(expectedEicr.getEicrData(), actualEicr.get(0).getEicrData());
    assertEquals(expectedEicr.getResponseData(), actualEicr.get(0).getResponseData());
  }

  @Test
  public void getMaxVersionId() throws JsonProcessingException {

    eicrDaoImpl.saveOrUpdate(expectedEicr);

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

    Eicr actualEicr = eicrDaoImpl.getEicrByCorrelationId(expectedEicr.getxCorrelationId());

    assertNotNull(actualEicr);
    assertEicr(expectedEicr, actualEicr);
  }

  @Test
  public void getEicrData_WithEicrId() {

    Eicr savedEicr = eicrDaoImpl.saveOrUpdate(expectedEicr);
    assertNotNull(savedEicr);
    Integer eicrId = savedEicr.getId();

    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("eicrId", eicrId.toString());

    List<Eicr> results = eicrDaoImpl.getEicrData(searchParams);

    assertNotNull(results);
    assertEquals(1, results.size());
    assertEquals(eicrId, results.get(0).getId());
    assertEicr(expectedEicr, results.get(0));
  }

  @Test
  public void getRRDate_withResponseDocId() {
    expectedEicr.setResponseDocId("test-response-doc-id-123");
    Eicr savedEicr = eicrDaoImpl.saveOrUpdate(expectedEicr);
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("responseDocId", "test-response-doc-id-123");
    List<Eicr> results = eicrDaoImpl.getRRData(searchParams);
    assertNotNull(results);
    assertEquals(1, results.size());
    Eicr actualEicr = results.get(0);
    assertEquals("test-response-doc-id-123", actualEicr.getResponseDocId());
    assertEquals(expectedEicr, actualEicr);
  }

  @Test
  public void getRRData_WithEicrId() {
    expectedEicr.setResponseData("Sample Response Data");
    Eicr savedEicr = eicrDaoImpl.saveOrUpdate(expectedEicr);
    assertNotNull(savedEicr);
    Integer eicrId = savedEicr.getId();

    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("eicrId", eicrId.toString());

    List<Eicr> results = eicrDaoImpl.getRRData(searchParams);
    assertEquals(1, results.size());
    assertEquals(eicrId, results.get(0).getId());
    assertNotNull(results.get(0).getResponseData());
    assertEicr(expectedEicr, results.get(0));
  }

  @Test
  public void testSaveOrUpdateEicr() {
    Eicr eicr = new Eicr();
    eicr.setxRequestId("req-001");
    eicr.setxCorrelationId("corr-001");
    eicr.setEicrDocId("doc-001");
    eicr.setSetId("set-001");
    eicr.setDocVersion(1);
    eicr.setEicrData("Sample EICR XML");
    eicr.setResponseType(Eicr.RR_RESPONSE_TYPE);
    eicr.setResponseXRequestId("res-req-001");
    eicr.setResponseDocId("res-doc-001");
    eicr.setResponseData("Sample RR Data");
    eicr.setFhirServerUrl("http://example.com/fhir");
    eicr.setLaunchPatientId("pat-123");
    eicr.setLaunchDetailsId(101);
    eicr.setEncounterId("enc-456");
    eicr.setProviderUUID("prov-789");
    eicr.setEhrDocRefId("ehr-456");
    eicr.setEicrProcStatus("COMPLETE");
    eicr.setRrProcStatus("PENDING");

    Eicr savedEicr = eicrDaoImpl.saveOrUpdate(eicr);

    assertNotNull(savedEicr);
    assertNotNull(savedEicr.getId());
    assertEquals("Sample RR Data", savedEicr.getResponseData());
    assertEquals("COMPLETE", savedEicr.getEicrProcStatus());
    assertEquals("PENDING", savedEicr.getRrProcStatus());
    assertEquals("req-001", savedEicr.getxRequestId());
    assertEquals("corr-001", savedEicr.getxCorrelationId());
    assertEquals("doc-001", savedEicr.getEicrDocId());
    assertEquals("set-001", savedEicr.getSetId());
    assertEquals(Integer.valueOf(1), savedEicr.getDocVersion());
    assertEquals("Sample EICR XML", savedEicr.getEicrData());
  }

  @Test
  public void testPrepareCriteriaWithAllParams() throws Exception {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("eicrDocId", "doc-001");
    searchParams.put("fhirServerUrl", "http://example.com/fhir");
    searchParams.put("setId", "set-123");
    searchParams.put("patientId", "pat-456");
    searchParams.put("encounterId", "enc-789");
    searchParams.put("version", "2");
    searchParams.put("xRequestId", "xreq-999");
    searchParams.put("startDate", "2023-01-01");
    searchParams.put("endDate", "2023-12-31");

    Criteria mockCriteria = mock(Criteria.class);
    when(mockCriteria.add(any(Criterion.class))).thenReturn(mockCriteria);

    EicrDaoImpl.prepareCriteria(mockCriteria, searchParams);

    verify(mockCriteria).add(eqRestriction("eicrDocId", "doc-001"));
    verify(mockCriteria).add(eqRestriction("fhirServerUrl", "http://example.com/fhir"));
    verify(mockCriteria).add(eqRestriction("setId", "set-123"));
    verify(mockCriteria).add(eqRestriction("launchPatientId", "pat-456"));
    verify(mockCriteria).add(eqRestriction("encounterId", "enc-789"));
    verify(mockCriteria).add(eqRestriction("docVersion", 2));
    verify(mockCriteria).add(eqRestriction("xRequestId", "xreq-999"));

    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    Date expectedStartDate = sdf.parse("2023-01-01");
    Date expectedEndDate = sdf.parse("2023-12-31");

    verify(mockCriteria).add(geRestriction("lastUpdated", expectedStartDate));
    verify(mockCriteria).add(leRestriction("lastUpdated", expectedEndDate));

    verifyNoMoreInteractions(mockCriteria);
  }

  @Test
  public void testPrepareCriteriaWithAllParams1() throws Exception {
    Map<String, String> searchParams = new HashMap<>();
    searchParams.put("eicrDocId", "doc-001");
    searchParams.put("fhirServerUrl", "http://example.com/fhir");
    searchParams.put("setId", "set-123");
    searchParams.put("patientId", "pat-456");
    searchParams.put("encounterId", "enc-789");
    searchParams.put("version", "2");
    searchParams.put("xRequestId", "xreq-999");
    searchParams.put("startDate", "2023-01-01");
    searchParams.put("endDate", "2023-12-31");

    Criteria mockCriteria = mock(Criteria.class);
    when(mockCriteria.add(any(Criterion.class))).thenReturn(mockCriteria);

    EicrDaoImpl.prepareCriteria(mockCriteria, searchParams);

    ArgumentCaptor<Criterion> criterionCaptor = ArgumentCaptor.forClass(Criterion.class);
    verify(mockCriteria, times(9)).add(criterionCaptor.capture());

    List<Criterion> capturedCriteria = criterionCaptor.getAllValues();

    assertSimpleExpression(capturedCriteria.get(0), "eicrDocId", "=", "doc-001");
    assertSimpleExpression(
        capturedCriteria.get(1), "fhirServerUrl", "=", "http://example.com/fhir");
    assertSimpleExpression(capturedCriteria.get(2), "setId", "=", "set-123");
    assertSimpleExpression(capturedCriteria.get(3), "launchPatientId", "=", "pat-456");
    assertSimpleExpression(capturedCriteria.get(4), "encounterId", "=", "enc-789");
    assertSimpleExpression(capturedCriteria.get(5), "docVersion", "=", 2);
    assertSimpleExpression(capturedCriteria.get(6), "xRequestId", "=", "xreq-999");

    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    assertSimpleExpression(capturedCriteria.get(7), "lastUpdated", ">=", sdf.parse("2023-01-01"));
    assertSimpleExpression(capturedCriteria.get(8), "lastUpdated", "<=", sdf.parse("2023-12-31"));
  }

  private void assertSimpleExpression(
      Criterion criterion, String expectedProperty, String expectedOp, Object expectedValue) {
    assertTrue(criterion instanceof SimpleExpression);
    SimpleExpression expr = (SimpleExpression) criterion;
    assertEquals(expectedProperty, expr.getPropertyName());
    assertEquals(expectedOp, expr.getOp());

    if (expectedValue instanceof Date) {
      assertEquals(((Date) expectedValue).getTime(), ((Date) expr.getValue()).getTime());
    } else {
      assertEquals(expectedValue, expr.getValue());
    }
  }

  private static Criterion eqRestriction(String property, Object value) {
    return argThat(
        criterion -> {
          if (!(criterion instanceof SimpleExpression)) return false;
          SimpleExpression expr = (SimpleExpression) criterion;
          return expr.getPropertyName().equals(property)
              && expr.getOp().equals("=")
              && expr.getValue().equals(value);
        });
  }

  private static Criterion geRestriction(String property, Object value) {
    return argThat(
        criterion -> {
          if (!(criterion instanceof SimpleExpression)) return false;
          SimpleExpression expr = (SimpleExpression) criterion;
          return expr.getPropertyName().equals(property)
              && expr.getOp().equals(">=")
              && expr.getValue().equals(value);
        });
  }

  private static Criterion leRestriction(String property, Object value) {
    return argThat(
        criterion -> {
          if (!(criterion instanceof SimpleExpression)) return false;
          SimpleExpression expr = (SimpleExpression) criterion;
          return expr.getPropertyName().equals(property)
              && expr.getOp().equals("<=")
              && expr.getValue().equals(value);
        });
  }

  public void assertEicr(Eicr expectedEicr, Eicr actualEicr) {
    assertEquals(expectedEicr.getxRequestId(), actualEicr.getxRequestId());
    assertEquals(expectedEicr.getxCorrelationId(), actualEicr.getxCorrelationId());
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
