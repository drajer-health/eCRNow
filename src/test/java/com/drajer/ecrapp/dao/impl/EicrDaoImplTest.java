package com.drajer.ecrapp.dao.impl;

import static org.junit.Assert.*;

import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.model.Eicr;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = SpringConfiguration.class)
@SpringBootTest
@AutoConfigureTestDatabase
@ActiveProfiles("test")
@Transactional
public class EicrDaoImplTest {
  @Autowired private EicrDaoImpl eicrDaoImpl;

  private static final ObjectMapper mapper = new ObjectMapper();
  private static String inputEicr;
  private Eicr expectedEicr;

  @BeforeClass
  public static void loadTestData() {
    inputEicr = TestUtils.getFileContentAsString("R4/Misc/eicr.json");
    assertNotNull("Input JSON should not be null", inputEicr);
  }

  @Before
  public void setUp() throws Exception {
    expectedEicr = mapper.readValue(inputEicr, Eicr.class);
    assertNotNull("Parsed EICR object should not be null", expectedEicr);
  }

  @Test
  public void testSaveOrUpdateEicr() {

    Eicr saved = eicrDaoImpl.saveOrUpdate(expectedEicr);

    assertNotNull("Saved object should not be null", saved);
    assertNotNull("Generated ID should not be null", saved.getId());

    assertFullEicrMatch(expectedEicr, saved);
  }

  @Test
  public void testGetEicrById() {

    Eicr saved = eicrDaoImpl.saveOrUpdate(expectedEicr);

    Eicr fetched = eicrDaoImpl.getEicrById(saved.getId());

    assertNotNull("Fetched EICR should not be null", fetched);
    assertEquals(saved.getId(), fetched.getId());
    assertFullEicrMatch(saved, fetched);
  }

  @Test
  public void testGetEicrByDocId() {

    eicrDaoImpl.saveOrUpdate(expectedEicr);

    Eicr fetched = eicrDaoImpl.getEicrByDocId(expectedEicr.getEicrDocId());

    assertNotNull("EICR should be fetched using docId", fetched);
    assertEquals(expectedEicr.getEicrDocId(), fetched.getEicrDocId());
    assertFullEicrMatch(expectedEicr, fetched);
  }

  @Test
  public void testGetEicrByCorrelationId() {

    eicrDaoImpl.saveOrUpdate(expectedEicr);

    Eicr fetched = eicrDaoImpl.getEicrByCorrelationId(expectedEicr.getxCorrelationId());

    assertNotNull("EICR should be fetched by correlation ID", fetched);
    assertEquals(expectedEicr.getxCorrelationId(), fetched.getxCorrelationId());
  }

  @Test
  public void testGetEicrAndRRByXRequestId() {

    eicrDaoImpl.saveOrUpdate(expectedEicr);

    List<Eicr> result = eicrDaoImpl.getEicrAndRRByXRequestId(expectedEicr.getxRequestId());

    assertNotNull(result);
    assertFalse(result.isEmpty());
    assertEquals(1, result.size());

    assertFullEicrMatch(expectedEicr, result.get(0));
  }

  @Test
  public void testGetMaxVersionId() throws Exception {

    eicrDaoImpl.saveOrUpdate(expectedEicr);

    JSONObject json = new JSONObject(inputEicr);
    json.put("docVersion", 1.0);

    Eicr second = mapper.readValue(json.toString(), Eicr.class);
    eicrDaoImpl.saveOrUpdate(second);

    Integer maxVersion = eicrDaoImpl.getMaxVersionId(expectedEicr);

    assertNotNull(maxVersion);
    assertEquals(second.getDocVersion(), maxVersion);
  }

  @Test
  public void testGetEicrData_WithSearchParams() {

    Eicr saved = eicrDaoImpl.saveOrUpdate(expectedEicr);

    Map<String, String> params = new HashMap<>();
    params.put(EicrDaoImpl.EICR_DOC_ID, saved.getEicrDocId());
    params.put(EicrDaoImpl.FHIR_SERVER_URL, saved.getFhirServerUrl());
    params.put(EicrDaoImpl.ENCOUNTER_ID, saved.getEncounterId());
    params.put(EicrDaoImpl.SET_ID, saved.getSetId());
    params.put("patientId", saved.getLaunchPatientId());
    params.put("version", saved.getDocVersion().toString());
    params.put(EicrDaoImpl.X_REQUEST_ID, saved.getxRequestId());
    params.put("eicrId", saved.getId().toString());

    List<Eicr> results = eicrDaoImpl.getEicrData(params);

    assertNotNull(results);
    assertEquals(1, results.size());
    assertFullEicrMatch(saved, results.get(0));
    assertFalse("Results should not be empty", results.isEmpty());
  }

  @Test
  public void testGetRRData_WithSearchParams() throws Exception {

    expectedEicr.setResponseDocId("1001");

    Eicr saved = eicrDaoImpl.saveOrUpdate(expectedEicr);

    Map<String, String> params = new HashMap<>();
    params.put(EicrDaoImpl.RESPONSE_DOC_ID, saved.getResponseDocId());

    List<Eicr> results = eicrDaoImpl.getRRData(params);

    assertNotNull("Result list should not be null", results);
    assertFalse("Result list should not be empty", results.isEmpty());
    assertEquals("Only one record should match", 1, results.size());

    Eicr result = results.get(0);

    assertEquals(saved.getResponseDocId(), result.getResponseDocId());
    assertEquals(saved.getId(), result.getId());
    assertEquals(saved.getxRequestId(), result.getxRequestId());
  }

  @Test
  public void testDeleteEicr() {

    Eicr saved = eicrDaoImpl.saveOrUpdate(expectedEicr);

    assertNotNull(saved);

    eicrDaoImpl.deleteEicr(saved);

    Eicr deleted = eicrDaoImpl.getEicrById(saved.getId());

    assertNull("Deleted EICR should be null", deleted);
  }

  @Test
  public void testGetEicrByDocId_NotFound() {
    Eicr result = eicrDaoImpl.getEicrByDocId("invalidDocId");
    assertNull("Result should be null for invalid docId", result);
  }

  @Test
  public void testGetMaxVersionId_WhenNoData() {
    Integer maxVersion = eicrDaoImpl.getMaxVersionId(expectedEicr);
    assertEquals("Should return 0 when no records exist", Integer.valueOf(0), maxVersion);
  }

  private void assertFullEicrMatch(Eicr expected, Eicr actual) {

    assertEquals(expected.getxRequestId(), actual.getxRequestId());
    assertEquals(expected.getxCorrelationId(), actual.getxCorrelationId());
    assertEquals(expected.getEicrDocId(), actual.getEicrDocId());
    assertEquals(expected.getSetId(), actual.getSetId());
    assertEquals(expected.getDocVersion(), actual.getDocVersion());
    assertEquals(expected.getEicrData(), actual.getEicrData());
    assertEquals(expected.getInitiatingAction(), actual.getInitiatingAction());
    assertEquals(expected.getResponseType(), actual.getResponseType());
    assertEquals(expected.getResponseDocId(), actual.getResponseDocId());
    assertEquals(expected.getResponseData(), actual.getResponseData());
    assertEquals(expected.getFhirServerUrl(), actual.getFhirServerUrl());
    assertEquals(expected.getLaunchPatientId(), actual.getLaunchPatientId());
    assertEquals(expected.getEncounterId(), actual.getEncounterId());
  }
}
