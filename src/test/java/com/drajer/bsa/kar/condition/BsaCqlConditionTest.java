package com.drajer.bsa.kar.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.hl7.fhir.r4.model.Duration;
import org.hl7.fhir.r4.model.Endpoint;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.opencds.cqf.fhir.cr.cpg.r4.R4LibraryEvaluationService;

public class BsaCqlConditionTest {

  private BsaCqlCondition condition;

  @Mock private Endpoint mockDataEndpoint;

  @Mock private Endpoint mockTerminologyEndpoint;

  @Mock private Endpoint mockLibraryEndpoint;

  @Mock private R4LibraryEvaluationService mockLibraryService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    condition = new BsaCqlCondition();
  }

  @Test
  public void constructor_InitializesCqlProcessor() {
    assertNotNull(condition.getConditionProcessor());
    assertTrue(condition.getConditionProcessor() instanceof CqlProcessor);
  }

  @Test
  public void dataEndpoint_SetAndGet() {
    condition.setDataEndpoint(mockDataEndpoint);
    assertSame(mockDataEndpoint, condition.getDataEndpoint());
  }

  @Test
  public void terminologyEndpoint_SetAndGet() {
    condition.setTerminologyEndpoint(mockTerminologyEndpoint);
    assertSame(mockTerminologyEndpoint, condition.getTerminologyEndpoint());
  }

  @Test
  public void libraryEndpoint_SetAndGet() {
    condition.setLibraryEndpoint(mockLibraryEndpoint);
    assertSame(mockLibraryEndpoint, condition.getLibraryEndpoint());
  }

  @Test
  public void patientId_SetAndGet() {
    String patientId = "patient-123";
    condition.setPatientId(patientId);
    assertEquals(patientId, condition.getPatientId());
  }

  @Test
  public void url_SetAndGet() {
    String url = "http://example.com/condition";
    condition.setUrl(url);
    assertEquals(url, condition.getUrl());
  }

  @Test
  public void normalReportingDuration_SetAndGet() {
    Duration duration = new Duration();
    duration.setValue(30);
    condition.setNormalReportingDuration(duration);
    assertNotNull(condition.getNormalReportingDuration());
    assertEquals(30, condition.getNormalReportingDuration().getValue().intValue());
  }

  @Test
  public void libraryEvaluationService_SetAndGet() {
    condition.setLibraryEvaluationService(mockLibraryService);
    assertSame(mockLibraryService, condition.getLibraryEvaluationService());
  }

  @Test
  public void allEndpoints_SetAndGet() {
    condition.setDataEndpoint(mockDataEndpoint);
    condition.setTerminologyEndpoint(mockTerminologyEndpoint);
    condition.setLibraryEndpoint(mockLibraryEndpoint);

    assertSame(mockDataEndpoint, condition.getDataEndpoint());
    assertSame(mockTerminologyEndpoint, condition.getTerminologyEndpoint());
    assertSame(mockLibraryEndpoint, condition.getLibraryEndpoint());
  }

  @Test
  public void allProperties_InitiallyNull() {
    assertNull(condition.getDataEndpoint());
    assertNull(condition.getTerminologyEndpoint());
    assertNull(condition.getLibraryEndpoint());
    assertNull(condition.getPatientId());
    assertNull(condition.getUrl());
    assertNull(condition.getNormalReportingDuration());
  }
}
