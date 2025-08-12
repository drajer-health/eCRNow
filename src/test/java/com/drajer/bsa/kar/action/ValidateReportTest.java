package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.internal.verification.VerificationModeFactory.atLeastOnce;
import static org.mockito.internal.verification.VerificationModeFactory.times;
import static org.powermock.api.mockito.PowerMockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
//import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@RunWith(PowerMockRunner.class)
public class ValidateReportTest {

  @InjectMocks ValidateReport validateReport;

  @Before
  public void setUp() throws Exception {
    ReflectionTestUtils.setField(validateReport, "validateEicrR11Data", true);
    ReflectionTestUtils.setField(validateReport, "validateEicrR31Data", true);
    RestTemplate restTemplate = mock(RestTemplate.class);
    ReflectionTestUtils.setField(validateReport, "restTemplate", restTemplate);
    ReflectionTestUtils.setField(validateReport, "validatorEndpoint", "http://validator-endpoint");
  }

  @Test
  public void testProcess_whenOutputFormatIsCDA_R30_shouldCallValidateCDA_R30() {
    KnowledgeArtifact kar = spy(new KnowledgeArtifact());
    kar.setKarId("kar-1");
    kar.setKarVersion("r4");
    EhrQueryService ehrService = mock(EhrQueryService.class);
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    context.setNotificationResourceId("enc-1");
    context.setNotificationResourceType("Encounter");
    context.setFhirServerBaseUrl("http://fhir-server.com");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    KnowledgeArtifactStatus artifactStatus = new KnowledgeArtifactStatus();
    artifactStatus.setVersionUniqueKarId("kar-1/r4");
    artifactStatus.setOutputFormat(BsaTypes.OutputContentType.CDA_R30);
    HealthcareSetting healthcareSetting = mock(HealthcareSetting.class);
    data.setHealthcareSetting(healthcareSetting);

    doReturn("kar-1/r4").when(kar).getVersionUniqueId();
    when(healthcareSetting.getArtifactStatus("kar-1/r4")).thenReturn(artifactStatus);

    BsaActionStatus status = validateReport.process(data, ehrService);

    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertEquals(BsaTypes.OutputContentType.CDA_R30, artifactStatus.getOutputFormat());
  }

  @Test
  public void testProcess_whenOutputFormatIsCdaR11_shouldCallValidateCdaR11() {
    KnowledgeArtifact kar = spy(new KnowledgeArtifact());
    kar.setKarId("kar-1");
    kar.setKarVersion("r4");
    EhrQueryService ehrService = mock(EhrQueryService.class);
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    context.setNotificationResourceId("enc-1");
    context.setNotificationResourceType("Encounter");
    context.setFhirServerBaseUrl("http://fhir-server.com");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    KnowledgeArtifactStatus artifactStatus = new KnowledgeArtifactStatus();
    artifactStatus.setVersionUniqueKarId("kar-1/r4");
    artifactStatus.setOutputFormat(BsaTypes.OutputContentType.CDA_R11);
    HealthcareSetting healthcareSetting = mock(HealthcareSetting.class);
    data.setHealthcareSetting(healthcareSetting);

    doReturn("kar-1/r4").when(kar).getVersionUniqueId();
    when(healthcareSetting.getArtifactStatus("kar-1/r4")).thenReturn(artifactStatus);

    BsaActionStatus status = validateReport.process(data, ehrService);

    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertEquals(BsaTypes.OutputContentType.CDA_R11, artifactStatus.getOutputFormat());
  }

  @Test
  public void testProcess_whenOutputFormatIsFHIR_shouldCallValidateFHIR() {
    KnowledgeArtifact kar = spy(new KnowledgeArtifact());
    kar.setKarId("kar-1");
    kar.setKarVersion("r4");
    EhrQueryService ehrService = mock(EhrQueryService.class);
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    context.setNotificationResourceId("enc-1");
    context.setNotificationResourceType("Encounter");
    context.setFhirServerBaseUrl("http://fhir-server.com");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    KnowledgeArtifactStatus artifactStatus = new KnowledgeArtifactStatus();
    artifactStatus.setVersionUniqueKarId("kar-1/r4");
    artifactStatus.setOutputFormat(BsaTypes.OutputContentType.FHIR);
    HealthcareSetting healthcareSetting = mock(HealthcareSetting.class);
    data.setHealthcareSetting(healthcareSetting);

    doReturn("kar-1/r4").when(kar).getVersionUniqueId();
    when(healthcareSetting.getArtifactStatus("kar-1/r4")).thenReturn(artifactStatus);

    BsaActionStatus status = validateReport.process(data, ehrService);

    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertEquals(BsaTypes.OutputContentType.FHIR, artifactStatus.getOutputFormat());
  }

  @Test
  public void testProcess_whenOutputFormatIsBOTH_shouldCallValidateBOTH() {
    KnowledgeArtifact kar = spy(new KnowledgeArtifact());
    kar.setKarId("kar-1");
    kar.setKarVersion("r4");
    EhrQueryService ehrService = mock(EhrQueryService.class);
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    context.setNotificationResourceId("enc-1");
    context.setNotificationResourceType("Encounter");
    context.setFhirServerBaseUrl("http://fhir-server.com");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    KnowledgeArtifactStatus artifactStatus = new KnowledgeArtifactStatus();
    artifactStatus.setVersionUniqueKarId("kar-1/r4");
    artifactStatus.setOutputFormat(BsaTypes.OutputContentType.BOTH);
    HealthcareSetting healthcareSetting = mock(HealthcareSetting.class);
    data.setHealthcareSetting(healthcareSetting);

    doReturn("kar-1/r4").when(kar).getVersionUniqueId();
    when(healthcareSetting.getArtifactStatus("kar-1/r4")).thenReturn(artifactStatus);

    BsaActionStatus status = validateReport.process(data, ehrService);

    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertEquals(BsaTypes.OutputContentType.BOTH, artifactStatus.getOutputFormat());
  }

  @Test
  public void testAddValidatedOutputById_shouldAddOutputForEachDataRequirement() {
    KnowledgeArtifact kar = spy(new KnowledgeArtifact());
    kar.setKarId("kar-1");
    kar.setKarVersion("r4");
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    context.setNotificationResourceId("enc-1");
    context.setNotificationResourceType("Encounter");
    context.setFhirServerBaseUrl("http://fhir-server.com");
    KarProcessingData data = spy(new KarProcessingData());
    data.setKar(kar);
    data.setNotificationContext(context);
    Resource resource = mock(Resource.class);
    DataRequirement dr1 = new DataRequirement();
    dr1.setId("dr-1");
    DataRequirement dr2 = new DataRequirement();
    dr2.setId("dr-2");
    List<DataRequirement> outputData = new ArrayList<>();
    outputData.add(dr1);
    outputData.add(dr2);
    ReflectionTestUtils.setField(validateReport, "outputData", outputData);

    validateReport.addValidatedOutputById(data, resource);

    verify(data, times(1)).addActionOutputById("dr-1", resource);
    verify(data, times(1)).addActionOutputById("dr-2", resource);
  }

  @Test
  public void testSetAndGetValidatorEndpoint() {
    validateReport.setValidatorEndpoint("http://validator.example.com");
    assertEquals("http://validator.example.com", validateReport.getValidatorEndpoint());
  }

  @Test
  public void testSetAndGetValidateEicrR11Data() {
    validateReport.setValidateEicrR11Data(true);
    assertTrue(validateReport.getValidateEicrR11Data());
  }

  @Test
  public void testSetAndGetValidateEicrFhirData() {
    validateReport.setValidateEicrFhirData(true);
    assertTrue(validateReport.getValidateEicrFhirData());
  }

  @Test
  public void testSetAndGetValidateEicrR31Data() {
    validateReport.setValidateEicrR31Data(true);
    assertTrue(validateReport.getValidateEicrR31Data());
  }

  @Test
  public void testSetAndGetEicrR11SchematronPath() {
    String path = "/path/to/eicrR11.sch";
    validateReport.setEicrR11SchematronPath(path);
    assertEquals(path, validateReport.getEicrR11SchematronPath());
  }

  @Test
  public void testSetAndGetEicrR31SchematronPath() {
    String path = "/path/to/eicrR31.sch";
    validateReport.setEicrR31SchematronPath(path);
    assertEquals(path, validateReport.getEicrR31SchematronPath());
  }

  @Test
  public void testValidateFhirOutput_withValidValidator() {
    KnowledgeArtifact kar = spy(new KnowledgeArtifact());
    kar.setKarId("kar-1");
    kar.setKarVersion("r4");
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    context.setNotificationResourceId("enc-1");
    context.setNotificationResourceType("Encounter");
    context.setFhirServerBaseUrl("http://fhir-server.com");
    KarProcessingData data = spy(new KarProcessingData());
    data.setKar(kar);
    data.setNotificationContext(context);
    DataRequirement dr1 = new DataRequirement();
    dr1.setId("dr-1");
    DataRequirement dr2 = new DataRequirement();
    dr2.setId("dr-2");
    List<DataRequirement> inputData = new ArrayList<>();
    inputData.add(dr1);
    inputData.add(dr2);
    List<DataRequirement> outputData = new ArrayList<>();
    outputData.add(dr1);
    outputData.add(dr2);
    Patient patient = new Patient();
    patient.setId("patient-001");
    patient.addName().setFamily("Doe").addGiven("John");
    Observation observation = new Observation();
    observation.setId("obs-001");
    observation.setStatus(Observation.ObservationStatus.FINAL);
    Set<Resource> resourceSet = new HashSet<>();
    resourceSet.add(patient);
    resourceSet.add(observation);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);
    ValidateReport validateReport = spy(new ValidateReport());
    Map<String, String> relatedDataIds = new HashMap<>();
    relatedDataIds.put("dr-1", "dr-1");
    relatedDataIds.put("dr-2", "dr-2");
    ReflectionTestUtils.setField(validateReport, "inputData", inputData);
    ReflectionTestUtils.setField(validateReport, "outputData", outputData);

    doReturn(relatedDataIds).when(validateReport).getInputDataIdToRelatedDataIdMap();
    FhirContext fhirContext = FhirContext.forR4();
    IParser parser = fhirContext.newJsonParser();
    ReflectionTestUtils.setField(validateReport, "jsonParser", parser);
    ReflectionTestUtils.setField(
        validateReport, "validatorEndpoint", "http://localhost:8080/fhir-validator");
    doReturn(resourceSet)
        .when(data)
        .getDataForId(anyString(), ArgumentMatchers.<Map<String, String>>any());
    RestTemplate restTemplate = mock(RestTemplate.class);
    String operationOutcomeJson =
        "{"
            + "\"resourceType\":\"OperationOutcome\","
            + "\"issue\":[{"
            + "\"severity\":\"information\","
            + "\"code\":\"informational\","
            + "\"diagnostics\":\"All good\""
            + "}]"
            + "}";

    ResponseEntity<String> mockResponse = new ResponseEntity<>(operationOutcomeJson, HttpStatus.OK);
    when(restTemplate.postForEntity(anyString(), any(String.class), any(Class.class)))
        .thenReturn(mockResponse);
    ReflectionTestUtils.setField(validateReport, "restTemplate", restTemplate);
    validateReport.validateFhirOutput(data, actStatus);

    assertNotEquals(BsaTypes.BsaActionStatusType.FAILED, actStatus.getActionStatus());
    verify(data, atLeastOnce()).addActionOutput(anyString(), any(Resource.class));
  }

  @Test
  public void testValidateFhirOutput_whenValidatorReturnsErrors() {
    KnowledgeArtifact kar = spy(new KnowledgeArtifact());
    kar.setKarId("kar-1");
    kar.setKarVersion("r4");
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    context.setNotificationResourceId("enc-1");
    context.setNotificationResourceType("Encounter");
    context.setFhirServerBaseUrl("http://fhir-server.com");
    KarProcessingData data = spy(new KarProcessingData());
    data.setKar(kar);
    data.setNotificationContext(context);
    DataRequirement dr1 = new DataRequirement();
    dr1.setId("dr-1");
    DataRequirement dr2 = new DataRequirement();
    dr2.setId("dr-2");
    List<DataRequirement> inputData = new ArrayList<>();
    inputData.add(dr1);
    inputData.add(dr2);
    List<DataRequirement> outputData = new ArrayList<>();
    outputData.add(dr1);
    outputData.add(dr2);
    Patient patient = new Patient();
    patient.setId("patient-001");
    patient.addName().setFamily("Doe").addGiven("John");
    Observation observation = new Observation();
    observation.setId("obs-001");
    observation.setStatus(Observation.ObservationStatus.FINAL);
    Set<Resource> resourceSet = new HashSet<>();
    resourceSet.add(patient);
    resourceSet.add(observation);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);
    ValidateReport validateReport = spy(new ValidateReport());
    Map<String, String> relatedDataIds = new HashMap<>();
    relatedDataIds.put("dr-1", "dr-1");
    relatedDataIds.put("dr-2", "dr-2");
    ReflectionTestUtils.setField(validateReport, "inputData", inputData);
    ReflectionTestUtils.setField(validateReport, "outputData", outputData);
    doReturn(relatedDataIds).when(validateReport).getInputDataIdToRelatedDataIdMap();
    FhirContext fhirContext = FhirContext.forR4();
    IParser parser = fhirContext.newJsonParser();
    ReflectionTestUtils.setField(validateReport, "jsonParser", parser);
    ReflectionTestUtils.setField(
        validateReport, "validatorEndpoint", "http://localhost:8080/fhir-validator");
    doReturn(resourceSet)
        .when(data)
        .getDataForId(anyString(), ArgumentMatchers.<Map<String, String>>any());
    RestTemplate restTemplate = mock(RestTemplate.class);
    String operationOutcomeJson =
        "{"
            + "\"resourceType\":\"OperationOutcome\","
            + "\"issue\":[{"
            + "\"severity\":\"error\","
            + "\"code\":\"invalid\","
            + "\"diagnostics\":\"Invalid resource\""
            + "}]"
            + "}";
    ResponseEntity<String> mockResponse = new ResponseEntity<>(operationOutcomeJson, HttpStatus.OK);
    when(restTemplate.postForEntity(anyString(), any(String.class), any(Class.class)))
        .thenReturn(mockResponse);
    ReflectionTestUtils.setField(validateReport, "restTemplate", restTemplate);

    validateReport.validateFhirOutput(data, actStatus);

    assertNotEquals(BsaTypes.BsaActionStatusType.FAILED, actStatus.getActionStatus());
  }
}
