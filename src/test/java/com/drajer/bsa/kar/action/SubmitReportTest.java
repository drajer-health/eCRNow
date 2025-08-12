package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.when;

import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.IOperation;
import ca.uhn.fhir.rest.gclient.IOperationProcessMsg;
import ca.uhn.fhir.rest.gclient.IOperationProcessMsgMode;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.*;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import com.drajer.bsa.routing.impl.RestfulTransportImpl;
import com.drajer.bsa.scheduler.BsaScheduler;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.sof.utils.FhirContextInitializer;
import java.util.*;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.*;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(PowerMockRunner.class)
public class SubmitReportTest {

  @InjectMocks SubmitReport submitReport;

  @Mock AuthorizationUtils authorizationUtils;

  @Mock BsaActionStatus status;

  @Mock PublicHealthAuthorityService publicHealthAuthorityService;

  @Before
  public void setUp() throws Exception {
    submitReport = spy(new SubmitReport());
  }

  @Test
  public void testSetCheckResponseActionId() {
    String expectedActionId = "check-response-123";

    submitReport.setCheckResponseActionId(expectedActionId);

    assertEquals(expectedActionId, submitReport.getCheckResponseActionId());
  }

  @Test
  public void process_OutputFormatShouldCompleteAndSubmitCda() {
    EhrQueryService ehrService = mock(EhrQueryService.class);
    DirectTransportImpl directSender = mock(DirectTransportImpl.class);
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.CDA_R30);
    Set<KnowledgeArtifactStatus> artifactStatus = new HashSet<>();
    artifactStatus.add(art);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(1);
    kars.setArtifactStatus(artifactStatus);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setKars(kars);
    healthcareSetting.setIsDirect(true);
    data.setHealthcareSetting(healthcareSetting);
    data.setSubmittedCdaData("<ClinicalDocument></ClinicalDocument>");
    submitReport.setDirectSender(directSender);
    ReflectionTestUtils.setField(submitReport, "ignoreTimers", false);
    KarExecutionState mockState = mock(KarExecutionState.class);
    KarExecutionStateService mockExecutionStateService = mock(KarExecutionStateService.class);
    when(mockExecutionStateService.saveOrUpdate(any(KarExecutionState.class)))
        .thenReturn(mockState);
    BsaScheduler scheduler = mock(BsaScheduler.class);
    ReflectionTestUtils.setField(submitReport, "scheduler", scheduler);
    data.setKarExecutionStateService(mockExecutionStateService);

    doReturn(true).when(submitReport).submitCdaOutput(data, status, healthcareSetting);

    BsaActionStatus status = submitReport.process(data, ehrService);

    assertNotNull(status);
    assertEquals(submitReport.getActionId(), status.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertNotNull(status);
  }

  @Test
  public void process_OutputFormatShouldCompleteAndSubmitFhir() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    DirectTransportImpl directSender = mock(DirectTransportImpl.class);
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.FHIR);
    Set<KnowledgeArtifactStatus> artifactStatus = new HashSet<>();
    artifactStatus.add(art);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(1);
    kars.setArtifactStatus(artifactStatus);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setKars(kars);
    healthcareSetting.setIsDirect(true);
    data.setHealthcareSetting(healthcareSetting);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);
    submitReport.setDirectSender(directSender);
    doNothing().when(submitReport).submitFhirOutput(data, actStatus, ehrService);

    BsaActionStatus status = submitReport.process(data, ehrService);

    assertNotNull(status);
    assertEquals(submitReport.getActionId(), status.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertNotNull(status);

    verify(submitReport).submitFhirOutput(any(), any(), any());
  }

  @Test
  public void process_OutputFormatShouldCompleteAndSubmitBoth() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.BOTH);
    Set<KnowledgeArtifactStatus> artifactStatus = new HashSet<>();
    artifactStatus.add(art);
    DirectTransportImpl directSender = mock(DirectTransportImpl.class);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    data.setSubmittedCdaData("<ClinicalDocument></ClinicalDocument>");
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(1);
    kars.setArtifactStatus(artifactStatus);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setKars(kars);
    healthcareSetting.setIsDirect(true);
    data.setHealthcareSetting(healthcareSetting);
    submitReport.setDirectSender(directSender);

    doNothing().when(submitReport).submitFhirOutput(data, actStatus, ehrService);
    doReturn(true).when(submitReport).submitCdaOutput(data, status, healthcareSetting);

    BsaActionStatus status = submitReport.process(data, ehrService);

    assertNotNull(status);
    assertEquals(submitReport.getActionId(), status.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertNotNull(status);
  }

  @Test
  public void testProcess_whenStatusIsScheduled() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    submitReport.setActionId("test-action-id");
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(submitReport, "timingData", timingSchedules);
    ReflectionTestUtils.setField(submitReport, "ignoreTimers", false);

    BsaActionStatus result = submitReport.process(data, ehrService);

    assertNotNull(result);
    assertEquals("test-action-id", result.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
  }

  @Test
  public void testSubmitCdaOutput_WithDirectTransport_ShouldSendViaDirectAndScheduleJob() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.BOTH);
    Set<KnowledgeArtifactStatus> artifactStatus = new HashSet<>();
    artifactStatus.add(art);
    DirectTransportImpl directSender = mock(DirectTransportImpl.class);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(1);
    kars.setArtifactStatus(artifactStatus);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setKars(kars);
    healthcareSetting.setIsDirect(true);
    data.setHealthcareSetting(healthcareSetting);
    data.setxRequestId("req123");
    data.setJobType(BsaTypes.BsaJobType.IMMEDIATE_REPORTING);
    MDC.put("xRequestId", "req123");
    BsaActionStatus status = mock(BsaActionStatus.class);
    data.setSubmittedCdaData("<ClinicalDocument></ClinicalDocument>");
    submitReport.setDirectSender(directSender);
    ReflectionTestUtils.setField(submitReport, "ignoreTimers", false);

    KarExecutionState mockState = mock(KarExecutionState.class);
    when(mockState.getId()).thenReturn(UUID.randomUUID());

    KarExecutionStateService mockExecutionStateService = mock(KarExecutionStateService.class);
    when(mockExecutionStateService.saveOrUpdate(any(KarExecutionState.class)))
        .thenReturn(mockState);
    BsaScheduler scheduler = mock(BsaScheduler.class);
    ReflectionTestUtils.setField(submitReport, "scheduler", scheduler);
    data.setKarExecutionStateService(mockExecutionStateService);

    boolean result = submitReport.submitCdaOutput(data, status, healthcareSetting);

    assertTrue(result);

    verify(directSender, times(1)).sendEicrDataUsingDirect(data);
  }

  @Test
  public void testSubmitCdaOutput_WithDirectTransport_IsRestAPIIsTrue() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.BOTH);
    Set<KnowledgeArtifactStatus> artifactStatus = new HashSet<>();
    artifactStatus.add(art);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(1);
    kars.setArtifactStatus(artifactStatus);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setKars(kars);
    healthcareSetting.setIsRestAPI(true);
    data.setHealthcareSetting(healthcareSetting);
    data.setSubmittedCdaData("<ClinicalDocument></ClinicalDocument>");
    RestfulTransportImpl restSubmitter = mock(RestfulTransportImpl.class);
    ReflectionTestUtils.setField(submitReport, "restSubmitter", restSubmitter);

    boolean result = submitReport.submitCdaOutput(data, status, healthcareSetting);

    assertTrue(result);

    verify(restSubmitter, times(1)).sendEicrDataUsingRestfulApi(data);
  }

  @Test
  public void testSubmitCdaOutput_WithDirectTransport_IsXdrTrue() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
    art.setVersionUniqueKarId("id|r4");
    art.setOutputFormat(BsaTypes.OutputContentType.BOTH);
    Set<KnowledgeArtifactStatus> artifactStatus = new HashSet<>();
    artifactStatus.add(art);
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(1);
    kars.setArtifactStatus(artifactStatus);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setKars(kars);
    healthcareSetting.setIsXdr(true);
    data.setHealthcareSetting(healthcareSetting);
    data.setSubmittedCdaData("<ClinicalDocument></ClinicalDocument>");
    RestfulTransportImpl restSubmitter = new RestfulTransportImpl();
    ReflectionTestUtils.setField(submitReport, "restSubmitter", restSubmitter);

    boolean result = submitReport.submitCdaOutput(data, status, healthcareSetting);

    assertTrue(result);
  }

  @Test
  public void testSubmitFhirOutput_ShouldSubmitResourcesToTrustedThirdParty() {
    KarProcessingData data = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);
    submitReport.setActionId("test-action-id");
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setTrustedThirdParty("https://api.trusted-pha.org/fhir/");
    when(data.getHealthcareSetting()).thenReturn(healthcareSetting);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("id");
    dataRequirement.setType("type");
    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("id1");
    dataRequirement1.setType("type1");
    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);
    ReflectionTestUtils.setField(submitReport, "inputData", listdr);
    Bundle bundle1 = new Bundle();
    bundle1.setId("bundle-001");
    Bundle bundle2 = new Bundle();
    bundle2.setId("bundle-002");
    Set<Resource> resourceSet = new HashSet<>();
    resourceSet.add(bundle1);
    resourceSet.add(bundle2);
    JSONObject mockTokenJson = new JSONObject();
    mockTokenJson.put("access_token", "mock-access-token");
    PublicHealthAuthority Pha = new PublicHealthAuthority();
    Pha.setFhirServerBaseURL("https://api.trusted-pha.org/fhir");
    Pha.setTokenUrl("https://auth.trusted-pha.org/token");

    doReturn(resourceSet)
        .when(data)
        .getDataForId(anyString(), ArgumentMatchers.<Map<String, String>>any());

    submitReport.setPublicHealthAuthorityService(publicHealthAuthorityService);
    submitReport.setAuthorizationUtils(authorizationUtils);
    when(authorizationUtils.getToken(any(PublicHealthAuthority.class))).thenReturn(mockTokenJson);
    FhirContextInitializer fhirContextInitializer = mock(FhirContextInitializer.class);
    submitReport.setFhirContextInitializer(fhirContextInitializer);
    when(publicHealthAuthorityService.getPublicHealthAuthorityByUrl(anyString())).thenReturn(Pha);
    IGenericClient mockClient = mock(IGenericClient.class);
    when(fhirContextInitializer.createClient(any(), any(), any(), any(), any()))
        .thenReturn(mockClient);
    IOperation mockOperation = mock(IOperation.class);
    IOperationProcessMsg mockProcessMessage = mock(IOperationProcessMsg.class);
    IOperationProcessMsgMode<IBaseResource> mockProcessMsgMode =
        (IOperationProcessMsgMode<IBaseResource>) mock(IOperationProcessMsgMode.class);
    when(mockClient.operation()).thenReturn(mockOperation);
    when(mockOperation.processMessage()).thenReturn(mockProcessMessage);
    when(mockProcessMessage.setMessageBundle(any(Bundle.class))).thenReturn(mockProcessMsgMode);
    when(mockProcessMsgMode.encodedJson()).thenReturn(mockProcessMsgMode);
    when(mockProcessMsgMode.execute()).thenReturn(new Bundle());

    submitReport.submitFhirOutput(data, actStatus, ehrService);

    verify(actStatus).setActionStatus(eq(BsaTypes.BsaActionStatusType.COMPLETED));
  }

  @Test
  public void testSubmitFhirOutput_ShouldCatchRuntimeException() {
    KarProcessingData data = mock(KarProcessingData.class);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);
    submitReport.setActionId("test-action-id");
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setTrustedThirdParty("https://api.trusted-pha.org/fhir/");
    when(data.getHealthcareSetting()).thenReturn(healthcareSetting);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("id");
    dataRequirement.setType("type");
    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("id1");
    dataRequirement1.setType("type1");
    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);
    ReflectionTestUtils.setField(submitReport, "inputData", listdr);
    Bundle bundle1 = new Bundle();
    bundle1.setId("bundle-001");
    Bundle bundle2 = new Bundle();
    bundle2.setId("bundle-002");
    Set<Resource> resourceSet = new HashSet<>();
    resourceSet.add(bundle1);
    resourceSet.add(bundle2);
    JSONObject mockTokenJson = new JSONObject();
    mockTokenJson.put("access_token", "mock-access-token");
    ReflectionTestUtils.setField(submitReport, "inputData", listdr);
    doReturn(resourceSet)
        .when(data)
        .getDataForId(anyString(), ArgumentMatchers.<Map<String, String>>any());
    PublicHealthAuthority pha = new PublicHealthAuthority();
    pha.setFhirServerBaseURL("https://api.trusted-pha.org/fhir");
    pha.setTokenUrl("https://auth.trusted-pha.org/token");
    when(publicHealthAuthorityService.getPublicHealthAuthorityByUrl(anyString())).thenReturn(pha);
    when(authorizationUtils.getToken(any())).thenReturn(mockTokenJson);
    FhirContextInitializer fhirContextInitializer = mock(FhirContextInitializer.class);
    IGenericClient mockClient = mock(IGenericClient.class);
    when(fhirContextInitializer.createClient(any(), any(), any(), any(), any()))
        .thenReturn(mockClient);
    IOperation mockOperation = mock(IOperation.class);
    IOperationProcessMsg mockProcessMessage = mock(IOperationProcessMsg.class);
    IOperationProcessMsgMode<IBaseResource> mockProcessMsgMode =
        (IOperationProcessMsgMode<IBaseResource>) mock(IOperationProcessMsgMode.class);
    when(mockClient.operation()).thenReturn(mockOperation);
    when(mockOperation.processMessage()).thenReturn(mockProcessMessage);
    when(mockProcessMessage.setMessageBundle(any(Bundle.class))).thenReturn(mockProcessMsgMode);
    when(mockProcessMsgMode.encodedJson()).thenReturn(mockProcessMsgMode);
    when(mockProcessMsgMode.execute()).thenReturn(new Bundle());

    when(mockProcessMsgMode.execute())
        .thenThrow(new RuntimeException("Simulated processing error"));
    submitReport.setPublicHealthAuthorityService(publicHealthAuthorityService);
    submitReport.setAuthorizationUtils(authorizationUtils);
    submitReport.setFhirContextInitializer(fhirContextInitializer);

    submitReport.submitFhirOutput(data, actStatus, ehrService);

    verify(mockProcessMsgMode).execute();
  }

  @Test
  public void testSubmissionEndpointGetterSetter() {
    String endpoint = "http://example.com/submit";
    submitReport.setSubmissionEndpoint(endpoint);
    assertEquals(endpoint, submitReport.getSubmissionEndpoint());
  }

  @Test
  public void testSetAndGetDirectSender() {
    DirectTransportImpl mockDirectSender = mock(DirectTransportImpl.class);

    submitReport.setDirectSender(mockDirectSender);
    DirectTransportImpl result = submitReport.getDirectSender();

    assertNotNull(result);
  }

  @Test
  public void testSetAndGetPhDao() {
    PublicHealthMessagesDao PhDao = mock(PublicHealthMessagesDao.class);

    submitReport.setPhDao(PhDao);
    PublicHealthMessagesDao result = submitReport.getPhDao();

    assertNotNull(result);
  }

  @Test
  public void testSetAndGetRestSubmitter() {
    RestfulTransportImpl mockRestSubmitter = mock(RestfulTransportImpl.class);

    submitReport.setRestSubmitter(mockRestSubmitter);

    RestfulTransportImpl result = submitReport.getRestSubmitter();

    assertNotNull(result);
  }

  @Test
  public void testSetAndGetAuthorizationUtils() {
    submitReport.setAuthorizationUtils(authorizationUtils);

    AuthorizationUtils result = submitReport.getAuthorizationUtils();

    assertNotNull(result);
  }
}
