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
import ca.uhn.fhir.rest.server.exceptions.InvalidRequestException;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.condition.BsaConditionProcessor;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.BsaCondition;
import com.drajer.bsa.kar.model.HealthcareSettingOperationalKnowledgeArtifacts;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.*;
import com.drajer.bsa.routing.impl.DirectTransportImpl;
import com.drajer.bsa.routing.impl.RestfulTransportImpl;
import com.drajer.bsa.scheduler.BsaScheduler;
import com.drajer.bsa.service.KarExecutionStateService;
import com.drajer.bsa.service.PublicHealthAuthorityService;
import com.drajer.bsa.utils.BsaServiceUtils;
import com.drajer.eca.model.TimingSchedule;
import com.drajer.sof.utils.FhirContextInitializer;
import java.util.*;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.UriType;
import org.json.JSONException;
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

    BsaActionStatus localStatus = submitReport.process(data, ehrService);

    assertNotNull(localStatus);
    assertEquals(submitReport.getActionId(), localStatus.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, localStatus.getActionStatus());
    assertNotNull(localStatus);
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

    BsaActionStatus localStatus = submitReport.process(data, ehrService);

    assertNotNull(localStatus);
    assertEquals(submitReport.getActionId(), localStatus.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, localStatus.getActionStatus());
    assertNotNull(localStatus);

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

    BsaActionStatus localStatus = submitReport.process(data, ehrService);

    assertNotNull(localStatus);
    assertEquals(submitReport.getActionId(), localStatus.getActionId());
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, localStatus.getActionStatus());
    assertNotNull(localStatus);
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
    BsaActionStatus localStatus = mock(BsaActionStatus.class);
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

    boolean result = submitReport.submitCdaOutput(data, localStatus, healthcareSetting);

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
  public void testSubmitFhirOutput_ShouldSubmitResourcesToTrustedThirdParty() throws JSONException {
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
  public void testSubmitFhirOutput_ShouldCatchRuntimeException() throws JSONException {
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

  @Test
  public void testSetAndGetBsaServiceUtils() {
    BsaServiceUtils utils = mock(BsaServiceUtils.class);
    submitReport.setBsaServiceUtils(utils);
    BsaServiceUtils result = submitReport.getBsaServiceUtils();
    assertNotNull(result);
    assertEquals(utils, result);
  }

  @Test
  public void testSubmitCdaOutput_UnknownTransportOption_LogsError() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    NotificationContext notificationContext = new NotificationContext();
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(notificationContext);
    data.setSubmittedCdaData("<ClinicalDocument></ClinicalDocument>");
    HealthcareSetting hs = new HealthcareSetting();
    hs.setIsDirect(false);
    hs.setIsRestAPI(false);
    hs.setIsXdr(false);
    BsaActionStatus localStatus = mock(BsaActionStatus.class);
    boolean result = submitReport.submitCdaOutput(data, localStatus, hs);
    assertTrue(result);
  }

  private static final String PHA_TOKEN_URL = "https://auth.pha.example.org/token";

  private IOperationProcessMsgMode<IBaseResource> stubProcessMessageClient(
      FhirContextInitializer initializer, String endpoint) {
    IGenericClient client = mock(IGenericClient.class);
    IOperation operation = mock(IOperation.class);
    IOperationProcessMsg processMessage = mock(IOperationProcessMsg.class);
    IOperationProcessMsgMode<IBaseResource> mode =
        (IOperationProcessMsgMode<IBaseResource>) mock(IOperationProcessMsgMode.class);
    when(initializer.createClient(any(), eq(endpoint), any(), any(), any())).thenReturn(client);
    when(client.operation()).thenReturn(operation);
    when(operation.processMessage()).thenReturn(processMessage);
    when(processMessage.setMessageBundle(any(Bundle.class))).thenReturn(mode);
    when(mode.encodedJson()).thenReturn(mode);
    return mode;
  }

  private KarProcessingData dataWithEicrToSubmit(
      HealthcareSetting healthcareSetting, String... receiverAddresses) {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("ecr-kar");
    Set<UriType> receivers = new HashSet<>();
    for (String address : receiverAddresses) {
      receivers.add(new UriType(address));
    }
    kar.setReceiverAddresses(receivers);

    Bundle eicr = new Bundle();
    eicr.setId("eicr-bundle-001");
    eicr.setType(Bundle.BundleType.MESSAGE);

    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setHealthcareSetting(healthcareSetting);
    data.setxRequestId("x-request-12345");
    data.addResourceById("eicr-report", eicr);

    DataRequirement input = new DataRequirement();
    input.setId("eicr-report");
    input.setType("Bundle");
    ReflectionTestUtils.setField(submitReport, "inputData", new ArrayList<>(List.of(input)));
    submitReport.setActionId("submit-report");
    return data;
  }

  private PublicHealthAuthority publicHealthAuthority(String serverBaseUrl) {
    PublicHealthAuthority pha = new PublicHealthAuthority();
    pha.setFhirServerBaseURL(serverBaseUrl);
    pha.setTokenUrl(PHA_TOKEN_URL);
    return pha;
  }

  private JSONObject accessToken(String token) throws JSONException {
    return new JSONObject().put("access_token", token);
  }

  private Bundle responseBundle() {
    Bundle response = new Bundle();
    response.setId("response-001");
    response.setType(Bundle.BundleType.MESSAGE);
    return response;
  }

  /**
   * The PHA URL is sent to in addition to (not instead of) the next target: submitFhirOutput uses a
   * separate "if" for the PHA URL, then "if trustedThirdParty / else if submissionEndpoint / else
   * receivers". So with both set, the PHA and the trusted third party receive the report, and the
   * submission endpoint is skipped.
   */
  @Test
  public void testSubmitFhirOutput_sendsToThePhaUrlAndTheTrustedThirdParty() throws Exception {
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setPhaUrl("https://pha.example.org/fhir/");
    healthcareSetting.setTrustedThirdParty("https://trusted.example.org/fhir");
    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir");
    KarProcessingData data =
        dataWithEicrToSubmit(healthcareSetting, "https://receiver.example.org/fhir");

    PublicHealthAuthority pha = publicHealthAuthority("https://pha.example.org/fhir");
    PublicHealthAuthorityService phaService = mock(PublicHealthAuthorityService.class);
    when(phaService.getPublicHealthAuthorityByUrl("https://pha.example.org/fhir")).thenReturn(pha);
    when(authorizationUtils.getToken(pha)).thenReturn(accessToken("pha-token"));
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    IOperationProcessMsgMode<IBaseResource> phaMode =
        stubProcessMessageClient(initializer, "https://pha.example.org/fhir");
    when(phaMode.execute()).thenReturn(responseBundle());
    IOperationProcessMsgMode<IBaseResource> trustedMode =
        stubProcessMessageClient(initializer, "https://trusted.example.org/fhir");
    when(trustedMode.execute()).thenReturn(responseBundle());
    submitReport.setPublicHealthAuthorityService(phaService);
    submitReport.setAuthorizationUtils(authorizationUtils);
    submitReport.setFhirContextInitializer(initializer);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);

    submitReport.submitFhirOutput(data, actStatus, mock(EhrQueryService.class));

    // The trailing "/" on the PHA URL is trimmed and the PHA's token is used.
    verify(initializer)
        .createClient(any(), eq("https://pha.example.org/fhir"), eq("pha-token"), any(), any());
    verify(initializer)
        .createClient(any(), eq("https://trusted.example.org/fhir"), any(), any(), any());
    verify(initializer, never())
        .createClient(any(), eq("https://endpoint.example.org/fhir"), any(), any(), any());
    verify(initializer, never())
        .createClient(any(), eq("https://receiver.example.org/fhir"), any(), any(), any());
    verify(actStatus).setActionStatus(BsaTypes.BsaActionStatusType.COMPLETED);
    assertFalse(data.getOutputDataById("response-001").isEmpty());
  }

  @Test
  public void testSubmitFhirOutput_retriesThePhaLookupWithTheProcessMessageSuffix()
      throws Exception {
    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir");
    KarProcessingData data = dataWithEicrToSubmit(new HealthcareSetting());

    PublicHealthAuthority pha = publicHealthAuthority("https://endpoint.example.org/fhir");
    PublicHealthAuthorityService phaService = mock(PublicHealthAuthorityService.class);
    when(phaService.getPublicHealthAuthorityByUrl("https://endpoint.example.org/fhir"))
        .thenReturn(null);
    when(phaService.getPublicHealthAuthorityByUrl(
            "https://endpoint.example.org/fhir/$process-message"))
        .thenReturn(pha);
    when(authorizationUtils.getToken(pha)).thenReturn(accessToken("suffix-token"));
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    IOperationProcessMsgMode<IBaseResource> mode =
        stubProcessMessageClient(initializer, "https://endpoint.example.org/fhir");
    when(mode.execute()).thenReturn(responseBundle());
    submitReport.setPublicHealthAuthorityService(phaService);
    submitReport.setAuthorizationUtils(authorizationUtils);
    submitReport.setFhirContextInitializer(initializer);

    submitReport.submitFhirOutput(data, mock(BsaActionStatus.class), mock(EhrQueryService.class));

    verify(phaService)
        .getPublicHealthAuthorityByUrl("https://endpoint.example.org/fhir/$process-message");
    verify(initializer)
        .createClient(
            any(), eq("https://endpoint.example.org/fhir"), eq("suffix-token"), any(), any());
  }

  @Test
  public void testSubmitFhirOutput_fansOutToEveryReceiverAndSurvivesOneFailing() throws Exception {
    KarProcessingData data =
        dataWithEicrToSubmit(
            new HealthcareSetting(),
            "https://a.receiver.example.org/fhir",
            "https://b.receiver.example.org/fhir");

    PublicHealthAuthorityService phaService = mock(PublicHealthAuthorityService.class);
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    when(initializer.createClient(
            any(), eq("https://a.receiver.example.org/fhir"), any(), any(), any()))
        .thenThrow(new IllegalStateException("receiver a is down"));
    IOperationProcessMsgMode<IBaseResource> mode =
        stubProcessMessageClient(initializer, "https://b.receiver.example.org/fhir");
    when(mode.execute()).thenReturn(responseBundle());
    submitReport.setPublicHealthAuthorityService(phaService);
    submitReport.setAuthorizationUtils(authorizationUtils);
    submitReport.setFhirContextInitializer(initializer);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);

    submitReport.submitFhirOutput(data, actStatus, mock(EhrQueryService.class));

    verify(initializer)
        .createClient(any(), eq("https://a.receiver.example.org/fhir"), any(), any(), any());
    verify(initializer)
        .createClient(any(), eq("https://b.receiver.example.org/fhir"), any(), any(), any());
    verify(actStatus).setActionStatus(BsaTypes.BsaActionStatusType.COMPLETED);
    assertEquals(
        "the working receiver's response is still recorded",
        1,
        data.getOutputDataById("response-001").size());
  }

  @Test
  public void testSubmitFhirOutput_recordsNothingWhenTheServerRejectsOrAnswersEmpty()
      throws Exception {
    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir/$process-message");
    KarProcessingData data = dataWithEicrToSubmit(new HealthcareSetting());
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    IOperationProcessMsgMode<IBaseResource> mode =
        stubProcessMessageClient(initializer, "https://endpoint.example.org/fhir/$process-message");
    submitReport.setPublicHealthAuthorityService(mock(PublicHealthAuthorityService.class));
    submitReport.setAuthorizationUtils(authorizationUtils);
    submitReport.setFhirContextInitializer(initializer);

    when(mode.execute()).thenThrow(new InvalidRequestException("bundle rejected"));
    submitReport.submitFhirOutput(data, mock(BsaActionStatus.class), mock(EhrQueryService.class));
    assertTrue("a rejected bundle must not be recorded", data.getActionOutputData().isEmpty());

    doReturn(null).when(mode).execute();
    submitReport.submitFhirOutput(data, mock(BsaActionStatus.class), mock(EhrQueryService.class));
    assertTrue("an empty response must not be recorded", data.getActionOutputData().isEmpty());
  }

  @Test
  public void testGetSubmissionOperation_defaultsToProcessMessage() {
    assertEquals("process-message", submitReport.getSubmissionOperation());

    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir");
    assertEquals("process-message", submitReport.getSubmissionOperation());

    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir/$process-message");
    assertEquals("process-message", submitReport.getSubmissionOperation());

    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir/$validate");
    assertEquals("validate", submitReport.getSubmissionOperation());
  }

  // =====================================================================================
  // New tests added for more coverage. The existing tests above are unchanged.
  // =====================================================================================

  // ---------------- process ----------------

  @Test
  public void process_cdaR11AndCdaR31_submitCdaAndRunSubActions() {
    for (BsaTypes.OutputContentType format :
        List.of(BsaTypes.OutputContentType.CDA_R11, BsaTypes.OutputContentType.CDA_R31)) {
      submitReport = spy(new SubmitReport());
      KarProcessingData data = dataWithOutputFormat(format);
      doReturn(true).when(submitReport).submitCdaOutput(any(), any(), any());

      BsaActionStatus result = submitReport.process(data, mock(EhrQueryService.class));

      assertEquals(
          format.toString(), BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
      verify(submitReport).submitCdaOutput(eq(data), any(), eq(data.getHealthcareSetting()));
      verify(submitReport, never()).submitFhirOutput(any(), any(), any());
      verify(submitReport).executeSubActions(eq(data), any());
      verify(submitReport).executeRelatedActions(eq(data), any());
    }
  }

  @Test
  public void process_bothFormat_submitsCdaAndFhir() {
    KarProcessingData data = dataWithOutputFormat(BsaTypes.OutputContentType.BOTH);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    doReturn(true).when(submitReport).submitCdaOutput(any(), any(), any());
    doNothing().when(submitReport).submitFhirOutput(any(), any(), any());

    submitReport.process(data, ehrService);

    verify(submitReport).submitCdaOutput(eq(data), any(), any());
    verify(submitReport).submitFhirOutput(eq(data), any(), eq(ehrService));
  }

  @Test
  public void process_unsupportedOutputFormat_submitsNothingButCompletes() {
    KarProcessingData data = dataWithOutputFormat(BsaTypes.OutputContentType.UNKNOWN);

    BsaActionStatus result = submitReport.process(data, mock(EhrQueryService.class));

    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    verify(submitReport, never()).submitCdaOutput(any(), any(), any());
    verify(submitReport, never()).submitFhirOutput(any(), any(), any());
  }

  @Test
  public void process_scheduledButTimersIgnored_stillSubmits() {
    KarProcessingData data = dataWithOutputFormat(BsaTypes.OutputContentType.FHIR);
    doReturn(BsaTypes.BsaActionStatusType.SCHEDULED).when(submitReport).processTimingData(any());
    ReflectionTestUtils.setField(submitReport, "ignoreTimers", true);
    doNothing().when(submitReport).submitFhirOutput(any(), any(), any());

    BsaActionStatus result = submitReport.process(data, mock(EhrQueryService.class));

    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    verify(submitReport).submitFhirOutput(eq(data), any(), any());
  }

  @Test
  public void process_conditionsNotMet_doesNotRunSubOrRelatedActions() {
    KarProcessingData data = dataWithOutputFormat(BsaTypes.OutputContentType.UNKNOWN);
    BsaCondition condition = mock(BsaCondition.class);
    BsaConditionProcessor processor = mock(BsaConditionProcessor.class);
    when(condition.getConditionProcessor()).thenReturn(processor);
    when(processor.evaluateExpression(
            any(BsaCondition.class),
            any(BsaAction.class),
            any(KarProcessingData.class),
            any(EhrQueryService.class)))
        .thenReturn(false);
    submitReport.setConditions(new ArrayList<>(List.of(condition)));

    BsaActionStatus result = submitReport.process(data, mock(EhrQueryService.class));

    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    verify(submitReport, never()).executeSubActions(any(), any());
    verify(submitReport, never()).executeRelatedActions(any(), any());
  }

  /**
   * Known issue: the output-format check is "(artStatus != null && (CDA_R11 || CDA_R30)) ||
   * artStatus.getOutputFormat() == CDA_R31", so a KAR without an artifact status reaches
   * artStatus.getOutputFormat() and throws NullPointerException.
   */
  @Test
  public void process_noArtifactStatusForTheKar_currentlyThrowsNullPointer() {
    KarProcessingData data = dataWithOutputFormat(null);
    EhrQueryService ehrService = mock(EhrQueryService.class);

    assertThrows(NullPointerException.class, () -> submitReport.process(data, ehrService));
  }

  // ---------------- submitCdaOutput ----------------

  @Test
  public void submitCdaOutput_directWithTimersIgnored_doesNotScheduleAResponseCheck() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setIsDirect(true);
    KarProcessingData data = new KarProcessingData();
    KarExecutionStateService stateService = mock(KarExecutionStateService.class);
    data.setKarExecutionStateService(stateService);
    DirectTransportImpl directSender = mock(DirectTransportImpl.class);
    BsaScheduler scheduler = mock(BsaScheduler.class);
    submitReport.setDirectSender(directSender);
    ReflectionTestUtils.setField(submitReport, "scheduler", scheduler);
    ReflectionTestUtils.setField(submitReport, "ignoreTimers", true);

    assertTrue(submitReport.submitCdaOutput(data, status, hs));

    verify(directSender).sendEicrDataUsingDirect(data);
    verifyNoInteractions(stateService);
    verifyNoInteractions(scheduler);
  }

  @Test
  public void submitCdaOutput_restWithoutSubmitter_fallsThroughToUnknownTransport() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setIsRestAPI(true);
    KarProcessingData data = new KarProcessingData();
    data.setSubmittedCdaData("<ClinicalDocument/>");

    // restSubmitter was never set -> the REST branch is skipped
    assertTrue(submitReport.submitCdaOutput(data, status, hs));
    assertNull(submitReport.getRestSubmitter());
  }

  @Test
  public void submitCdaOutput_restWithoutCdaData_fallsThroughToXdr() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setIsRestAPI(true);
    hs.setIsXdr(true);
    KarProcessingData data = new KarProcessingData();
    data.setSubmittedCdaData("");
    RestfulTransportImpl restSubmitter = mock(RestfulTransportImpl.class);
    submitReport.setRestSubmitter(restSubmitter);

    assertTrue(submitReport.submitCdaOutput(data, status, hs));

    verify(restSubmitter, never()).sendEicrDataUsingRestfulApi(any());
  }

  // ---------------- submitFhirOutput ----------------

  @Test
  public void submitFhirOutput_noInputData_submitsNothing() {
    ReflectionTestUtils.setField(submitReport, "inputData", null);
    KarProcessingData data = new KarProcessingData();
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    submitReport.setFhirContextInitializer(initializer);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);

    submitReport.submitFhirOutput(data, actStatus, mock(EhrQueryService.class));

    verifyNoInteractions(initializer);
    verify(actStatus, never()).setActionStatus(any());
  }

  @Test
  public void submitFhirOutput_inputWithoutData_submitsNothing() {
    DataRequirement input = new DataRequirement();
    input.setId("nothing-here");
    input.setType("Bundle");
    ReflectionTestUtils.setField(submitReport, "inputData", new ArrayList<>(List.of(input)));
    KarProcessingData data = mock(KarProcessingData.class);
    when(data.getDataForId(anyString(), ArgumentMatchers.<Map<String, String>>any()))
        .thenReturn(null);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);

    submitReport.submitFhirOutput(data, actStatus, mock(EhrQueryService.class));

    verify(actStatus, never()).setActionStatus(any());
  }

  @Test
  public void submitFhirOutput_noHealthcareSetting_sendsToTheSubmissionEndpoint() {
    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir");
    KarProcessingData data = dataWithEicrToSubmit(null);
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    IOperationProcessMsgMode<IBaseResource> mode =
        stubProcessMessageClient(initializer, "https://endpoint.example.org/fhir");
    when(mode.execute()).thenReturn(responseBundle());
    submitReport.setPublicHealthAuthorityService(mock(PublicHealthAuthorityService.class));
    submitReport.setAuthorizationUtils(authorizationUtils);
    submitReport.setFhirContextInitializer(initializer);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);

    submitReport.submitFhirOutput(data, actStatus, mock(EhrQueryService.class));

    // no PHA found -> sent without a token
    verify(initializer)
        .createClient(any(), eq("https://endpoint.example.org/fhir"), eq(""), any(), any());
    verify(actStatus).setActionStatus(BsaTypes.BsaActionStatusType.COMPLETED);
    assertEquals(1, data.getOutputDataById("response-001").size());
  }

  @Test
  public void submitFhirOutput_blankTargetsAndNoReceivers_completesWithoutSending() {
    HealthcareSetting hs = new HealthcareSetting();
    hs.setPhaUrl(" ");
    hs.setTrustedThirdParty("");
    submitReport.setSubmissionEndpoint("");
    KarProcessingData data = dataWithEicrToSubmit(hs);
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    submitReport.setFhirContextInitializer(initializer);
    BsaActionStatus actStatus = mock(BsaActionStatus.class);

    submitReport.submitFhirOutput(data, actStatus, mock(EhrQueryService.class));

    verifyNoInteractions(initializer);
    verify(actStatus).setActionStatus(BsaTypes.BsaActionStatusType.COMPLETED);
  }

  @Test
  public void submitFhirOutput_tokenNotAvailable_sendsEveryBundleWithoutAToken() {
    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir/");
    KarProcessingData data = dataWithEicrToSubmit(new HealthcareSetting());
    Bundle secondEicr = new Bundle();
    secondEicr.setId("eicr-bundle-002");
    data.addResourceById("eicr-report", secondEicr);

    PublicHealthAuthority pha = publicHealthAuthority("https://endpoint.example.org/fhir");
    PublicHealthAuthorityService phaService = mock(PublicHealthAuthorityService.class);
    when(phaService.getPublicHealthAuthorityByUrl("https://endpoint.example.org/fhir"))
        .thenReturn(pha);
    when(authorizationUtils.getToken(pha)).thenReturn(null);
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);
    IOperationProcessMsgMode<IBaseResource> mode =
        stubProcessMessageClient(initializer, "https://endpoint.example.org/fhir");
    when(mode.execute()).thenReturn(responseBundle());
    submitReport.setPublicHealthAuthorityService(phaService);
    submitReport.setAuthorizationUtils(authorizationUtils);
    submitReport.setFhirContextInitializer(initializer);

    submitReport.submitFhirOutput(data, mock(BsaActionStatus.class), mock(EhrQueryService.class));

    // trailing "/" trimmed, empty token, one call per bundle
    verify(initializer, times(2))
        .createClient(any(), eq("https://endpoint.example.org/fhir"), eq(""), any(), any());
    verify(phaService, never())
        .getPublicHealthAuthorityByUrl("https://endpoint.example.org/fhir/$process-message");
  }

  // ---------------- getters, setters and logging ----------------

  @Test
  public void testPublicHealthAuthorityServiceAndFhirContextInitializerAccessors() {
    PublicHealthAuthorityService phaService = mock(PublicHealthAuthorityService.class);
    FhirContextInitializer initializer = mock(FhirContextInitializer.class);

    submitReport.setPublicHealthAuthorityService(phaService);
    submitReport.setFhirContextInitializer(initializer);

    assertSame(phaService, submitReport.getPublicHealthAuthorityService());
    assertSame(initializer, submitReport.getFhirContextInitializer());
  }

  @Test
  public void testPrintSummaryAndLog() {
    submitReport.setActionId("submit-report");
    submitReport.setCheckResponseActionId("check-response");
    submitReport.setSubmissionEndpoint("https://endpoint.example.org/fhir");

    submitReport.printSummary();
    submitReport.log();

    verify(submitReport).printSummary();
    verify(submitReport).log();
  }

  // ---------------- helpers for the new tests ----------------

  /** KAR "id|r4" with the given output format; null means the KAR has no artifact status. */
  private KarProcessingData dataWithOutputFormat(BsaTypes.OutputContentType format) {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarVersion("r4");

    Set<KnowledgeArtifactStatus> statuses = new HashSet<>();
    if (format != null) {
      KnowledgeArtifactStatus art = new KnowledgeArtifactStatus();
      art.setVersionUniqueKarId("id|r4");
      art.setOutputFormat(format);
      statuses.add(art);
    }
    HealthcareSettingOperationalKnowledgeArtifacts kars =
        new HealthcareSettingOperationalKnowledgeArtifacts();
    kars.setId(1);
    kars.setArtifactStatus(statuses);
    HealthcareSetting hs = new HealthcareSetting();
    hs.setFhirServerBaseURL("http://example.com/fhir");
    hs.setKars(kars);

    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setHealthcareSetting(hs);
    data.setNotificationContext(new NotificationContext());
    return data;
  }
}
