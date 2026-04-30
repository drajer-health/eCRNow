// package com.drajer.eca.model;
//
// import static com.helger.commons.mock.CommonsAssert.assertEquals;
// import static org.junit.Assert.assertNotNull;
// import static org.junit.Assert.assertTrue;
// import static org.mockito.ArgumentMatchers.anyInt;
// import static org.mockito.Mockito.*;
//
// import com.drajer.eca.model.EventTypes.EcrActionTypes;
// import com.drajer.eca.model.EventTypes.WorkflowEvent;
// import com.drajer.ecrapp.model.Eicr;
// import com.drajer.ecrapp.service.EicrRRService;
// import com.drajer.ecrapp.service.WorkflowService;
// import com.drajer.ecrapp.util.ApplicationUtils;
// import com.drajer.routing.RestApiSender;
// import com.drajer.routing.impl.DirectEicrSender;
// import com.drajer.sof.model.LaunchDetails;
// import java.util.Date;
// import java.util.HashSet;
// import java.util.Set;
// import org.hl7.fhir.r4.model.PlanDefinition.ActionRelationshipType;
// import org.junit.Before;
// import org.junit.Test;
// import org.junit.runner.RunWith;
// import org.mockito.InjectMocks;
// import org.powermock.api.mockito.PowerMockito;
// import org.powermock.core.classloader.annotations.PowerMockIgnore;
// import org.powermock.core.classloader.annotations.PrepareForTest;
// import org.powermock.modules.junit4.PowerMockRunner;
//
// @RunWith(PowerMockRunner.class)
// @PrepareForTest({EcaUtils.class, ApplicationUtils.class, WorkflowService.class,
// ActionRepo.class})
// @PowerMockIgnore({"com.sun.org.apache.xerces.*", "javax.xml.*", "org.xml.*",
// "javax.management.*"})
// public class SubmitEicrActionTest {
//
//  private LaunchDetails mockDetails;
//  private PatientExecutionState mockState;
//  private RelatedAction mockRelActn;
//  private ActionRepo mockActionRepo;
//
//  @InjectMocks SubmitEicrAction submitEicrAction;
//
//  Set<SubmitEicrStatus> mockSubmitEicrStatus;
//
//  Set<Integer> mockIds;
//
//  private WorkflowEvent launchType = WorkflowEvent.SCHEDULED_JOB;
//
//  @Before
//  public void setUp() {
//
//    mockDetails = PowerMockito.mock(LaunchDetails.class);
//    mockState = PowerMockito.mock(PatientExecutionState.class);
//    mockRelActn = PowerMockito.mock(RelatedAction.class);
//
//    if (mockActionRepo == null) {
//
//      mockActionRepo = PowerMockito.mock(ActionRepo.class);
//    }
//
//    PowerMockito.mockStatic(ActionRepo.class);
//    PowerMockito.mockStatic(EcaUtils.class);
//    PowerMockito.mockStatic(ApplicationUtils.class);
//    PowerMockito.mockStatic(WorkflowService.class);
//  }
//
//  @Test
//  public void testExecute_RelatedActionNotCompleted() throws Exception {
//
//    submitEicrAction.addRelatedAction(mockRelActn);
//
//    setUpMockData();
//
//    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
//    when(mockRelActn.getRelatedAction()).thenReturn(submitEicrAction);
//    when(mockState.hasActionCompleted(any())).thenReturn(false);
//
//    String taskInstanceId = "";
//    // Test
//    submitEicrAction.execute(mockDetails, launchType, taskInstanceId);
//
//    // Validate
//    verify(mockState, times(1)).hasActionCompleted("123");
//    verify(mockRelActn, times(0)).getDuration();
//  }
//
//  @Test
//  public void testExecute_RelatedActionCompleted() {
//
//    submitEicrAction.addRelatedAction(mockRelActn);
//
//    setUpMockData();
//
//    when(mockRelActn.getRelationship()).thenReturn(ActionRelationshipType.AFTER);
//    when(mockRelActn.getRelatedAction()).thenReturn(submitEicrAction);
//    when(mockState.hasActionCompleted(any())).thenReturn(true);
//
//    String taskInstanceId = "";
//    // Test
//    submitEicrAction.execute(mockDetails, launchType, taskInstanceId);
//
//    // validate
//    assertNotNull(mockState.getEicrIdForCompletedActions("123"));
//    assertNotNull(mockState.getEicrsReadyForSubmission());
//  }
//
//  @Test
//  public void testExecute_RelatedActionNotPresent() {
//
//    setUpMockData();
//
//    when(mockRelActn.getRelatedAction()).thenReturn(null);
//
//    String taskInstanceId = "";
//    submitEicrAction.execute(mockDetails, launchType, taskInstanceId);
//
//    // validate
//    assertNotNull(mockState.getEicrIdForCompletedActions("123"));
//    assertNotNull(mockState.getEicrsReadyForSubmission());
//  }
//
//  public void setUpMockData() {
//    submitEicrAction.setActionId("123");
//    mockSubmitEicrStatus = new HashSet<>();
//    when(ApplicationUtils.getDetailStatus(mockDetails)).thenReturn(mockState);
//    when(mockState.getSubmitEicrStatus()).thenReturn(mockSubmitEicrStatus);
//  }
//
//  @Test
//  public void testPrintMethod() {
//
//    SubmitEicrAction action = new SubmitEicrAction();
//    action.print();
//    assertNotNull(action);
//  }
//
//  @Test
//  public void testSubmitEicrs_EicrNotFound_WithAssertAndSet() {
//    Set<Integer> ids = new HashSet<>();
//    ids.add(10);
//    PowerMockito.when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
//    EicrRRService rrService = PowerMockito.mock(EicrRRService.class);
//    when(mockActionRepo.getEicrRRService()).thenReturn(rrService);
//    when(rrService.getEicrById(10)).thenReturn(null);
//
//    try {
//      submitEicrAction.submitEicrs(mockDetails, mockState, ids, "");
//      org.junit.Assert.fail("Expected RuntimeException");
//
//    } catch (RuntimeException ex) {
//      assertNotNull(ex.getMessage());
//      assertTrue(ex.getMessage().contains("No Eicr found for submission"));
//    }
//  }
//
//  @Test
//  public void testSubmitEicrs_RestTransport_Success() {
//    Set<Integer> ids = new HashSet<>();
//    ids.add(1);
//    Eicr ecr = PowerMockito.mock(Eicr.class);
//    when(ecr.getEicrData()).thenReturn("<xml/>");
//    when(ecr.getxCorrelationId()).thenReturn("corr-1");
//    PowerMockito.when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
//    EicrRRService rrService = PowerMockito.mock(EicrRRService.class);
//    when(mockActionRepo.getEicrRRService()).thenReturn(rrService);
//    when(rrService.getEicrById(1)).thenReturn(ecr);
//    RestApiSender restSender = PowerMockito.mock(RestApiSender.class);
//    when(mockActionRepo.getRestTransport()).thenReturn(restSender);
//    when(mockDetails.getRestAPIURL()).thenReturn("http://rest");
//    when(mockDetails.getId()).thenReturn(100);
//    when(mockDetails.getStartDate()).thenReturn(new Date());
//    Set<SubmitEicrStatus> submitStatuses = new HashSet<>();
//    when(mockState.getSubmitEicrStatus()).thenReturn(submitStatuses);
//    submitEicrAction.setActionId("ACTION-1");
//    submitEicrAction.submitEicrs(mockDetails, mockState, ids, "task-1");
//    assertEquals(1, submitStatuses.size());
//    SubmitEicrStatus status = submitStatuses.iterator().next();
//    assertEquals("ACTION-1", status.getActionId());
//    assertEquals("1", status.geteICRId());
//    assertTrue(status.getEicrSubmitted());
//    assertEquals(EventTypes.JobStatus.COMPLETED, status.getJobStatus());
//    verify(restSender, times(1)).sendEicrXmlDocument(any(), any(), any());
//  }
//
//  @Test(expected = RuntimeException.class)
//  public void testSubmitEicrs_NoTransport() {
//    Eicr ecr = PowerMockito.mock(Eicr.class);
//    when(ecr.getEicrData()).thenReturn("<xml/>");
//    when(ecr.getxCorrelationId()).thenReturn("corr-123");
//    PowerMockito.when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
//    EicrRRService rrService = PowerMockito.mock(EicrRRService.class);
//    when(mockActionRepo.getEicrRRService()).thenReturn(rrService);
//    when(rrService.getEicrById(1)).thenReturn(ecr);
//    when(mockActionRepo.getDirectTransport()).thenReturn(null);
//    when(mockActionRepo.getRestTransport()).thenReturn(null);
//    when(mockDetails.getRestAPIURL()).thenReturn(null);
//    when(mockDetails.getDirectHost()).thenReturn(null);
//    when(mockDetails.getSmtpUrl()).thenReturn(null);
//    when(mockDetails.getId()).thenReturn(10);
//    when(mockDetails.getStartDate()).thenReturn(new Date());
//    Set<SubmitEicrStatus> submitStatuses = new HashSet<>();
//    when(mockState.getSubmitEicrStatus()).thenReturn(submitStatuses);
//    submitEicrAction.setActionId("A-3");
//    assertTrue(submitStatuses.isEmpty());
//    submitEicrAction.submitEicrs(mockDetails, mockState, Set.of(1), "task-3");
//  }
//
//  @Test
//  public void testSubmitEicrs_DirectTransport() throws Exception {
//    Eicr ecr = PowerMockito.mock(Eicr.class);
//    when(ecr.getEicrData()).thenReturn("<xml/>");
//    when(ecr.getxCorrelationId()).thenReturn("corr-123");
//    PowerMockito.when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
//    EicrRRService rrService = PowerMockito.mock(EicrRRService.class);
//    when(mockActionRepo.getEicrRRService()).thenReturn(rrService);
//    when(rrService.getEicrById(1)).thenReturn(ecr);
//    DirectEicrSender directSender = PowerMockito.mock(DirectEicrSender.class);
//    when(mockActionRepo.getDirectTransport()).thenReturn(directSender);
//    when(mockActionRepo.getRestTransport()).thenReturn(null);
//    when(mockDetails.getRestAPIURL()).thenReturn(null);
//    when(mockDetails.getDirectHost()).thenReturn("direct-host");
//    when(mockDetails.getSmtpUrl()).thenReturn(null);
//    when(mockDetails.getId()).thenReturn(10);
//    when(mockDetails.getStartDate()).thenReturn(new Date());
//
//    Set<SubmitEicrStatus> submitStatuses = new HashSet<>();
//    when(mockState.getSubmitEicrStatus()).thenReturn(submitStatuses);
//
//    PowerMockito.mockStatic(WorkflowService.class);
//    PowerMockito.doNothing()
//        .when(
//            WorkflowService.class,
//            "scheduleJob",
//            anyInt(),
//            any(org.hl7.fhir.r4.model.Duration.class),
//            any(EcrActionTypes.class),
//            any(Date.class),
//            any());
//
//    submitEicrAction.setActionId("A-1");
//
//    submitEicrAction.submitEicrs(mockDetails, mockState, Set.of(1), "task-1");
//
//    verify(directSender, times(1)).sendData(mockDetails, "<xml/>", "corr-123");
//
//    assertEquals(1, submitStatuses.size());
//    SubmitEicrStatus status = submitStatuses.iterator().next();
//    assertTrue(status.getEicrSubmitted());
//    assertEquals("A-1", status.getActionId());
//    assertEquals("1", status.geteICRId());
//    assertEquals(EventTypes.JobStatus.COMPLETED, status.getJobStatus());
//  }
//
//  @Test(expected = RuntimeException.class)
//  public void testSubmitEicrs_NoTransports() throws Exception {
//    Eicr ecr = PowerMockito.mock(Eicr.class);
//    when(ecr.getEicrData()).thenReturn("<xml/>");
//    when(ecr.getxCorrelationId()).thenReturn("corr-123");
//    PowerMockito.when(ActionRepo.getInstance()).thenReturn(mockActionRepo);
//    EicrRRService rrService = PowerMockito.mock(EicrRRService.class);
//    when(mockActionRepo.getEicrRRService()).thenReturn(rrService);
//    when(rrService.getEicrById(1)).thenReturn(ecr);
//
//    when(mockActionRepo.getDirectTransport()).thenReturn(null);
//    when(mockActionRepo.getRestTransport()).thenReturn(null);
//
//    when(mockDetails.getRestAPIURL()).thenReturn(null);
//    when(mockDetails.getDirectHost()).thenReturn(null);
//    when(mockDetails.getSmtpUrl()).thenReturn(null);
//    when(mockDetails.getId()).thenReturn(10);
//    when(mockDetails.getStartDate()).thenReturn(new Date());
//
//    Set<SubmitEicrStatus> submitStatuses = new HashSet<>();
//    when(mockState.getSubmitEicrStatus()).thenReturn(submitStatuses);
//
//    submitEicrAction.setActionId("A-2");
//
//    submitEicrAction.submitEicrs(mockDetails, mockState, Set.of(1), "task-2");
//
//    assertTrue(submitStatuses.isEmpty());
//  }
// }
