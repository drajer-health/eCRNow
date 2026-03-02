package com.drajer.eca.model;

import static com.helger.commons.mock.CommonsAssert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import com.drajer.eca.model.EventTypes.JobStatus;
import java.util.HashSet;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class PatientExecutionStateTest {

  @InjectMocks PatientExecutionState patientExecutionState;
  @Mock CreateEicrStatus createEicrStatus;
  @Mock CloseOutEicrStatus closeOutEicrStatus;
  @Mock PeriodicUpdateEicrStatus periodicUpdateEicrStatus;
  private PatientExecutionState state;

  @Mock RRStatus rrStatus1;

  @Mock RRStatus rrStatus2;

  @Mock SubmitEicrStatus submitEicr1;

  @Mock SubmitEicrStatus submitEicr2;

  @Before
  public void setup() {
    MockitoAnnotations.initMocks(this);
    populatePatientExecution();

    state = new PatientExecutionState();
    Set<RRStatus> rrSet = new HashSet<>();
    rrSet.add(rrStatus1);
    rrSet.add(rrStatus2);
    patientExecutionState.setRrStatus(rrSet);

    Set<SubmitEicrStatus> submitSet = new HashSet<>();
    submitSet.add(submitEicr1);
    submitSet.add(submitEicr2);
    patientExecutionState.setSubmitEicrStatus(submitSet);
  }

  @Test
  public void hasEicrBeenCreatedTest() throws Exception {
    // This will test that Eicr has been created via Create Eicr Timer
    when(createEicrStatus.getEicrCreated()).thenReturn(true);
    when(createEicrStatus.getJobStatus())
        .thenReturn(JobStatus.COMPLETED)
        .thenReturn(JobStatus.NOT_STARTED);
    Boolean response = patientExecutionState.hasEicrBeenCreated();
    assertTrue(response);

    // This will assert Eicr has been creatd via Close Out Eicr Timer
    when(closeOutEicrStatus.getEicrClosed()).thenReturn(true);
    when(closeOutEicrStatus.getJobStatus())
        .thenReturn(JobStatus.COMPLETED)
        .thenReturn(JobStatus.NOT_STARTED);
    response = patientExecutionState.hasEicrBeenCreated();
    assertTrue(response);

    // This will assert that a Eicr created via Periodic timer
    when(periodicUpdateEicrStatus.getEicrUpdated()).thenReturn(true);
    when(periodicUpdateEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    response = patientExecutionState.hasEicrBeenCreated();
    assertTrue(response);
  }

  @Test
  public void hasEicrBeenCreatedWithCompletedFalseTest() throws Exception {
    // This will test that with eicrCreatedStatus completed and eicrCreated false
    when(createEicrStatus.getEicrCreated()).thenReturn(false);
    when(createEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    Boolean response = patientExecutionState.hasEicrBeenCreated();
    assertFalse(response);
  }

  private void populatePatientExecution() {
    patientExecutionState.setCreateEicrStatus(createEicrStatus);
    patientExecutionState.setCloseOutEicrStatus(closeOutEicrStatus);
    Set<PeriodicUpdateEicrStatus> patientUpdateStateSet =
        patientExecutionState.getPeriodicUpdateStatus();
    patientUpdateStateSet.add(periodicUpdateEicrStatus);
    patientExecutionState.setPeriodicUpdateStatus(patientUpdateStateSet);
  }

  @Test
  public void testGetEicrsForRRCheck() {
    when(rrStatus1.geteICRId()).thenReturn("1");
    when(rrStatus2.geteICRId()).thenReturn("2");
    when(submitEicr1.geteICRId()).thenReturn("1");
    when(submitEicr1.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    when(submitEicr2.geteICRId()).thenReturn("3");
    when(submitEicr2.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    Set<Integer> result = patientExecutionState.getEicrsForRRCheck();
    Set<Integer> expected = new HashSet<>();
    expected.add(3);
    assertEquals(
        "getEicrsForRRCheck should return only submit EICR IDs not in RRStatus", expected, result);
  }

  @Test
  public void testGetEicrsReadyForSubmission() {

    ValidateEicrStatus val1 = Mockito.mock(ValidateEicrStatus.class);
    ValidateEicrStatus val2 = Mockito.mock(ValidateEicrStatus.class);
    when(val1.geteICRId()).thenReturn("1");
    when(val2.geteICRId()).thenReturn("2");

    when(val1.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    when(val2.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    Set<ValidateEicrStatus> validateSet = new HashSet<>();
    validateSet.add(val1);
    validateSet.add(val2);
    patientExecutionState.setValidateEicrStatus(validateSet);

    SubmitEicrStatus submit1 = Mockito.mock(SubmitEicrStatus.class);
    when(submit1.geteICRId()).thenReturn("1"); // already submitted
    when(submit1.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    Set<SubmitEicrStatus> submitSet = new HashSet<>();
    submitSet.add(submit1);
    patientExecutionState.setSubmitEicrStatus(submitSet);
    Set<Integer> result = patientExecutionState.getEicrsReadyForSubmission();

    assertTrue(result.contains(2));
    assertFalse(result.contains(1));
    assertEquals(1, result.size());
  }

  @Test
  public void testGetEicrsReadyForValidation() {
    ValidateEicrStatus val1 = Mockito.mock(ValidateEicrStatus.class);
    when(val1.geteICRId()).thenReturn("1");
    when(val1.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    Set<ValidateEicrStatus> validateSet = new HashSet<>();
    validateSet.add(val1);
    patientExecutionState.setValidateEicrStatus(validateSet);
    when(createEicrStatus.geteICRId()).thenReturn("2");
    when(createEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    when(closeOutEicrStatus.geteICRId()).thenReturn("3");
    when(closeOutEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    when(periodicUpdateEicrStatus.geteICRId()).thenReturn("4");
    when(periodicUpdateEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    Set<PeriodicUpdateEicrStatus> periodicSet = new HashSet<>();
    periodicSet.add(periodicUpdateEicrStatus);
    patientExecutionState.setPeriodicUpdateStatus(periodicSet);
    Set<Integer> result = patientExecutionState.getEicrsReadyForValidation();

    assertTrue(result.contains(2));
    assertTrue(result.contains(3));
    assertTrue(result.contains(4));
    assertFalse(result.contains(1));
    assertEquals(3, result.size());
  }

  @Test
  public void testGetEicrIdForCompletedActions() {
    when(createEicrStatus.getActionId()).thenReturn("action-create");
    when(createEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    when(createEicrStatus.geteICRId()).thenReturn("101");
    when(closeOutEicrStatus.getActionId()).thenReturn("action-close");
    when(closeOutEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    when(closeOutEicrStatus.geteICRId()).thenReturn("102");
    when(periodicUpdateEicrStatus.getActionId()).thenReturn("action-periodic");
    when(periodicUpdateEicrStatus.getJobStatus()).thenReturn(JobStatus.COMPLETED);
    when(periodicUpdateEicrStatus.geteICRId()).thenReturn("103");
    Set<PeriodicUpdateEicrStatus> periodicSet = new HashSet<>();
    periodicSet.add(periodicUpdateEicrStatus);
    patientExecutionState.setPeriodicUpdateStatus(periodicSet);
    Set<Integer> result = patientExecutionState.getEicrIdForCompletedActions("action-create");
    assertTrue(result.contains(101));
    assertEquals(1, result.size());

    result = patientExecutionState.getEicrIdForCompletedActions("action-close");
    assertTrue(result.contains(102));
    assertEquals(1, result.size());

    result = patientExecutionState.getEicrIdForCompletedActions("action-periodic");
    assertTrue(result.contains(103));
    assertEquals(1, result.size());

    result = patientExecutionState.getEicrIdForCompletedActions("unknown-action");
    assertTrue(result.isEmpty());
  }

  @Test
  public void testHasActionCompleted_matchTriggerStatus() {
    state.getMatchTriggerStatus().setActionId("match");
    state.getMatchTriggerStatus().setJobStatus(JobStatus.COMPLETED);

    assertTrue(state.hasActionCompleted("match"));
  }

  @Test
  public void testHasActionCompleted_createEicrStatus() {
    state.getCreateEicrStatus().setActionId("create");
    state.getCreateEicrStatus().setJobStatus(JobStatus.COMPLETED);

    assertTrue(state.hasActionCompleted("create"));
  }

  @Test
  public void testHasActionCompleted_periodicUpdateStatus() {
    state.getCreateEicrStatus().setActionId("create");
    state.getCreateEicrStatus().setJobStatus(JobStatus.NOT_STARTED);

    PeriodicUpdateEicrStatus pd = new PeriodicUpdateEicrStatus();
    pd.setActionId("create");
    pd.setJobStatus(JobStatus.COMPLETED);
    pd.seteICRId("101");
    state.getPeriodicUpdateStatus().add(pd);

    assertTrue(state.hasActionCompleted("create"));
  }

  @Test
  public void testHasActionCompleted_closeOutEicrStatus() {
    state.getCloseOutEicrStatus().setActionId("close");
    state.getCloseOutEicrStatus().setJobStatus(JobStatus.COMPLETED);

    assertTrue(state.hasActionCompleted("close"));
  }

  @Test
  public void testHasActionCompleted_validateEicrStatus() {
    ValidateEicrStatus val = new ValidateEicrStatus();
    val.setActionId("validate");
    val.setJobStatus(JobStatus.COMPLETED);
    state.getValidateEicrStatus().add(val);

    assertTrue(state.hasActionCompleted("validate"));
  }

  @Test
  public void testHasActionCompleted_submitEicrStatus() {
    SubmitEicrStatus submit = new SubmitEicrStatus();
    submit.setActionId("submit");
    submit.setJobStatus(JobStatus.COMPLETED);
    state.getSubmitEicrStatus().add(submit);

    assertTrue(state.hasActionCompleted("submit"));
  }

  @Test
  public void testHasActionCompleted_unknownAction() {
    assertFalse(state.hasActionCompleted("unknown"));
  }
}
