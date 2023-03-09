package com.drajer.eca.model;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import com.drajer.eca.model.EventTypes.JobStatus;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PatientExecutionStateTest {

  @InjectMocks PatientExecutionState patientExecutionState;
  @Mock CreateEicrStatus createEicrStatus;
  @Mock CloseOutEicrStatus closeOutEicrStatus;
  @Mock PeriodicUpdateEicrStatus periodicUpdateEicrStatus;

  @Before
  public void setup() {
    MockitoAnnotations.initMocks(this);
    populatePatientExecution();
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
}
