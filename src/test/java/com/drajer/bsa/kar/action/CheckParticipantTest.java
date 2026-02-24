package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.model.BsaTypes.BsaActionStatusType;
import com.drajer.bsa.model.KarProcessingData;
import java.util.*;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.junit.Before;
import org.junit.Test;

public class CheckParticipantTest {

  private EhrQueryService ehrService;
  private KarProcessingData data;
  private CheckParticipant action;

  @Before
  public void setUp() {
    ehrService = mock(EhrQueryService.class);
    data = new KarProcessingData();
    data.setExecutionSequenceId("exec-1");
    action = new CheckParticipant();
    action.setActionId("action-1");
  }

  @Test
  public void testProcess_ActionCompletesSuccessfully() {
    Patient patient = new Patient();
    patient.setId("patient-real-1");
    Set<Resource> patientSet = new HashSet<>();
    patientSet.add(patient);
    Map<ResourceType, Set<Resource>> fetchedResources = new HashMap<>();
    fetchedResources.put(ResourceType.Patient, patientSet);
    when(ehrService.getFilteredData(data, action.getInputData())).thenReturn(fetchedResources);
    BsaActionStatus status = action.process(data, ehrService);
    assertNotNull(status);
    assertEquals(BsaActionStatusType.COMPLETED, status.getActionStatus());
    assertEquals("action-1", status.getActionId());
  }

  @Test
  public void testProcess_ActionScheduled() {
    CheckParticipant scheduledAction =
        new CheckParticipant() {
          @Override
          public BsaActionStatusType processTimingData(KarProcessingData data) {
            return BsaActionStatusType.SCHEDULED;
          }
        };
    scheduledAction.setActionId("action-2");
    DataRequirement dr = new DataRequirement();
    dr.setType("Patient");
    List<DataRequirement> inputRequirements = new ArrayList<>();
    inputRequirements.add(dr);
    scheduledAction.setInputData(inputRequirements);
    BsaActionStatus status = scheduledAction.process(data, ehrService);
    assertNotNull(status);
    assertEquals(BsaActionStatusType.SCHEDULED, status.getActionStatus());
    assertEquals("action-2", status.getActionId());
  }
}
