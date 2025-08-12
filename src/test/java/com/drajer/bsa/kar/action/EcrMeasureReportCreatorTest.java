package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.powermock.api.mockito.PowerMockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.utils.R3ToR2DataConverterUtils;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.model.R4FhirData;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({R3ToR2DataConverterUtils.class})
public class EcrMeasureReportCreatorTest {

  @InjectMocks EcrMeasureReportCreator ecrMeasureReportCreator;

  @Before
  public void setUp() {
    PowerMockito.mockStatic(R3ToR2DataConverterUtils.class);
    ecrMeasureReportCreator = spy(new EcrMeasureReportCreator());
  }

  @Test
  public void testCreateReport_addsMeasureReportsToBundle() {
    String id = "test-id";
    String profile = "test-profile";
    MeasureReport report1 = new MeasureReport();
    report1.setId("mr1");
    MeasureReport report2 = new MeasureReport();
    report2.setId("mr2");
    Set<Resource> resources = new HashSet<>();
    resources.add(report1);
    resources.add(report2);
    KnowledgeArtifact kar = mock(KnowledgeArtifact.class);
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    KnowledgeArtifactStatus artifactStatus = new KnowledgeArtifactStatus();
    artifactStatus.setOutputFormat(BsaTypes.OutputContentType.FHIR);
    KarProcessingData data = spy(new KarProcessingData());
    data.setKar(kar);
    data.setKarStatus(artifactStatus);
    data.setNotificationContext(notificationContext);
    HashMap<String, Set<Resource>> outputDataMap = new HashMap<>();
    outputDataMap.put("mr1", new HashSet<>(resources));
    outputDataMap.put("mr2", new HashSet<>(resources));
    data.setActionOutputDataById(outputDataMap);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    data.setHealthcareSetting(healthcareSetting);
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaAction act = mock(BsaAction.class);

    R4FhirData r4FhirData = mock(R4FhirData.class);
    LaunchDetails launchDetails = mock(LaunchDetails.class);
    Pair<R4FhirData, LaunchDetails> mockPair = new Pair<>(r4FhirData, launchDetails);
    Set<Resource> resourcesTobeAdded = new HashSet<>();
    Patient patient1 = new Patient();
    patient1.setId(new IdType("Patient", "patient-001"));
    patient1.addName().setFamily("Doe").addGiven("John");
    resourcesTobeAdded.add(patient1);
    Patient patient2 = new Patient();
    patient2.setId(new IdType("Patient", "patient-002"));
    patient2.addName().setFamily("Smith").addGiven("Jane");
    resourcesTobeAdded.add(patient2);
    when(R3ToR2DataConverterUtils.convertKarProcessingDataForCdaGeneration(
            any(KarProcessingData.class), any(BsaAction.class)))
        .thenReturn(mockPair);

    List<String> listid = new ArrayList<>();
    listid.add("mr1");
    listid.add("mr2");
    Composition mockComp = new Composition();
    mockComp.setId(new IdType("Composition", "comp-1"));
    Set<Resource> mockResourcesToAdd = new HashSet<>();
    Patient mockPatient = new Patient();
    mockPatient.setId("patient-1");
    mockResourcesToAdd.add(mockPatient);
    Set<Resource> mockEncounterResourcesToAdd = new HashSet<>();
    Encounter mockEncounter = new Encounter();
    mockEncounter.setId("Encounter-1");
    mockEncounterResourcesToAdd.add(mockEncounter);
    Set<Resource> mockLocationResourcesToAdd = new HashSet<>();
    Location mockLocation = new Location();
    mockLocation.setId("Location-1");
    mockLocationResourcesToAdd.add(mockLocation);
    Organization org = new Organization();
    org.setId("org-1");

    when(data.getResourcesByType(ResourceType.Patient.toString())).thenReturn(mockResourcesToAdd);
    when(data.getResourcesByType(ResourceType.Encounter.toString()))
        .thenReturn(mockEncounterResourcesToAdd);
    when(data.getResourcesByType(ResourceType.Location)).thenReturn(mockLocationResourcesToAdd);
    when(kar.getOuputVariableIdsForResourceType(ResourceType.MeasureReport)).thenReturn(listid);
    doReturn(mockComp)
        .when(ecrMeasureReportCreator)
        .createComposition(data, resourcesTobeAdded, mockPair);
    when(data.getOutputDataById(any())).thenReturn(resources);

    Resource result = ecrMeasureReportCreator.createReport(data, ehrService, id, profile, act);

    assertTrue(result instanceof Bundle);
    Bundle resultBundle = (Bundle) result;
    List<Bundle.BundleEntryComponent> entries = resultBundle.getEntry();

    assertNotNull(result);
    assertFalse(entries.isEmpty());
    assertTrue(entries.stream().anyMatch(e -> e.getResource().getId().equals("mr1")));
    assertTrue(entries.stream().anyMatch(e -> e.getResource().getId().equals("mr2")));
  }
}
