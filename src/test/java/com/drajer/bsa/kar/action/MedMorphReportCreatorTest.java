package com.drajer.bsa.kar.action;

import static com.drajer.bsa.kar.action.MedMorphReportCreator.*;
import static org.junit.Assert.*;
import static org.powermock.api.mockito.PowerMockito.mock;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.utils.BsaServiceUtils;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest({BsaServiceUtils.class})
public class MedMorphReportCreatorTest {

  @InjectMocks MedMorphReportCreator medMorphReportCreator;

  @Test
  public void testCreateReport_shouldReturnValidBundleWithHeaderAndContent() {
    String bundleId = UUID.randomUUID().toString();
    String profile = "http://example.org/fhir/StructureDefinition/eicr-message";
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("karname");
    kar.setKarVersion("r4");
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://fhirserver.org/fhir/Patient/001"));
    receiverAddresses.add(new UriType("http://fhirserver.org/fhir/Encounter/02E"));
    kar.setReceiverAddresses(receiverAddresses);
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(kar);
    Patient patient = new Patient();
    patient.setId("001");
    Encounter encounter = new Encounter();
    encounter.setId("002E");
    Practitioner practitioner = new Practitioner();
    practitioner.setId("003P");
    Observation observation = new Observation();
    observation.setId("004O");
    DiagnosticReport diagnosticReport = new DiagnosticReport();
    diagnosticReport.setId("005D");
    DocumentReference documentReference = new DocumentReference();
    documentReference.setId("006DR");
    Set<Resource> patients = new HashSet<>();
    patients.add(patient);
    Set<Resource> encounters = new HashSet<>();
    encounters.add(encounter);
    Set<Resource> practitioners = new HashSet<>();
    practitioners.add(practitioner);
    Set<Resource> observations = new HashSet<>();
    observations.add(observation);
    Set<Resource> diagnosticReports = new HashSet<>();
    diagnosticReports.add(diagnosticReport);
    Set<Resource> documentReferences = new HashSet<>();
    documentReferences.add(documentReference);
    HealthcareSetting setting = new HealthcareSetting();
    setting.setOrgName("Test Org");
    setting.setOrgId("org-123");
    setting.setOrgIdSystem("http://org-system");
    setting.setFhirServerBaseURL("http://fhir-server.com");
    EhrQueryService ehrService = mock(EhrQueryService.class);
    BsaAction action = mock(BsaAction.class);
    HashMap<ResourceType, Set<Resource>> resourceMap = new HashMap<>();
    resourceMap.put(ResourceType.Patient, patients);
    resourceMap.put(ResourceType.Encounter, encounters);
    resourceMap.put(ResourceType.Practitioner, practitioners);
    resourceMap.put(ResourceType.Observation, observations);
    resourceMap.put(ResourceType.DiagnosticReport, diagnosticReports);
    resourceMap.put(ResourceType.DocumentReference, documentReferences);
    karProcessingData.setFhirInputDataByType(resourceMap);
    karProcessingData.setHealthcareSetting(setting);
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    notificationContext.setTriggerEvent("encounter-update");
    karProcessingData.setNotificationContext(notificationContext);

    Resource result =
        medMorphReportCreator.createReport(
            karProcessingData, ehrService, bundleId, profile, action);

    assertNotNull(result);
    assertTrue(result instanceof Bundle);
    Bundle bundle = (Bundle) result;

    assertEquals(Bundle.BundleType.MESSAGE, bundle.getType());
    assertEquals(bundleId, bundle.getId());

    assertEquals(2, bundle.getEntry().size());
    assertEquals("Bundle type should be MESSAGE", Bundle.BundleType.MESSAGE, bundle.getType());
    assertNotNull("Bundle should have a timestamp", bundle.getTimestamp());
  }

  @Test
  public void createMessageHeader_shouldPopulateFieldsCorrectly() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("karname");
    kar.setKarVersion("r4");
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://fhirserver.org/fhir/Patient/001"));
    kar.setReceiverAddresses(receiverAddresses);
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(kar);
    Patient patient = new Patient();
    patient.setId("001");
    Encounter encounter = new Encounter();
    encounter.setId("002E");
    Practitioner practitioner = new Practitioner();
    practitioner.setId("003P");
    Observation observation = new Observation();
    observation.setId("004O");
    DiagnosticReport diagnosticReport = new DiagnosticReport();
    diagnosticReport.setId("005D");
    DocumentReference documentReference = new DocumentReference();
    documentReference.setId("006DR");
    Set<Resource> patients = new HashSet<>();
    patients.add(patient);
    Set<Resource> encounters = new HashSet<>();
    encounters.add(encounter);
    Set<Resource> practitioners = new HashSet<>();
    practitioners.add(practitioner);
    Set<Resource> observations = new HashSet<>();
    observations.add(observation);
    Set<Resource> diagnosticReports = new HashSet<>();
    diagnosticReports.add(diagnosticReport);
    Set<Resource> documentReferences = new HashSet<>();
    documentReferences.add(documentReference);
    HealthcareSetting setting = new HealthcareSetting();
    setting.setOrgName("Test Org");
    setting.setOrgId("org-123");
    setting.setOrgIdSystem("http://org-system");
    setting.setFhirServerBaseURL("http://fhir-server.com");
    HashMap<ResourceType, Set<Resource>> resourceMap = new HashMap<>();
    resourceMap.put(ResourceType.Patient, patients);
    resourceMap.put(ResourceType.Encounter, encounters);
    resourceMap.put(ResourceType.Practitioner, practitioners);
    resourceMap.put(ResourceType.Observation, observations);
    resourceMap.put(ResourceType.DiagnosticReport, diagnosticReports);
    resourceMap.put(ResourceType.DocumentReference, documentReferences);
    karProcessingData.setFhirInputDataByType(resourceMap);
    karProcessingData.setHealthcareSetting(setting);
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    notificationContext.setTriggerEvent("encounter-update");
    karProcessingData.setNotificationContext(notificationContext);

    MessageHeader header = medMorphReportCreator.createMessageHeader(karProcessingData);

    assertNotNull(header.getId());
    assertNotNull(header.getMeta());
    assertEquals(MESSAGE_HEADER_PROFILE, header.getMeta().getProfile().get(0).getValue());

    assertEquals(1, header.getExtension().size());
    Extension ext = header.getExtension().get(0);
    assertEquals(MESSAGE_PROCESSING_CATEGORY_EXT_URL, ext.getUrl());
    assertTrue(ext.getValue() instanceof StringType);
    assertEquals(MESSAGE_PROCESSING_CATEGORY_CODE, ((StringType) ext.getValue()).getValue());

    assertEquals(1, header.getDestination().size());
    assertEquals(
        "http://fhirserver.org/fhir/Patient/001", header.getDestination().get(0).getEndpoint());

    assertEquals("http://fhir-server.com", header.getSource().getEndpoint());

    Coding event = (Coding) header.getEvent();
    assertEquals(MESSAGE_TYPE, event.getSystem());
    assertEquals("healthcare-survey-report-message", event.getCode());

    CodeableConcept reason = header.getReason();
    assertNotNull(reason);
    assertEquals("encounter-update", reason.getCodingFirstRep().getCode());
    assertEquals(NAMED_EVENT_URL, reason.getCodingFirstRep().getSystem());
  }

  @Test
  public void createContentBundle_shouldGenerateValidBundleEntries() {
    String FHIR_BASE = "http://example.org/fhir";
    Observation obs1 = new Observation();
    obs1.setId("Observation/obs-001");
    Condition cond1 = new Condition();
    cond1.setId("cond-002");
    Patient patient = new Patient();
    patient.setId("http://external.org/fhir/Patient/pat-003");
    Set<Resource> inputData = new HashSet<>();
    inputData.add(obs1);
    inputData.add(cond1);
    inputData.add(patient);

    Bundle bundle = medMorphReportCreator.createContentBundle(inputData, FHIR_BASE);

    assertNotNull(bundle.getId());
    assertEquals(Bundle.BundleType.COLLECTION, bundle.getType());
    assertNotNull(bundle.getMeta());
    assertNotNull(bundle.getTimestamp());

    List<Bundle.BundleEntryComponent> entries = bundle.getEntry();
    assertEquals(3, entries.size());
  }
}
