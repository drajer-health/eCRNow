package com.drajer.bsa.kar.action;

import static com.drajer.bsa.kar.action.HcsReportCreator.*;
import static org.junit.Assert.*;
import static org.powermock.api.mockito.PowerMockito.*;

import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.fhirecr.FhirGeneratorConstants;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.codesystems.V3ParticipationType;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest(ReportCreationUtilities.class)
public class HcsReportCreatorTest {

  @InjectMocks HcsReportCreator reportCreator;

  @Before
  public void setUp() throws Exception {
    mockStatic(ReportCreationUtilities.class);
  }

  @Test
  public void testCreateReport() {
    EhrQueryService mockEhrService = mock(EhrQueryService.class);
    BsaAction mockAction = mock(BsaAction.class);
    String id = "bundle-hcs-001";
    String profile =
        "http://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-reporting-bundle";
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("karname");
    kar.setKarVersion("r4");
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(kar);
    Patient patient = mock(Patient.class);
    Encounter encounter = mock(Encounter.class);
    Practitioner practitioner = mock(Practitioner.class);
    Observation observation = mock(Observation.class);
    DiagnosticReport diagnosticReport = mock(DiagnosticReport.class);
    DocumentReference documentReference = mock(DocumentReference.class);
    Location location = mock(Location.class);
    Organization organization = mock(Organization.class);
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
    Set<Resource> organizations = new HashSet<>();
    patients.add(organization);
    Set<Resource> locations = new HashSet<>();
    patients.add(location);

    HashMap<ResourceType, Set<Resource>> resourceMap = new HashMap<>();
    resourceMap.put(ResourceType.Patient, patients);
    resourceMap.put(ResourceType.Encounter, encounters);
    resourceMap.put(ResourceType.Practitioner, practitioners);
    resourceMap.put(ResourceType.Observation, observations);
    resourceMap.put(ResourceType.DiagnosticReport, diagnosticReports);
    resourceMap.put(ResourceType.DocumentReference, documentReferences);
    resourceMap.put(ResourceType.Organization, organizations);
    resourceMap.put(ResourceType.Location, locations);
    karProcessingData.setFhirInputDataByType(resourceMap);

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    karProcessingData.setNotificationContext(notificationContext);
    Organization Org = new Organization();
    Org.setId("org-123");
    Org.setName("Test Organization");
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    karProcessingData.setHealthcareSetting(healthcareSetting);

    when(ReportCreationUtilities.getOrganization(karProcessingData)).thenReturn(Org);

    when(patient.getResourceType()).thenReturn(ResourceType.Patient);
    when(patient.getIdElement()).thenReturn(new IdType("Patient", "p1"));
    when(encounter.getResourceType()).thenReturn(ResourceType.Encounter);
    when(encounter.getIdElement()).thenReturn(new IdType("Encounter", "e1"));
    when(practitioner.getResourceType()).thenReturn(ResourceType.Practitioner);
    when(practitioner.getIdElement()).thenReturn(new IdType("Practitioner", "pract-1"));
    when(observation.getResourceType()).thenReturn(ResourceType.Observation);
    when(observation.getIdElement()).thenReturn(new IdType("Observation", "obs-1"));
    when(diagnosticReport.getResourceType()).thenReturn(ResourceType.DiagnosticReport);
    when(diagnosticReport.getIdElement()).thenReturn(new IdType("DiagnosticReport", "dr1"));
    when(documentReference.getResourceType()).thenReturn(ResourceType.DocumentReference);
    when(documentReference.getIdElement()).thenReturn(new IdType("DocumentReference", "doc1"));
    when(organization.getResourceType()).thenReturn(ResourceType.Organization);
    when(organization.getIdElement()).thenReturn(new IdType("Organization", "org1"));
    when(location.getResourceType()).thenReturn(ResourceType.Location);
    when(location.getIdElement()).thenReturn(new IdType("Location", "loc1"));

    Resource result =
        reportCreator.createReport(karProcessingData, mockEhrService, id, profile, mockAction);

    Bundle bundle = (Bundle) result;
    assertEquals(Bundle.BundleType.MESSAGE, bundle.getType());
    assertEquals(id, bundle.getId());

    assertNotNull("Bundle metadata should not be null", bundle.getMeta());
    assertEquals("Bundle type should be MESSAGE", Bundle.BundleType.MESSAGE, bundle.getType());
    assertNotNull("Bundle should have a timestamp", bundle.getTimestamp());

    List<Bundle.BundleEntryComponent> entries = bundle.getEntry();
    assertEquals("Bundle should contain 3 entries", 3, entries.size());

    Bundle.BundleEntryComponent headerEntry = entries.get(0);
    String expectedHeaderFullUrl =
        "http://example.com/fhir/MessageHeader/"
            + headerEntry.getResource().getIdElement().getIdPart();
    assertEquals(
        "Header fullUrl should match expected format",
        expectedHeaderFullUrl,
        headerEntry.getFullUrl());
    Bundle.BundleEntryComponent contentBundleEntry = entries.get(1);
    String expectedContentBundleFullUrl =
        "http://example.com/fhir/Bundle/"
            + contentBundleEntry.getResource().getIdElement().getIdPart();
    assertEquals(
        "Content Bundle fullUrl should match expected format",
        expectedContentBundleFullUrl,
        contentBundleEntry.getFullUrl());
    Bundle.BundleEntryComponent orgEntry = entries.get(2);
    String expectedOrgFullUrl =
        "http://example.com/fhir/Organization/" + orgEntry.getResource().getIdElement().getIdPart();
    assertEquals(
        "Sender fullUrl should match expected format", expectedOrgFullUrl, orgEntry.getFullUrl());
  }

  @Test
  public void testReferenceTo() {
    Organization org = new Organization();
    org.setId("org-123");

    Reference reference = reportCreator.referenceTo(org);

    assertNotNull("Reference should not be null", reference);
    assertEquals("Organization/org-123", reference.getReference());
  }

  @Test
  public void testCreateSender_WhenNotificationResourceIsEncounter() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("karname");
    kar.setKarVersion("r4");
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(kar);
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    karProcessingData.setNotificationContext(notificationContext);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setOrgName("General Hospital of Springfield");
    healthcareSetting.setOrgId("12345");
    healthcareSetting.setOrgIdSystem("http://example.org/org-id-system");
    karProcessingData.setHealthcareSetting(healthcareSetting);

    Organization result = reportCreator.createSender(karProcessingData);

    assertEquals(ResourceType.Organization, result.getResourceType());
    assertNotNull(result);
    assertTrue(result.hasId());
    assertEquals("General Hospital of Springfield", result.getName());
    assertTrue(result.getActive());
  }

  @Test
  public void testCreateSender_WhenNotificationResourceIsNotEncounter() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("karname");
    kar.setKarVersion("r4");
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(kar);
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    karProcessingData.setNotificationContext(notificationContext);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setOrgName("General Hospital of Springfield");
    healthcareSetting.setOrgId("12345");
    healthcareSetting.setOrgIdSystem("http://example.org/org-id-system");
    karProcessingData.setHealthcareSetting(healthcareSetting);

    Organization result = reportCreator.createSender(karProcessingData);

    assertNotNull(result);
    assertEquals(ResourceType.Organization, result.getResourceType());
    assertTrue(result.hasId());
    assertEquals(SENDER_ORG_PROFILE, result.getMeta().getProfile().get(0).getValue());
    assertEquals("General Hospital of Springfield", result.getName());
    assertTrue(result.getActive());
  }

  @Test
  public void testCreateMessageHeader() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    Set<UriType> dests = new HashSet<>();
    dests.add(new UriType("http://example.com/receiver1"));
    dests.add(new UriType("http://example.com/receiver2"));
    kar.setKarId("id");
    kar.setKarName("karname");
    kar.setKarVersion("r4");
    kar.setReceiverAddresses(dests);
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(kar);
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setTriggerEvent("event-code");
    karProcessingData.setNotificationContext(notificationContext);
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setOrgName("General Hospital of Springfield");
    healthcareSetting.setOrgId("12345");
    healthcareSetting.setOrgIdSystem("http://example.org/org-id-system");
    karProcessingData.setHealthcareSetting(healthcareSetting);

    MessageHeader header = reportCreator.createMessageHeader(karProcessingData);

    assertNotNull(header);
    assertNotNull(header.getId());
    assertTrue(header.hasMeta());

    List<Extension> extensions = header.getExtension();
    assertEquals(1, extensions.size());
    Extension ext = extensions.get(0);
    assertEquals(REPORT_INITIATION_TYPE_EXT_URL, ext.getUrl());

    assertNotNull(header.getEventCoding());
    assertEquals(MESSAGE_TYPE, header.getEventCoding().getSystem());
    assertEquals("healthcare-survey-report-message", header.getEventCoding().getCode());

    CodeableConcept reason = header.getReason();
    assertNotNull(reason);
    assertEquals("event-code", reason.getCodingFirstRep().getCode());
    assertEquals(NAMED_EVENT_URL, reason.getCodingFirstRep().getSystem());
  }

  @Test
  public void testCreateComposition() {
    KarProcessingData karProcessingData = mock(KarProcessingData.class);
    Patient patient = new Patient();
    patient.setId("patient-1");
    Set<Resource> patients = new HashSet<>();
    patients.add(patient);
    when(karProcessingData.getResourcesByType(ResourceType.Patient.toString()))
        .thenReturn(patients);
    Set<Resource> resToBeAdded = new HashSet<>();

    Encounter encounter = new Encounter();
    encounter.setId("encounter-1");
    Set<Resource> encounters = new HashSet<>();
    encounters.add(encounter);
    when(karProcessingData.getResourcesByType(ResourceType.Encounter.toString()))
        .thenReturn(encounters);

    Location location = new Location();
    location.setId("location-1");
    Set<Resource> locations = new HashSet<>();
    locations.add(location);
    when(karProcessingData.getResourcesByType(ResourceType.Location)).thenReturn(locations);

    Organization organization = new Organization();
    organization.setId("org-1");
    Set<Resource> organizations = new HashSet<>();
    organizations.add(organization);
    when(karProcessingData.getResourcesByType(ResourceType.Organization)).thenReturn(organizations);

    Practitioner practitioner = new Practitioner();
    practitioner.setId("practitioner-1");
    Set<Resource> practitioners = new HashSet<>();
    practitioners.add(practitioner);
    when(karProcessingData.getResourcesByType(ResourceType.Practitioner)).thenReturn(practitioners);
    when(ReportCreationUtilities.getOrganization(karProcessingData)).thenReturn(organization);

    Composition result = reportCreator.createComposition(karProcessingData, resToBeAdded);

    assertNotNull(result);
    assertEquals(Composition.CompositionStatus.FINAL, result.getStatus());
    assertNotNull(result.getCustodian());
    assertNotNull(result.getCustodian().getResource());
    assertNotNull(result.getIdentifier());
    assertTrue(result.hasSection());
    assertNotNull(result.getType());
    assertNotNull(result.getMeta());
    assertTrue(resToBeAdded.contains(patient));
    assertTrue(resToBeAdded.contains(encounter));
    assertTrue(resToBeAdded.contains(location));
    assertTrue(resToBeAdded.contains(organization));
    assertTrue(resToBeAdded.contains(practitioner));
  }

  @Test
  public void testCreateContentBundle() {
    String id = "bundle-hcs-001";
    String profile =
        "http://hl7.org/fhir/us/health-care-surveys-reporting/StructureDefinition/hcs-reporting-bundle";
    EhrQueryService ehrService = mock(EhrQueryService.class);
    KarProcessingData karProcessingData = new KarProcessingData();
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    karProcessingData.setNotificationContext(notificationContext);

    Patient patient = new Patient();
    patient.setId("p1");

    Encounter encounter = new Encounter();
    encounter.setId("pr1");

    Set<Resource> patients = new HashSet<>();
    patients.add(patient);
    Set<Resource> encounters = new HashSet<>();
    encounters.add(encounter);
    HashMap<ResourceType, Set<Resource>> fhirData = new HashMap<>();
    fhirData.put(ResourceType.Patient, patients);
    fhirData.put(ResourceType.Encounter, encounters);

    karProcessingData.setFhirInputDataByType(fhirData);

    Bundle bundle = reportCreator.createContentBundle(karProcessingData, ehrService, id, profile);

    assertNotNull(bundle);
    assertEquals(Bundle.BundleType.COLLECTION, bundle.getType());

    Bundle.BundleEntryComponent entry = bundle.getEntry().get(0);
    assertNotNull(entry.getFullUrl());

    Resource res = entry.getResource();
    assertNotNull(entry.getFullUrl());
    assertNotNull(res);
    String expectedFullUrl =
        "http://example.com/fhir/"
            + res.getResourceType().name()
            + "/"
            + res.getIdElement().getIdPart();
    assertEquals(expectedFullUrl, entry.getFullUrl());
  }

  @Test
  public void testAddAuthors() {
    KarProcessingData mockKd = mock(KarProcessingData.class);
    Composition composition = new Composition();

    Practitioner practitioner = new Practitioner();
    practitioner.setId("practitioner-123");
    List<Practitioner> practitionerList = new ArrayList<>();
    practitionerList.add(practitioner);

    when(ReportCreationUtilities.getPractitioners(mockKd, V3ParticipationType.AUT))
        .thenReturn(practitionerList);

    List<Practitioner> result = reportCreator.addAuthors(mockKd, composition);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals("practitioner-123", result.get(0).getId());

    List<Reference> authors = composition.getAuthor();
    assertNotNull(authors);
    assertEquals(1, authors.size());
    assertNotNull(authors.get(0).getResource());
  }

  @Test
  public void testGetDeviceAuthor() {

    Device device = reportCreator.getDeviceAuthor();

    assertNotNull(device);
    List<Device.DeviceDeviceNameComponent> deviceNames = device.getDeviceName();
    assertNotNull("Device name list should not be null", deviceNames);
    assertEquals(1, deviceNames.size());
    Device.DeviceDeviceNameComponent nameComponent = deviceNames.get(0);
    assertNotNull(nameComponent.getName(), "Device name should not be null");
    assertEquals("eCRNow/Backend Service App", nameComponent.getName());
  }

  @Test
  public void testResourceHasMatchedCode_WithCondition() {
    Condition condition = new Condition();
    CodeableConcept code = new CodeableConcept().addCoding(new Coding().setCode("code123"));
    condition.setCode(code);

    ReportableMatchedTriggerCode expectedMatch = new ReportableMatchedTriggerCode();
    expectedMatch.setCode("code123");

    CheckTriggerCodeStatus mockCtcs = mock(CheckTriggerCodeStatus.class);
    when(mockCtcs.getMatchedCode(code)).thenReturn(new Pair<>(true, expectedMatch));

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        reportCreator.resourceHasMatchedCode(condition, mockCtcs);

    assertNotNull(result);
  }

  @Test
  public void testRemoveExtensions_DomainResource() {
    Patient patient = new Patient();
    patient.addExtension(
        new Extension(
            "http://hl7.org/fhir/us/core/StructureDefinition/ext1", new StringType("value1")));
    patient.addExtension(
        new Extension(
            "http://custom.org/StructureDefinition/custom-ext", new StringType("value2")));

    assertEquals(2, patient.getExtension().size());
    reportCreator.removeExtensions(patient);

    List<Extension> filtered = patient.getExtension();
    assertEquals(1, filtered.size());
    assertTrue(filtered.get(0).getUrl().contains("us/core"));
  }

  @Test
  public void testRemoveExtensions_ObservationPerformerExtension() {
    Observation observation = new Observation();
    Reference performer = new Reference("Practitioner/123");

    performer.addExtension(
        new Extension(
            "http://hl7.org/fhir/StructureDefinition/event-performerFunction",
            new StringType("function")));
    performer.addExtension(
        new Extension(
            "http://example.com/fhir/StructureDefinition/extra", new StringType("extra")));
    observation.addPerformer(performer);

    assertEquals(2, observation.getPerformerFirstRep().getExtension().size());

    reportCreator.removeExtensions(observation);

    List<Extension> filtered = observation.getPerformerFirstRep().getExtension();
    assertEquals(2, filtered.size());
  }

  @Test
  public void testIsVitalsSection_ValidSection() {
    Composition.SectionComponent section = new Composition.SectionComponent();
    Coding coding = new Coding();
    coding.setSystem(FhirGeneratorConstants.LOINC_CS_URL);
    coding.setCode(FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE);

    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = reportCreator.isVitalsSection(section);

    assertTrue(result);
  }

  @Test
  public void testIsSocialHistorySection_ValidSection() {
    Composition.SectionComponent section = new Composition.SectionComponent();
    Coding coding = new Coding();
    coding.setSystem(FhirGeneratorConstants.LOINC_CS_URL);
    coding.setCode(FhirGeneratorConstants.SOCIAL_HISTORY_SECTION_LOINC_CODE);

    CodeableConcept concept = new CodeableConcept();
    concept.addCoding(coding);
    section.setCode(concept);

    Boolean result = reportCreator.isSocialHistorySection(section);

    assertTrue(result);
  }
}
