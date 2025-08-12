package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;
import static org.mockito.Mockito.verify;
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
import org.hl7.fhir.r4.model.Composition.SectionComponent;
import org.hl7.fhir.r4.model.Narrative;
import org.hl7.fhir.r4.model.Narrative.NarrativeStatus;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
public class CcrrReportCreatorTest {

  @InjectMocks CcrrReportCreator ccrrReportCreator;

  EhrQueryService mockEhrService;
  BsaAction mockAction;
  private static final String DEVICE_NAME = "eCRNow/Backend Service App";

  @Before
  public void setUp() throws Exception {
    mockEhrService = mock(EhrQueryService.class);
    mockAction = mock(BsaAction.class);
  }

  @Test
  public void testCreateReport() {
    EhrQueryService mockEhrService = mock(EhrQueryService.class);
    BsaAction mockAction = mock(BsaAction.class);
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

    HashMap<ResourceType, Set<Resource>> resourceMap = new HashMap<>();
    resourceMap.put(ResourceType.Patient, patients);
    resourceMap.put(ResourceType.Encounter, encounters);
    resourceMap.put(ResourceType.Practitioner, practitioners);
    resourceMap.put(ResourceType.Observation, observations);
    resourceMap.put(ResourceType.DiagnosticReport, diagnosticReports);
    resourceMap.put(ResourceType.DocumentReference, documentReferences);
    karProcessingData.setFhirInputDataByType(resourceMap);

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    notificationContext.setNotificationResourceType("Encounter");
    karProcessingData.setNotificationContext(notificationContext);

    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    karProcessingData.setHealthcareSetting(healthcareSetting);
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

    Resource result =
        ccrrReportCreator.createReport(
            karProcessingData, mockEhrService, "123", "someProfile", mockAction);

    assertNotNull(result);
    assertTrue(result instanceof Bundle);
    Bundle bundle = (Bundle) result;
    assertEquals(ResourceType.Bundle, bundle.getResourceType());
    assertEquals(
        "Expected 3 entries: MessageHeader, ContentBundle, and Sender Organization",
        3,
        bundle.getEntry().size());
    assertNotNull("Bundle metadata should not be null", bundle.getMeta());
    assertEquals("Bundle type should be MESSAGE", Bundle.BundleType.MESSAGE, bundle.getType());
    assertNotNull("Bundle should have a timestamp", bundle.getTimestamp());
  }

  @Test
  public void testCreateReports_WhenNotificationResourceTypeIsNotEncounter() {
    EhrQueryService mockEhrService = mock(EhrQueryService.class);
    BsaAction mockAction = mock(BsaAction.class);
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://receiver1.example.com"));
    receiverAddresses.add(new UriType("http://receiver2.example.com"));
    kar.setReceiverAddresses(receiverAddresses);
    MessageHeader messageHeader = new MessageHeader();
    messageHeader.setId("mh-1");
    messageHeader.setEvent(
        new Coding().setSystem("http://example.com/event").setCode("example-event"));
    messageHeader.getSource().setEndpoint("http://example.com/fhir");

    Reference ref = new Reference("Bundle/content-bundle-id");
    messageHeader.addFocus(ref);
    KarProcessingData karProcessingData = new KarProcessingData();
    karProcessingData.setKar(kar);

    Patient patient = mock(Patient.class);
    Encounter encounter = mock(Encounter.class);
    Practitioner practitioner = mock(Practitioner.class);
    Observation observation = mock(Observation.class);
    DiagnosticReport diagnosticReport = mock(DiagnosticReport.class);
    DocumentReference documentReference = mock(DocumentReference.class);
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

    HashMap<ResourceType, Set<Resource>> resourceMap = new HashMap<>();
    resourceMap.put(ResourceType.Patient, patients);
    resourceMap.put(ResourceType.Encounter, encounters);
    resourceMap.put(ResourceType.Practitioner, practitioners);
    resourceMap.put(ResourceType.Observation, observations);
    resourceMap.put(ResourceType.DiagnosticReport, diagnosticReports);
    resourceMap.put(ResourceType.DocumentReference, documentReferences);
    karProcessingData.setFhirInputDataByType(resourceMap);

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    karProcessingData.setNotificationContext(notificationContext);

    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    healthcareSetting.setId(55);
    karProcessingData.setHealthcareSetting(healthcareSetting);

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

    Resource result =
        ccrrReportCreator.createReport(
            karProcessingData, mockEhrService, "123", "someProfile", mockAction);

    assertNotNull(result);
    Bundle bundle = (Bundle) result;
    assertEquals(ResourceType.Bundle, bundle.getResourceType());
    assertEquals(
        "Expected 3 entries: MessageHeader, ContentBundle, and Sender Organization",
        3,
        bundle.getEntry().size());
    assertNotNull("Bundle metadata should not be null", bundle.getMeta());
    assertEquals("Bundle type should be MESSAGE", Bundle.BundleType.MESSAGE, bundle.getType());
    assertNotNull("Bundle should have a timestamp", bundle.getTimestamp());
  }

  @Test
  public void testReferenceTo() {
    Resource mockResource = mock(Resource.class);

    when(mockResource.fhirType()).thenReturn("Patient");
    when(mockResource.getId()).thenReturn("123");

    Reference result = ccrrReportCreator.referenceTo(mockResource);

    assertNotNull(result);

    assertEquals("Patient/123", result.getReference());
  }

  @Test
  public void testGetDeviceAuthors() {
    Device device = ccrrReportCreator.getDeviceAuthor();
    assertNotNull(device);

    List<Device.DeviceDeviceNameComponent> deviceNames = device.getDeviceName();
    assertNotNull(deviceNames);
    assertEquals(1, deviceNames.size());

    Device.DeviceDeviceNameComponent dnc = deviceNames.get(0);
    assertEquals(DEVICE_NAME, dnc.getName());
  }

  @Test
  public void testGetDeviceAuthor() {
    Device device = ccrrReportCreator.getDeviceAuthor();
    assertNotNull(device);

    List<Device.DeviceDeviceNameComponent> deviceNames = device.getDeviceName();
    assertNotNull(deviceNames);
    assertEquals(1, deviceNames.size());

    Device.DeviceDeviceNameComponent dnc = deviceNames.get(0);
    assertEquals(DEVICE_NAME, dnc.getName());
  }

  @Test
  public void testCreateMessageHeader() {
    KarProcessingData karProcessingData = new KarProcessingData();
    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setTriggerEvent("TriggerEvent");
    karProcessingData.setNotificationContext(notificationContext);

    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting.setFhirServerBaseURL("http://example.com/fhir");
    karProcessingData.setHealthcareSetting(healthcareSetting);

    KnowledgeArtifact kar = new KnowledgeArtifact();
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://receiver1.example.com"));
    receiverAddresses.add(new UriType("http://receiver2.example.com"));
    kar.setReceiverAddresses(receiverAddresses);
    karProcessingData.setKar(kar);

    MessageHeader messageHeader = ccrrReportCreator.createMessageHeader(karProcessingData);

    assertNotNull(messageHeader.getId(), "MessageHeader should have an ID");

    assertNotNull(messageHeader.getEventCoding());
    assertEquals(2, messageHeader.getDestination().size());

    CodeableConcept reason = messageHeader.getReason();
    assertEquals("TriggerEvent", reason.getCodingFirstRep().getCode());
    assertEquals(
        "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-triggerdefinition-namedevents",
        reason.getCodingFirstRep().getSystem());

    Extension ext = messageHeader.getExtension().get(0);
    assertEquals(
        "http://hl7.org/fhir/us/medmorph/StructureDefinition/us-ph-report-initiation-type",
        ext.getUrl());
    CodeableConcept value = (CodeableConcept) ext.getValue();
    Coding coding = value.getCodingFirstRep();
    assertEquals(
        "http://hl7.org/fhir/us/medmorph/CodeSystem/us-ph-report-initiation-types",
        coding.getSystem());
  }

  @Test
  public void testCreateContentBundle() {
    EhrQueryService mockEhrService = mock(EhrQueryService.class);
    KarProcessingData karProcessingData = new KarProcessingData();

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    karProcessingData.setNotificationContext(notificationContext);

    Patient patient = new Patient();
    patient.setId("p1");

    Practitioner practitioner = new Practitioner();
    practitioner.setId("pr1");

    Set<Resource> patients = new HashSet<>();
    patients.add(patient);
    Set<Resource> practitioners = new HashSet<>();
    practitioners.add(practitioner);
    HashMap<ResourceType, Set<Resource>> fhirData = new HashMap<>();
    fhirData.put(ResourceType.Patient, patients);
    fhirData.put(ResourceType.Practitioner, practitioners);

    karProcessingData.setFhirInputDataByType(fhirData);

    Bundle contentBundle =
        ccrrReportCreator.createContentBundle(
            karProcessingData, mockEhrService, "123", "someProfile");

    assertNotNull(contentBundle);
    assertEquals(Bundle.BundleType.COLLECTION, contentBundle.getType());
    assertNotNull(contentBundle.getTimestamp());
    assertNotNull(contentBundle.getMeta());
  }

  @Test
  public void testCreateComposition() {
    KarProcessingData karData = new KarProcessingData();
    Patient patient = new Patient();
    patient.setId("p1");
    patient
        .addIdentifier()
        .setSystem("http://hospital.smarthealth.org/patients")
        .setValue("123456");
    patient.addName().setFamily("Doe").addGiven("John");
    patient.setGender(Enumerations.AdministrativeGender.MALE);
    patient.setBirthDateElement(new DateType("1980-05-15"));
    Encounter encounter = new Encounter();
    encounter.setId("encounter-1");
    encounter.setStatus(Encounter.EncounterStatus.FINISHED);
    Coding coding = new Coding();
    coding.setCode("AMB");
    coding.setDisplay("ambulatory");
    coding.setSystem("http://terminology.hl7.org/CodeSystem/v3-ActCode");
    encounter.setClass_(coding);
    CodeableConcept codeableConcept = new CodeableConcept();
    codeableConcept
        .addCoding()
        .setSystem("http://snomed.info/sct")
        .setCode("185349003")
        .setDisplay("Encounter for check up");
    codeableConcept.setText("Encounter for check up");
    encounter.addType(codeableConcept);
    encounter.setSubject(new Reference("Patient/p1").setDisplay("John Doe"));
    Organization org = new Organization();
    org.setId("org-1");
    org.addIdentifier().setSystem("http://hospital.smarthealth.org/organizations").setValue("999");
    org.setName("Springfield Medical Center");
    org.addType()
        .addCoding()
        .setSystem("http://terminology.hl7.org/CodeSystem/organization-type")
        .setCode("prov")
        .setDisplay("Healthcare Provider");

    Set<Resource> patients = new HashSet<>();
    patients.add(patient);
    Set<Resource> encounters = new HashSet<>();
    encounters.add(encounter);
    Set<Resource> orgs = new HashSet<>();
    orgs.add(org);
    HashMap<ResourceType, Set<Resource>> fhirData = new HashMap<>();
    fhirData.put(ResourceType.Patient, patients);
    fhirData.put(ResourceType.Encounter, encounters);
    fhirData.put(ResourceType.Organization, orgs);

    karData.setFhirInputDataByType(fhirData);

    NotificationContext notificationContext = new NotificationContext();
    notificationContext.setFhirServerBaseUrl("http://example.com/fhir");
    karData.setNotificationContext(notificationContext);

    Set<Resource> resToBeAdded = new HashSet<>();

    Composition composition = ccrrReportCreator.createComposition(karData, resToBeAdded);

    assertNotNull(composition);
    assertEquals(Composition.CompositionStatus.FINAL, composition.getStatus());
    assertEquals("Central Cancer Registry Report", composition.getTitle());
    assertNotNull(composition.getType());
    assertNotNull(composition.getMeta());
    assertTrue(composition.hasSection());

    assertTrue(resToBeAdded.contains(patient));
    assertTrue(resToBeAdded.contains(encounter));
    assertTrue(resToBeAdded.contains(org));

    assertNotNull(composition.getCustodian());
    assertEquals(
        org.getIdElement().getIdPart(),
        composition.getCustodian().getResource().getIdElement().getIdPart());

    assertNotNull(composition.getIdentifier());
    assertEquals(composition.getId(), composition.getIdentifier().getValue());
  }

  @Test
  public void testPopulateReasonForVisitNarrative() {
    SectionComponent section = new SectionComponent();
    KarProcessingData kd = new KarProcessingData();

    ccrrReportCreator.populateReasonForVisitNarrative(section, kd);

    assertNotNull(section.getText());
    Narrative narrative = section.getText();

    assertEquals(NarrativeStatus.ADDITIONAL, narrative.getStatus());
    assertEquals(
        "<div xmlns=\"http://www.w3.org/1999/xhtml\">No Information</div>",
        narrative.getDivAsString());
  }

  @Test
  public void testResourceHasMatchedCode_withCondition() {

    CheckTriggerCodeStatus ctcs = mock(CheckTriggerCodeStatus.class);

    Condition condition = new Condition();
    CodeableConcept codeableConcept = mock(CodeableConcept.class);
    condition.setCode(codeableConcept);

    ReportableMatchedTriggerCode matchedCode = mock(ReportableMatchedTriggerCode.class);
    Pair<Boolean, ReportableMatchedTriggerCode> expectedPair = new Pair<>(true, matchedCode);

    when(ctcs.getMatchedCode(codeableConcept)).thenReturn(expectedPair);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(condition, ctcs);

    verify(ctcs).getMatchedCode(codeableConcept);
    assertEquals(expectedPair, result);
  }

  @Test
  public void testIsResultsSection_ReturnsTrue() {
    SectionComponent section = new SectionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(FhirGeneratorConstants.LOINC_CS_URL);
    coding.setCode(FhirGeneratorConstants.RESULTS_SECTION_LOINC_CODE);
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = ccrrReportCreator.isResultsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.TRUE, result);
  }

  @Test
  public void testIsResultsSection_WrongSystem_ReturnsFalse() {
    SectionComponent section = new SectionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem("http://incorrect-system.org");
    coding.setCode(FhirGeneratorConstants.RESULTS_SECTION_LOINC_CODE);
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = ccrrReportCreator.isResultsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.FALSE, result);
  }

  @Test
  public void testIsResultsSection_WrongCode_ReturnsFalse() {
    SectionComponent section = new SectionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();
    coding.setSystem(FhirGeneratorConstants.LOINC_CS_URL);
    coding.setCode("12345-6");
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = ccrrReportCreator.isResultsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.FALSE, result);
  }

  @Test
  public void testIsResultsSection_NullCode_ReturnsFalse() {
    SectionComponent section = new SectionComponent();
    Boolean result = ccrrReportCreator.isResultsSection(section);
    assertFalse(result);
  }

  @Test
  public void testIsVitalsSection_ReturnsTrue() {
    SectionComponent section = new SectionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();

    coding.setSystem(FhirGeneratorConstants.LOINC_CS_URL);
    coding.setCode(FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE);
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = ccrrReportCreator.isVitalsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.TRUE, result);
  }

  @Test
  public void testIsVitalsSection_WrongSystem_ReturnsFalse() {
    SectionComponent section = new SectionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();

    coding.setSystem("http://wrong-system.org");
    coding.setCode(FhirGeneratorConstants.VITAL_SIGNS_SECTION_LOINC_CODE);
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = ccrrReportCreator.isVitalsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.FALSE, result);
  }

  @Test
  public void testIsVitalsSection_WrongCode_ReturnsFalse() {
    SectionComponent section = new SectionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();

    coding.setSystem(FhirGeneratorConstants.LOINC_CS_URL);
    coding.setCode("wrong-code");
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = ccrrReportCreator.isVitalsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.FALSE, result);
  }

  @Test
  public void testIsVitalsSection_NoCodeableConcept_ReturnsFalse() {
    SectionComponent section = new SectionComponent();
    Boolean result = ccrrReportCreator.isVitalsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.FALSE, result);
  }

  @Test
  public void testIsVitalsSection_NullCodeInCoding_ReturnsFalse() {
    SectionComponent section = new SectionComponent();
    CodeableConcept codeableConcept = new CodeableConcept();
    Coding coding = new Coding();

    coding.setSystem(FhirGeneratorConstants.LOINC_CS_URL);
    coding.setCode(null);
    codeableConcept.addCoding(coding);
    section.setCode(codeableConcept);

    Boolean result = ccrrReportCreator.isVitalsSection(section);
    assertNotNull(result);
    assertEquals(Boolean.FALSE, result);
  }

  @Test
  public void testRemoveExtensions_DomainResource() {
    Patient patient = new Patient();
    patient.addExtension(new Extension("http://example.org/ext1", new StringType("value1")));
    patient.addExtension(
        new Extension("http://hl7.org/fhir/us/core/ext2", new StringType("value2")));
    patient.addExtension(
        new Extension("http://hl7.org/fhir/us/ecr/ext3", new StringType("value3")));

    ccrrReportCreator.removeExtensions(patient);

    List<Extension> exts = patient.getExtension();
    assertEquals(2, exts.size());
  }

  @Test
  public void testRemoveExtensions_ObservationPerformer() {
    Observation obs = new Observation();
    Reference performerRef = new Reference("Practitioner/123");
    performerRef.addExtension(
        new Extension(
            "http://hl7.org/fhir/StructureDefinition/event-performerFunction",
            new StringType("function1")));
    performerRef.addExtension(
        new Extension("http://example.org/irrelevant", new StringType("junk")));
    obs.addPerformer(performerRef);

    ccrrReportCreator.removeExtensions(obs);

    List<Extension> updatedExts = obs.getPerformer().get(0).getExtension();

    assertEquals(2, updatedExts.size());
  }

  @Test
  public void testFilterWithProfileMatch() {
    Patient r1 = new Patient();
    Meta meta1 = new Meta();
    meta1.addProfile("http://example.org/ProfileA");
    r1.setMeta(meta1);
    Patient r2 = new Patient();
    Meta meta2 = new Meta();
    meta2.addProfile("http://example.org/ProfileB");
    r2.setMeta(meta2);
    Patient r3 = new Patient();

    Set<Resource> resources = new HashSet<>();
    resources.add(r1);
    resources.add(r2);
    resources.add(r3);

    Set<Resource> filtered =
        ccrrReportCreator.filterResourcesByProfile(
            resources, "http://example.org/ProfileA", Collections.emptySet());

    assertTrue(filtered.contains(r1));
    assertFalse(filtered.contains(r2));
    assertFalse(filtered.contains(r3));
  }

  @Test
  public void testFilterWithEmptyProfileAndProfilesToIgnore() {
    Patient r1 = new Patient();
    Meta meta1 = new Meta();
    meta1.addProfile("http://example.org/ProfileA");
    r1.setMeta(meta1);
    Patient r2 = new Patient();
    Meta meta2 = new Meta();
    meta2.addProfile("http://example.org/ProfileB");
    r2.setMeta(meta2);
    Patient r3 = new Patient();
    Set<Resource> resources = new HashSet<>();
    resources.add(r1);
    resources.add(r2);
    resources.add(r3);

    Set<String> profilesToIgnore = new HashSet<>();
    profilesToIgnore.add("http://example.org/ProfileA");

    Set<Resource> filtered =
        ccrrReportCreator.filterResourcesByProfile(resources, "", profilesToIgnore);

    assertFalse(filtered.contains(r1));
    assertFalse(filtered.contains(r2));
    assertTrue(filtered.contains(r3));
  }

  @Test
  public void testFilterWithEmptyProfileAndEmptyProfilesToIgnore() {
    Patient r1 = new Patient();
    Meta meta1 = new Meta();
    meta1.addProfile("http://example.org/ProfileA");
    r1.setMeta(meta1);
    Patient r2 = new Patient();

    Set<Resource> resources = new HashSet<>();
    resources.add(r1);
    resources.add(r2);

    Set<Resource> filtered =
        ccrrReportCreator.filterResourcesByProfile(resources, "", Collections.emptySet());

    assertTrue(filtered.contains(r1));
    assertTrue(filtered.contains(r2));
    assertEquals(2, filtered.size());
  }
}
