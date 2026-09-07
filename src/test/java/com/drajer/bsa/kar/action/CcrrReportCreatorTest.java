package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointSystem;
import org.hl7.fhir.r4.model.ContactPoint.ContactPointUse;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.core.io.ClassPathResource;

public class CcrrReportCreatorTest {

  FhirContext r4Context = FhirContext.forR4();
  KarProcessingData karProcessingData;
  EhrQueryService ehrQueryService;
  public static final String PROFILE =
      "http//://hl7.org/fhir/us/central-cancer-registry-reporting/StructureDefinition/ccrr-reporting-bundle";
  CcrrReportCreator ccrrReportCreator;

  @Before
  public void setUp() {
    ccrrReportCreator = new CcrrReportCreator();
    ehrQueryService = Mockito.mock(EhrQueryService.class);
    karProcessingData = new KarProcessingData();
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus());
    karProcessingData.setPhm(null);
    NotificationContext notificationContext = getNotificationContext();
    karProcessingData.setNotificationContext(notificationContext);
    karProcessingData.setHealthcareSetting(getHealthcareSetting());
    karProcessingData.setxRequestId("32");
    karProcessingData.setxCorrelationId(null);
    karProcessingData.setNotificationBundle(
        (Bundle)
            r4Context.newJsonParser().parseResource(notificationContext.getNotificationData()));
    karProcessingData.setFhirInputDataByType(getFilteredByType("/R4/LoadingQueryBundle.json"));
    karProcessingData.setKar(getKnowledgeArtifact());
  }

  @Test
  public void testCreateReport() {

    karProcessingData.setKarStatus(getKnowledgeArtifactStatus());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();
    Bundle actualReport =
        (Bundle)
            ccrrReportCreator.createReport(
                karProcessingData, ehrQueryService, "example", PROFILE, bsaAction);
    assertTrue(actualReport.hasId());
    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/Ccrr-report/report.json");
    assertEquals(exceptedReport.getEntry().size(), actualReport.getEntry().size());

    String actualComposition = getComposition(actualReport);
    String expectedComposition = getComposition(exceptedReport);
    assertEquals(expectedComposition, actualComposition);
  }

  @Test
  public void testreferenceTo() {
    Resource patient = TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    ccrrReportCreator.referenceTo(patient);
    assertTrue(patient.hasId());
    assertNotNull(patient);
  }

  @Test
  public void testpopulateReasonForVisitNarrative() {
    Composition.SectionComponent sectionComponent = new Composition.SectionComponent();
    sectionComponent.fhirType();
    ccrrReportCreator.populateReasonForVisitNarrative(sectionComponent);
    assertNotNull("Section component should be populated after method call", sectionComponent);
  }

  public String getComposition(Bundle bundle) {
    for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
      if (entry.getResource() instanceof Composition) {
        return r4Context.newJsonParser().encodeResourceToString((Composition) entry.getResource());
      }
    }
    return null;
  }

  private BsaAction getBsaAction() {
    BsaAction action = new CreateReport();
    HashMap<String, String> inputRelatedData = new HashMap<>();
    inputRelatedData.put("1234", "1234");
    inputRelatedData.put("123", "123");
    action.setInputDataIdToRelatedDataIdMap(inputRelatedData);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setType("Patient");
    dataRequirement.setId("1234");
    DataRequirement dataRequirementEncounter = new DataRequirement();
    dataRequirementEncounter.setType("Encounter");
    dataRequirementEncounter.setId("123");

    List<DataRequirement> dataRequirements = new ArrayList<>();
    dataRequirements.add(dataRequirement);
    dataRequirements.add(dataRequirementEncounter);
    action.setInputData(dataRequirements);

    HashMap<String, Set<Resource>> actionOutputDataById = new HashMap<>();
    Set<Resource> res = new HashSet<>();
    res.add(TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json"));
    actionOutputDataById.put("1234", res);
    karProcessingData.setActionOutputDataById(actionOutputDataById);
    Set<Resource> resEnc = new HashSet<>();
    resEnc.add(
        TestUtils.loadResourceDataFromFile(
            Encounter.class, "R4/Encounter/Encounter_97953900.json"));
    actionOutputDataById.put("123", resEnc);
    karProcessingData.setActionOutputDataById(actionOutputDataById);
    return action;
  }

  private HashMap<ResourceType, Set<Resource>> getFilteredByType(String filePath) {
    HashMap<ResourceType, Set<Resource>> groupedResources = new HashMap<>();
    try {
      Bundle bundle = loadBundleFromFile(filePath);

      for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
        Resource resource = entry.getResource();
        if (resource != null) {

          groupedResources
              .computeIfAbsent(resource.getResourceType(), k -> new HashSet<>())
              .add(resource);
        }
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
    return groupedResources;
  }

  private Bundle loadBundleFromFile(String filename) {
    try (InputStream in = new ClassPathResource(filename).getInputStream()) {
      return r4Context.newJsonParser().parseResource(Bundle.class, in);
    } catch (Exception e) {
      return null;
    }
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus() {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(1);
    status.setIsActive(true);
    status.setOutputFormat(BsaTypes.OutputContentType.FHIR);
    status.setKarVersion("1.0.0");
    status.setKarId("cancer-specification-bundle-example");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    return status;
  }

  private HealthcareSetting getHealthcareSetting() {
    HealthcareSetting healthcareSetting =
        (HealthcareSetting)
            TestUtils.getResourceAsObject("Bsa/HealthCareSettings.json", HealthcareSetting.class);
    return healthcareSetting;
  }

  private KnowledgeArtifact getKnowledgeArtifact() {
    KnowledgeArtifact knowledgeArtifact = new KnowledgeArtifact();
    Set<UriType> receiverAddresses = new HashSet<>();
    receiverAddresses.add(new UriType("http://receiver1.example.com"));
    receiverAddresses.add(new UriType("http://receiver2.example.com"));
    knowledgeArtifact.setReceiverAddresses(receiverAddresses);
    return knowledgeArtifact;
  }

  private NotificationContext getNotificationContext() {
    NotificationContext context =
        TestUtils.readFileContents(
            "Bsa/NotificationContext/NotificationContext.json",
            new TypeReference<NotificationContext>() {});
    Bundle nb = loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    context.setNotificationData(r4Context.newJsonParser().encodeResourceToString(nb));
    context.setNotificationResourceType("Encounter");

    return context;
  }

  // ========== CREATE SENDER TESTS ==========

  @Test
  public void testCreateSender_HasId() {
    Organization org = ccrrReportCreator.createSender(karProcessingData);
    assertNotNull("Organization should have ID", org.getId());
    assertFalse("ID should not be empty", org.getId().isEmpty());
  }

  @Test
  public void testCreateSender_HasMeta() {
    Organization org = ccrrReportCreator.createSender(karProcessingData);
    assertNotNull("Organization should have meta", org.getMeta());
  }

  @Test
  public void testCreateSender_IsActive() {
    Organization org = ccrrReportCreator.createSender(karProcessingData);
    assertTrue("Organization should be active", org.getActive());
  }

  @Test
  public void testCreateSender_HasTelecom() {
    Organization org = ccrrReportCreator.createSender(karProcessingData);
    assertNotNull("Should have telecom list", org.getTelecom());
    assertTrue("Telecom list should not be empty", org.getTelecom().size() > 0);
  }

  @Test
  public void testCreateSender_TelecomHasEmailWork() {
    Organization org = ccrrReportCreator.createSender(karProcessingData);
    ContactPoint cp = org.getTelecom().get(0);
    assertEquals("Should be WORK use", ContactPointUse.WORK, cp.getUse());
    assertEquals("Should be EMAIL system", ContactPointSystem.EMAIL, cp.getSystem());
  }

  @Test
  public void testCreateSender_HasAddress() {
    Organization org = ccrrReportCreator.createSender(karProcessingData);
    assertNotNull("Should have address", org.getAddress());
    assertTrue("Address should not be empty", org.getAddress().size() > 0);
  }

  @Test
  public void testCreateSender_HasIdentifier() {
    Organization org = ccrrReportCreator.createSender(karProcessingData);
    assertNotNull("Should have identifier", org.getIdentifier());
    assertTrue("Identifier should not be empty", org.getIdentifier().size() > 0);
  }

  @Test
  public void testGetDeviceAuthor_NotNull() {
    Device device = ccrrReportCreator.getDeviceAuthor();
    assertNotNull("Device should not be null", device);
  }

  @Test
  public void testGetDeviceAuthor_HasDeviceNames() {
    Device device = ccrrReportCreator.getDeviceAuthor();
    assertTrue("Should have device names", device.hasDeviceName());
  }

  @Test
  public void testGetDeviceAuthor_DeviceNameCount() {
    Device device = ccrrReportCreator.getDeviceAuthor();
    assertEquals("Should have one device name", 1, device.getDeviceName().size());
  }

  @Test
  public void testGetDeviceAuthor_DeviceNameNotNull() {
    Device device = ccrrReportCreator.getDeviceAuthor();
    Device.DeviceDeviceNameComponent dnc = device.getDeviceNameFirstRep();
    assertNotNull("Device name component should not be null", dnc);
    assertNotNull("Device name should not be null", dnc.getName());
  }

  @Test
  public void testResourceHasMatchedCode_ConditionWithMatch() {
    Condition condition = new Condition();
    CodeableConcept code = new CodeableConcept();
    code.addCoding().setSystem("http://snomed.info/sct").setCode("12345-6");
    condition.setCode(code);

    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);
    ReportableMatchedTriggerCode rmtc = new ReportableMatchedTriggerCode();
    rmtc.setCode("12345-6");
    Mockito.when(ctcs.getMatchedCode(Mockito.any())).thenReturn(new Pair<>(true, rmtc));

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(condition, ctcs);

    assertNotNull("Result should not be null", result);
    assertTrue("Should find match", result.getValue0());
    assertNotNull("Code should not be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_ConditionNoMatch() {
    Condition condition = new Condition();
    CodeableConcept code = new CodeableConcept();
    condition.setCode(code);

    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);
    Mockito.when(ctcs.getMatchedCode(Mockito.any())).thenReturn(new Pair<>(false, null));

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(condition, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should not find match", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_Observation() {
    Observation obs = new Observation();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(obs, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for observation", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_MedicationRequest() {
    MedicationRequest mr = new MedicationRequest();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(mr, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for medication request", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_ServiceRequest() {
    ServiceRequest sr = new ServiceRequest();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(sr, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for service request", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_Immunization() {
    Immunization imm = new Immunization();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(imm, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for immunization", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_Procedure() {
    Procedure proc = new Procedure();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        ccrrReportCreator.resourceHasMatchedCode(proc, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for procedure", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testRemoveExtensions_PatientKeepsCoreExtension() {
    Patient patient = new Patient();
    List<Extension> exts = new ArrayList<>();

    Extension coreExt = new Extension();
    coreExt.setUrl("http://hl7.org/fhir/us/core/test");
    exts.add(coreExt);

    Extension otherExt = new Extension();
    otherExt.setUrl("http://example.org/other");
    exts.add(otherExt);

    patient.setExtension(exts);
    ccrrReportCreator.removeExtensions(patient);

    assertTrue(
        "Should keep us/core extension",
        patient.getExtension().stream().anyMatch(e -> e.getUrl().contains("us/core")));
  }

  @Test
  public void testRemoveExtensions_PatientRemovesNonMatching() {
    Patient patient = new Patient();
    List<Extension> exts = new ArrayList<>();

    Extension otherExt1 = new Extension();
    otherExt1.setUrl("http://example.org/ext1");
    exts.add(otherExt1);

    Extension otherExt2 = new Extension();
    otherExt2.setUrl("http://example.org/ext2");
    exts.add(otherExt2);

    patient.setExtension(exts);
    ccrrReportCreator.removeExtensions(patient);

    assertTrue("Should remove non-matching extensions", patient.getExtension().isEmpty());
  }

  @Test
  public void testRemoveExtensions_ObservationPerformerExtensions() {
    Observation obs = new Observation();
    Reference perfRef = new Reference();

    List<Extension> perfExts = new ArrayList<>();
    Extension perfExt = new Extension();
    perfExt.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perfExts.add(perfExt);

    Extension otherExt = new Extension();
    otherExt.setUrl("http://example.org/other");
    perfExts.add(otherExt);

    perfRef.setExtension(perfExts);
    obs.addPerformer(perfRef);

    ccrrReportCreator.removeExtensions(obs);

    assertTrue(
        "Should preserve performer function",
        obs.getPerformer().get(0).getExtension().stream()
            .anyMatch(e -> e.getUrl().contains("event-performerFunction")));
  }

  @Test
  public void testRemoveExtensions_ObservationRemoveDuplicatePerformerFunction() {
    Observation obs = new Observation();
    Reference perfRef = new Reference();

    List<Extension> perfExts = new ArrayList<>();
    Extension perf1 = new Extension();
    perf1.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perfExts.add(perf1);

    Extension perf2 = new Extension();
    perf2.setUrl("http://hl7.org/fhir/StructureDefinition/event-performerFunction");
    perfExts.add(perf2);

    Extension other = new Extension();
    other.setUrl("http://example.org/other");
    perfExts.add(other);

    perfRef.setExtension(perfExts);
    obs.addPerformer(perfRef);

    ccrrReportCreator.removeExtensions(obs);

    long perfCount =
        obs.getPerformer().get(0).getExtension().stream()
            .filter(e -> e.getUrl().contains("event-performerFunction"))
            .count();
    assertEquals("Should have only 1 performer function", 1, perfCount);
  }

  @Test
  public void testResourceHasProfile_WithMatchingProfile() throws Exception {
    Patient patient = new Patient();
    Meta meta = new Meta();
    meta.addProfile("http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient");
    patient.setMeta(meta);

    Method method =
        CcrrReportCreator.class.getDeclaredMethod(
            "resourceHasProfile", Resource.class, String.class);
    method.setAccessible(true);

    Boolean result =
        (Boolean)
            method.invoke(
                ccrrReportCreator,
                patient,
                "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient");

    assertTrue("Should find matching profile", result);
  }

  @Test
  public void testResourceHasProfile_WithNonMatchingProfile() throws Exception {
    Patient patient = new Patient();
    Meta meta = new Meta();
    meta.addProfile("http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient");
    patient.setMeta(meta);

    Method method =
        CcrrReportCreator.class.getDeclaredMethod(
            "resourceHasProfile", Resource.class, String.class);
    method.setAccessible(true);

    Boolean result =
        (Boolean) method.invoke(ccrrReportCreator, patient, "http://example.org/other");

    assertFalse("Should not find non-matching profile", result);
  }

  @Test
  public void testResourceHasProfile_WithoutMeta() throws Exception {
    Patient patient = new Patient();

    Method method =
        CcrrReportCreator.class.getDeclaredMethod(
            "resourceHasProfile", Resource.class, String.class);
    method.setAccessible(true);

    Boolean result =
        (Boolean)
            method.invoke(
                ccrrReportCreator,
                patient,
                "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient");

    assertFalse("Should return false when no meta", result);
  }

  @Test
  public void testResourceHasProfile_WithoutProfiles() throws Exception {
    Patient patient = new Patient();
    Meta meta = new Meta();
    patient.setMeta(meta);

    Method method =
        CcrrReportCreator.class.getDeclaredMethod(
            "resourceHasProfile", Resource.class, String.class);
    method.setAccessible(true);

    Boolean result =
        (Boolean)
            method.invoke(
                ccrrReportCreator,
                patient,
                "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient");

    assertFalse("Should return false when no profiles", result);
  }

  @Test
  public void testResourceHasProfile_WithMultipleProfiles() throws Exception {
    Patient patient = new Patient();
    Meta meta = new Meta();
    meta.addProfile("http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient");
    meta.addProfile("http://example.org/profile1");
    meta.addProfile("http://example.org/profile2");
    patient.setMeta(meta);

    Method method =
        CcrrReportCreator.class.getDeclaredMethod(
            "resourceHasProfile", Resource.class, String.class);
    method.setAccessible(true);

    Boolean result =
        (Boolean) method.invoke(ccrrReportCreator, patient, "http://example.org/profile1");

    assertTrue("Should find profile in multiple profiles", result);
  }

  @Test
  public void testResourceHasProfile_CaseSensitiveMatch() throws Exception {
    Patient patient = new Patient();
    Meta meta = new Meta();
    meta.addProfile("http://hl7.org/fhir/us/core/StructureDefinition/US-CORE-PATIENT");
    patient.setMeta(meta);

    Method method =
        CcrrReportCreator.class.getDeclaredMethod(
            "resourceHasProfile", Resource.class, String.class);
    method.setAccessible(true);

    Boolean result =
        (Boolean)
            method.invoke(
                ccrrReportCreator,
                patient,
                "http://hl7.org/fhir/us/core/StructureDefinition/us-core-patient");

    assertFalse("Should be case sensitive", result);
  }

  @Test
  public void testGetSectionComponent_AdmissionMedications() {
    Composition.SectionComponent sc =
        ccrrReportCreator.getSectionComponent(BsaTypes.SectionTypeEnum.ADMISSION_MEDICATIONS);
    assertNotNull("Should return section component", sc);
  }
}
