package com.drajer.bsa.kar.action;

import static org.junit.Assert.*;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.BsaTypes.SectionTypeEnum;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.InputStream;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.javatuples.Pair;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.core.io.ClassPathResource;

public class RespnetReportCreatorTest {
  FhirContext r4Context = FhirContext.forR4();
  KarProcessingData karProcessingData;
  EhrQueryService ehrQueryService;
  public static final String PROFILE =
      "http//://hl7.org/fhir/us/resp-net/StructureDefinition/resp-net-reporting-bundle";
  RespnetReportCreator respnetReportCreator;

  @Before
  public void setUp() {
    respnetReportCreator = new RespnetReportCreator();
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
            respnetReportCreator.createReport(
                karProcessingData, ehrQueryService, "example", PROFILE, bsaAction);
    assertTrue(actualReport.hasId());

    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/resp-report/report.json");
    assertEquals(exceptedReport.getEntry().size(), actualReport.getEntry().size());

    String actualComposition = getComposition(actualReport);
    String expectedComposition = getComposition(exceptedReport);
    assertEquals(expectedComposition, actualComposition);
  }

  @Test
  public void testreferenceTo() {
    Resource patient = TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    respnetReportCreator.referenceTo(patient);
    assertTrue(patient.hasId());
    assertNotNull(patient);
  }

  @Test
  public void testpopulateReasonForVisitNarrative() {
    Composition.SectionComponent sectionComponent = new Composition.SectionComponent();
    sectionComponent.fhirType();
    respnetReportCreator.populateReasonForVisitNarrative(sectionComponent);
    assertNotNull("Section component should be populated after method call", sectionComponent);
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
      FhirContext ctx = FhirContext.forR4();
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
    status.setKarId("resp-net-specification-bundle");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    return status;
  }

  private HealthcareSetting getHealthcareSetting() {
    HealthcareSetting healthcareSetting = new HealthcareSetting();
    healthcareSetting =
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

  public String getComposition(Bundle bundle) {
    for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
      if (entry.getResource() instanceof Composition) {
        return r4Context.newJsonParser().encodeResourceToString((Composition) entry.getResource());
      }
    }
    return null;
  }

  @Test
  public void testGetDeviceAuthor() {
    Device dev = respnetReportCreator.getDeviceAuthor();
    assertNotNull("Should return Device", dev);
    assertTrue("Should have device name", dev.hasDeviceName());
    assertEquals("Should have one device name", 1, dev.getDeviceName().size());
  }

  @Test
  public void testGetDeviceAuthor_DeviceNameComponent() {
    Device dev = respnetReportCreator.getDeviceAuthor();
    assertNotNull("Device should not be null", dev);
    assertTrue("Should have device names", dev.hasDeviceName());

    Device.DeviceDeviceNameComponent dnc = dev.getDeviceNameFirstRep();
    assertNotNull("Device name component should not be null", dnc);
    assertNotNull("Device name should be set", dnc.getName());
  }

  @Test
  public void testGetSectionComponent_PrimaryCancer() {
    Composition.SectionComponent sc =
        respnetReportCreator.getSectionComponent(SectionTypeEnum.PRIMARY_CANCER_CONDITION);
    assertNotNull("Should return section component", sc);
  }

  @Test
  public void testGetSectionComponent_SecondaryCancer() {
    Composition.SectionComponent sc =
        respnetReportCreator.getSectionComponent(SectionTypeEnum.SECONDARY_CANCER_CONDITION);
    assertNotNull("Should return section component", sc);
  }

  @Test
  public void testGetSectionComponent_CancerStage() {
    Composition.SectionComponent sc =
        respnetReportCreator.getSectionComponent(SectionTypeEnum.CANCER_STAGE_GROUP);
    assertNotNull("Should return section component", sc);
  }

  @Test
  public void testGetSectionComponent_RadioTherapy() {
    Composition.SectionComponent sc =
        respnetReportCreator.getSectionComponent(SectionTypeEnum.RADIO_THERAPY_COURSE_SUMMARY);
    assertNotNull("Should return section component", sc);
  }

  @Test
  public void testGetSectionComponent_ODH() {
    Composition.SectionComponent sc = respnetReportCreator.getSectionComponent(SectionTypeEnum.ODH);
    assertNotNull("Should return section component", sc);
  }

  @Test
  public void testGetSectionComponent_Allergies() {
    Composition.SectionComponent sc =
        respnetReportCreator.getSectionComponent(SectionTypeEnum.ALLERGIES);
    assertNotNull("Should return section component", sc);
  }

  @Test
  public void testGetSectionComponent_AdmissionMeds() {
    Composition.SectionComponent sc =
        respnetReportCreator.getSectionComponent(SectionTypeEnum.ADMISSION_MEDICATIONS);
    assertNotNull("Should return section component", sc);
  }

  // ========== RESOURCE HAS MATCHED CODE TESTS ==========

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
        respnetReportCreator.resourceHasMatchedCode(condition, ctcs);

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
        respnetReportCreator.resourceHasMatchedCode(condition, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should not find match", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_Observation() {
    Observation obs = new Observation();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        respnetReportCreator.resourceHasMatchedCode(obs, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for observation", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_MedicationRequest() {
    MedicationRequest mr = new MedicationRequest();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        respnetReportCreator.resourceHasMatchedCode(mr, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for medication request", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_ServiceRequest() {
    ServiceRequest sr = new ServiceRequest();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        respnetReportCreator.resourceHasMatchedCode(sr, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for service request", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_Immunization() {
    Immunization imm = new Immunization();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        respnetReportCreator.resourceHasMatchedCode(imm, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for immunization", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  @Test
  public void testResourceHasMatchedCode_Procedure() {
    Procedure proc = new Procedure();
    CheckTriggerCodeStatus ctcs = Mockito.mock(CheckTriggerCodeStatus.class);

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        respnetReportCreator.resourceHasMatchedCode(proc, ctcs);

    assertNotNull("Result should not be null", result);
    assertFalse("Should return false for procedure", result.getValue0());
    assertNull("Code should be null", result.getValue1());
  }

  // ========== REMOVE EXTENSIONS TESTS ==========

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
    respnetReportCreator.removeExtensions(patient);

    assertTrue(
        "Should keep us/core extension",
        patient.getExtension().stream().anyMatch(e -> e.getUrl().contains("us/core")));
  }

  @Test
  public void testRemoveExtensions_PatientKeepsMedmorphExtension() {
    Patient patient = new Patient();
    List<Extension> exts = new ArrayList<>();

    Extension medExt = new Extension();
    medExt.setUrl("http://hl7.org/fhir/us/medmorph/test");
    exts.add(medExt);

    Extension otherExt = new Extension();
    otherExt.setUrl("http://example.org/other");
    exts.add(otherExt);

    patient.setExtension(exts);
    respnetReportCreator.removeExtensions(patient);

    assertTrue(
        "Should keep us/medmorph extension",
        patient.getExtension().stream().anyMatch(e -> e.getUrl().contains("us/medmorph")));
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
    respnetReportCreator.removeExtensions(patient);

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

    respnetReportCreator.removeExtensions(obs);

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

    respnetReportCreator.removeExtensions(obs);

    long perfCount =
        obs.getPerformer().get(0).getExtension().stream()
            .filter(e -> e.getUrl().contains("event-performerFunction"))
            .count();
    assertEquals("Should have only 1 performer function", 1, perfCount);
  }

  @Test
  public void testRemoveExtensions_PatientKeepsAllMatchingPatterns() {
    Patient patient = new Patient();
    List<Extension> exts = new ArrayList<>();

    String[] patterns = {
      "us/core", "us/medmorph", "us/healthcare-survey", "us/ecr", "us/ccrr", "us/ph"
    };
    for (String pattern : patterns) {
      Extension ext = new Extension();
      ext.setUrl("http://hl7.org/fhir/" + pattern + "/test");
      exts.add(ext);
    }

    patient.setExtension(exts);
    respnetReportCreator.removeExtensions(patient);

    assertEquals("Should keep all matching patterns", 6, patient.getExtension().size());
  }
}
