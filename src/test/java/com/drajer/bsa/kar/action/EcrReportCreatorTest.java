package com.drajer.bsa.kar.action;

import static com.drajer.bsa.model.BsaTypes.ActionType.CHECK_TRIGGER_CODES;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.BsaAction;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.kar.model.KnowledgeArtifactStatus;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.eca.model.ActionRepo;
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.ecrapp.service.impl.EicrServiceImpl;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.InputStream;
import java.time.Instant;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.hl7.fhir.r4.model.Bundle.BundleType;
import org.hl7.fhir.r4.model.codesystems.ObservationCategory;
import org.javatuples.Pair;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class EcrReportCreatorTest {

  @InjectMocks EcrReportCreator ecrReportCreator;

  FhirContext r4Context = FhirContext.forR4();
  KarProcessingData karProcessingData;
  EhrQueryService ehrQueryService;
  public static final String PROFILE =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-document-bundle";

  @Before
  public void setUp() {
    ReflectionTestUtils.setField(AESEncryption.class, "secretKey", "123");
    ehrQueryService = Mockito.mock(EhrQueryService.class);
    karProcessingData = new KarProcessingData();
    karProcessingData.setPhm(null);
    NotificationContext notificationContext = getNotificationContext();
    karProcessingData.setNotificationContext(notificationContext);
    karProcessingData.setScheduledJobData(getScheduledJobData());
    karProcessingData.setHealthcareSetting(getHealthcareSetting());
    karProcessingData.setxRequestId("32");
    karProcessingData.setxCorrelationId(null);
    karProcessingData.setNotificationBundle(
        (Bundle)
            r4Context.newJsonParser().parseResource(notificationContext.getNotificationData()));
    karProcessingData.setFhirInputDataByType(getFilteredByType("/R4/LoadingQueryBundle.json"));
    karProcessingData.setKar(getKnowledgeArtifact());
    karProcessingData.getActionStatusByType(CHECK_TRIGGER_CODES);
  }

  @Test
  public void testCreateReport_FhirType() {
    CheckTriggerCodeStatus bsaActionStatus = new CheckTriggerCodeStatus();
    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/ecr-report/fhirReport.json");
    bsaActionStatus.setActionId("action1");
    bsaActionStatus.setActionType(CHECK_TRIGGER_CODES);
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath("Condition");
    matchedTriggerCodes.addCode("Condition");
    matchedCodes.add(matchedTriggerCodes);
    bsaActionStatus.setMatchedCodes(matchedCodes);
    List<BsaActionStatus> actionStatuses = new ArrayList<>();
    actionStatuses.add(bsaActionStatus);
    HashMap<String, List<BsaActionStatus>> actionStatus = new HashMap<>();
    actionStatus.put("trigger", actionStatuses);
    karProcessingData.setActionStatus(actionStatus);
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();
    Bundle actualReport =
        (Bundle)
            ecrReportCreator.createReport(
                karProcessingData, ehrQueryService, inputData, "example", PROFILE, bsaAction);
    Assert.assertNotNull(actualReport);
    assertEquals(exceptedReport.getEntry().size(), actualReport.getEntry().size());

    String actualComposition = getComposition(actualReport);
    String expectedComposition = getComposition(exceptedReport);
    assertEquals(expectedComposition, actualComposition);
  }

  @Test
  public void testCreateReport_R11() {
    CheckTriggerCodeStatus bsaActionStatus = new CheckTriggerCodeStatus();
    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/ecr-report/ecir_r11.xml");
    bsaActionStatus.setActionId("action1");
    bsaActionStatus.setActionType(CHECK_TRIGGER_CODES);
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath("Condition");
    matchedTriggerCodes.addCode("Condition");
    matchedCodes.add(matchedTriggerCodes);
    bsaActionStatus.setMatchedCodes(matchedCodes);
    List<BsaActionStatus> actionStatuses = new ArrayList<>();
    actionStatuses.add(bsaActionStatus);
    HashMap<String, List<BsaActionStatus>> actionStatus = new HashMap<>();
    actionStatus.put("trigger", actionStatuses);
    karProcessingData.setActionStatus(actionStatus);
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus_CDA_R11());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();
    Bundle actualReport =
        (Bundle)
            ecrReportCreator.createReport(
                karProcessingData, ehrQueryService, inputData, "example", PROFILE, bsaAction);
    assertNotNull(actualReport);

    Bundle documentBundle =
        (Bundle)
            actualReport.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(r -> r instanceof Bundle)
                .findFirst()
                .orElse(null);

    assertNotNull(documentBundle);

    boolean documentReferenceExists =
        documentBundle.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .anyMatch(
                r ->
                    r instanceof DocumentReference
                        && ((DocumentReference) r)
                            .getType()
                            .getCodingFirstRep()
                            .getCode()
                            .equals("55751-2"));

    assertTrue(documentReferenceExists);
  }

  @Test
  public void testCreateReport_R31() {
    CheckTriggerCodeStatus bsaActionStatus = new CheckTriggerCodeStatus();
    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/ecr-report/ecir_r11.xml");
    bsaActionStatus.setActionId("action1");
    bsaActionStatus.setActionType(CHECK_TRIGGER_CODES);
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath("Condition");
    matchedTriggerCodes.addCode("Condition");
    matchedCodes.add(matchedTriggerCodes);
    bsaActionStatus.setMatchedCodes(matchedCodes);
    List<BsaActionStatus> actionStatuses = new ArrayList<>();
    actionStatuses.add(bsaActionStatus);
    HashMap<String, List<BsaActionStatus>> actionStatus = new HashMap<>();
    actionStatus.put("trigger", actionStatuses);
    karProcessingData.setActionStatus(actionStatus);
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus_CDA_R11());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();
    Bundle actualReport =
        (Bundle)
            ecrReportCreator.createReport(
                karProcessingData, ehrQueryService, inputData, "example", PROFILE, bsaAction);
    assertNotNull(actualReport);

    Bundle documentBundle =
        (Bundle)
            actualReport.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(r -> r instanceof Bundle)
                .findFirst()
                .orElse(null);

    assertNotNull(documentBundle);

    boolean documentReferenceExists =
        documentBundle.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .anyMatch(
                r ->
                    r instanceof DocumentReference
                        && ((DocumentReference) r)
                            .getType()
                            .getCodingFirstRep()
                            .getCode()
                            .equals("55751-2"));

    assertTrue(documentReferenceExists);
  }

  @Test
  public void testCreateReport_Both() {
    CheckTriggerCodeStatus bsaActionStatus = new CheckTriggerCodeStatus();
    Bundle exceptedReport = TestUtils.loadBundleFromFile("Bsa/report/ecr-report/ecir_r11.xml");
    bsaActionStatus.setActionId("action1");
    bsaActionStatus.setActionType(CHECK_TRIGGER_CODES);
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath("Condition");
    matchedTriggerCodes.addCode("Condition");
    matchedCodes.add(matchedTriggerCodes);
    bsaActionStatus.setMatchedCodes(matchedCodes);
    List<BsaActionStatus> actionStatuses = new ArrayList<>();
    actionStatuses.add(bsaActionStatus);
    HashMap<String, List<BsaActionStatus>> actionStatus = new HashMap<>();
    actionStatus.put("trigger", actionStatuses);
    karProcessingData.setActionStatus(actionStatus);
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus_BOTH());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();
    Bundle actualReport =
        (Bundle)
            ecrReportCreator.createReport(
                karProcessingData, ehrQueryService, inputData, "example", PROFILE, bsaAction);
    assertNotNull(actualReport);

    Bundle documentBundle =
        (Bundle)
            actualReport.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(r -> r instanceof Bundle)
                .findFirst()
                .orElse(null);

    assertNotNull(documentBundle);

    boolean documentReferenceExists =
        documentBundle.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .anyMatch(
                r ->
                    r instanceof DocumentReference
                        && ((DocumentReference) r)
                            .getType()
                            .getCodingFirstRep()
                            .getCode()
                            .equals("55751-2"));

    assertTrue(documentReferenceExists);

    Bundle eicrFhirBundle =
        (Bundle)
            actualReport.getEntry().stream()
                .map(Bundle.BundleEntryComponent::getResource)
                .filter(r -> r instanceof Bundle)
                .map(r -> (Bundle) r)
                .filter(
                    b ->
                        b.getEntry().stream()
                            .map(Bundle.BundleEntryComponent::getResource)
                            .anyMatch(res -> res instanceof Composition))
                .findFirst()
                .orElse(null);
    assertNotNull(eicrFhirBundle);

    long totalReportCount =
        actualReport.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .filter(r -> r instanceof Bundle)
            .count();
    assertEquals(3, totalReportCount);
  }

  @Test
  public void testObservationDirectMatch() {

    Observation obs = new Observation();
    obs.setCode(createConcept("http://loinc.org", "1234-5"));

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(obs, "http://loinc.org", "1234-5", ObservationCategory.LABORATORY);

    assertMatched(result, "1234-5");
  }

  @Test
  public void testDiagnosticReportMatch() {

    DiagnosticReport dr = new DiagnosticReport();
    dr.setCode(createConcept("http://loinc.org", "1234-5"));

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(dr, "http://loinc.org", "1234-5", null);

    assertMatched(result, "1234-5");
  }

  @Test
  public void testImmunizationMatch() {

    Immunization imm = new Immunization();
    imm.setVaccineCode(createConcept("http://loinc.org", "1234-5"));
    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(imm, "http://loinc.org", "1234-5", null);
    assertMatched(result, "1234-5");
  }

  @Test
  public void testUnsupportedResource() {

    Patient patient = new Patient();

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(patient, "http://loinc.org", "1234-5", null);
    assertFalse(result.getValue0());
    assertNull(result.getValue1());
  }

  @Test
  public void testMedicationRequest_CodeableConcept_Match() {

    MedicationRequest mr = new MedicationRequest();
    mr.setMedication(createConcept("http://loinc.org", "1234-5"));

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(mr, "http://loinc.org", "1234-5", null);
    assertMatched(result, "1234-5");
  }

  @Test
  public void testMedicationRequest_MedicationReferenceOnly() {

    MedicationRequest mr = new MedicationRequest();
    mr.setMedication(new Reference("Medication/1"));
    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(mr, "http://loinc.org", "1234-5", null);

    assertFalse(result.getValue0());
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
    status.setKarId("rctc-release-2023-02-03-Bundle-rctc");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    return status;
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus_R31() {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(1);
    status.setIsActive(true);
    status.setOutputFormat(BsaTypes.OutputContentType.CDA_R31);
    status.setKarVersion("3.0.1");
    status.setKarId("rctc-release-3.0.1-Bundle-rctc");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    return status;
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus_CDA_R11() {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(1);
    status.setIsActive(true);
    status.setOutputFormat(BsaTypes.OutputContentType.CDA_R11);
    status.setKarVersion("1.0.0");
    status.setKarId("rctc-release-2023-02-03-Bundle-rctc");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    EicrServiceImpl eicrService = Mockito.mock(EicrServiceImpl.class);
    ActionRepo.getInstance().setEicrRRService(eicrService);
    Mockito.lenient().when(eicrService.getMaxVersionId(any())).thenReturn(0);

    return status;
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus_BOTH() {
    KnowledgeArtifactStatus status = new KnowledgeArtifactStatus();
    status.setId(1);
    status.setIsActive(true);
    status.setOutputFormat(BsaTypes.OutputContentType.BOTH);
    status.setKarVersion("1.0.0");
    status.setKarId("rctc-release-2023-02-03-Bundle-rctc");
    status.setLastActivationDate(new Date());
    status.setSubscriptionsEnabled(false);
    status.setCovidOnly(false);
    EicrServiceImpl eicrService = Mockito.mock(EicrServiceImpl.class);
    ActionRepo.getInstance().setEicrRRService(eicrService);
    Mockito.lenient().when(eicrService.getMaxVersionId(any())).thenReturn(0);

    return status;
  }

  private ScheduledJobData getScheduledJobData() {
    ScheduledJobData jobData =
        new ScheduledJobData.Builder()
            .karExecutionStateId(UUID.randomUUID())
            .actionId(
                "check-for-immediate-reporting-PlanDefinition/http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification")
            .actionType(BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW)
            .expirationTime(Instant.now())
            .jobId(
                "http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification_EXECUTE_REPORTING_WORKFLOW_3a0172db-42e0-4d4c-8d35-29674e3d6108_fe9a6129-f988-49c9-859d-9e86f1b00548%22")
            .xRequestId("32")
            .jobType(BsaTypes.BsaJobType.IMMEDIATE_REPORTING)
            .mdcContext(new HashMap<>())
            .build();
    Map<String, String> mcContext = new HashMap<>();
    mcContext.put("requestId", "32");
    mcContext.put("correlationId", null);
    mcContext.put("domain-logicalDomainId", null);

    jobData.setMdcContext(mcContext);
    return jobData;
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

  NotificationContext getNotificationContext() {
    NotificationContext context =
        TestUtils.readFileContents(
            "Bsa/NotificationContext/NotificationContext.json",
            new TypeReference<NotificationContext>() {});
    Bundle nb = loadBundleFromFile("Bsa/NotificationBundleEncounterCloseWithoutPeriord.json");
    context.setNotificationData(r4Context.newJsonParser().encodeResourceToString(nb));
    context.setNotificationResourceType("Encounter");

    return context;
  }

  private CheckTriggerCodeStatus createCheckTriggerCodeStatus(String system, String code) {

    MatchedTriggerCodes mtc = new MatchedTriggerCodes();
    mtc.setValueSet("TestVS");
    mtc.setValueSetOid("1.2.3");
    mtc.setValueSetVersion("1");
    mtc.setMatchedCodes(Set.of(system + "|" + code));

    CheckTriggerCodeStatus ctcs = new CheckTriggerCodeStatus();
    ctcs.setMatchedCodes(new ArrayList<>(List.of(mtc)));

    return ctcs;
  }

  private Pair<Boolean, ReportableMatchedTriggerCode> execute(
      Resource resource, String system, String code, ObservationCategory category) {

    CheckTriggerCodeStatus ctcs = createCheckTriggerCodeStatus(system, code);

    return ecrReportCreator.resourceHasMatchedCode(resource, ctcs, category);
  }

  private void assertMatched(
      Pair<Boolean, ReportableMatchedTriggerCode> result, String expectedCode) {

    assertTrue(result.getValue0());
    assertNotNull(result.getValue1());
  }

  private CodeableConcept createConcept(String system, String code) {
    CodeableConcept cc = new CodeableConcept();
    cc.addCoding().setSystem(system).setCode(code);
    return cc;
  }

  public String getComposition(Bundle bundle) {
    for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
      if (entry.getResource() instanceof Composition) {
        return r4Context.newJsonParser().encodeResourceToString((Composition) entry.getResource());
      }
    }
    return null;
  }

  public String getEicrBundle(Bundle bundle) {
    for (Bundle.BundleEntryComponent entry : bundle.getEntry()) {
      if (entry.getResource() instanceof Bundle) {
        return r4Context.newJsonParser().encodeResourceToString((Bundle) entry.getResource());
      }
    }
    return null;
  }

  // ========== NEW TEST CASES FOR MEASURE REPORTS AND CDA R30/R31 ==========

  @Test
  public void testGetMeasureReports_FromResourcesByType() {
    // Test when measure reports are found in resources by type
    MeasureReport mr1 = new MeasureReport();
    mr1.setId("mr-1");
    MeasureReport mr2 = new MeasureReport();
    mr2.setId("mr-2");

    Set<Resource> measureReports = new HashSet<>();
    measureReports.add(mr1);
    measureReports.add(mr2);

    karProcessingData.addResourcesByType(ResourceType.MeasureReport, measureReports);

    Set<Resource> result = ecrReportCreator.getMeasureReports(karProcessingData);

    assertNotNull("Measure reports should not be null", result);
    assertEquals("Should find 2 measure reports", 2, result.size());
    assertTrue(
        "Should contain mr-1",
        result.stream().anyMatch(r -> "mr-1".equals(r.getIdElement().getIdPart())));
    assertTrue(
        "Should contain mr-2",
        result.stream().anyMatch(r -> "mr-2".equals(r.getIdElement().getIdPart())));
  }

  @Test
  public void testGetMeasureReports_FromOutputDataByIdUpperCase() {
    // Test when measure reports are found in output data by ID (uppercase)
    MeasureReport mr1 = new MeasureReport();
    mr1.setId("output-mr-1");

    karProcessingData.addActionOutputById("MeasureReport", mr1);

    Set<Resource> result = ecrReportCreator.getMeasureReports(karProcessingData);

    assertNotNull("Measure reports should not be null", result);
    assertEquals("Should find 1 measure report", 1, result.size());
    assertTrue(
        "Should contain output-mr-1",
        result.stream().anyMatch(r -> "output-mr-1".equals(r.getIdElement().getIdPart())));
  }

  @Test
  public void testGetMeasureReports_FromOutputDataByIdLowerCase() {
    // Test when measure reports are found in output data by ID (lowercase)
    MeasureReport mr1 = new MeasureReport();
    mr1.setId("output-mr-lowercase");

    karProcessingData.addActionOutputById("measurereport", mr1);

    Set<Resource> result = ecrReportCreator.getMeasureReports(karProcessingData);

    assertNotNull("Measure reports should not be null", result);
    assertEquals("Should find 1 measure report", 1, result.size());
    assertTrue(
        "Should contain output-mr-lowercase",
        result.stream().anyMatch(r -> "output-mr-lowercase".equals(r.getIdElement().getIdPart())));
  }

  @Test
  public void testGetMeasureReports_EmptyWhenNoneFound() {
    // Test when no measure reports are found
    Set<Resource> result = ecrReportCreator.getMeasureReports(karProcessingData);

    assertNotNull("Result should not be null", result);
    assertTrue("Result should be empty", result.isEmpty());
  }

  @Test
  public void testGetMeasureReports_PreferResourcesByType() {
    // Test that resources by type are preferred over output data
    MeasureReport mrByType = new MeasureReport();
    mrByType.setId("by-type");
    Set<Resource> resourcesByType = new HashSet<>();
    resourcesByType.add(mrByType);

    MeasureReport mrByOutput = new MeasureReport();
    mrByOutput.setId("by-output");

    karProcessingData.addResourcesByType(ResourceType.MeasureReport, resourcesByType);
    karProcessingData.addActionOutputById("MeasureReport", mrByOutput);

    Set<Resource> result = ecrReportCreator.getMeasureReports(karProcessingData);

    assertEquals("Should find 1 measure report from resources", 1, result.size());
    assertTrue(
        "Should only contain by-type",
        result.stream().anyMatch(r -> "by-type".equals(r.getIdElement().getIdPart())));
    assertFalse(
        "Should NOT contain by-output",
        result.stream().anyMatch(r -> "by-output".equals(r.getIdElement().getIdPart())));
  }

  @Test
  public void testCreateReport_CDA_R30() {
    // Test CDA R30 report creation
    CheckTriggerCodeStatus bsaActionStatus = new CheckTriggerCodeStatus();
    bsaActionStatus.setActionId("action1");
    bsaActionStatus.setActionType(CHECK_TRIGGER_CODES);
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath("Condition");
    matchedTriggerCodes.addCode("Condition");
    matchedCodes.add(matchedTriggerCodes);
    bsaActionStatus.setMatchedCodes(matchedCodes);
    List<BsaActionStatus> actionStatuses = new ArrayList<>();
    actionStatuses.add(bsaActionStatus);
    HashMap<String, List<BsaActionStatus>> actionStatus = new HashMap<>();
    actionStatus.put("trigger", actionStatuses);
    karProcessingData.setActionStatus(actionStatus);
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus_CDA_R30());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();

    Bundle actualReport =
        (Bundle)
            ecrReportCreator.createReport(
                karProcessingData, ehrQueryService, inputData, "example", PROFILE, bsaAction);

    assertNotNull("Report should not be null", actualReport);
    assertEquals("Report type should be MESSAGE", BundleType.MESSAGE, actualReport.getType());
    assertTrue("Report should have entries", actualReport.getEntry().size() > 0);
    assertTrue(
        "Report should contain MessageHeader",
        actualReport.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .anyMatch(r -> r instanceof MessageHeader));
  }

  @Test
  public void testCreateReport_CDA_R31() {
    // Test CDA R31 report creation (uses CDA_R31 format)
    CheckTriggerCodeStatus bsaActionStatus = new CheckTriggerCodeStatus();
    bsaActionStatus.setActionId("action1");
    bsaActionStatus.setActionType(CHECK_TRIGGER_CODES);
    List<MatchedTriggerCodes> matchedCodes = new ArrayList<>();
    MatchedTriggerCodes matchedTriggerCodes = new MatchedTriggerCodes();
    matchedTriggerCodes.setMatchedPath("Condition");
    matchedTriggerCodes.addCode("Condition");
    matchedCodes.add(matchedTriggerCodes);
    bsaActionStatus.setMatchedCodes(matchedCodes);
    List<BsaActionStatus> actionStatuses = new ArrayList<>();
    actionStatuses.add(bsaActionStatus);
    HashMap<String, List<BsaActionStatus>> actionStatus = new HashMap<>();
    actionStatus.put("trigger", actionStatuses);
    karProcessingData.setActionStatus(actionStatus);
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus_CDA_R31());
    Set<Resource> inputData = new HashSet<>();
    Resource resource =
        TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    inputData.add(resource);
    BsaAction bsaAction = getBsaAction();

    Bundle actualReport =
        (Bundle)
            ecrReportCreator.createReport(
                karProcessingData, ehrQueryService, inputData, "example", PROFILE, bsaAction);

    assertNotNull("Report should not be null", actualReport);
    assertEquals("Report type should be MESSAGE", BundleType.MESSAGE, actualReport.getType());
    assertTrue("Report should have entries", actualReport.getEntry().size() > 0);
  }

  @Test
  public void testGetCdaR31Report_WithEhrService() {
    // Test getCdaR31Report with EhrQueryService
    karProcessingData.setKarStatus(getKnowledgeArtifactStatus_CDA_R31());
    BsaAction bsaAction = getBsaAction();

    Bundle documentBundle =
        ecrReportCreator.getCdaR31Report(
            karProcessingData, ehrQueryService, "dataReq-1", PROFILE, bsaAction);

    assertNotNull("Document bundle should not be null", documentBundle);
    assertEquals("Bundle type should be DOCUMENT", BundleType.DOCUMENT, documentBundle.getType());
    assertTrue("Bundle should have entries", documentBundle.getEntry().size() > 0);
    assertTrue(
        "Bundle should contain DocumentReference",
        documentBundle.getEntry().stream()
            .map(Bundle.BundleEntryComponent::getResource)
            .anyMatch(r -> r instanceof DocumentReference));
  }

  @Test
  public void testCreateMessageHeader_WithCdaFlag() {
    // Test createMessageHeader with CDA flag set to true
    Set<UriType> receivers = new HashSet<>();
    receivers.add(new UriType("http://test-receiver.com"));
    karProcessingData.getKar().setReceiverAddresses(receivers);

    Bundle contentBundle = new Bundle();
    contentBundle.setId("bundle-123");

    MessageHeader mh = ecrReportCreator.createMessageHeader(karProcessingData, true, contentBundle);

    assertNotNull("MessageHeader should not be null", mh);
    assertNotNull("MessageHeader should have ID", mh.getId());
    assertNotNull("MessageHeader should have event", mh.getEvent());
    assertEquals(
        "Event code should indicate CDA message",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.CDA_EICR_MESSAGE),
        mh.getEvent() instanceof Coding
            ? ((Coding) mh.getEvent()).getCode()
            : mh.getEvent().toString());
  }

  @Test
  public void testCreateMessageHeader_WithoutCdaFlag() {
    // Test createMessageHeader with CDA flag set to false
    Set<UriType> receivers = new HashSet<>();
    receivers.add(new UriType("http://test-receiver.com"));
    karProcessingData.getKar().setReceiverAddresses(receivers);

    Bundle contentBundle = new Bundle();
    contentBundle.setId("bundle-456");

    MessageHeader mh =
        ecrReportCreator.createMessageHeader(karProcessingData, false, contentBundle);

    assertNotNull("MessageHeader should not be null", mh);
    assertNotNull("MessageHeader should have event", mh.getEvent());
    assertEquals(
        "Event code should indicate EICR message",
        BsaTypes.getMessageTypeString(BsaTypes.MessageType.EICR_CASE_REPORT_MESSAGE),
        mh.getEvent() instanceof Coding
            ? ((Coding) mh.getEvent()).getCode()
            : mh.getEvent().toString());
  }

  @Test
  public void testCreateReportingBundle() {
    // Test createReportingBundle
    String profile = "http://test.profile";

    Bundle reportingBundle = ecrReportCreator.createReportingBundle(profile);

    assertNotNull("Bundle should not be null", reportingBundle);
    assertEquals("Bundle type should be MESSAGE", BundleType.MESSAGE, reportingBundle.getType());
    assertNotNull("Bundle should have ID", reportingBundle.getId());
    assertNotNull("Bundle should have timestamp", reportingBundle.getTimestamp());
    assertNotNull("Bundle should have meta", reportingBundle.getMeta());
  }

  // Helper methods for new tests
  private KnowledgeArtifactStatus getKnowledgeArtifactStatus_CDA_R30() {
    KnowledgeArtifactStatus status = getKnowledgeArtifactStatus();
    status.setOutputFormat(BsaTypes.OutputContentType.CDA_R30);
    return status;
  }

  private KnowledgeArtifactStatus getKnowledgeArtifactStatus_CDA_R31() {
    KnowledgeArtifactStatus status = getKnowledgeArtifactStatus();
    status.setOutputFormat(BsaTypes.OutputContentType.CDA_R31);
    return status;
  }
}
