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

    assertMatched(result);
  }

  @Test
  public void testDiagnosticReportMatch() {

    DiagnosticReport dr = new DiagnosticReport();
    dr.setCode(createConcept("http://loinc.org", "1234-5"));

    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(dr, "http://loinc.org", "1234-5", null);

    assertMatched(result);
  }

  @Test
  public void testImmunizationMatch() {

    Immunization imm = new Immunization();
    imm.setVaccineCode(createConcept("http://loinc.org", "1234-5"));
    Pair<Boolean, ReportableMatchedTriggerCode> result =
        execute(imm, "http://loinc.org", "1234-5", null);
    assertMatched(result);
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
    assertMatched(result);
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

  //  private ScheduledJobData getScheduledJobData() {
  //    ScheduledJobData jobData =
  //        new ScheduledJobData.Builder()
  //            .karExecutionStateId(UUID.randomUUID())
  //            .actionId(
  //
  // "check-for-immediate-reporting-PlanDefinition/http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification")
  //            .actionType(BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW)
  //            .expirationTime(Instant.now())
  //            .jobId(
  //
  // "http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification_EXECUTE_REPORTING_WORKFLOW_3a0172db-42e0-4d4c-8d35-29674e3d6108_fe9a6129-f988-49c9-859d-9e86f1b00548%22")
  //            .xRequestId("32")
  //            .jobType(BsaTypes.BsaJobType.IMMEDIATE_REPORTING)
  //            .mdcContext(new HashMap<>())
  //            .build();
  //    Map<String, String> mcContext = new HashMap<>();
  //    mcContext.put("requestId", "32");
  //    mcContext.put("correlationId", null);
  //    mcContext.put("domain-logicalDomainId", null);
  //
  //    jobData.setMdcContext(mcContext);
  //    return jobData;
  //  }
  private ScheduledJobData getScheduledJobData() {
    ScheduledJobData jobData =
        new ScheduledJobData(
            UUID.randomUUID(), // karExecutionStateId
            "check-for-immediate-reporting-PlanDefinition/http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification", // actionId
            BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW, // actionType
            Instant.now(), // expirationTime
            "http://ersd.aimsplatform.org/fhir/PlanDefinition/us-ecr-specification_EXECUTE_REPORTING_WORKFLOW_3a0172db-42e0-4d4c-8d35-29674e3d6108_fe9a6129-f988-49c9-859d-9e86f1b00548%22", // jobId
            "32", // xRequestId
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING, // jobType
            new HashMap<>()); // mdcContext
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

  private void assertMatched(Pair<Boolean, ReportableMatchedTriggerCode> result) {

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

  @Test
  public void testGetMeasureReports_FromResourcesByType() {
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
    Set<Resource> result = ecrReportCreator.getMeasureReports(karProcessingData);

    assertNotNull("Result should not be null", result);
    assertTrue("Result should be empty", result.isEmpty());
  }

  @Test
  public void testGetMeasureReports_PreferResourcesByType() {
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
    String profile = "http://test.profile";

    Bundle reportingBundle = ecrReportCreator.createReportingBundle(profile);

    assertNotNull("Bundle should not be null", reportingBundle);
    assertEquals("Bundle type should be MESSAGE", BundleType.MESSAGE, reportingBundle.getType());
    assertNotNull("Bundle should have ID", reportingBundle.getId());
    assertNotNull("Bundle should have timestamp", reportingBundle.getTimestamp());
    assertNotNull("Bundle should have meta", reportingBundle.getMeta());
  }

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

  @Test
  public void resourceHasMatchedCode_realQueryResources_matchOnTheirOwnCode() {
    Bundle data = TestUtils.loadBundleFromFile("R4/LoadingQueryBundle.json");
    java.util.function.Function<String, Resource> real =
        id ->
            data.getEntry().stream()
                .map(e -> e.getResource())
                .filter(r -> r.getIdElement().getIdPart().equals(id))
                .findFirst()
                .get();
    String loinc = "http://loinc.org";

    Pair<Boolean, ReportableMatchedTriggerCode> condition =
        execute(
            real.apply("390cf06f-ef96-4cb6-b377-e90fc78a5805"),
            "http://snomed.info/sct",
            "186747009",
            null);
    Pair<Boolean, ReportableMatchedTriggerCode> report =
        execute(real.apply("43962602-9bf2-4bd5-b7b7-549392c18e80"), loinc, "96910-5", null);
    Pair<Boolean, ReportableMatchedTriggerCode> request =
        execute(real.apply("536b2fd1-6962-4362-9a27-fa5a0cf94df8"), loinc, "89648-0", null);
    Pair<Boolean, ReportableMatchedTriggerCode> vaccine =
        execute(
            real.apply("7ae40495-f4bc-438c-85a7-ce18d8658293"),
            "http://hl7.org/fhir/sid/cvx",
            "18",
            null);
    Pair<Boolean, ReportableMatchedTriggerCode> lab =
        execute(
            real.apply("f7e55c9f-0612-42e3-8e52-67f4f3a25712"),
            loinc,
            "96910-5",
            ObservationCategory.LABORATORY);

    assertMatched(condition);
    assertEquals("186747009", condition.getValue1().getCode());
    assertEquals("http://snomed.info/sct", condition.getValue1().getCodeSystem());
    assertEquals("1.2.3", condition.getValue1().getValueSetOid());
    assertMatched(report);
    assertEquals("96910-5", report.getValue1().getCode());
    assertMatched(request);
    assertEquals("89648-0", request.getValue1().getCode());
    assertMatched(vaccine);
    assertEquals("18", vaccine.getValue1().getCode());
    assertMatched(lab);
    assertEquals("96910-5", lab.getValue1().getCode());
  }

  @Test
  public void resourceHasMatchedCode_observation_usesComponentCodeOnlyForLaboratory() {
    Observation exposure =
        (Observation)
            TestUtils.loadBundleFromFile("R4/LoadingQueryBundle.json").getEntry().stream()
                .map(e -> e.getResource())
                .filter(r -> r.getIdElement().getIdPart().startsWith("e23e9d00"))
                .findFirst()
                .get();

    Pair<Boolean, ReportableMatchedTriggerCode> laboratory =
        execute(exposure, "http://loinc.org", "87729-0", ObservationCategory.LABORATORY);
    Pair<Boolean, ReportableMatchedTriggerCode> socialHistory =
        execute(exposure, "http://loinc.org", "87729-0", ObservationCategory.SOCIALHISTORY);

    assertMatched(laboratory);
    assertEquals("87729-0", laboratory.getValue1().getCode());
    assertFalse(socialHistory.getValue0());
    assertNull(socialHistory.getValue1());
  }

  @Test
  public void resourceHasMatchedCode_medicationsAndProcedure_matchOnRealCodes() {
    MedicationAdministration administration =
        TestUtils.loadResourceDataFromFile(
            MedicationAdministration.class,
            "R4/Medication/MedicationAdministration_codeableMedication.json");
    MedicationRequest request =
        TestUtils.loadResourceDataFromFile(
            MedicationRequest.class, "R4/Medication/MedicationRequestwithCodeabeMedication.json");
    MedicationStatement statement = new MedicationStatement();
    statement.setMedication(administration.getMedicationCodeableConcept());
    Procedure procedure =
        TestUtils.loadResourceDataFromFile(Procedure.class, "R4/Procedure/Procedure.json");
    String rxnorm = "http://www.nlm.nih.gov/research/umls/rxnorm";

    Pair<Boolean, ReportableMatchedTriggerCode> given =
        execute(administration, rxnorm, "161", null);
    Pair<Boolean, ReportableMatchedTriggerCode> ordered = execute(request, rxnorm, "6470", null);
    Pair<Boolean, ReportableMatchedTriggerCode> taken = execute(statement, rxnorm, "161", null);
    Pair<Boolean, ReportableMatchedTriggerCode> performed =
        execute(procedure, "http://snomed.info/sct", "80146002", null);

    assertMatched(given);
    assertEquals("161", given.getValue1().getCode());
    assertMatched(ordered);
    assertEquals("6470", ordered.getValue1().getCode());
    assertMatched(taken);
    assertEquals("161", taken.getValue1().getCode());
    assertMatched(performed);
    assertEquals("80146002", performed.getValue1().getCode());
  }

  @Test
  public void resourceHasMatchedCode_medicationReferencesAndOtherResources_areNotMatched() {
    MedicationRequest byReference =
        TestUtils.loadResourceDataFromFile(
            MedicationRequest.class, "R4/Medication/MedicationRequest_medication_Reference.json");
    MedicationStatement statementByReference =
        TestUtils.loadResourceDataFromFile(
            MedicationStatement.class,
            "R4/Medication/MedicationStatement_reference_medication.json");
    Patient patient = TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    MedicationAdministration administration =
        TestUtils.loadResourceDataFromFile(
            MedicationAdministration.class,
            "R4/Medication/MedicationAdministration_hasRequest.json");

    assertFalse(execute(byReference, "http://snomed.info/sct", "1", null).getValue0());
    assertFalse(execute(statementByReference, "http://snomed.info/sct", "1", null).getValue0());
    assertFalse(execute(patient, "http://snomed.info/sct", "1", null).getValue0());
    assertFalse(execute(administration, "http://snomed.info/sct", "1", null).getValue0());
  }

  @Test
  public void addExtensionIfAppropriate_realTriggerCondition_addsTheTriggerCodeExtension() {
    Condition condition =
        (Condition)
            TestUtils.loadBundleFromFile("R4/LoadingQueryBundle.json").getEntry().stream()
                .map(e -> e.getResource())
                .filter(r -> r.getIdElement().getIdPart().startsWith("390cf06f"))
                .findFirst()
                .get();
    CheckTriggerCodeStatus ctcs =
        createCheckTriggerCodeStatus("http://snomed.info/sct", "186747009");
    ctcs.getMatchedCodes().get(0).setMatchedPath("Condition");
    ctcs.setActionType(CHECK_TRIGGER_CODES);
    karProcessingData.addActionStatus("trigger", ctcs);
    Reference ref = new Reference("Condition/" + condition.getIdElement().getIdPart());
    Reference prefixed = new Reference("Condition/" + condition.getIdElement().getIdPart());

    ecrReportCreator.addExtensionIfAppropriate(
        ref, condition, karProcessingData, ResourceType.Condition, null);
    ctcs.getMatchedCodes().get(0).setValueSetOid("urn:oid:2.16.840.1.113762");
    ecrReportCreator.addExtensionIfAppropriate(
        prefixed, condition, karProcessingData, ResourceType.Condition, null);

    Extension flag =
        ref.getExtensionByUrl(
            "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-trigger-code-flag-extension");
    assertEquals(
        "urn:oid:1.2.3", flag.getExtensionByUrl("triggerCodeValueSet").getValue().primitiveValue());
    assertEquals(
        "1", flag.getExtensionByUrl("triggerCodeValueSetVersion").getValue().primitiveValue());
    Coding triggerCode = (Coding) flag.getExtensionByUrl("triggerCode").getValue();
    assertEquals("http://snomed.info/sct", triggerCode.getSystem());
    assertEquals("186747009", triggerCode.getCode());
    assertEquals(
        "urn:oid:2.16.840.1.113762",
        prefixed
            .getExtension()
            .get(0)
            .getExtensionByUrl("triggerCodeValueSet")
            .getValue()
            .primitiveValue());
  }

  @Test
  public void addExtensionIfAppropriate_withoutTriggerStatusOrMatch_addsNothing() {
    Condition condition =
        TestUtils.loadResourceDataFromFile(
            Condition.class, "R4/Condition/Condition-code-inactive.json");
    Reference noStatus = new Reference("Condition/1");
    Reference noMatchForType = new Reference("Condition/2");
    Reference otherCode = new Reference("Condition/3");

    ecrReportCreator.addExtensionIfAppropriate(
        noStatus, condition, new KarProcessingData(), ResourceType.Condition, null);
    CheckTriggerCodeStatus observationOnly =
        createCheckTriggerCodeStatus("http://snomed.info/sct", "186747009");
    observationOnly.getMatchedCodes().get(0).setMatchedPath("Observation");
    observationOnly.setActionType(CHECK_TRIGGER_CODES);
    karProcessingData.addActionStatus("trigger", observationOnly);
    ecrReportCreator.addExtensionIfAppropriate(
        noMatchForType, condition, karProcessingData, ResourceType.Condition, null);
    observationOnly.getMatchedCodes().get(0).setMatchedPath("Condition");
    ecrReportCreator.addExtensionIfAppropriate(
        otherCode, condition, karProcessingData, ResourceType.Condition, null);

    assertFalse(noStatus.hasExtension());
    assertFalse(noMatchForType.hasExtension());
    assertFalse(otherCode.hasExtension());
  }
}
