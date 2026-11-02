package com.drajer.bsa.kar.action;

import static com.drajer.bsa.model.BsaTypes.ActionType.CHECK_TRIGGER_CODES;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

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
import com.drajer.eca.model.MatchedTriggerCodes;
import com.drajer.ecrapp.security.AESEncryption;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.hl7.fhir.r4.model.Bundle;
import org.hl7.fhir.r4.model.DataRequirement;
import org.hl7.fhir.r4.model.Encounter;
import org.hl7.fhir.r4.model.MeasureReport;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;
import org.hl7.fhir.r4.model.ResourceType;
import org.hl7.fhir.r4.model.UriType;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

public class EcrMeasureReportCreatorTest {

  private static final String PROFILE =
      "http://hl7.org/fhir/us/ecr/StructureDefinition/eicr-document-bundle";

  private final FhirContext r4Context = FhirContext.forR4();
  private EhrQueryService ehrService;
  private KarProcessingData data;
  private BsaAction action;
  private Patient patient;
  private EcrMeasureReportCreator creator;

  @Before
  public void setUp() {
    ReflectionTestUtils.setField(AESEncryption.class, "secretKey", "123");
    ehrService = mock(EhrQueryService.class);
    creator = new EcrMeasureReportCreator();

    NotificationContext context =
        TestUtils.readFileContents(
            "Bsa/NotificationContext/NotificationContext.json",
            new TypeReference<NotificationContext>() {});
    context.setNotificationData(
        r4Context
            .newJsonParser()
            .encodeResourceToString(
                TestUtils.loadBundleFromFile(
                    "Bsa/NotificationBundleEncounterCloseWithoutPeriord.json")));
    context.setNotificationResourceType("Encounter");

    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setReceiverAddresses(Collections.singleton(new UriType("http://receiver1.example.com")));

    KnowledgeArtifactStatus karStatus = new KnowledgeArtifactStatus();
    karStatus.setId(1);
    karStatus.setIsActive(true);
    karStatus.setOutputFormat(BsaTypes.OutputContentType.FHIR);
    karStatus.setKarVersion("1.0.0");
    karStatus.setKarId("rctc-release-2023-02-03-Bundle-rctc");
    karStatus.setLastActivationDate(new Date());
    karStatus.setSubscriptionsEnabled(false);
    karStatus.setCovidOnly(false);

    MatchedTriggerCodes matched = new MatchedTriggerCodes();
    matched.setMatchedPath("Condition");
    matched.addCode("Condition");
    CheckTriggerCodeStatus triggerStatus = new CheckTriggerCodeStatus();
    triggerStatus.setActionId("action1");
    triggerStatus.setActionType(CHECK_TRIGGER_CODES);
    triggerStatus.setMatchedCodes(new ArrayList<>(Collections.singletonList(matched)));
    HashMap<String, List<BsaActionStatus>> actionStatus = new HashMap<>();
    actionStatus.put("trigger", new ArrayList<>(Collections.singletonList(triggerStatus)));

    data = new KarProcessingData();
    data.setPhm(null);
    data.setNotificationContext(context);
    //    data.setScheduledJobData(
    //        new ScheduledJobData.Builder()
    //            .karExecutionStateId(UUID.randomUUID())
    //            .actionType(BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW)
    //            .expirationTime(Instant.now())
    //            .xRequestId("32")
    //            .jobType(BsaTypes.BsaJobType.IMMEDIATE_REPORTING)
    //            .mdcContext(new HashMap<>())
    //            .build());
    data.setScheduledJobData(
        new ScheduledJobData(
            UUID.randomUUID(), // karExecutionStateId
            null, // actionId
            BsaTypes.ActionType.EXECUTE_REPORTING_WORKFLOW, // actionType
            Instant.now(), // expirationTime
            null, // jobId
            "32", // xRequestId
            BsaTypes.BsaJobType.IMMEDIATE_REPORTING, // jobType
            new HashMap<>())); // mdcContext
    data.setHealthcareSetting(
        (HealthcareSetting)
            TestUtils.getResourceAsObject("Bsa/HealthCareSettings.json", HealthcareSetting.class));
    data.setxRequestId("32");
    data.setNotificationBundle(
        (Bundle) r4Context.newJsonParser().parseResource(context.getNotificationData()));
    HashMap<ResourceType, Set<Resource>> loadingQueryData = new HashMap<>();
    for (Bundle.BundleEntryComponent entry :
        TestUtils.loadBundleFromFile("R4/LoadingQueryBundle.json").getEntry()) {
      loadingQueryData
          .computeIfAbsent(entry.getResource().getResourceType(), k -> new HashSet<>())
          .add(entry.getResource());
    }
    data.setFhirInputDataByType(loadingQueryData);
    data.setKar(kar);
    data.setKarStatus(karStatus);
    data.setActionStatus(actionStatus);

    patient = TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json");
    DataRequirement patientRequirement = new DataRequirement();
    patientRequirement.setType("Patient");
    patientRequirement.setId("1234");
    DataRequirement encounterRequirement = new DataRequirement();
    encounterRequirement.setType("Encounter");
    encounterRequirement.setId("123");
    action = new CreateReport();
    action.setInputDataIdToRelatedDataIdMap(new HashMap<>(Map.of("1234", "1234", "123", "123")));
    action.setInputData(new ArrayList<>(List.of(patientRequirement, encounterRequirement)));
    HashMap<String, Set<Resource>> outputById = new HashMap<>();
    outputById.put("1234", new HashSet<>(Collections.singleton(patient)));
    outputById.put(
        "123",
        new HashSet<>(
            Collections.singleton(
                TestUtils.loadResourceDataFromFile(
                    Encounter.class, "R4/Encounter/Encounter_97953900.json"))));
    data.setActionOutputDataById(outputById);
  }

  @Test
  public void testCreateReport_addsCollectionBundleCarryingInputDataAndMeasureReports() {
    int baselineEntries = baselineEntryCount();
    MeasureReport measureReport = new MeasureReport();
    measureReport.setId("measure-report-1");
    measureReport.setStatus(MeasureReport.MeasureReportStatus.COMPLETE);
    measureReport.setType(MeasureReport.MeasureReportType.SUMMARY);
    Set<Resource> measureReports = new HashSet<>();
    measureReports.add(measureReport);
    data.addResourcesByType(ResourceType.MeasureReport, measureReports);

    Bundle report =
        (Bundle)
            creator.createReport(
                data,
                ehrService,
                new HashSet<>(Collections.singleton(patient)),
                "example",
                PROFILE,
                action);

    assertNotNull(report);
    assertEquals(
        "exactly one entry is added to the eCR bundle",
        baselineEntries + 1,
        report.getEntry().size());

    Bundle.BundleEntryComponent added = report.getEntry().get(report.getEntry().size() - 1);
    Bundle measureBundle = (Bundle) added.getResource();
    assertEquals(Bundle.BundleType.COLLECTION, measureBundle.getType());
    assertNotNull(measureBundle.getTimestamp());
    assertEquals(2, measureBundle.getEntry().size());
    assertTrue(
        measureBundle.getEntry().stream().anyMatch(e -> e.getResource() instanceof MeasureReport));
    assertTrue(measureBundle.getEntry().stream().anyMatch(e -> e.getResource() instanceof Patient));
    assertEquals("http://localhost:9011/fhir/Bundle/" + measureBundle.getId(), added.getFullUrl());
  }

  @Test
  public void testCreateReport_withInputDataOnlyStillAddsTheCollectionBundle() {
    int baselineEntries = baselineEntryCount();

    Bundle report =
        (Bundle)
            creator.createReport(
                data,
                ehrService,
                new HashSet<>(Collections.singleton(patient)),
                "example",
                PROFILE,
                action);

    assertEquals(baselineEntries + 1, report.getEntry().size());
    Bundle measureBundle =
        (Bundle) report.getEntry().get(report.getEntry().size() - 1).getResource();
    assertEquals(1, measureBundle.getEntry().size());
    assertTrue(measureBundle.getEntry().get(0).getResource() instanceof Patient);
  }

  @Test
  public void testCreateReport_withNothingToCarryAddsNoEntry() {
    int baselineEntries = baselineEntryCount();

    Bundle report =
        (Bundle)
            creator.createReport(data, ehrService, new HashSet<>(), "example", PROFILE, action);

    assertEquals(baselineEntries, report.getEntry().size());
  }

  private int baselineEntryCount() {
    Bundle baseline =
        (Bundle)
            new EcrReportCreator()
                .createReport(data, ehrService, new HashSet<>(), "example", PROFILE, action);
    return baseline.getEntry().size();
  }
}
