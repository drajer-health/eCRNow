package com.drajer.bsa.kar.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import com.drajer.bsa.dao.PublicHealthMessagesDao;
import com.drajer.bsa.ehr.service.EhrQueryService;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.BsaTypes;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.bsa.scheduler.ScheduledJobData;
import com.drajer.eca.model.TimingSchedule;
import java.util.*;
import org.hl7.fhir.r4.model.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(MockitoJUnitRunner.class)
public class CreateReportTest {

  @InjectMocks CreateReport createReport;

  @Mock EhrQueryService ehrService;

  @Mock PublicHealthMessagesDao mockPhDao;

  @Mock ReportCreator mockReportCreator;

  @Before
  public void setUp() {
    String logDir = "java.io.tmpdir";
    ReflectionTestUtils.setField(createReport, "jsonParser", FhirContext.forR4().newJsonParser());
    ReflectionTestUtils.setField(createReport, "xmlParser", FhirContext.forR4().newXmlParser());
    ReflectionTestUtils.setField(createReport, "logDirectory", logDir);
    Map<String, ReportCreator> testRegistry = new HashMap<>();
    testRegistry.put(
        "http://example.org/fhir/StructureDefinition/overridden-profile", mockReportCreator);
    ReflectionTestUtils.setField(ReportCreator.class, "reportingClasses", testRegistry);
  }

  @Test
  public void testProcess_withConditionsMet_QueriesIsNotNull() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    FhirQueryFilter fhirQueryFilter = new FhirQueryFilter();
    fhirQueryFilter.setQueryString("query");
    fhirQueryFilter.setDataReqId("reqid");
    fhirQueryFilter.setRelatedDataId("RelatedDataId");
    Map<String, FhirQueryFilter> queries = new HashMap<>();
    queries.put("Observation", fhirQueryFilter);
    ReflectionTestUtils.setField(createReport, "inputDataRequirementQueries", queries);
    Map<ResourceType, Set<Resource>> Data = new HashMap<>();
    Practitioner practitioner = new Practitioner();
    practitioner.setId("Practitioner/123");
    Set<Resource> practitioners = new HashSet<>();
    practitioners.add(practitioner);
    Data.put(ResourceType.Practitioner, practitioners);

    when(ehrService.loadJurisdicationData(data)).thenReturn(Data);

    BsaActionStatus result = createReport.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    verify(ehrService).executeQuery(eq(data), eq("Observation"), eq(fhirQueryFilter));
  }

  @Test
  public void testProcess_withConditionsMet_QueriesIsNull() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    Map<ResourceType, Set<Resource>> Data = new HashMap<>();
    Practitioner practitioner = new Practitioner();
    practitioner.setId("Practitioner/123");
    Set<Resource> practitioners = new HashSet<>();
    practitioners.add(practitioner);
    Data.put(ResourceType.Practitioner, practitioners);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("id");
    dataRequirement.setType("type");
    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("id1");
    dataRequirement1.setType("type1");
    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);
    ReflectionTestUtils.setField(createReport, "inputData", listdr);
    when(ehrService.loadJurisdicationData(data)).thenReturn(Data);

    BsaActionStatus result = createReport.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    verify(ehrService).getFilteredData(data, listdr);
  }

  @Test
  public void testProcess_withConditionsMet() {
    createReport.setActionId("A-id");
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    Map<ResourceType, Set<Resource>> Data = new HashMap<>();
    Practitioner practitioner = new Practitioner();
    practitioner.setId("Practitioner/123");
    Set<Resource> practitioners = new HashSet<>();
    practitioners.add(practitioner);
    Data.put(ResourceType.Practitioner, practitioners);
    DataRequirement dataRequirement = new DataRequirement();
    dataRequirement.setId("id");
    dataRequirement.setType("type");
    DataRequirement dataRequirement1 = new DataRequirement();
    dataRequirement1.setId("id1");
    dataRequirement1.setType("type1");
    CanonicalType profile =
        new CanonicalType("http://example.org/fhir/StructureDefinition/sample-report-profile");
    profile.setValueAsString("http://example.org/fhir/StructureDefinition/overridden-profile");
    List<CanonicalType> profileLists = new ArrayList<>();
    profileLists.add(profile);
    dataRequirement.setProfile(profileLists);
    CanonicalType profile1 =
        new CanonicalType("http://example.org/fhir/StructureDefinition/sample-report-profile");
    profile1.setValueAsString("http://example.org/fhir/StructureDefinition/overridden-profile");
    List<CanonicalType> profileList = new ArrayList<>();
    profileList.add(profile1);
    dataRequirement1.setProfile(profileList);
    List<DataRequirement> listdr = new ArrayList<>();
    listdr.add(dataRequirement);
    listdr.add(dataRequirement1);
    ReflectionTestUtils.setField(createReport, "outputData", listdr);
    Patient patient = new Patient();
    patient.setId("Patient/123");
    patient.addName().setFamily("Doe").addGiven("John");
    patient.setGender(Enumerations.AdministrativeGender.MALE);

    when(mockReportCreator.createReport(any(), any(), any(), anyString(), anyString(), any()))
        .thenReturn(patient);

    BsaActionStatus result = createReport.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.COMPLETED, result.getActionStatus());
    assertEquals("A-id", result.getActionId());
  }

  @Test
  public void testProcess_withScheduled() {
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    NotificationContext context = new NotificationContext();
    context.setPatientId("patient-123");
    context.setId(UUID.randomUUID());
    context.setNotificationProcessingStatus("SUSPENDED");
    context.setEncounterEndTime(new Date());
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    TimingSchedule timingSchedule = mock(TimingSchedule.class);
    List<TimingSchedule> timingSchedules = new ArrayList<>();
    timingSchedules.add(timingSchedule);
    ReflectionTestUtils.setField(createReport, "timingData", timingSchedules);
    ReflectionTestUtils.setField(createReport, "ignoreTimers", false);

    BsaActionStatus result = createReport.process(data, ehrService);

    assertNotNull(result);
    assertEquals(BsaTypes.BsaActionStatusType.SCHEDULED, result.getActionStatus());
  }

  @Test
  public void testCreatePublicHealthMessageForCda() {
    String actionType = "CreateReport";
    String actionId = "A-id";
    Attachment attachment = mock(Attachment.class);
    DocumentReference.DocumentReferenceContentComponent contentComponent =
        new DocumentReference.DocumentReferenceContentComponent();
    contentComponent.setAttachment(attachment);
    DocumentReference docRef = new DocumentReference();
    docRef.setId("DocumentReference/doc-123");
    docRef.setSubject(new Reference("Patient/123"));
    docRef.addContent(contentComponent);
    MessageHeader header = new MessageHeader();
    header.setId("header-123");
    header.getEventCoding().setCode("event-code");
    Bundle bundle = new Bundle();
    bundle.addEntry().setResource(header);
    bundle.addEntry().setResource(docRef);
    NotificationContext context = new NotificationContext();
    context.setFhirServerBaseUrl("http://example.com/fhir");
    context.setPatientId("Patient/123");
    context.setNotificationResourceId("Encounter/456");
    context.setNotificationResourceType("Encounter");
    context.setId(UUID.randomUUID());
    CheckTriggerCodeStatus status = new CheckTriggerCodeStatus();
    status.setActionId("a-id");
    status.setTriggerMatchStatus(true);
    Set<CheckTriggerCodeStatus> statuses = new HashSet<>();
    statuses.add(status);
    CheckTriggerCodeStatusList statusList = new CheckTriggerCodeStatusList();
    statusList.setStatuses(statuses);
    KnowledgeArtifact kar = new KnowledgeArtifact();
    kar.setKarId("K-id");
    kar.setKarName("kar name");
    kar.setKarVersion("r4");
    KarProcessingData data = new KarProcessingData();
    data.setKar(kar);
    data.setNotificationContext(context);
    data.setxRequestId("x-request-12345");
    data.setxCorrelationId("x-correlation-67890");
    ScheduledJobData scheduledJobData = mock(ScheduledJobData.class);
    data.setScheduledJobData(scheduledJobData);
    PublicHealthMessage publicHealthMessage = new PublicHealthMessage();
    publicHealthMessage.setSubmittedVersionNumber(2);
    data.setPhm(publicHealthMessage);
    data.setCurrentTriggerMatchStatus(statusList);

    lenient().when(mockPhDao.getMaxVersionId(any())).thenReturn(1);
    when(attachment.getData()).thenReturn("data".getBytes());

    createReport.createPublicHealthMessageForCda(data, actionType, bundle, actionId);

    verify(attachment).getData();
    verify(scheduledJobData).getJobId();
  }
}
