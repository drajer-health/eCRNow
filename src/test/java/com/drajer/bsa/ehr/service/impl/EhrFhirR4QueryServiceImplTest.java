package com.drajer.bsa.ehr.service.impl;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.gclient.*;
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException;
import com.drajer.bsa.auth.AuthorizationUtils;
import com.drajer.bsa.dao.HealthcareSettingsDao;
import com.drajer.bsa.kar.model.FhirQueryFilter;
import com.drajer.bsa.kar.model.KnowledgeArtifact;
import com.drajer.bsa.model.HealthcareSetting;
import com.drajer.bsa.model.KarProcessingData;
import com.drajer.bsa.model.NotificationContext;
import com.drajer.bsa.model.PublicHealthMessage;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.test.util.TestUtils;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.r4.model.*;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.util.ReflectionTestUtils;

public class EhrFhirR4QueryServiceImplTest {

  @InjectMocks private EhrFhirR4QueryServiceImpl ehrFhirR4QueryService;

  @Mock private AuthorizationUtils authUtils;

  @Mock private HealthcareSettingsDao hsDao;

  @Mock private FhirContextInitializer fhirContextInitializer;

  @Mock private FhirOperationExecutor fhirOperationExecutor;

  @Mock private FhirContext fhirContext;

  @Mock private IGenericClient fhirClient;

  @Mock private KarProcessingData karProcessingData;

  @Mock private NotificationContext notificationContext;

  @Mock private HealthcareSetting healthcareSetting;

  @Mock private PublicHealthMessage publicHealthMessage;

  @Mock private IDelete delete;

  @Mock private IDeleteTyped deleteTyped;

  @Mock private ICreate create;

  @Mock private ICreateTyped createTyped;

  @Mock private IUpdate update;

  @Mock private IUpdateTyped iUpdateTyped;

  @Mock private JSONObject tokenResponse;

  @Mock private IUntypedQuery iUntypedQuery;

  @Mock private IQuery iQuery;

  @Mock private FhirQueryFilter fhirQueryFilter;

  @Mock private KnowledgeArtifact knowledgeArtifact;

  @Mock private IGetPage iGetPage;

  @Mock private IGetPageTyped iGetPageTyped;

  @Mock private IRead iRead;

  @Mock private IReadTyped iReadTyped;

  @Mock private IReadExecutable iReadExecutable;

  private HashMap<String, HashMap<String, String>> customQueries;
  static final String ENCOUNTER_WITH_PARTICIPANT_JSON =
      "R4/Encounter/Encounter_withOneParticipant.json";
  static final String ENCOUNTER_BUNDLE_JSON = "R4/Encounter/EncounterBundle_97953900.json";
  static final String ENCOUNTER_JSON = "R4/Encounter/enc.json";
  static final String ENCOUNTER_CONTEXT_JSON = "R4/Encounter/encounter.json";
  static final String CONDITION_JSON = "R4/Condition/Condition.json";
  static final String CONDITION_FILTER_BY_DTTM_JSON = "R4/Condition/Condition_FilterByDtTm.json";
  static final String CONDITION_CODE_INACTIVE_JSON = "R4/Condition/Condition-code-inactive.json";
  static final String OBSERVATION_PERFORMER_JSON = "R4/Observation/Observation_performer.json";
  static final String SERVICEREQUEST_REVOKED_JSON =
      "R4/ServiceRequest/ServiceRequest_status_revoked.json";
  static final String DIAGNOSTICREPORT_WITH_SPECIMEN_JSON =
      "R4/DiagnosticReport/Diagnosticreport_with_Specimen.json";
  static final String IMMUNIZATION_NOTDONE_JSON =
      "R4/Immunization/Immunization_status_notdone.json";
  static final String IMMUNIZATION_RESOURCE_JSON = "R4/Immunization/ImmunizationResource.json";
  static final String MEDICATION_JSON = "R4/Medication/Medication.json";
  static final String MEDREQ_CANCEL_JSON = "R4/Medication/MedicationRequest_status_cancel.json";
  static final String MEDREQ_REFERENCE_JSON =
      "R4/Medication/MedicationRequest_medication_Reference.json";
  static final String MEDADM_STOPPED_JSON =
      "R4/Medication/MedicationAdministration_status_stopped.json";
  static final String MEDADM_HAS_REQUEST_JSON =
      "R4/Medication/MedicationAdministration_hasRequest.json";
  static final String MEDSTMT_STOPPED_JSON =
      "R4/Medication/Medicationstatement_status_stopped.json";
  static final String MEDSTMT_REFERENCE_JSON =
      "R4/Medication/MedicationStatement_reference_medication.json";
  static final String MEDDISPENSE_JSON = "R4/Medication/MedicationDispense.json";

  static final String QUERY_RCTC_100 =
      "AppData/custom-queries/rctc-release-1.2.2.0-Bundle-rctc-1.0.0.queries";
  static final String QUERY_RCTC_101 =
      "AppData/custom-queries/rctc-release-1.2.2.0-Bundle-rctc-1.0.1.queries";

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(karProcessingData.hasValidAccessToken()).thenReturn(true);
    when(karProcessingData.getAccessToken()).thenReturn("mockAccessToken");
    Mockito.lenient().when(fhirContextInitializer.getFhirContext("R4")).thenReturn(fhirContext);
    Mockito.lenient().when(karProcessingData.hasValidAccessToken()).thenReturn(true);
    Mockito.lenient().when(authUtils.getToken(healthcareSetting)).thenReturn(tokenResponse);

    Mockito.lenient().when(karProcessingData.getHealthcareSetting()).thenReturn(healthcareSetting);
    Mockito.lenient()
        .when(karProcessingData.getNotificationContext())
        .thenReturn(notificationContext);
    Mockito.lenient().when(karProcessingData.getPhm()).thenReturn(publicHealthMessage);
    Mockito.lenient().when(publicHealthMessage.getSubmissionTime()).thenReturn(new Date());
    Mockito.lenient().when(notificationContext.getPatientId()).thenReturn("12345");
    Mockito.lenient().when(karProcessingData.getContextPatientId()).thenReturn("patientId");
    Mockito.lenient().when(karProcessingData.getContextEncounterId()).thenReturn("encounterId");

    Mockito.lenient()
        .when(healthcareSetting.getEhrAccessTokenExpirationTime())
        .thenReturn(new Date());
    Mockito.lenient().when(healthcareSetting.getFhirServerBaseURL()).thenReturn("fhirserverurl");
    Mockito.lenient().when(healthcareSetting.getEncounterStartThreshold()).thenReturn("30");
    Mockito.lenient().when(healthcareSetting.getEncounterEndThreshold()).thenReturn("30");
    Mockito.lenient()
        .when(notificationContext.getEhrLaunchContext())
        .thenReturn("ehrLaunchContext");
    Mockito.lenient().when(notificationContext.getxRequestId()).thenReturn("32");
    Mockito.lenient().when(notificationContext.getEncounterStartTime()).thenReturn(new Date());
    Mockito.lenient().when(notificationContext.getEncounterEndTime()).thenReturn(new Date());

    Mockito.lenient()
        .when(
            fhirContextInitializer.createClient(any(FhirContext.class), any(), any(), any(), any()))
        .thenReturn(fhirClient);
    Mockito.lenient().when(fhirClient.delete()).thenReturn(delete);
    Mockito.lenient().when(delete.resourceById(any(), any())).thenReturn(deleteTyped);

    when(fhirClient.search()).thenReturn(iUntypedQuery);
    when(iUntypedQuery.byUrl(anyString())).thenReturn(iQuery);
    when(iQuery.returnBundle(any(Class.class))).thenReturn(iQuery);
    Mockito.lenient().when(fhirClient.loadPage()).thenReturn(iGetPage);
    Mockito.lenient().when(iGetPage.next(any(Bundle.class))).thenReturn(iGetPageTyped);
    Mockito.lenient().when(fhirClient.read()).thenReturn(iRead);
    Mockito.lenient().when(iRead.resource(anyString())).thenReturn(iReadTyped);
    //
    Mockito.when(iReadTyped.withId(Mockito.anyString())).thenReturn(iReadExecutable);

    Mockito.lenient().when(fhirQueryFilter.getResourceType()).thenReturn(ResourceType.Encounter);

    // Run the operation passed to FhirOperationExecutor against the mocked fhirClient
    Mockito.lenient()
        .when(fhirOperationExecutor.execute(any(), any(), any()))
        .thenAnswer(
            invocation -> {
              Function<IGenericClient, Object> op = invocation.getArgument(2);
              return op.apply(fhirClient);
            });
  }

  @Test
  public void deleteResourceTest() {
    ehrFhirR4QueryService.deleteResource(karProcessingData, ResourceType.Observation, "123");

    verify(fhirClient, times(1)).delete();
    verify(delete, times(1)).resourceById(eq("Observation"), eq("123"));
    verify(deleteTyped, times(1)).execute();
  }

  @Test
  public void createResourceTest() {
    Mockito.lenient().when(fhirClient.create()).thenReturn(create);
    Mockito.lenient().when(create.resource(any(Resource.class))).thenReturn(createTyped);
    ehrFhirR4QueryService.createResource(karProcessingData, new Observation());

    verify(fhirClient, times(1)).create();
    verify(create, times(1)).resource(any(Resource.class));
    verify(createTyped, times(1)).execute();
  }

  @Test
  public void updateResourceTest() {
    Mockito.lenient().when(fhirClient.update()).thenReturn(update);
    Mockito.lenient().when(update.resource(any(Resource.class))).thenReturn(iUpdateTyped);
    ehrFhirR4QueryService.updateResource(karProcessingData, new Observation());

    verify(fhirClient, times(1)).update();
    verify(update, times(1)).resource(any(Resource.class));
  }

  @Test
  public void getClient_ValidAcesssToken_false_Test() {
    Mockito.lenient().when(karProcessingData.hasValidAccessToken()).thenReturn(false);
    Mockito.lenient().when(authUtils.getToken(healthcareSetting)).thenReturn(tokenResponse);

    ehrFhirR4QueryService.getClient(karProcessingData, fhirContext);
    verify(authUtils, times(1)).getToken(healthcareSetting);
    verify(fhirContextInitializer, times(1))
        .createClient(any(FhirContext.class), any(), any(), any(), any());
    assertEquals("mockAccessToken", karProcessingData.getAccessToken());
    assertFalse(karProcessingData.hasValidAccessToken());
  }

  @Test
  public void loadJurisdicationDataTest() {

    String encounterString = ENCOUNTER_WITH_PARTICIPANT_JSON;
    Encounter encounter = TestUtils.loadResourceDataFromFile(Encounter.class, encounterString);
    Set<Resource> encounterSet = new HashSet<>();
    encounterSet.add(encounter);

    Mockito.lenient()
        .when(karProcessingData.getResourcesByType("Encounter"))
        .thenReturn(encounterSet);

    ehrFhirR4QueryService.loadJurisdicationData(karProcessingData);

    assertEquals(
        karProcessingData.getFhirInputDataByType(),
        ehrFhirR4QueryService.loadJurisdicationData(karProcessingData));
    assertEquals(1, encounterSet.size());
  }

  @Test
  public void getResourceByUrlTest() {
    ehrFhirR4QueryService.getResourceByUrl(karProcessingData, "Observation", "mockserverurl");

    assertTrue(karProcessingData.getFhirInputDataByType().isEmpty());
    verify(fhirContextInitializer, times(1))
        .createClient(any(FhirContext.class), any(), any(), any(), any());
  }

  @Test
  public void getResourcesByPatientIdTest() {
    Bundle bundle = TestUtils.loadBundleFromFile(ENCOUNTER_BUNDLE_JSON);
    when(iQuery.execute()).thenReturn(bundle);
    ehrFhirR4QueryService.getResourcesByPatientId(
        fhirClient,
        fhirContext,
        "Encounter",
        "mocksearchurl",
        karProcessingData,
        ResourceType.Patient,
        "1233345");
    verify(fhirClient, times(1)).search();
    verify(iUntypedQuery, times(1)).byUrl(anyString());
    verify(iQuery, times(1)).returnBundle(any(Class.class));
    verify(iQuery, times(1)).execute();
  }

  @Test
  public void testGetResourcesByPatientId_BaseServerResponseException() {
    BaseServerResponseException exception = mock(BaseServerResponseException.class);
    OperationOutcome operationOutcome = new OperationOutcome();

    when(exception.getOperationOutcome()).thenReturn(operationOutcome);

    when(fhirClient.search()).thenReturn(iUntypedQuery);
    when(iUntypedQuery.byUrl(anyString())).thenReturn(iQuery);
    when(iQuery.returnBundle(any(Class.class))).thenReturn(iQuery);
    when(iQuery.execute()).thenThrow(exception);
    IParser mockParser = mock(IParser.class);
    when(fhirContext.newJsonParser()).thenReturn(mockParser);
    when(mockParser.encodeResourceToString(operationOutcome)).thenReturn("mocked outcome");
    ehrFhirR4QueryService.getResourcesByPatientId(
        fhirClient,
        fhirContext,
        "Patient",
        "mocksearchurl",
        karProcessingData,
        ResourceType.Patient,
        "1233345");
    verify(exception, atLeastOnce()).getOperationOutcome();
    verify(fhirContext).newJsonParser();
    verify(mockParser).encodeResourceToString(operationOutcome);
  }

  @Test
  public void constructR4DocumentReferenceTest() {
    DocumentReference documentReference =
        ehrFhirR4QueryService.constructR4DocumentReference(
            "mockpayload",
            "mockurl",
            "mockid",
            "mocktype",
            "mocktitle",
            "mockdesc",
            "mockcontenturl",
            "mockcontenttype",
            "mockcontentdata");
    assertNotNull(
        ehrFhirR4QueryService.constructR4DocumentReference(
            "mockpayload",
            "mockurl",
            "mockid",
            "mocktype",
            "mocktitle",
            "mockdesc",
            "mockcontenturl",
            "mockcontenttype",
            "mockcontentdata"));
    assertEquals(Enumerations.DocumentReferenceStatus.CURRENT, documentReference.getStatus());
    assertEquals(DocumentReference.ReferredDocumentStatus.FINAL, documentReference.getDocStatus());
    assertNotNull(documentReference);
  }

  @Test
  public void executeSearchQueryTest() {

    Mockito.lenient().when(fhirQueryFilter.getResourceType()).thenReturn(ResourceType.Encounter);
    Bundle bundle = TestUtils.loadBundleFromFile(ENCOUNTER_BUNDLE_JSON);
    when(iQuery.execute()).thenReturn(bundle);
    ehrFhirR4QueryService.executeSearchQuery(karProcessingData, "id", fhirQueryFilter, "mockQuery");

    verify(fhirClient, times(1)).search();
    verify(iUntypedQuery, times(1)).byUrl(anyString());
    verify(iQuery, times(1)).returnBundle(any(Class.class));
    verify(iQuery, times(1)).execute();
  }

  @Test
  public void executeSearchQueryTest_BaseServerResponseException() {

    Mockito.lenient().when(fhirQueryFilter.getResourceType()).thenReturn(ResourceType.Encounter);
    BaseServerResponseException exception = mock(BaseServerResponseException.class);
    OperationOutcome operationOutcome = new OperationOutcome();
    when(exception.getOperationOutcome()).thenReturn(operationOutcome);
    when(iQuery.execute()).thenThrow(exception);

    IParser mockParser = mock(IParser.class);
    when(fhirContext.newJsonParser()).thenReturn(mockParser);
    when(mockParser.encodeResourceToString(operationOutcome)).thenReturn("mocked Outcome");

    ehrFhirR4QueryService.executeSearchQuery(karProcessingData, "id", fhirQueryFilter, "mockQuery");

    verify(fhirClient, times(1)).search();
    verify(iUntypedQuery, times(1)).byUrl(anyString());
    verify(iQuery, times(1)).returnBundle(any(Class.class));
    verify(iQuery, times(1)).execute();
    verify(exception, atLeastOnce()).getOperationOutcome();
    verify(mockParser, atLeastOnce()).encodeResourceToString(operationOutcome);
  }

  @Test
  public void executeQueryTest() throws IOException {
    File queryFile = new ClassPathResource(QUERY_RCTC_100).getFile();
    HashMap<String, HashMap<String, String>> customQueriesMap = processQueryFile(queryFile);
    ReflectionTestUtils.setField(ehrFhirR4QueryService, "customQueries", customQueriesMap);

    String baseName = FilenameUtils.getBaseName(queryFile.getName());

    when(karProcessingData.getKarIdForCustomQueries()).thenReturn(baseName);
    when(karProcessingData.getKar()).thenReturn(knowledgeArtifact);
    when(karProcessingData.getNotificationContext()).thenReturn(notificationContext);
    when(notificationContext.getFhirServerBaseUrl()).thenReturn("mockfhirserverurl");
    Bundle bundle = TestUtils.loadBundleFromFile(ENCOUNTER_BUNDLE_JSON);
    when(iQuery.execute()).thenReturn(bundle);

    Bundle.BundleLinkComponent nextLink = new Bundle.BundleLinkComponent();
    nextLink.setRelation(IBaseBundle.LINK_NEXT);
    nextLink.setUrl("https://fhir-ehr-code.cerner.com/r4/Encounter?_page=2");
    bundle.addLink(nextLink);

    Bundle nextBundle = new Bundle();
    Mockito.lenient().when(iGetPageTyped.execute()).thenReturn(nextBundle).thenReturn(new Bundle());

    ehrFhirR4QueryService.executeQuery(
        karProcessingData, "suspectedDisorderConditions", fhirQueryFilter);

    verify(fhirClient, times(1)).search();
    verify(iUntypedQuery, times(1)).byUrl(anyString());
    verify(iQuery, times(1)).returnBundle(any(Class.class));
    verify(iQuery, times(1)).execute();
  }

  @Test
  public void getFilteredDataTest() {
    Map<String, ResourceType> resTypes = new HashMap<>();
    resTypes.put("mrdata", ResourceType.MedicationRequest);
    Map<ResourceType, Set<Resource>> setMap = new HashMap<>();
    Set<Resource> patientSet = new HashSet<>();
    patientSet.add(new Patient().setId("patientId"));
    setMap.put(ResourceType.Patient, patientSet);
    Patient mockPatient = new Patient();
    mockPatient.setId("patientId");
    when(iReadExecutable.execute()).thenReturn(mockPatient);

    Resource resource = TestUtils.loadResourceDataFromFile(Encounter.class, ENCOUNTER_JSON);
    Mockito.when(iReadExecutable.execute()).thenReturn(resource);

    when(notificationContext.getPatientId()).thenReturn("patientId");
    when(notificationContext.getNotificationResourceType())
        .thenReturn(ResourceType.Encounter.toString());
    when(notificationContext.getNotificationResourceId()).thenReturn("encounterId");
    when(karProcessingData.getNotificationContext()).thenReturn(notificationContext);
    when(karProcessingData.getFhirInputDataByType())
        .thenReturn((HashMap<ResourceType, Set<Resource>>) setMap);

    ehrFhirR4QueryService.getFilteredData(karProcessingData, resTypes);
    assertEquals(1, karProcessingData.getFhirInputDataByType().size());
    verify(iRead, times(1)).resource("Patient");
  }

  @Test
  public void getFilteredDataTest_condition() {
    Map<String, ResourceType> resTypes = new HashMap<>();
    resTypes.put("Condition", ResourceType.Condition);
    Map<ResourceType, Set<Resource>> setMap = new HashMap<>();
    Set<Resource> patientSet = new HashSet<>();
    patientSet.add(new Patient().setId("patientId"));
    setMap.put(ResourceType.Patient, patientSet);
    Patient mockPatient = new Patient();
    mockPatient.setId("patientId");
    when(iReadExecutable.execute()).thenReturn(mockPatient);

    Resource resource = TestUtils.loadResourceDataFromFile(Condition.class, CONDITION_JSON);
    Mockito.when(iReadExecutable.execute()).thenReturn(resource);

    Bundle bundle = TestUtils.loadBundleFromFile(CONDITION_FILTER_BY_DTTM_JSON);
    when(iQuery.execute()).thenReturn(bundle);

    when(notificationContext.getPatientId()).thenReturn("patientId");
    when(notificationContext.getNotificationResourceType())
        .thenReturn(ResourceType.Encounter.toString());
    when(notificationContext.getNotificationResourceId()).thenReturn("encounterId");
    when(karProcessingData.getNotificationContext()).thenReturn(notificationContext);
    when(karProcessingData.getFhirInputDataByType())
        .thenReturn((HashMap<ResourceType, Set<Resource>>) setMap);

    Map<ResourceType, Set<Resource>> actual =
        ehrFhirR4QueryService.getFilteredData(karProcessingData, resTypes);

    assertNotNull(actual);
    assertEquals(1, actual.size());
    verify(fhirClient, times(1)).search();
    verify(iUntypedQuery, times(1)).byUrl(anyString());
    verify(iQuery, times(1)).returnBundle(any(Class.class));
  }

  @Test
  public void initializeCustomQueriesTest() {
    ReflectionTestUtils.setField(
        ehrFhirR4QueryService, "customQueryDirectory", "src/test/resources/AppData/custom-queries");
    ehrFhirR4QueryService.initializeCustomQueries();
    HashMap<String, HashMap<String, String>> customQueries1 =
        (HashMap<String, HashMap<String, String>>)
            ReflectionTestUtils.getField(ehrFhirR4QueryService, "customQueries");
    assertNotNull(customQueries1);
  }

  @Test
  public void substituteContextParamsTest_with_Encounter_start_end_Date() {
    String queryToExecute = TestUtils.getFileContentAsString(QUERY_RCTC_100);
    Encounter encounter =
        TestUtils.loadResourceDataFromFile(Encounter.class, ENCOUNTER_CONTEXT_JSON);
    Mockito.lenient().when(karProcessingData.getContextEncounter()).thenReturn(encounter);
    ehrFhirR4QueryService.substituteContextParams(karProcessingData, queryToExecute, true);
    verify(karProcessingData, times(2)).getContextEncounter();
  }

  @Test
  public void substituteContextParamsTest_with_Last_Sub() {
    String queryToExecute = TestUtils.getFileContentAsString(QUERY_RCTC_101);
    Encounter encoounter =
        TestUtils.loadResourceDataFromFile(Encounter.class, ENCOUNTER_CONTEXT_JSON);
    Mockito.lenient().when(karProcessingData.getContextEncounter()).thenReturn(encoounter);
    ehrFhirR4QueryService.substituteContextParams(karProcessingData, queryToExecute, true);
    verify(karProcessingData, times(2)).getContextEncounter();
  }

  @Test
  public void substituteContextParamsTest_encounter_null() {
    String queryToExecute = TestUtils.getFileContentAsString(QUERY_RCTC_100);
    String actual =
        ehrFhirR4QueryService.substituteContextParams(karProcessingData, queryToExecute, false);
    assertNotNull(actual);
    assertFalse(actual.contains("{{context.encounterId}}"));
  }

  @Test
  public void fetchResourcesTest() {
    Bundle bundle = TestUtils.loadBundleFromFile(ENCOUNTER_BUNDLE_JSON);
    when(iQuery.execute()).thenReturn(bundle);
    Set<Resource> actual =
        ehrFhirR4QueryService.fetchResources(
            fhirClient, karProcessingData, fhirContext, "Encounter?patient=Patient/1234");
    assertNotNull(actual);
    verify(fhirClient, times(1)).search();
    verify(iUntypedQuery, times(1)).byUrl(anyString());
    verify(iQuery, times(1)).returnBundle(any(Class.class));
  }

  @Test
  public void getResourceByIdTest() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(Observation.class, OBSERVATION_PERFORMER_JSON);
    Mockito.when(iReadExecutable.execute()).thenReturn(resource);
    Resource actual =
        ehrFhirR4QueryService.getResourceById(karProcessingData, "observation", "1234", true);
    assertEquals("Observation", actual.getResourceType().name());
    verify(fhirClient, times(1)).read();
    verify(iRead, times(1)).resource("observation");
  }

  @Test
  public void getFilteredDataTest_with_DataReq() {
    List<DataRequirement> dataRequirements = new ArrayList<>();
    DataRequirement dr = new DataRequirement();
    dr.setType("Patient");
    dr.setId("12344567");
    dataRequirements.add(dr);
    Resource resource = TestUtils.loadResourceDataFromFile(Encounter.class, ENCOUNTER_JSON);
    Mockito.when(iReadExecutable.execute()).thenReturn(resource);
    Set<Resource> resourceSet = new HashSet<>();
    resourceSet.add(resource);
    HashMap<ResourceType, Set<Resource>> map = new HashMap<>();
    map.put(ResourceType.Encounter, resourceSet);

    Mockito.lenient()
        .when(karProcessingData.getOutputDataById(anyString()))
        .thenReturn(resourceSet);
    when(notificationContext.getPatientId()).thenReturn("patientId");
    Mockito.lenient().when(karProcessingData.getFhirInputDataByType()).thenReturn(map);
    when(karProcessingData.getNotificationContext()).thenReturn(notificationContext);
    Map<ResourceType, Set<Resource>> actual =
        ehrFhirR4QueryService.getFilteredData(karProcessingData, dataRequirements);

    assertNotNull(actual);
    assertEquals(1, actual.size());
    assertTrue(actual.containsKey(ResourceType.Encounter));
    assertEquals(resourceSet, actual.get(ResourceType.Encounter));
    verify(karProcessingData, times(1)).getOutputDataById("12344567");
  }

  @Test
  public void testObservationInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(Observation.class, "R4/Observation/Observation.json");
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testConditionInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(Condition.class, CONDITION_CODE_INACTIVE_JSON);
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testServiceRequestInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(ServiceRequest.class, SERVICEREQUEST_REVOKED_JSON);
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testMedicationRequestInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(MedicationRequest.class, MEDREQ_CANCEL_JSON);
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testMedicationAdministrationInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(MedicationAdministration.class, MEDADM_STOPPED_JSON);
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testMedicationStatementInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(MedicationStatement.class, MEDSTMT_STOPPED_JSON);
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testDiagnosticReportInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(
            DiagnosticReport.class, "R4/DiagnosticReport/DiagnosticReport_status.json");
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testImmunizationInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(Immunization.class, IMMUNIZATION_NOTDONE_JSON);
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testProcedureInvalid() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(Procedure.class, "R4/Procedure/procedure_status.json");
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(false, result);
  }

  @Test
  public void testMedicationValid() {
    Resource resource = TestUtils.loadResourceDataFromFile(Medication.class, MEDICATION_JSON);
    Boolean result = ehrFhirR4QueryService.isValidResource(resource);
    assertEquals(true, result);
  }

  @Test
  public void testMedicationRequestReference() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(MedicationRequest.class, MEDREQ_REFERENCE_JSON);

    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(resource);

    Medication medication = new Medication();
    medication.setId("med0316");
    when(karProcessingData.getResourceById("med0316", ResourceType.Medication))
        .thenReturn(medication);

    Resource actual =
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient, fhirContext, "resource", "mocksearchurl", karProcessingData);

    assertNotNull(actual);
    assertEquals(resource.getResourceType(), actual.getResourceType());
    assertTrue(!karProcessingData.getResourceById("med0316", ResourceType.Medication).isEmpty());
  }

  @Test
  public void testMedicationAdministrationHasRequest() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(MedicationAdministration.class, MEDADM_HAS_REQUEST_JSON);
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(resource);

    Medication medication = new Medication();
    medication.setId("med0316");
    when(karProcessingData.getResourceById("med0316", ResourceType.Medication))
        .thenReturn(medication);

    MedicationRequest medicationRequest = new MedicationRequest();
    medicationRequest.setId("medreq001");
    when(karProcessingData.getResourceById("medreq001", ResourceType.MedicationRequest))
        .thenReturn(medicationRequest);

    Resource actual =
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient,
            fhirContext,
            ResourceType.MedicationAdministration.toString(),
            "medicationAdministration?patient=Patient/1234",
            karProcessingData);

    assertNotNull(actual);
    assertEquals(resource.getResourceType(), actual.getResourceType());
    assertTrue(!karProcessingData.getResourceById("med0316", ResourceType.Medication).isEmpty());
    assertTrue(
        !karProcessingData.getResourceById("medreq001", ResourceType.MedicationRequest).isEmpty());
  }

  @Test
  public void testMedicationStatementReference() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(MedicationStatement.class, MEDSTMT_REFERENCE_JSON);
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(resource);

    Medication medication = new Medication();
    medication.setId("med0316");
    when(karProcessingData.getResourceById("med0316", ResourceType.Medication))
        .thenReturn(medication);

    Resource actual =
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient,
            fhirContext,
            ResourceType.MedicationStatement.toString(),
            "medicationStatement?patient=Patient//789",
            karProcessingData);

    assertNotNull(actual);
    assertEquals(resource.getResourceType(), actual.getResourceType());
    assertTrue(!karProcessingData.getResourceById("med0316", ResourceType.Medication).isEmpty());
  }

  @Test
  public void testMedicationDispense() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(MedicationDispense.class, MEDDISPENSE_JSON);
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(resource);

    Medication medication = new Medication();
    medication.setId("med0316");
    when(karProcessingData.getResourceById("med0316", ResourceType.Medication))
        .thenReturn(medication);

    Resource actual =
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient,
            fhirContext,
            ResourceType.MedicationDispense.toString(),
            "medicationDispense?patient=Patient/12",
            karProcessingData);

    assertNotNull(actual);
    assertEquals(resource.getResourceType(), actual.getResourceType());
    assertTrue(!karProcessingData.getResourceById("med0316", ResourceType.Medication).isEmpty());
  }

  @Test
  public void testImmunizationResource() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(Immunization.class, IMMUNIZATION_RESOURCE_JSON);
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(resource);
    Practitioner practitioner = new Practitioner();
    practitioner.setId("pract12");
    when(karProcessingData.getResourceById("pract12", ResourceType.Practitioner))
        .thenReturn(practitioner);

    Organization organization = new Organization();
    organization.setId("org123");
    when(karProcessingData.getResourceById("org123", ResourceType.Organization))
        .thenReturn(organization);

    Resource actual =
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient,
            fhirContext,
            "resource",
            "Immunization?patient=Patient/1234",
            karProcessingData);

    assertNotNull(actual);
    assertEquals(resource.getResourceType(), actual.getResourceType());
    assertTrue(!karProcessingData.getResourceById("pract12", ResourceType.Practitioner).isEmpty());
    assertTrue(!karProcessingData.getResourceById("org123", ResourceType.Organization).isEmpty());
  }

  @Test
  public void testObservationWithPerformer() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(Observation.class, OBSERVATION_PERFORMER_JSON);
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(resource);

    Practitioner practitioner = new Practitioner();
    practitioner.setId("prac123");
    when(karProcessingData.getResourceById("prac123", ResourceType.Practitioner))
        .thenReturn(practitioner);

    Specimen specimen = new Specimen();
    specimen.setId("spec001");
    when(karProcessingData.getResourceById("spec001", ResourceType.Specimen)).thenReturn(specimen);

    Resource actual =
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient,
            fhirContext,
            ResourceType.Observation.toString(),
            "Observation?patient=Patient/123",
            karProcessingData);

    assertNotNull(actual);
    assertEquals(resource.getResourceType(), actual.getResourceType());
    assertTrue(!karProcessingData.getResourceById("prac123", ResourceType.Practitioner).isEmpty());
    assertTrue(!karProcessingData.getResourceById("spec001", ResourceType.Specimen).isEmpty());
  }

  @Test
  public void testDiagnosticReportWithSpecimen() {
    Resource resource =
        TestUtils.loadResourceDataFromFile(
            DiagnosticReport.class, DIAGNOSTICREPORT_WITH_SPECIMEN_JSON);
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(resource);

    Specimen specimen = new Specimen();
    specimen.setId("spec0012");
    when(karProcessingData.getResourceById("spec0012", ResourceType.Specimen)).thenReturn(specimen);

    Observation observation = new Observation();
    observation.setId("obs123");
    when(karProcessingData.getResourceById("obs123", ResourceType.Observation))
        .thenReturn(observation);
    Resource actual =
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient,
            fhirContext,
            ResourceType.Specimen.toString(),
            "specimen?patient=Patient/23",
            karProcessingData);

    assertNotNull(actual);
    assertEquals(resource.getResourceType(), actual.getResourceType());
    assertTrue(!karProcessingData.getResourceById("spec0012", ResourceType.Specimen).isEmpty());
    assertTrue(!karProcessingData.getResourceById("obs123", ResourceType.Observation).isEmpty());
  }

  @Test
  public void testGetResourceById_BaseServerResponseException() {

    BaseServerResponseException ex = new BaseServerResponseException(500, "server error") {};
    when(iReadTyped.withId(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenThrow(ex);

    Resource result =
        ehrFhirR4QueryService.getResourceById(
            fhirClient, karProcessingData, fhirContext, "Observation", "123", false);

    assertNull(result);
  }

  private HashMap<String, HashMap<String, String>> processQueryFile(File queryFile) {
    HashMap<String, String> queries = new HashMap<>();
    String filenameWithoutExt = FilenameUtils.getBaseName(queryFile.getName());

    try (InputStream input = FileUtils.openInputStream(queryFile)) {
      Properties prop = new Properties();

      prop.load(input);

      prop.forEach((key, value) -> queries.put((String) key, (String) value));

      if (customQueries != null) {
        if (!customQueries.containsKey(filenameWithoutExt)) {
          customQueries.put(filenameWithoutExt, queries);
        }
      } else {
        customQueries = new HashMap<>();
        customQueries.put(filenameWithoutExt, queries);
      }

    } catch (Exception e) {
      // Ignore file read/parse failures in this test helper; return whatever was loaded so far
    }
    return customQueries;
  }

  @Test
  public void isValidResource_acceptsRealResourcesInAUsableState() {
    List<Resource> usable =
        List.of(
            TestUtils.loadResourceDataFromFile(
                Observation.class, "R4/Observation/ObservationResource.json"),
            TestUtils.loadResourceDataFromFile(Condition.class, CONDITION_JSON),
            TestUtils.loadResourceDataFromFile(
                ServiceRequest.class, "R4/ServiceRequest/ServiceRequest.json"),
            TestUtils.loadResourceDataFromFile(MedicationRequest.class, MEDREQ_REFERENCE_JSON),
            TestUtils.loadResourceDataFromFile(
                MedicationAdministration.class, MEDADM_HAS_REQUEST_JSON),
            TestUtils.loadResourceDataFromFile(MedicationStatement.class, MEDSTMT_REFERENCE_JSON),
            TestUtils.loadResourceDataFromFile(
                DiagnosticReport.class, DIAGNOSTICREPORT_WITH_SPECIMEN_JSON),
            TestUtils.loadResourceDataFromFile(Immunization.class, IMMUNIZATION_RESOURCE_JSON),
            TestUtils.loadResourceDataFromFile(Procedure.class, "R4/Procedure/Procedure.json"),
            TestUtils.loadResourceDataFromFile(Patient.class, "R4/Patient/Patient.json"));

    for (Resource resource : usable) {
      assertTrue(resource.fhirType(), ehrFhirR4QueryService.isValidResource(resource));
    }
  }

  @Test
  public void isValidResource_rejectsConditionsWithAnUnusableClinicalOrVerificationStatus() {
    Condition refuted = TestUtils.loadResourceDataFromFile(Condition.class, CONDITION_JSON);
    refuted.getVerificationStatus().getCodingFirstRep().setCode("refuted");
    Condition enteredInError = TestUtils.loadResourceDataFromFile(Condition.class, CONDITION_JSON);
    enteredInError.getVerificationStatus().getCodingFirstRep().setCode("entered-in-error");
    Condition resolved = TestUtils.loadResourceDataFromFile(Condition.class, CONDITION_JSON);
    resolved.getClinicalStatus().getCodingFirstRep().setCode("resolved");

    assertFalse(ehrFhirR4QueryService.isValidResource(refuted));
    assertFalse(ehrFhirR4QueryService.isValidResource(enteredInError));
    assertFalse(ehrFhirR4QueryService.isValidResource(resolved));
  }

  @Test
  public void isValidResource_handlesBundleEntriesAndOperationOutcomes() {
    Bundle.BundleEntryComponent usableEntry =
        new Bundle.BundleEntryComponent()
            .setResource(TestUtils.loadResourceDataFromFile(Condition.class, CONDITION_JSON));

    assertTrue(ehrFhirR4QueryService.isValidResource(usableEntry));
    assertFalse(ehrFhirR4QueryService.isValidResource((Bundle.BundleEntryComponent) null));
    assertFalse(ehrFhirR4QueryService.isValidResource(new OperationOutcome()));
  }

  @Test
  public void getResourceByUrl_fetchesTheMedicationsAReportReferences() {
    Medication medication = TestUtils.loadResourceDataFromFile(Medication.class, MEDICATION_JSON);
    MedicationRequest request =
        TestUtils.loadResourceDataFromFile(MedicationRequest.class, MEDREQ_REFERENCE_JSON);
    List<Resource> reports =
        List.of(
            TestUtils.loadResourceDataFromFile(MedicationRequest.class, MEDREQ_REFERENCE_JSON),
            TestUtils.loadResourceDataFromFile(
                MedicationAdministration.class, MEDADM_HAS_REQUEST_JSON),
            TestUtils.loadResourceDataFromFile(MedicationStatement.class, MEDSTMT_REFERENCE_JSON),
            TestUtils.loadResourceDataFromFile(MedicationDispense.class, MEDDISPENSE_JSON));
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);

    for (Resource report : reports) {
      KarProcessingData kd = new KarProcessingData();
      when(iReadExecutable.execute()).thenReturn(report, medication, request);

      Resource actual =
          ehrFhirR4QueryService.getResourceByUrl(
              fhirClient, fhirContext, report.fhirType(), "url?patient=Patient/1", kd);

      assertSame(report, actual);
      assertSame(medication, kd.getResourceById("med0316", ResourceType.Medication));
      assertTrue(kd.getResourcesByType(ResourceType.Medication).contains(medication));
      if (report instanceof MedicationAdministration) {
        assertSame(request, kd.getResourceById("medreq001", ResourceType.MedicationRequest));
      }
    }
  }

  @Test
  public void getResourceByUrl_fetchesAnImmunizationsPractitionerOnlyOnce() {
    Immunization immunization =
        TestUtils.loadResourceDataFromFile(Immunization.class, IMMUNIZATION_RESOURCE_JSON);
    Practitioner practitioner =
        TestUtils.loadResourceDataFromFile(
            Practitioner.class, "R4/Practitioner/Practitioner_4122622.json");
    KarProcessingData kd = new KarProcessingData();
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(immunization, practitioner);

    ehrFhirR4QueryService.getResourceByUrl(
        fhirClient, fhirContext, "Immunization", "Immunization?patient=Patient/1", kd);

    assertSame(practitioner, kd.getResourceById("example", ResourceType.Practitioner));
    verify(iReadExecutable, times(2)).execute();
  }

  @Test
  public void getResourceByUrl_fetchesAnObservationsPractitionerAndSpecimen() {
    Observation observation =
        TestUtils.loadResourceDataFromFile(Observation.class, OBSERVATION_PERFORMER_JSON);
    Practitioner practitioner =
        TestUtils.loadResourceDataFromFile(
            Practitioner.class, "R4/Practitioner/Practitioner_4122622.json");
    Specimen specimen = new Specimen();
    specimen.setId("specimen001");
    KarProcessingData kd = new KarProcessingData();
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(observation, practitioner, specimen);

    ehrFhirR4QueryService.getResourceByUrl(
        fhirClient, fhirContext, "Observation", "Observation?patient=Patient/1", kd);

    assertSame(practitioner, kd.getResourceById("pract001", ResourceType.Practitioner));
    assertSame(specimen, kd.getResourceById("specimen001", ResourceType.Specimen));
  }

  @Test
  public void getResourceByUrl_fetchesADiagnosticReportsSpecimenAndResultObservation() {
    DiagnosticReport report =
        TestUtils.loadResourceDataFromFile(
            DiagnosticReport.class, DIAGNOSTICREPORT_WITH_SPECIMEN_JSON);
    Specimen specimen = new Specimen();
    specimen.setId("specimen001");
    Observation result =
        TestUtils.loadResourceDataFromFile(
            Observation.class, "R4/Observation/ObservationResource.json");
    KarProcessingData kd = new KarProcessingData();
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenReturn(report, specimen, result);

    ehrFhirR4QueryService.getResourceByUrl(
        fhirClient, fhirContext, "DiagnosticReport", "DiagnosticReport?patient=Patient/1", kd);

    assertSame(specimen, kd.getResourceById("specimen001", ResourceType.Specimen));
    assertSame(result, kd.getResourceById("obs001", ResourceType.Observation));
  }

  @Test
  public void getResourceByUrl_keepsAServiceRequestersPractitionerButNotAServerError() {
    ServiceRequest request =
        TestUtils.loadResourceDataFromFile(
            ServiceRequest.class, "R4/ServiceRequest/ServiceRequest.json");
    Practitioner practitioner =
        TestUtils.loadResourceDataFromFile(
            Practitioner.class, "R4/Practitioner/Practitioner_4122622.json");
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    KarProcessingData fetched = new KarProcessingData();
    when(iReadExecutable.execute()).thenReturn(request, practitioner);

    ehrFhirR4QueryService.getResourceByUrl(
        fhirClient, fhirContext, "ServiceRequest", "ServiceRequest?patient=Patient/1", fetched);

    assertSame(practitioner, fetched.getResourceById("example", ResourceType.Practitioner));

    KarProcessingData failed = new KarProcessingData();
    when(iReadExecutable.execute()).thenReturn(request, new OperationOutcome());

    ehrFhirR4QueryService.getResourceByUrl(
        fhirClient, fhirContext, "ServiceRequest", "ServiceRequest?patient=Patient/1", failed);

    assertNull(failed.getResourceById("example", ResourceType.Practitioner));
  }

  // =====================================================================================
  // New tests added for more coverage. The existing tests above are unchanged.
  // =====================================================================================

  // ---------------- simple delegations ----------------

  @Test
  public void getContext_returnsTheR4ContextFromTheInitializer() {
    assertSame(fhirContext, ehrFhirR4QueryService.getContext());
  }

  @Test
  public void getAuthorizationToken_delegatesToAuthUtils() {
    JSONObject token = new JSONObject().put("access_token", "abc");
    when(authUtils.getToken(healthcareSetting)).thenReturn(token);

    assertSame(token, ehrFhirR4QueryService.getAuthorizationToken(healthcareSetting));
  }

  // ---------------- getClient / retrieveAndUpdateAccessToken ----------------

  @Test
  public void getClient_expiredToken_storesNewTokenExpiryAndProviderId() {
    JSONObject token =
        new JSONObject()
            .put("access_token", "new-token")
            .put("expires_in", 3600)
            .put("uuid", "provider-9");
    when(karProcessingData.hasValidAccessToken()).thenReturn(false);
    when(authUtils.getToken(healthcareSetting)).thenReturn(token);

    assertSame(fhirClient, ehrFhirR4QueryService.getClient(karProcessingData, fhirContext));

    verify(healthcareSetting).setEhrAccessToken("new-token");
    verify(healthcareSetting).setEhrAccessTokenExpiryDuration(3600);
    verify(healthcareSetting).setDefaultProviderId("provider-9");
    verify(healthcareSetting).setEhrAccessTokenExpirationTime(any(Date.class));
    verify(hsDao).saveOrUpdate(healthcareSetting);
  }

  // ---------------- initializeCustomQueries / processQueryFile ----------------

  @Test
  public void initializeCustomQueries_missingDirectory_leavesQueriesEmpty() {
    ReflectionTestUtils.setField(
        ehrFhirR4QueryService, "customQueryDirectory", "does/not/exist/anywhere");

    ehrFhirR4QueryService.initializeCustomQueries();

    assertNull(ReflectionTestUtils.getField(ehrFhirR4QueryService, "customQueries"));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void initializeCustomQueries_loadsOnlyValidQueryFilesAndOnlyOnce() throws IOException {
    Path dir = Files.createTempDirectory("custom-queries");
    Files.writeString(
        dir.resolve("good-kar.queries"), "dr1=Condition?patient={{context.patientId}}");
    Files.writeString(dir.resolve("notes.txt"), "dr1=ignored");
    // A malformed \\u escape makes Properties.load throw -> the catch in processQueryFile
    Files.writeString(dir.resolve("broken-kar.queries"), "dr1=\\u00zz");
    ReflectionTestUtils.setField(ehrFhirR4QueryService, "customQueryDirectory", dir.toString());

    ehrFhirR4QueryService.initializeCustomQueries();
    // second load: the file is already registered -> "Not adding the queries" branch
    ehrFhirR4QueryService.initializeCustomQueries();

    HashMap<String, HashMap<String, String>> loaded =
        (HashMap<String, HashMap<String, String>>)
            ReflectionTestUtils.getField(ehrFhirR4QueryService, "customQueries");
    assertNotNull(loaded);
    assertEquals(Set.of("good-kar"), loaded.keySet());
    assertEquals("Condition?patient={{context.patientId}}", loaded.get("good-kar").get("dr1"));
  }

  // ---------------- getFilteredData(Map) ----------------

  @Test
  public void getFilteredData_map_patientAndEncounterNotFound_noSearchForPatientOrEncounter() {
    when(iReadExecutable.execute()).thenReturn(new OperationOutcome());
    when(notificationContext.getNotificationResourceType())
        .thenReturn(ResourceType.Encounter.toString());
    when(notificationContext.getNotificationResourceId()).thenReturn("encounterId");
    Map<String, ResourceType> resTypes = new HashMap<>();
    resTypes.put("patient-req", ResourceType.Patient);
    resTypes.put("encounter-req", ResourceType.Encounter);

    ehrFhirR4QueryService.getFilteredData(karProcessingData, resTypes);

    verify(karProcessingData, never()).addResourcesByType(anyMap());
    verify(fhirClient, never()).search();
  }

  @Test
  public void getFilteredData_map_notificationForPatient_doesNotFetchAnEncounter() {
    Patient patient = new Patient();
    patient.setId("12345");
    when(iReadExecutable.execute()).thenReturn(patient);
    when(notificationContext.getNotificationResourceType())
        .thenReturn(ResourceType.Patient.toString());

    ehrFhirR4QueryService.getFilteredData(karProcessingData, new HashMap<>());

    verify(iRead, never()).resource(ResourceType.Encounter.toString());
    verify(karProcessingData, times(1)).addResourcesByType(anyMap());
  }

  // ---------------- getFilteredData(List<DataRequirement>) ----------------

  @Test
  public void getFilteredData_dataRequirements_fetchesOnlyWhatIsNotAlreadyLoaded() {
    // Patient not found -> the "res != null" false branch
    when(iReadExecutable.execute()).thenReturn(null);
    when(notificationContext.getFhirServerBaseUrl()).thenReturn("http://ehr");
    when(karProcessingData.getOutputDataById(anyString())).thenReturn(null);

    Condition condition = new Condition();
    condition.setId("cond-1");
    when(karProcessingData.getResourcesByType("Condition"))
        .thenReturn(new HashSet<>(Set.of(condition)));
    when(karProcessingData.getResourcesByType("Observation")).thenReturn(new HashSet<>());
    when(iQuery.execute()).thenReturn(bundleOf(observation("obs-1", null)));

    ehrFhirR4QueryService.getFilteredData(
        karProcessingData,
        List.of(
            dataRequirement("cond-req", "Condition"),
            dataRequirement("obs-req", "Observation"),
            dataRequirement("enc-req", "Encounter")));

    // only Observation had nothing loaded, so only Observation is searched
    verify(iUntypedQuery, times(1)).byUrl("http://ehr/Observation?patient=12345");
    verify(karProcessingData).addResourcesByType(eq(ResourceType.Condition), any());
    verify(karProcessingData).addResourcesByType(eq(ResourceType.Observation), any());
    verify(karProcessingData).addResourcesById(eq("enc-req"), any());
    verify(karProcessingData, never()).addResourcesByType(anyMap());
  }

  // ---------------- getResourceById / getResourceByUrl / fetchResources errors ----------------

  @Test
  public void getResourceById_errorsAreLoggedAndReturnNull() {
    IParser parser = mock(IParser.class);
    when(fhirContext.newJsonParser()).thenReturn(parser);

    when(iReadExecutable.execute()).thenThrow(serverError(new OperationOutcome()));
    assertNull(
        ehrFhirR4QueryService.getResourceById(
            fhirClient, karProcessingData, fhirContext, "Observation", "1", true));
    verify(parser).encodeResourceToString(any(OperationOutcome.class));

    reset(iReadExecutable);
    when(iReadExecutable.execute()).thenThrow(new IllegalStateException("socket closed"));
    assertNull(
        ehrFhirR4QueryService.getResourceById(
            fhirClient, karProcessingData, fhirContext, "Observation", "1", true));
  }

  @Test
  public void getResourceById_withoutFiltering_returnsAnInvalidResource() {
    Observation cancelled = observation("obs-c", Observation.ObservationStatus.CANCELLED);
    when(iReadExecutable.execute()).thenReturn(cancelled);

    assertSame(
        cancelled,
        ehrFhirR4QueryService.getResourceById(
            fhirClient, karProcessingData, fhirContext, "Observation", "obs-c", false));
    assertNull(
        ehrFhirR4QueryService.getResourceById(
            fhirClient, karProcessingData, fhirContext, "Observation", "obs-c", true));
  }

  @Test
  public void getResourceByUrl_errorsAreLoggedAndReturnNull() {
    IParser parser = mock(IParser.class);
    when(fhirContext.newJsonParser()).thenReturn(parser);
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);

    when(iReadExecutable.execute()).thenThrow(serverError(new OperationOutcome()));
    assertNull(
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient, fhirContext, "Observation", "url", karProcessingData));
    verify(parser).encodeResourceToString(any(OperationOutcome.class));

    reset(iReadExecutable);
    when(iReadExecutable.execute()).thenThrow(serverError(null));
    assertNull(
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient, fhirContext, "Observation", "url", karProcessingData));

    reset(iReadExecutable);
    when(iReadExecutable.execute()).thenThrow(new IllegalStateException("socket closed"));
    assertNull(
        ehrFhirR4QueryService.getResourceByUrl(
            fhirClient, fhirContext, "Observation", "url", karProcessingData));
  }

  @Test
  public void getResourceByUrl_operationOutcomeOrInvalidResource_isReturnedButNotExpanded() {
    KarProcessingData kd = new KarProcessingData();

    serveUrlRead(new OperationOutcome());
    assertTrue(
        ehrFhirR4QueryService.getResourceByUrl(fhirClient, fhirContext, "Observation", "u", kd)
            instanceof OperationOutcome);

    Observation cancelled = observation("obs-c", Observation.ObservationStatus.CANCELLED);
    cancelled.addPerformer(new Reference("Practitioner/p1"));
    serveUrlRead(cancelled);
    assertSame(
        cancelled,
        ehrFhirR4QueryService.getResourceByUrl(fhirClient, fhirContext, "Observation", "u", kd));

    verify(iReadTyped, never()).withId(anyString());
  }

  @Test
  public void fetchResources_searchFailure_returnsEmptySet() {
    when(iQuery.execute()).thenThrow(new IllegalStateException("socket closed"));

    assertTrue(
        ehrFhirR4QueryService
            .fetchResources(fhirClient, karProcessingData, fhirContext, "Observation?patient=1")
            .isEmpty());
  }

  // ---------------- getResourcesByPatientId ----------------

  @Test
  public void getResourcesByPatientId_skipsOperationOutcomesAndFollowsNextPage() {
    Bundle firstPage = bundleOf(observation("obs-1", null), new OperationOutcome());
    firstPage.addLink().setRelation(IBaseBundle.LINK_NEXT).setUrl("http://ehr/next");
    when(iQuery.execute()).thenReturn(firstPage);
    when(iGetPageTyped.execute()).thenReturn(bundleOf(observation("obs-2", null)));

    ehrFhirR4QueryService.getResourcesByPatientId(
        fhirClient,
        fhirContext,
        "Observation",
        "url",
        karProcessingData,
        ResourceType.Observation,
        "obs-req");

    assertEquals(3, firstPage.getEntry().size());
    verify(karProcessingData).addResourcesByType(anyMap());
    verify(karProcessingData).addResourcesById(any(HashMap.class));
  }

  @Test
  public void getResourcesByPatientId_emptyBundle_addsNothing() {
    Bundle selfLinkOnly = new Bundle();
    selfLinkOnly.addLink().setRelation("self").setUrl("http://ehr/self");
    when(iQuery.execute()).thenReturn(selfLinkOnly);

    ehrFhirR4QueryService.getResourcesByPatientId(
        fhirClient,
        fhirContext,
        "Observation",
        "url",
        karProcessingData,
        ResourceType.Observation,
        "obs-req");

    verify(karProcessingData, never()).addResourcesByType(anyMap());
  }

  @Test
  public void getResourcesByPatientId_failuresAreLogged() {
    // null bundle -> NullPointerException in pagination -> generic catch
    when(iQuery.execute()).thenReturn(null);
    ehrFhirR4QueryService.getResourcesByPatientId(
        fhirClient,
        fhirContext,
        "Observation",
        "url",
        karProcessingData,
        ResourceType.Observation,
        "obs-req");

    // server error without an OperationOutcome
    reset(iQuery);
    when(iQuery.returnBundle(any(Class.class))).thenReturn(iQuery);
    when(iQuery.execute()).thenThrow(serverError(null));
    ehrFhirR4QueryService.getResourcesByPatientId(
        fhirClient,
        fhirContext,
        "Observation",
        "url",
        karProcessingData,
        ResourceType.Observation,
        "obs-req");

    verify(karProcessingData, never()).addResourcesByType(anyMap());
  }

  // ---------------- executeSearchQuery / getResourcesFromSearch ----------------

  @Test
  public void executeSearchQuery_keepsValidResourcesAndGroupsThemByType() {
    Bundle bundle =
        bundleOf(
            condition("cond-1"),
            condition("cond-2"),
            observation("obs-bad", Observation.ObservationStatus.ENTEREDINERROR));
    // a link that is not "next" -> no further page is loaded
    bundle.addLink().setRelation("self").setUrl("http://ehr/self");
    when(iQuery.execute()).thenReturn(bundle);

    ehrFhirR4QueryService.executeSearchQuery(karProcessingData, "dr-1", fhirQueryFilter, "q");

    verify(karProcessingData).storeResourceById(eq("cond-1"), any());
    verify(karProcessingData).storeResourceById(eq("cond-2"), any());
    verify(karProcessingData, never()).storeResourceById(eq("obs-bad"), any());
    verify(karProcessingData).addResourcesById(any(HashMap.class));
  }

  @Test
  public void executeSearchQuery_emptyBundleOrFailure_addsNothing() {
    when(iQuery.execute()).thenReturn(new Bundle());
    ehrFhirR4QueryService.executeSearchQuery(karProcessingData, "dr-1", fhirQueryFilter, "q");

    reset(iQuery);
    when(iQuery.returnBundle(any(Class.class))).thenReturn(iQuery);
    when(iQuery.execute()).thenThrow(serverError(null));
    ehrFhirR4QueryService.executeSearchQuery(karProcessingData, "dr-1", fhirQueryFilter, "q");

    reset(iQuery);
    when(iQuery.returnBundle(any(Class.class))).thenReturn(iQuery);
    when(iQuery.execute()).thenThrow(new IllegalStateException("socket closed"));
    ehrFhirR4QueryService.executeSearchQuery(karProcessingData, "dr-1", fhirQueryFilter, "q");

    verify(karProcessingData, never()).addResourcesById(any(HashMap.class));
  }

  // ---------------- executeQuery / getQuery ----------------

  @Test
  public void executeQuery_alreadyFetched_doesNothing() {
    when(fhirQueryFilter.getRelatedDataId()).thenReturn("rel-1");
    when(karProcessingData.isDataAlreadyFetched("dr-1", "rel-1")).thenReturn(true);

    ehrFhirR4QueryService.executeQuery(karProcessingData, "dr-1", fhirQueryFilter);

    verify(karProcessingData, never()).getKar();
  }

  @Test
  public void executeQuery_noQueryAvailable_doesNotCallTheEhr() {
    useDefaultQuery("");

    ehrFhirR4QueryService.executeQuery(karProcessingData, "dr-1", fhirQueryFilter);

    verify(fhirClient, never()).search();
    verify(fhirClient, never()).read();
  }

  @Test
  public void executeQuery_readQuery_addsTheResourceForTheDataRequirement() {
    useDefaultQuery("Patient/{{context.patientId}}");
    Patient patient = new Patient();
    patient.setId("patientId");
    serveUrlRead(patient);

    ehrFhirR4QueryService.executeQuery(karProcessingData, "dr-1", fhirQueryFilter);

    verify(iReadTyped).withUrl("Patient/patientId");
    verify(karProcessingData).addResourceById("dr-1", patient);
    verify(karProcessingData).addResourceByType(ResourceType.Patient, patient);
  }

  @Test
  public void executeQuery_readQueryReturningOperationOutcome_isNotAdded() {
    useDefaultQuery("Patient/{{context.patientId}}");
    serveUrlRead(new OperationOutcome());

    ehrFhirR4QueryService.executeQuery(karProcessingData, "dr-1", fhirQueryFilter);

    verify(karProcessingData, never()).addResourceById(anyString(), any());
  }

  @Test
  public void executeQuery_customQueryForTheRelatedDataId_isUsed() {
    useDefaultQuery("Condition?patient={{context.patientId}}");
    useCustomQueries("kar-q", Map.of("rel-1", "Observation?patient={{context.patientId}}"));

    ehrFhirR4QueryService.executeQuery(karProcessingData, "dr-1", fhirQueryFilter);

    verify(iUntypedQuery).byUrl("http://ehr/Observation?patient=patientId");
  }

  @Test
  public void executeQuery_customFileWithoutAMatchingKey_usesTheDefaultQuery() {
    useDefaultQuery("Condition?patient={{context.patientId}}");
    useCustomQueries("kar-q", Map.of("another-dr", "Observation?patient=x"));

    ehrFhirR4QueryService.executeQuery(karProcessingData, "dr-1", fhirQueryFilter);

    verify(iUntypedQuery).byUrl("http://ehr/Condition?patient=patientId");
  }

  @Test
  public void executeQuery_noCustomFileForThisKar_usesTheDefaultQuery() {
    useDefaultQuery("Condition?patient={{context.patientId}}");
    useCustomQueries("some-other-kar", Map.of("dr-1", "Observation?patient=x"));

    ehrFhirR4QueryService.executeQuery(karProcessingData, "dr-1", fhirQueryFilter);

    verify(iUntypedQuery).byUrl("http://ehr/Condition?patient=patientId");
  }

  // ---------------- substituteContextParams ----------------

  @Test
  public void substituteContextParams_encounterFromEhrWithStartOnly_usesNotificationEndTime() {
    Encounter enc = new Encounter();
    enc.setId("encounterId");
    enc.getPeriod().setStart(new Date(1705276800000L));
    serveReadsById(Map.of("encounterId", enc));

    String result =
        ehrFhirR4QueryService.substituteContextParams(
            karProcessingData,
            "Observation?patient={{context.patientId}}&encounter={{context.encounterId}}"
                + "&date=ge{{context.encounterStartDate}}&date=le{{context.encounterEndDate}}",
            false);

    assertFalse(result, result.contains("{{"));
    assertTrue(result.contains("patient=patientId&encounter=encounterId"));
    verify(iReadTyped).withId("encounterId");
  }

  @Test
  public void substituteContextParams_encounterWithoutPeriod_usesNotificationTimes() {
    Encounter enc = new Encounter();
    enc.setId("encounterId");
    serveReadsById(Map.of("encounterId", enc));
    Date start = new Date(1705276800000L);
    when(notificationContext.getEncounterStartTime()).thenReturn(start);
    when(notificationContext.getEncounterEndTime()).thenReturn(null); // -> "today"

    String result =
        ehrFhirR4QueryService.substituteContextParams(
            karProcessingData,
            "Observation?date=ge{{context.encounterStartDate}}&date=le{{context.encounterEndDate}}",
            true);

    assertEquals(
        "Observation?date=ge"
            + DateFormatUtils.format(start, "yyy-MM-dd")
            + "&date=le"
            + DateFormatUtils.format(new Date(), "yyy-MM-dd"),
        result);
  }

  @Test
  public void substituteContextParams_noContextEncounterId_usesNotificationTimes() {
    when(karProcessingData.getContextEncounterId()).thenReturn("");

    String result =
        ehrFhirR4QueryService.substituteContextParams(
            karProcessingData,
            "Observation?date=ge{{context.encounterStartDate}}&date=le{{context.encounterEndDate}}",
            false);

    assertFalse(result, result.contains("{{"));
    verify(fhirClient, never()).read();
  }

  @Test
  public void substituteContextParams_lastSubmissionDateAndEncounterClass() {
    String query =
        "Observation?_lastUpdated=gt{{context.lastReportSubmissionDate}}"
            + "&class={{context.encounterClass}}";
    Date submitted = new Date(1705276800000L);

    // previous report with a submission time, encounter class from the notification
    when(publicHealthMessage.getSubmissionTime()).thenReturn(submitted);
    when(notificationContext.getEncounterClass()).thenReturn("AMB");
    assertEquals(
        "Observation?_lastUpdated=gt"
            + DateFormatUtils.format(submitted, "yyy-MM-dd")
            + "&class=AMB",
        ehrFhirR4QueryService.substituteContextParams(karProcessingData, query, false));

    // previous report without a submission time, no encounter class -> today and IMP
    String today = DateFormatUtils.format(new Date(), "yyy-MM-dd");
    when(publicHealthMessage.getSubmissionTime()).thenReturn(null);
    when(notificationContext.getEncounterClass()).thenReturn(null);
    assertEquals(
        "Observation?_lastUpdated=gt" + today + "&class=IMP",
        ehrFhirR4QueryService.substituteContextParams(karProcessingData, query, false));

    // no previous report at all -> today
    when(karProcessingData.getPhm()).thenReturn(null);
    assertEquals(
        "Observation?_lastUpdated=gt" + today + "&class=IMP",
        ehrFhirR4QueryService.substituteContextParams(karProcessingData, query, false));
  }

  // ---------------- constructR4DocumentReference ----------------

  @Test
  public void constructR4DocumentReference_withoutOptionalValues() {
    for (String missing : new String[] {null, ""}) {
      DocumentReference docRef =
          ehrFhirR4QueryService.constructR4DocumentReference(
              missing, "p1", missing, missing, "text/xml", "title", "code", "display", "system");

      assertFalse(docRef.hasAuthor());
      assertFalse(docRef.getContentFirstRep().getAttachment().hasData());
      assertFalse(docRef.getContext().hasEncounter());
      assertEquals("Patient/p1", docRef.getSubject().getReference());
    }
  }

  // ---------------- isValidResource ----------------

  @Test
  public void isValidResource_observationStatuses() {
    for (Observation.ObservationStatus s :
        List.of(
            Observation.ObservationStatus.CANCELLED,
            Observation.ObservationStatus.ENTEREDINERROR,
            Observation.ObservationStatus.UNKNOWN)) {
      assertFalse(s.toCode(), valid(observation("o", s)));
    }
    assertTrue(valid(observation("o", Observation.ObservationStatus.FINAL)));
    assertTrue(valid(observation("o", null)));
  }

  @Test
  public void isValidResource_serviceRequestStatuses() {
    for (ServiceRequest.ServiceRequestStatus s :
        List.of(
            ServiceRequest.ServiceRequestStatus.REVOKED,
            ServiceRequest.ServiceRequestStatus.ENTEREDINERROR,
            ServiceRequest.ServiceRequestStatus.DRAFT,
            ServiceRequest.ServiceRequestStatus.ONHOLD,
            ServiceRequest.ServiceRequestStatus.UNKNOWN)) {
      assertFalse(s.toCode(), valid(new ServiceRequest().setStatus(s)));
    }
    assertTrue(valid(new ServiceRequest().setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE)));
    assertTrue(valid(new ServiceRequest()));
  }

  @Test
  public void isValidResource_medicationRequestStatuses() {
    for (MedicationRequest.MedicationRequestStatus s :
        List.of(
            MedicationRequest.MedicationRequestStatus.ENTEREDINERROR,
            MedicationRequest.MedicationRequestStatus.STOPPED,
            MedicationRequest.MedicationRequestStatus.CANCELLED,
            MedicationRequest.MedicationRequestStatus.DRAFT,
            MedicationRequest.MedicationRequestStatus.UNKNOWN)) {
      assertFalse(s.toCode(), valid(new MedicationRequest().setStatus(s)));
    }
    assertTrue(
        valid(new MedicationRequest().setStatus(MedicationRequest.MedicationRequestStatus.ACTIVE)));
    assertTrue(valid(new MedicationRequest()));
  }

  @Test
  public void isValidResource_medicationAdministrationStatuses() {
    for (MedicationAdministration.MedicationAdministrationStatus s :
        List.of(
            MedicationAdministration.MedicationAdministrationStatus.ENTEREDINERROR,
            MedicationAdministration.MedicationAdministrationStatus.STOPPED,
            MedicationAdministration.MedicationAdministrationStatus.NOTDONE,
            MedicationAdministration.MedicationAdministrationStatus.UNKNOWN)) {
      assertFalse(s.toCode(), valid(new MedicationAdministration().setStatus(s)));
    }
    assertTrue(
        valid(
            new MedicationAdministration()
                .setStatus(MedicationAdministration.MedicationAdministrationStatus.COMPLETED)));
    assertTrue(valid(new MedicationAdministration()));
  }

  @Test
  public void isValidResource_medicationStatementStatuses() {
    for (MedicationStatement.MedicationStatementStatus s :
        List.of(
            MedicationStatement.MedicationStatementStatus.ENTEREDINERROR,
            MedicationStatement.MedicationStatementStatus.STOPPED,
            MedicationStatement.MedicationStatementStatus.NOTTAKEN,
            MedicationStatement.MedicationStatementStatus.UNKNOWN)) {
      assertFalse(s.toCode(), valid(new MedicationStatement().setStatus(s)));
    }
    assertTrue(
        valid(
            new MedicationStatement()
                .setStatus(MedicationStatement.MedicationStatementStatus.ACTIVE)));
    assertTrue(valid(new MedicationStatement()));
  }

  @Test
  public void isValidResource_diagnosticReportStatuses() {
    for (DiagnosticReport.DiagnosticReportStatus s :
        List.of(
            DiagnosticReport.DiagnosticReportStatus.ENTEREDINERROR,
            DiagnosticReport.DiagnosticReportStatus.CANCELLED,
            DiagnosticReport.DiagnosticReportStatus.UNKNOWN)) {
      assertFalse(s.toCode(), valid(new DiagnosticReport().setStatus(s)));
    }
    assertTrue(
        valid(new DiagnosticReport().setStatus(DiagnosticReport.DiagnosticReportStatus.FINAL)));
    assertTrue(valid(new DiagnosticReport()));
  }

  @Test
  public void isValidResource_immunizationStatuses() {
    for (Immunization.ImmunizationStatus s :
        List.of(
            Immunization.ImmunizationStatus.ENTEREDINERROR,
            Immunization.ImmunizationStatus.NOTDONE)) {
      assertFalse(s.toCode(), valid(new Immunization().setStatus(s)));
    }
    assertTrue(valid(new Immunization().setStatus(Immunization.ImmunizationStatus.COMPLETED)));
    assertTrue(valid(new Immunization()));
  }

  @Test
  public void isValidResource_procedureStatuses() {
    for (Procedure.ProcedureStatus s :
        List.of(
            Procedure.ProcedureStatus.ENTEREDINERROR,
            Procedure.ProcedureStatus.STOPPED,
            Procedure.ProcedureStatus.UNKNOWN)) {
      assertFalse(s.toCode(), valid(new Procedure().setStatus(s)));
    }
    assertTrue(valid(new Procedure().setStatus(Procedure.ProcedureStatus.COMPLETED)));
    assertTrue(valid(new Procedure()));
  }

  @Test
  public void isValidResource_otherResourceTypes() {
    assertTrue(valid(new Medication()));
    assertFalse(valid(new OperationOutcome()));
    assertTrue(valid(new Practitioner()));
  }

  @Test
  public void isValidResource_conditionClinicalAndVerificationStatuses() {
    String clinical = "http://terminology.hl7.org/CodeSystem/condition-clinical";
    String verification = "http://terminology.hl7.org/CodeSystem/condition-ver-status";

    for (String code : List.of("inactive", "resolved", "remission", "unknown")) {
      Condition c = new Condition();
      c.getClinicalStatus().addCoding().setSystem(clinical).setCode(code);
      assertFalse(code, valid(c));
    }
    for (String code : List.of("refuted", "entered-in-error")) {
      Condition c = new Condition();
      c.getVerificationStatus().addCoding().setSystem(verification).setCode(code);
      assertFalse(code, valid(c));
    }

    Condition active = new Condition();
    active.getClinicalStatus().addCoding().setSystem(clinical).setCode("active");
    active.getVerificationStatus().addCoding().setSystem(verification).setCode("confirmed");
    assertTrue(valid(active));

    assertTrue("no statuses at all", valid(new Condition()));
  }

  @Test
  public void doesCodeableConceptContain_allPaths() {
    String sys = "http://example.org/cs";

    assertFalse(ehrFhirR4QueryService.doesCodeableConceptContain(sys, "a", null));
    assertFalse(ehrFhirR4QueryService.doesCodeableConceptContain(sys, "a", new CodeableConcept()));

    CodeableConcept cc = new CodeableConcept();
    cc.addCoding().setCode("a"); // no system
    cc.addCoding().setSystem("http://other.org").setCode("a"); // wrong system
    cc.addCoding().setSystem(sys); // no code
    cc.addCoding().setSystem(sys).setCode("b"); // wrong code
    assertFalse(ehrFhirR4QueryService.doesCodeableConceptContain(sys, "a", cc));

    cc.addCoding().setSystem(sys).setCode("a");
    assertTrue(ehrFhirR4QueryService.doesCodeableConceptContain(sys, "a", cc));
  }

  @Test
  public void sortResourcesByType_groupsResourcesOfTheSameType() {
    HashMap<ResourceType, Set<Resource>> byType = new HashMap<>();

    ehrFhirR4QueryService.sortResourcesByType(entry(condition("c1")), byType);
    ehrFhirR4QueryService.sortResourcesByType(entry(condition("c2")), byType);
    ehrFhirR4QueryService.sortResourcesByType(entry(observation("o1", null)), byType);

    assertEquals(2, byType.get(ResourceType.Condition).size());
    assertEquals(1, byType.get(ResourceType.Observation).size());
  }

  // ---------------- populateSecondaryResources (through getResourceByUrl) ----------------

  @Test
  public void secondaryResources_medicationRequest() {
    Medication fetchedMed = medication("med-new");
    serveReadsById(Map.of("med-new", fetchedMed));
    KarProcessingData kd = new KarProcessingData();
    kd.storeResourceById("med-known", medication("med-known"));

    readByUrl(kd, new MedicationRequest().setMedication(new Reference("Medication/med-new")));
    readByUrl(kd, new MedicationRequest().setMedication(new Reference("Medication/med-known")));
    readByUrl(kd, new MedicationRequest().setMedication(new Reference().setDisplay("no id")));
    readByUrl(kd, new MedicationRequest().setMedication(new CodeableConcept().setText("aspirin")));
    readByUrl(kd, new MedicationRequest().setMedication(new Reference("Medication/med-missing")));

    assertSame(fetchedMed, kd.getResourceById("med-new", ResourceType.Medication));
    assertNull(kd.getResourceById("med-missing", ResourceType.Medication));
    verify(iReadTyped, never()).withId("med-known");
  }

  @Test
  public void secondaryResources_medicationAdministration() {
    MedicationRequest fetchedRequest = new MedicationRequest();
    fetchedRequest.setId("mr-new");
    serveReadsById(
        Map.of(
            "med-new",
            medication("med-new"),
            "mr-new",
            fetchedRequest,
            "mr-outcome",
            new OperationOutcome()));
    KarProcessingData kd = new KarProcessingData();
    kd.storeResourceById("med-known", medication("med-known"));
    MedicationRequest knownRequest = new MedicationRequest();
    knownRequest.setId("mr-known");
    kd.storeResourceById("mr-known", knownRequest);

    readByUrl(
        kd,
        new MedicationAdministration()
            .setMedication(new Reference("Medication/med-new"))
            .setRequest(new Reference("MedicationRequest/mr-new")));
    readByUrl(
        kd,
        new MedicationAdministration()
            .setMedication(new Reference("Medication/med-known"))
            .setRequest(new Reference("MedicationRequest/mr-known")));
    readByUrl(
        kd,
        new MedicationAdministration()
            .setMedication(new Reference().setDisplay("no id"))
            .setRequest(new Reference().setDisplay("no id")));
    readByUrl(
        kd,
        new MedicationAdministration()
            .setMedication(new CodeableConcept().setText("aspirin"))
            .setRequest(new Reference("MedicationRequest/mr-outcome")));
    readByUrl(kd, new MedicationAdministration().setMedication(new Reference("Medication/x")));

    assertNotNull(kd.getResourceById("med-new", ResourceType.Medication));
    assertSame(fetchedRequest, kd.getResourceById("mr-new", ResourceType.MedicationRequest));
    // the request is read without filtering, so an OperationOutcome comes back and is skipped
    assertNull(kd.getResourceById("mr-outcome", ResourceType.OperationOutcome));
    verify(iReadTyped, never()).withId("med-known");
    verify(iReadTyped, never()).withId("mr-known");
  }

  @Test
  public void secondaryResources_medicationStatementAndDispense() {
    serveReadsById(Map.of("med-new", medication("med-new")));
    KarProcessingData kd = new KarProcessingData();
    kd.storeResourceById("med-known", medication("med-known"));

    readByUrl(kd, new MedicationStatement().setMedication(new Reference("Medication/med-new")));
    readByUrl(kd, new MedicationStatement().setMedication(new Reference("Medication/med-known")));
    readByUrl(kd, new MedicationStatement().setMedication(new Reference().setDisplay("no id")));
    readByUrl(kd, new MedicationStatement().setMedication(new CodeableConcept().setText("x")));
    readByUrl(kd, new MedicationStatement().setMedication(new Reference("Medication/missing")));

    readByUrl(kd, new MedicationDispense().setMedication(new Reference("Medication/med-new")));
    readByUrl(kd, new MedicationDispense().setMedication(new Reference("Medication/med-known")));
    readByUrl(kd, new MedicationDispense().setMedication(new Reference().setDisplay("no id")));
    readByUrl(kd, new MedicationDispense().setMedication(new CodeableConcept().setText("x")));
    readByUrl(kd, new MedicationDispense().setMedication(new Reference("Medication/missing")));

    assertNotNull(kd.getResourceById("med-new", ResourceType.Medication));
    assertNull(kd.getResourceById("missing", ResourceType.Medication));
    verify(iReadTyped, never()).withId("med-known");
  }

  @Test
  public void secondaryResources_immunization() {
    Practitioner practitioner = practitioner("pr-1");
    Organization manufacturer = organization("man-1");
    serveReadsById(Map.of("pr-1", practitioner, "man-1", manufacturer));
    KarProcessingData kd = new KarProcessingData();

    Immunization imm = new Immunization();
    imm.addPerformer(); // no actor
    imm.addPerformer().setActor(new Reference("Organization/org-perf")); // not a practitioner
    imm.addPerformer().setActor(new Reference().setDisplay("no reference"));
    imm.addPerformer().setActor(new Reference("Practitioner/pr-1"));
    imm.setManufacturer(new Reference("Organization/man-1"));
    readByUrl(kd, imm);

    // manufacturer that is not an Organization is ignored
    readByUrl(kd, new Immunization().setManufacturer(new Reference("Practitioner/pr-x")));

    assertSame(practitioner, kd.getResourceById("pr-1", ResourceType.Practitioner));
    assertSame(manufacturer, kd.getResourceById("man-1", ResourceType.Organization));
    verify(iReadTyped, never()).withId("org-perf");
    verify(iReadTyped, never()).withId("pr-x");
  }

  @Test
  public void secondaryResources_observation() {
    Practitioner practitioner = practitioner("p1");
    Specimen specimen = specimen("s1");
    ServiceRequest serviceRequest = serviceRequest("sr1");
    serveReadsById(Map.of("p1", practitioner, "s1", specimen, "sr1", serviceRequest));
    KarProcessingData kd = new KarProcessingData();

    Observation obs = observation("obs-1", null);
    obs.addPerformer(new Reference("Organization/o1"));
    obs.addPerformer(new Reference("Practitioner/p1"));
    obs.setSpecimen(new Reference("Specimen/s1"));
    obs.addBasedOn(new Reference("ServiceRequest/sr1"));
    obs.addBasedOn(new Reference("CarePlan/cp1"));
    readByUrl(kd, obs);

    Observation groupSpecimen = observation("obs-2", null);
    groupSpecimen.setSpecimen(new Reference("Group/g1"));
    readByUrl(kd, groupSpecimen);

    assertSame(practitioner, kd.getResourceById("p1", ResourceType.Practitioner));
    assertSame(specimen, kd.getResourceById("s1", ResourceType.Specimen));
    assertSame(serviceRequest, kd.getResourceById("sr1", ResourceType.ServiceRequest));
    verify(iReadTyped, never()).withId("o1");
    verify(iReadTyped, never()).withId("cp1");
    verify(iReadTyped, never()).withId("g1");
  }

  @Test
  public void secondaryResources_diagnosticReport() {
    Observation fetchedResult = observation("obs-fetched", null);
    fetchedResult.addPerformer(new Reference("Practitioner/p1"));
    fetchedResult.addPerformer(new Reference("Organization/o4"));
    fetchedResult.addBasedOn(new Reference("ServiceRequest/sr2"));
    fetchedResult.addBasedOn(new Reference("CarePlan/cp2"));
    fetchedResult.setSpecimen(new Reference("Specimen/s2"));
    Observation specimenOnlyResult = observation("obs-spec", null);
    specimenOnlyResult.setSpecimen(new Reference("Group/g2"));

    Map<String, Resource> served = new HashMap<>();
    served.put("s1", specimen("s1"));
    served.put("sr1", serviceRequest("sr1"));
    served.put("obs-fetched", fetchedResult);
    served.put("obs-spec", specimenOnlyResult);
    served.put("obs-bad", observation("obs-bad", Observation.ObservationStatus.CANCELLED));
    served.put("p1", practitioner("p1"));
    served.put("sr2", serviceRequest("sr2"));
    served.put("s2", specimen("s2"));
    served.put("p3", practitioner("p3"));
    served.put("o3", organization("o3"));
    serveReadsById(served);

    KarProcessingData kd = new KarProcessingData();
    Observation knownResult = observation("obs-known", null);
    kd.storeResourceById("obs-known", knownResult);

    DiagnosticReport report = new DiagnosticReport();
    report.setId("dr-1");
    report.addSpecimen(new Reference("Specimen/s1"));
    report.addSpecimen(new Reference("Group/g1"));
    report.addBasedOn(new Reference("ServiceRequest/sr1"));
    report.addBasedOn(new Reference("CarePlan/cp1"));
    report.addResult(new Reference("Observation/obs-known"));
    report.addResult(new Reference("Observation/obs-fetched"));
    report.addResult(new Reference("Observation/obs-spec"));
    report.addResult(new Reference("Observation/obs-bad"));
    report.addResult(new Reference("Observation/obs-missing"));
    report.addPerformer(new Reference("Practitioner/p3"));
    report.addPerformer(new Reference("Organization/o3"));
    report.addPerformer(new Reference("Patient/pt1")); // neither Practitioner nor Organization
    report.addPerformer(new Reference("Foo/1")); // unknown resource type -> null
    report.addPerformer(new Reference("123")); // no resource type -> null
    readByUrl(kd, report);

    assertNotNull(kd.getResourceById("s1", ResourceType.Specimen));
    assertNotNull(kd.getResourceById("sr1", ResourceType.ServiceRequest));
    assertSame(fetchedResult, kd.getResourceById("obs-fetched", ResourceType.Observation));
    assertNull(kd.getResourceById("obs-bad", ResourceType.Observation));
    assertNotNull(kd.getResourceById("p1", ResourceType.Practitioner));
    assertNotNull(kd.getResourceById("sr2", ResourceType.ServiceRequest));
    assertNotNull(kd.getResourceById("s2", ResourceType.Specimen));
    assertNotNull(kd.getResourceById("p3", ResourceType.Practitioner));
    assertNotNull(kd.getResourceById("o3", ResourceType.Organization));
    verify(iReadTyped, never()).withId("obs-known");
    verify(iReadTyped, never()).withId("pt1");
    verify(iReadTyped, never()).withId("g1");
    verify(iReadTyped, never()).withId("cp1");
  }

  @Test
  public void secondaryResources_serviceRequestRequesterThatIsNotAPractitioner() {
    KarProcessingData kd = new KarProcessingData();

    readByUrl(kd, new ServiceRequest().setRequester(new Reference("Organization/o1")));
    readByUrl(kd, new ServiceRequest());

    verify(iReadTyped, never()).withId(anyString());
  }

  // ---------------- loadJurisdicationData ----------------

  @Test
  public void loadJurisdicationData_loadsPractitionersOrganizationsAndLocationsOnce() {
    serveReadsById(
        Map.of(
            "p1", practitioner("p1"),
            "org-1", organization("org-1"),
            "loc-1", location("loc-1")));

    Encounter enc1 = new Encounter();
    enc1.setId("enc-1");
    enc1.addParticipant().setIndividual(new Reference("Practitioner/p1"));
    enc1.addParticipant().setIndividual(new Reference("Practitioner/p1")); // same id again
    enc1.addParticipant().setIndividual(new Reference("Practitioner/p-known")); // already in kd
    enc1.addParticipant().setIndividual(new Reference("Practitioner/p-missing")); // not found
    enc1.setServiceProvider(new Reference("Organization/org-1"));
    enc1.addLocation().setLocation(new Reference("Location/loc-1"));
    enc1.addLocation().setLocation(new Reference("Location/loc-known")); // already in kd
    enc1.addLocation().setLocation(new Reference("Location/loc-missing")); // not found
    enc1.addLocation(); // no location reference

    Encounter enc2 = new Encounter();
    enc2.setId("enc-2");
    enc2.setServiceProvider(new Reference("Organization/org-known")); // already in kd

    Encounter enc3 = new Encounter();
    enc3.setId("enc-3");
    enc3.setServiceProvider(new Reference().setDisplay("no reference")); // no reference element

    Encounter enc4 = new Encounter();
    enc4.setId("enc-4");
    enc4.setServiceProvider(new Reference("Organization/org-missing")); // not found

    when(karProcessingData.getResourcesByType(ResourceType.Encounter.toString()))
        .thenReturn(new HashSet<>(List.of(enc1, enc2, enc3, enc4)));
    when(karProcessingData.containsResourceWithId("p-known", ResourceType.Practitioner))
        .thenReturn(true);
    when(karProcessingData.containsResourceWithId("loc-known", ResourceType.Location))
        .thenReturn(true);
    when(karProcessingData.containsResourceWithId("org-known", ResourceType.Organization))
        .thenReturn(true);

    ehrFhirR4QueryService.loadJurisdicationData(karProcessingData);

    verify(karProcessingData, times(1)).storeResourceById(eq("p1"), any());
    verify(karProcessingData).storeResourceById(eq("org-1"), any());
    verify(karProcessingData).storeResourceById(eq("loc-1"), any());
    verify(iReadTyped, times(1)).withId("p1");
    verify(iReadTyped, never()).withId("p-known");
    verify(iReadTyped, never()).withId("loc-known");
    verify(iReadTyped, never()).withId("org-known");
    // practitioners, locations and organizations are each added once
    verify(karProcessingData, times(3)).addResourcesByType(anyMap());
  }

  @Test
  public void loadJurisdicationData_encounterWithNothingToLoad_addsNothing() {
    Encounter bare = new Encounter();
    bare.setId("enc-bare");
    when(karProcessingData.getResourcesByType(ResourceType.Encounter.toString()))
        .thenReturn(new HashSet<>(List.of(bare)));

    ehrFhirR4QueryService.loadJurisdicationData(karProcessingData);

    verify(karProcessingData, never()).addResourcesByType(anyMap());
    verify(iReadTyped, never()).withId(anyString());
  }

  // ---------------- helpers for the new tests ----------------

  private static Location location(String id) {
    Location l = new Location();
    l.setId(id);
    return l;
  }

  private Boolean valid(Resource resource) {
    return ehrFhirR4QueryService.isValidResource(resource);
  }

  /** getResourceByUrl(client, ...) for the given resource; secondary reads go to serveReadsById. */
  private void readByUrl(KarProcessingData kd, Resource resource) {
    serveUrlRead(resource);
    ehrFhirR4QueryService.getResourceByUrl(
        fhirClient, fhirContext, resource.fhirType(), "url?patient=Patient/1", kd);
  }

  private void serveUrlRead(Resource resource) {
    IReadExecutable exec = readReturning(resource);
    when(iReadTyped.withUrl(anyString())).thenReturn(exec);
  }

  /** read().resource(x).withId(id) returns the resource registered for that id, else null. */
  private void serveReadsById(Map<String, Resource> byId) {
    Map<String, IReadExecutable> execs = new HashMap<>();
    byId.forEach((id, r) -> execs.put(id, readReturning(r)));
    IReadExecutable notFound = readReturning(null);
    when(iReadTyped.withId(anyString()))
        .thenAnswer(inv -> execs.getOrDefault(inv.getArgument(0), notFound));
  }

  private static IReadExecutable readReturning(Resource resource) {
    IReadExecutable exec = mock(IReadExecutable.class);
    when(exec.execute()).thenReturn(resource);
    return exec;
  }

  private void useDefaultQuery(String query) {
    when(fhirQueryFilter.getRelatedDataId()).thenReturn("rel-1");
    when(karProcessingData.getKar()).thenReturn(knowledgeArtifact);
    when(karProcessingData.getKarIdForCustomQueries()).thenReturn("kar-q");
    when(knowledgeArtifact.getQueryForDataRequirement("dr-1", "rel-1")).thenReturn(query);
    when(notificationContext.getFhirServerBaseUrl()).thenReturn("http://ehr");
    when(iQuery.execute()).thenReturn(new Bundle());
  }

  private void useCustomQueries(String karFile, Map<String, String> queries) {
    HashMap<String, HashMap<String, String>> custom = new HashMap<>();
    custom.put(karFile, new HashMap<>(queries));
    ReflectionTestUtils.setField(ehrFhirR4QueryService, "customQueries", custom);
  }

  private static BaseServerResponseException serverError(OperationOutcome outcome) {
    BaseServerResponseException ex = new BaseServerResponseException(500, "server error") {};
    if (outcome != null) {
      ex.setOperationOutcome(outcome);
    }
    return ex;
  }

  private static Bundle bundleOf(Resource... resources) {
    Bundle bundle = new Bundle();
    for (Resource r : resources) {
      bundle.addEntry().setResource(r);
    }
    return bundle;
  }

  private static Bundle.BundleEntryComponent entry(Resource resource) {
    return new Bundle.BundleEntryComponent().setResource(resource);
  }

  private static DataRequirement dataRequirement(String id, String type) {
    DataRequirement dr = new DataRequirement();
    dr.setId(id);
    dr.setType(type);
    return dr;
  }

  private static Observation observation(String id, Observation.ObservationStatus status) {
    Observation obs = new Observation();
    obs.setId(id);
    if (status != null) {
      obs.setStatus(status);
    }
    return obs;
  }

  private static Condition condition(String id) {
    Condition c = new Condition();
    c.setId(id);
    return c;
  }

  private static Medication medication(String id) {
    Medication m = new Medication();
    m.setId(id);
    return m;
  }

  private static Practitioner practitioner(String id) {
    Practitioner p = new Practitioner();
    p.setId(id);
    return p;
  }

  private static Organization organization(String id) {
    Organization o = new Organization();
    o.setId(id);
    return o;
  }

  private static Specimen specimen(String id) {
    Specimen s = new Specimen();
    s.setId(id);
    return s;
  }

  private static ServiceRequest serviceRequest(String id) {
    ServiceRequest sr = new ServiceRequest();
    sr.setId(id);
    sr.setStatus(ServiceRequest.ServiceRequestStatus.ACTIVE);
    return sr;
  }
}
