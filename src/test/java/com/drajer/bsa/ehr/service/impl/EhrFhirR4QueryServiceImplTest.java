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
import java.util.*;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
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
    String actual =
        ehrFhirR4QueryService.substituteContextParams(karProcessingData, queryToExecute, true);
    verify(karProcessingData, times(2)).getContextEncounter();
    //        assertTrue(actual.contains("ge2025-09-08"));

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
            fhirClient, fhirContext, "Encounter?patient=Patient/1234");
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
    when(iReadTyped.withUrl(anyString())).thenReturn(iReadExecutable);
    when(iReadExecutable.execute()).thenThrow(ex);

    Resource result =
        ehrFhirR4QueryService.getResourceById(fhirClient, fhirContext, "Observation", "123", false);

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
        } else {
        }
      } else {
        customQueries = new HashMap<>();
        customQueries.put(filenameWithoutExt, queries);
      }

    } catch (Exception e) {

    }
    return customQueries;
  }
}
