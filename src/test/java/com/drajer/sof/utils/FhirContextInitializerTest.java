package com.drajer.sof.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.context.PerformanceOptionsEnum;
import ca.uhn.fhir.model.dstu2.resource.Bundle;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.api.ServerValidationModeEnum;
import ca.uhn.fhir.rest.client.interceptor.AdditionalRequestHeadersInterceptor;
import ca.uhn.fhir.rest.gclient.ICreate;
import ca.uhn.fhir.rest.gclient.ICreateTyped;
import ca.uhn.fhir.rest.gclient.IGetPage;
import ca.uhn.fhir.rest.gclient.IGetPageTyped;
import ca.uhn.fhir.rest.gclient.IQuery;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import ca.uhn.fhir.rest.gclient.IUntypedQuery;
import ca.uhn.fhir.rest.server.exceptions.ForbiddenOperationException;
import ca.uhn.fhir.rest.server.exceptions.InternalErrorException;
import com.drajer.bsa.ehr.service.EhrHeaderInterceptorInterface;
import com.drajer.eca.model.EventTypes;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.ecrapp.fhir.utils.ecrretry.EcrFhirRetryClient;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.test.util.TestUtils;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import java.io.IOException;
import java.util.Date;
import org.apache.commons.lang3.time.DateUtils;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.r4.model.Patient;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

public class FhirContextInitializerTest {

  private IUntypedQuery mockIUntypedQuery;
  private IQuery mockIquery;
  private IQuery mockIquery2;
  private Bundle mockBundle;
  private FhirContext context;
  private IGenericClient client;
  private LaunchDetails launchDetails;

  @Mock FHIRRetryTemplate retryTemplateMock;
  @Mock EhrHeaderInterceptorInterface headerInterceptorMock;

  private FhirContextInitializer fhirContextInitializer;

  @Before
  public void init() {
    MockitoAnnotations.initMocks(this);
    // The constructor now takes only the retry template; headerInterceptor is an @Autowired
    // package-private field, so it is set directly (same package as the main class).
    fhirContextInitializer = new FhirContextInitializer(retryTemplateMock);
    fhirContextInitializer.headerInterceptor = headerInterceptorMock;
    context = Mockito.mock(FhirContext.class);
    client = Mockito.mock(IGenericClient.class);
    mockIUntypedQuery = Mockito.mock(IUntypedQuery.class);
    mockIquery = Mockito.mock(IQuery.class);
    mockIquery2 = Mockito.mock(IQuery.class);
    mockBundle = Mockito.mock(Bundle.class);

    launchDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);

    // Every field below is a @Value that Spring fills in at runtime. Outside the
    // container they stay null, and createClient multiplies the Integers while
    // checkSkipResource calls isEmpty() on the Strings - so the whole class threw
    // NullPointerException in a plain unit test. These are the declared defaults.
    ReflectionTestUtils.setField(fhirContextInitializer, "socketTimeout", 3);
    ReflectionTestUtils.setField(fhirContextInitializer, "connectionTimeout", 3);
    ReflectionTestUtils.setField(fhirContextInitializer, "poolMaxPerRoute", 10);
    ReflectionTestUtils.setField(fhirContextInitializer, "poolMaxTotal", 100);
    ReflectionTestUtils.setField(fhirContextInitializer, "connectionReqTimeOut", 30);
    ReflectionTestUtils.setField(fhirContextInitializer, "pagingCountEnabled", false);
    ReflectionTestUtils.setField(fhirContextInitializer, "pagingCount", 500);
    ReflectionTestUtils.setField(fhirContextInitializer, "pagingResource", "");
    ReflectionTestUtils.setField(fhirContextInitializer, "queryByDateEnabled", false);
    ReflectionTestUtils.setField(fhirContextInitializer, "dateResources", "");
    ReflectionTestUtils.setField(fhirContextInitializer, "lastUpdatedResources", "");
    ReflectionTestUtils.setField(fhirContextInitializer, "queryByEncounterEnabled", false);
    ReflectionTestUtils.setField(fhirContextInitializer, "resourcesByEncounter", "");
    ReflectionTestUtils.setField(fhirContextInitializer, "queryByLastTriggerDateEnabled", false);
    ReflectionTestUtils.setField(fhirContextInitializer, "skipResource", "");
    ReflectionTestUtils.setField(fhirContextInitializer, "skipTriggerResource", "");
  }

  // @Test
  public void getResourceByPatientIdTest()
      throws JsonParseException, JsonMappingException, IOException {

    when(client.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(any())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(mockBundle);

    IBaseBundle bundleResponse =
        ReflectionTestUtils.invokeMethod(
            fhirContextInitializer,
            "getResourceByPatientId",
            launchDetails,
            client,
            context,
            "Encounter");

    assertNotNull(bundleResponse);
  }

  // @Test
  public void getObservationByPatientIdTest()
      throws JsonParseException, JsonMappingException, IOException {

    when(client.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(any())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(mockBundle);

    IBaseBundle bundleResponse =
        ReflectionTestUtils.invokeMethod(
            fhirContextInitializer,
            "getObservationByPatientId",
            launchDetails,
            client,
            context,
            "Encounter",
            "Laboratory");

    assertNotNull(bundleResponse);
  }

  // @Test
  public void getObservationByPatientIdAndCodeTest()
      throws JsonParseException, JsonMappingException, IOException {

    when(client.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(any())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(mockBundle);

    IBaseBundle bundleResponse =
        ReflectionTestUtils.invokeMethod(
            fhirContextInitializer,
            "getResourceByPatientIdAndCode",
            launchDetails,
            client,
            context,
            "Encounter",
            "Laboratory",
            "Practitioner");

    assertNotNull(bundleResponse);
  }

  // @Test
  public void testPerformanceInCreateClient() {

    FhirContext fhirContext = FhirContext.forR4();
    Date launchDetailsStartDate = DateUtils.addDays(new Date(), -20);
    LaunchDetails mockDetails = mock(LaunchDetails.class);
    when(mockDetails.getEncounterId()).thenReturn("123");
    when(mockDetails.getFhirVersion()).thenReturn("R4");
    when(mockDetails.getEhrServerURL()).thenReturn("");
    when(mockDetails.getAccessToken()).thenReturn("");
    when(mockDetails.getxRequestId()).thenReturn("");
    when(mockDetails.getStartDate()).thenReturn(launchDetailsStartDate);
    when(retryTemplateMock.isRetryEnabled()).thenReturn(false);

    client =
        fhirContextInitializer.createClient(
            fhirContext,
            mockDetails.getEhrServerURL(),
            mockDetails.getAccessToken(),
            mockDetails.getxRequestId(),
            null);
    assertNotNull(fhirContext.getPerformanceOptions());
    assertTrue(
        fhirContext
            .getPerformanceOptions()
            .contains(PerformanceOptionsEnum.DEFERRED_MODEL_SCANNING));
    assertEquals(
        ServerValidationModeEnum.NEVER,
        fhirContext.getRestfulClientFactory().getServerValidationMode());
    assertEquals(fhirContext.getRestfulClientFactory().getSocketTimeout(), (60 * 1000));
  }

  private static final String EHR_BASE = "http://localhost:9011/FHIR";

  @Test
  public void testGetFhirContext_mapsVersionStringToContext() {
    assertEquals(
        FhirVersionEnum.DSTU2,
        fhirContextInitializer.getFhirContext("DSTU2").getVersion().getVersion());
    assertEquals(
        FhirVersionEnum.R4, fhirContextInitializer.getFhirContext("R4").getVersion().getVersion());
    assertEquals(
        "an unrecognised version must fall through to the DSTU2 default",
        FhirVersionEnum.DSTU2,
        fhirContextInitializer.getFhirContext("R5").getVersion().getVersion());
  }

  @Test
  public void testCreateClient_appliesConfiguredTimeoutsAndRegistersInterceptors() {
    FhirContext fhirContext = FhirContext.forR4();
    AdditionalRequestHeadersInterceptor ehrHeaders = new AdditionalRequestHeadersInterceptor();
    when(headerInterceptorMock.getHeaderInterceptor("tenant-ctx")).thenReturn(ehrHeaders);
    when(retryTemplateMock.isRetryEnabled()).thenReturn(false);

    IGenericClient created =
        fhirContextInitializer.createClient(
            fhirContext, EHR_BASE, "bearer-token-abc", "req-0001", "tenant-ctx");

    assertTrue(created instanceof FhirClient);
    assertEquals("req-0001", ((FhirClient) created).getHttpInterceptor().getXReqId());
    verify(headerInterceptorMock).getHeaderInterceptor("tenant-ctx");

    // createClient writes 60s first, then overwrites it with the configured socketTimeout.
    assertEquals(3 * 1000, fhirContext.getRestfulClientFactory().getSocketTimeout());
    assertEquals(3 * 1000, fhirContext.getRestfulClientFactory().getConnectTimeout());
    assertEquals(
        ServerValidationModeEnum.NEVER,
        fhirContext.getRestfulClientFactory().getServerValidationMode());
    assertTrue(
        fhirContext
            .getPerformanceOptions()
            .contains(PerformanceOptionsEnum.DEFERRED_MODEL_SCANNING));
  }

  @Test
  public void testCreateClient_withoutTokenOrEhrContext_skipsThoseInterceptors() {
    when(retryTemplateMock.isRetryEnabled()).thenReturn(false);

    IGenericClient created =
        fhirContextInitializer.createClient(FhirContext.forR4(), EHR_BASE, "", "req-0002", null);

    assertTrue(created instanceof FhirClient);
    verify(headerInterceptorMock, never()).getHeaderInterceptor(anyString());
  }

  @Test
  public void testCreateClient_retryEnabled_returnsRetryingClient() {
    when(retryTemplateMock.isRetryEnabled()).thenReturn(true);

    IGenericClient created =
        fhirContextInitializer.createClient(
            FhirContext.forR4(), EHR_BASE, "bearer-token-abc", "req-0003", null);

    assertTrue(
        created instanceof EcrFhirRetryClient,
        "a retry-enabled template must yield an EcrFhirRetryClient");
  }

  @Test
  public void testCreateClient_fromLaunchDetails_usesLaunchDetailsRequestId() {
    launchDetails.setEhrServerURL(EHR_BASE);
    launchDetails.setxRequestId("req-launch-01");
    when(retryTemplateMock.isRetryEnabled()).thenReturn(false);

    IGenericClient created =
        fhirContextInitializer.createClient(
            FhirContext.forR4(), launchDetails, EventTypes.QueryType.LOADING_QUERY);

    assertTrue(created instanceof FhirClient);
    assertEquals("req-launch-01", ((FhirClient) created).getHttpInterceptor().getXReqId());
  }

  @Test
  public void testCreateClient_fromLaunchDetails_retryEnabled() {
    launchDetails.setEhrServerURL(EHR_BASE);
    launchDetails.setxRequestId("req-launch-02");
    when(retryTemplateMock.isRetryEnabled()).thenReturn(true);

    IGenericClient created =
        fhirContextInitializer.createClient(
            FhirContext.forR4(), launchDetails, EventTypes.QueryType.TRIGGER_QUERY);

    assertTrue(created instanceof EcrFhirRetryClient);
  }

  @Test
  public void testCheckSkipResource_honoursSkipListAndQueryType() {
    ReflectionTestUtils.setField(fhirContextInitializer, "skipResource", "Immunization|Device");
    ReflectionTestUtils.setField(fhirContextInitializer, "skipTriggerResource", "Observation");

    FhirClient loading = newFhirClient(EventTypes.QueryType.LOADING_QUERY);
    FhirClient trigger = newFhirClient(EventTypes.QueryType.TRIGGER_QUERY);

    assertTrue(fhirContextInitializer.checkSkipResource("Immunization", loading));
    assertFalse(fhirContextInitializer.checkSkipResource("Condition", loading));

    assertTrue(
        fhirContextInitializer.checkSkipResource("Observation", trigger),
        "the trigger-query skip list only applies to a TRIGGER_QUERY client");
    assertFalse(fhirContextInitializer.checkSkipResource("Observation", loading));
    assertFalse(fhirContextInitializer.checkSkipResource("Condition", trigger));
  }

  /** Uses the values shipped in application.properties rather than invented ones. */
  @Test
  public void testGetCustomQueryParameters_withProductionConfiguration() {
    ReflectionTestUtils.setField(fhirContextInitializer, "pagingCountEnabled", true);
    ReflectionTestUtils.setField(
        fhirContextInitializer,
        "pagingResource",
        "ServiceRequest|MedicationRequest|Observation|DiagnosticReport");
    ReflectionTestUtils.setField(fhirContextInitializer, "queryByDateEnabled", true);
    ReflectionTestUtils.setField(
        fhirContextInitializer, "dateResources", "Observation|DiagnosticReport|Immunization");
    ReflectionTestUtils.setField(
        fhirContextInitializer, "lastUpdatedResources", "ServiceRequest|MedicationRequest");
    ReflectionTestUtils.setField(fhirContextInitializer, "queryByEncounterEnabled", true);
    ReflectionTestUtils.setField(
        fhirContextInitializer, "resourcesByEncounter", "Condition|DiagnosticReport");

    launchDetails.setStartDate(new Date(1705276800000L)); // 2024-01-15 UTC
    launchDetails.setEncounterId("97953900");
    FhirClient client = newFhirClient(EventTypes.QueryType.LOADING_QUERY);

    String observation =
        ReflectionTestUtils.invokeMethod(
            fhirContextInitializer,
            "getCustomQueryParameters",
            "Observation",
            launchDetails,
            client);
    assertEquals("&_count=500&date=ge2024-01-15", observation);

    String medicationRequest =
        ReflectionTestUtils.invokeMethod(
            fhirContextInitializer,
            "getCustomQueryParameters",
            "MedicationRequest",
            launchDetails,
            client);
    assertEquals(
        "a lastUpdated resource must use the _lastUpdated parameter instead of date",
        "&_count=500&_lastUpdated=ge2024-01-15T00:00:00.000Z",
        medicationRequest);

    String condition =
        ReflectionTestUtils.invokeMethod(
            fhirContextInitializer, "getCustomQueryParameters", "Condition", launchDetails, client);
    assertEquals(
        "&encounter=97953900"
            + "&category=problem-list-item,encounter-diagnosis"
            + "&clinical-status=active,recurrence,relapse",
        condition);
  }

  @Test
  public void testGetResourceByPatientId_buildsPatientScopedUrl() {
    launchDetails.setEhrServerURL(EHR_BASE);
    launchDetails.setLaunchPatientId("12742571");
    launchDetails.setFhirVersion("R4");

    org.hl7.fhir.r4.model.Bundle bundle = r4BundleWithOneCondition();
    FhirClient client = newFhirClient(EventTypes.QueryType.LOADING_QUERY);
    when(client.client.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(anyString())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(org.hl7.fhir.r4.model.Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(bundle);

    IBaseBundle response =
        fhirContextInitializer.getResourceByPatientId(
            launchDetails, client, FhirContext.forR4(), "Immunization");

    assertSame(bundle, response);
    ArgumentCaptor<String> url = ArgumentCaptor.forClass(String.class);
    verify(mockIUntypedQuery).byUrl(url.capture());
    assertEquals(EHR_BASE + "/Immunization?patient=12742571", url.getValue());
  }

  @Test
  public void testGetResourceByPatientId_skippedResourceIsNeverQueried() {
    ReflectionTestUtils.setField(fhirContextInitializer, "skipResource", "Condition");
    FhirClient client = newFhirClient(EventTypes.QueryType.LOADING_QUERY);

    assertNull(
        fhirContextInitializer.getResourceByPatientId(
            launchDetails, client, FhirContext.forR4(), "Condition"));
    verify(client.client, never()).search();
  }

  @Test
  public void testGetResourceBundleByUrl_r4FollowsNextPageLinks() {
    launchDetails.setLaunchPatientId("12742571");
    launchDetails.setFhirVersion("R4");

    org.hl7.fhir.r4.model.Bundle firstPage = r4BundleWithOneCondition();
    firstPage.addLink().setRelation(IBaseBundle.LINK_NEXT).setUrl(EHR_BASE + "/Condition?page=2");
    org.hl7.fhir.r4.model.Bundle secondPage = r4BundleWithOneCondition();

    IGenericClient genericClient = mock(IGenericClient.class);
    when(genericClient.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(anyString())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(org.hl7.fhir.r4.model.Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(firstPage);

    IGetPage getPage = mock(IGetPage.class);
    IGetPageTyped getPageTyped = mock(IGetPageTyped.class);
    when(genericClient.loadPage()).thenReturn(getPage);
    when(getPage.next(any(org.hl7.fhir.r4.model.Bundle.class))).thenReturn(getPageTyped);
    when(getPageTyped.execute()).thenReturn(secondPage);

    IBaseBundle result =
        FhirContextInitializer.getResourceBundleByUrl(
            launchDetails,
            genericClient,
            FhirContext.forR4(),
            "Condition",
            EHR_BASE + "/Condition");

    assertSame(firstPage, result);
    assertEquals(
        "the second page entry must be appended to the first page", 2, firstPage.getEntry().size());
  }

  @Test
  public void testGetResourceBundleByUrl_dstu2() {
    launchDetails.setLaunchPatientId("12742571");
    launchDetails.setFhirVersion("DSTU2");

    Bundle dstu2Bundle = new Bundle();
    dstu2Bundle.addEntry().setFullUrl(EHR_BASE + "/Condition/con-1");

    IGenericClient genericClient = mock(IGenericClient.class);
    when(genericClient.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(anyString())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(dstu2Bundle);

    IBaseBundle result =
        FhirContextInitializer.getResourceBundleByUrl(
            launchDetails,
            genericClient,
            FhirContext.forDstu2(),
            "Condition",
            EHR_BASE + "/Condition");

    assertSame(dstu2Bundle, result);
  }

  @Test
  public void testGetResourceBundleByUrl_swallowsFailuresAndReturnsNull() {
    launchDetails.setLaunchPatientId("12742571");
    launchDetails.setFhirVersion("R4");
    launchDetails.setScope("system/Condition.read");
    FhirContext context = FhirContext.forR4();

    IGenericClient genericClient = mock(IGenericClient.class);
    when(genericClient.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(anyString())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(org.hl7.fhir.r4.model.Bundle.class)).thenReturn(mockIquery2);

    when(mockIquery2.execute()).thenThrow(new ForbiddenOperationException("scope not granted"));
    assertNull(
        "an out-of-scope query must be logged and return null",
        FhirContextInitializer.getResourceBundleByUrl(
            launchDetails, genericClient, context, "Condition", EHR_BASE + "/Condition"));

    reset(mockIquery2);
    when(mockIquery2.execute()).thenThrow(new InternalErrorException("server blew up"));
    assertNull(
        FhirContextInitializer.getResourceBundleByUrl(
            launchDetails, genericClient, context, "Condition", EHR_BASE + "/Condition"));

    reset(mockIquery2);
    when(mockIquery2.execute()).thenThrow(new IllegalStateException("socket closed"));
    assertNull(
        FhirContextInitializer.getResourceBundleByUrl(
            launchDetails, genericClient, context, "Condition", EHR_BASE + "/Condition"));
  }

  @Test
  public void testGetResouceById_returnsResourceAndSwallowsFailures() {
    launchDetails.setScope("system/Patient.read");
    FhirContext context = FhirContext.forR4();
    FhirClient client = newFhirClient(EventTypes.QueryType.LOADING_QUERY);

    Patient patient = new Patient();
    patient.setId("12742571");

    IRead read = mock(IRead.class);
    IReadTyped readTyped = mock(IReadTyped.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);
    when(client.client.read()).thenReturn(read);
    when(read.resource("Patient")).thenReturn(readTyped);
    when(readTyped.withId("12742571")).thenReturn(readExecutable);
    when(readExecutable.execute()).thenReturn(patient);

    assertSame(
        patient,
        fhirContextInitializer.getResouceById(
            launchDetails, client, context, "Patient", "12742571"));

    reset(readExecutable);
    when(readExecutable.execute()).thenThrow(new ForbiddenOperationException("no scope"));
    assertNull(
        fhirContextInitializer.getResouceById(
            launchDetails, client, context, "Patient", "12742571"));

    reset(readExecutable);
    when(readExecutable.execute()).thenThrow(new IllegalStateException("socket closed"));
    assertNull(
        fhirContextInitializer.getResouceById(
            launchDetails, client, context, "Patient", "12742571"));
  }

  @Test
  public void testSubmitResource_returnsOutcomeAndSwallowsFailures() {
    IGenericClient genericClient = mock(IGenericClient.class);
    ICreate create = mock(ICreate.class);
    ICreateTyped createTyped = mock(ICreateTyped.class);
    MethodOutcome outcome = new MethodOutcome();

    Patient patient = new Patient();
    patient.setId("12742571");

    when(genericClient.create()).thenReturn(create);
    when(create.resource(patient)).thenReturn(createTyped);
    when(createTyped.prettyPrint()).thenReturn(createTyped);
    when(createTyped.encodedJson()).thenReturn(createTyped);
    when(createTyped.execute()).thenReturn(outcome);

    assertSame(outcome, fhirContextInitializer.submitResource(genericClient, patient));

    reset(createTyped);
    when(createTyped.prettyPrint()).thenReturn(createTyped);
    when(createTyped.encodedJson()).thenReturn(createTyped);
    when(createTyped.execute()).thenThrow(new InternalErrorException("rejected"));
    assertNull(
        "a submission failure must be logged rather than propagated",
        fhirContextInitializer.submitResource(genericClient, patient));
  }

  @Test
  public void testHeaderInterceptorAccessorsAndSetters() {
    assertSame(headerInterceptorMock, fhirContextInitializer.getHeaderInterceptor());

    // The setters now really replace the injected values.
    EhrHeaderInterceptorInterface newInterceptor = mock(EhrHeaderInterceptorInterface.class);
    FHIRRetryTemplate newRetryTemplate = mock(FHIRRetryTemplate.class);
    fhirContextInitializer.setHeaderInterceptor(newInterceptor);
    fhirContextInitializer.setRetryTemplate(newRetryTemplate);

    assertSame(newInterceptor, fhirContextInitializer.getHeaderInterceptor());
    assertSame(newRetryTemplate, fhirContextInitializer.retryTemplate);
  }

  @Test
  public void testGetObservationByPatientId_addsTheCategoryToThePatientQuery() {
    launchDetails.setEhrServerURL(EHR_BASE);
    launchDetails.setLaunchPatientId("12742571");
    launchDetails.setFhirVersion("R4");
    org.hl7.fhir.r4.model.Bundle bundle = r4BundleWithOneCondition();
    FhirClient client = newFhirClient(EventTypes.QueryType.LOADING_QUERY);
    when(client.client.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(anyString())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(org.hl7.fhir.r4.model.Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(bundle);

    IBaseBundle response =
        fhirContextInitializer.getObservationByPatientId(
            launchDetails, client, FhirContext.forR4(), "Observation", "laboratory");

    assertSame(bundle, response);
    ArgumentCaptor<String> url = ArgumentCaptor.forClass(String.class);
    verify(mockIUntypedQuery).byUrl(url.capture());
    assertEquals(EHR_BASE + "/Observation?patient=12742571&category=laboratory", url.getValue());
  }

  @Test
  public void testGetResourceByPatientIdAndCode_addsTheSystemAndCodeToThePatientQuery() {
    launchDetails.setEhrServerURL(EHR_BASE);
    launchDetails.setLaunchPatientId("12742571");
    launchDetails.setFhirVersion("R4");
    org.hl7.fhir.r4.model.Bundle bundle = r4BundleWithOneCondition();
    FhirClient client = newFhirClient(EventTypes.QueryType.LOADING_QUERY);
    when(client.client.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(anyString())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(org.hl7.fhir.r4.model.Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(bundle);

    IBaseBundle response =
        fhirContextInitializer.getResourceByPatientIdAndCode(
            launchDetails,
            client,
            FhirContext.forR4(),
            "Observation",
            "94558-4",
            "http://loinc.org");

    assertSame(bundle, response);
    ArgumentCaptor<String> url = ArgumentCaptor.forClass(String.class);
    verify(mockIUntypedQuery).byUrl(url.capture());
    assertEquals(
        EHR_BASE + "/Observation?patient=12742571&code=http://loinc.org|94558-4", url.getValue());
  }

  @Test
  public void testObservationQueries_skippedResourceIsNeverQueried() {
    ReflectionTestUtils.setField(fhirContextInitializer, "skipResource", "Observation");
    FhirClient client = newFhirClient(EventTypes.QueryType.LOADING_QUERY);

    assertNull(
        fhirContextInitializer.getObservationByPatientId(
            launchDetails, client, FhirContext.forR4(), "Observation", "laboratory"));
    assertNull(
        fhirContextInitializer.getResourceByPatientIdAndCode(
            launchDetails, client, FhirContext.forR4(), "Observation", "94558-4", "loinc"));
    verify(client.client, never()).search();
  }

  @Test
  public void testGetResourceBundleByUrl_dstu2FollowsNextPageLinks() {
    launchDetails.setLaunchPatientId("12742571");
    launchDetails.setFhirVersion("DSTU2");
    Bundle firstPage = new Bundle();
    firstPage.addEntry().setFullUrl(EHR_BASE + "/Condition/con-1");
    firstPage.addLink().setRelation("next").setUrl(EHR_BASE + "/Condition?page=2");
    Bundle secondPage = new Bundle();
    secondPage.addEntry().setFullUrl(EHR_BASE + "/Condition/con-2");
    IGenericClient genericClient = mock(IGenericClient.class);
    when(genericClient.search()).thenReturn(mockIUntypedQuery);
    when(mockIUntypedQuery.byUrl(anyString())).thenReturn(mockIquery);
    when(mockIquery.returnBundle(Bundle.class)).thenReturn(mockIquery2);
    when(mockIquery2.execute()).thenReturn(firstPage);
    IGetPage getPage = mock(IGetPage.class);
    IGetPageTyped getPageTyped = mock(IGetPageTyped.class);
    when(genericClient.loadPage()).thenReturn(getPage);
    when(getPage.next(any(Bundle.class))).thenReturn(getPageTyped);
    when(getPageTyped.execute()).thenReturn(secondPage);

    IBaseBundle result =
        FhirContextInitializer.getResourceBundleByUrl(
            launchDetails,
            genericClient,
            FhirContext.forDstu2(),
            "Condition",
            EHR_BASE + "/Condition");

    assertSame(firstPage, result);
    assertEquals(2, firstPage.getEntry().size());
    assertEquals(EHR_BASE + "/Condition/con-2", firstPage.getEntry().get(1).getFullUrl());
  }

  private FhirClient newFhirClient(EventTypes.QueryType queryType) {
    return new FhirClient(mock(IGenericClient.class), "req-0001", queryType);
  }

  private org.hl7.fhir.r4.model.Bundle r4BundleWithOneCondition() {
    org.hl7.fhir.r4.model.Bundle bundle = new org.hl7.fhir.r4.model.Bundle();
    org.hl7.fhir.r4.model.Condition condition = new org.hl7.fhir.r4.model.Condition();
    condition.setId("con-1");
    bundle.addEntry().setResource(condition);
    return bundle;
  }
}
