package com.drajer.sof.utils;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.interceptor.api.IInterceptorService;
import ca.uhn.fhir.model.primitive.IdDt;
import ca.uhn.fhir.model.primitive.UriDt;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.MethodOutcome;
import ca.uhn.fhir.rest.api.RequestFormatParamStyleEnum;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.api.IHttpClient;
import ca.uhn.fhir.rest.gclient.*;
import com.drajer.eca.model.EventTypes;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class FhirClientTest {

  @Mock private IGenericClient mockClient;

  @Mock private FhirHttpHeaderInterceptor mockInterceptor;

  @Mock private IInterceptorService mockInterceptorService;

  @Mock private IHttpClient mockHttpClient;

  @Mock private IBaseResource mockResource;

  @Mock private IBaseBundle mockBundle;

  @Mock private MethodOutcome mockMethodOutcome;

  private FhirClient fhirClient;
  private final String testRequestId = "test-request-id";
  private final EventTypes.QueryType testQueryType = EventTypes.QueryType.NONE;

  @Before
  public void setUp() {

    FhirHttpHeaderInterceptor realInterceptor = new FhirHttpHeaderInterceptor(testRequestId);

    fhirClient = new FhirClient(mockClient, testRequestId, testQueryType);

    fhirClient.interceptor = mockInterceptor;
  }

  @Test
  public void testConstructor() {
    FhirClient client = new FhirClient(mockClient, testRequestId, testQueryType);

    assertNotNull(client.getHttpInterceptor());
    assertSame(mockClient, client.client);
    assertEquals(testQueryType, client.queryType);
  }

  @Test
  public void testGetHttpInterceptor() {
    FhirHttpHeaderInterceptor interceptor = fhirClient.getHttpInterceptor();
    assertSame(mockInterceptor, interceptor);
    verify(mockClient, never()).unregisterInterceptor(any());
  }

  @Test
  public void testCapabilities() {
    IFetchConformanceUntyped mockCapabilities = mock(IFetchConformanceUntyped.class);
    when(mockClient.capabilities()).thenReturn(mockCapabilities);

    IFetchConformanceUntyped result = fhirClient.capabilities();
    verify(mockInterceptor).reset();
    assertSame(mockCapabilities, result);
  }

  @Test
  public void testCreate() {
    ICreate mockCreate = mock(ICreate.class);
    when(mockClient.create()).thenReturn(mockCreate);

    ICreate result = fhirClient.create();
    verify(mockInterceptor).reset();
    assertSame(mockCreate, result);
    verify(mockClient, times(1)).create();
  }

  @Test
  public void testDelete() {
    IDelete mockDelete = mock(IDelete.class);
    when(mockClient.delete()).thenReturn(mockDelete);

    IDelete result = fhirClient.delete();
    verify(mockInterceptor).reset();
    assertSame(mockDelete, result);
  }

  @Test
  public void testFetchConformance() {
    IFetchConformanceUntyped mockFetch = mock(IFetchConformanceUntyped.class);
    when(mockClient.fetchConformance()).thenReturn(mockFetch);

    IFetchConformanceUntyped result = fhirClient.fetchConformance();
    verify(mockInterceptor).reset();
    assertSame(mockFetch, result);
  }

  @Test
  public void testForceConformanceCheck() {
    fhirClient.forceConformanceCheck();
    verify(mockInterceptor).reset();
    verify(mockClient).forceConformanceCheck();
  }

  @Test
  public void testHistory() {
    IHistory mockHistory = mock(IHistory.class);
    when(mockClient.history()).thenReturn(mockHistory);

    IHistory result = fhirClient.history();
    verify(mockInterceptor).reset();
    assertSame(mockHistory, result);
  }

  @Test
  public void testLoadPage() {
    IGetPage mockGetPage = mock(IGetPage.class);
    when(mockClient.loadPage()).thenReturn(mockGetPage);

    IGetPage result = fhirClient.loadPage();
    verify(mockInterceptor).incrementPageNum();
    assertSame(mockGetPage, result);
    verify(mockInterceptor, never()).reset();
  }

  @Test
  public void testMeta() {
    IMeta mockMeta = mock(IMeta.class);
    when(mockClient.meta()).thenReturn(mockMeta);

    IMeta result = fhirClient.meta();
    verify(mockInterceptor).reset();
    assertSame(mockMeta, result);
  }

  @Test
  public void testOperation() {
    IOperation mockOperation = mock(IOperation.class);
    when(mockClient.operation()).thenReturn(mockOperation);

    IOperation result = fhirClient.operation();
    verify(mockInterceptor).reset();
    assertSame(mockOperation, result);
  }

  @Test
  public void testPatch() {
    IPatch mockPatch = mock(IPatch.class);
    when(mockClient.patch()).thenReturn(mockPatch);

    IPatch result = fhirClient.patch();
    verify(mockInterceptor).reset();
    assertSame(mockPatch, result);
  }

  @Test
  public void testRead() {
    IRead mockRead = mock(IRead.class);
    when(mockClient.read()).thenReturn(mockRead);

    IRead result = fhirClient.read();
    verify(mockInterceptor).reset();
    assertSame(mockRead, result);
  }

  @Test
  public void testReadWithClassAndString() {
    String testId = "test-id";
    when(mockClient.read(any(Class.class), eq(testId))).thenReturn(mockResource);

    IBaseResource result = fhirClient.read(IBaseResource.class, testId);
    verify(mockInterceptor).reset();
    assertSame(mockResource, result);
    verify(mockClient).read(eq(IBaseResource.class), eq(testId));
  }

  @Test
  public void testReadWithClassAndUriDt() {
    IBaseResource mockResource = mock(IBaseResource.class);
    UriDt uri = new UriDt("http://test.com");
    when(mockClient.read(any(Class.class), any(UriDt.class))).thenReturn(mockResource);

    IBaseResource result = fhirClient.read(IBaseResource.class, uri);
    verify(mockInterceptor).reset();
    assertSame(mockResource, result);
  }

  @Test
  public void testReadWithUriDt() {
    UriDt uri = new UriDt("http://test.com");
    when(mockClient.read(any(UriDt.class))).thenReturn(mockResource);

    IBaseResource result = fhirClient.read(uri);
    verify(mockInterceptor).reset();
    assertSame(mockResource, result);
  }

  @Test
  public void testGetInterceptorService() {
    when(mockClient.getInterceptorService()).thenReturn(mockInterceptorService);

    IInterceptorService result = fhirClient.getInterceptorService();
    assertSame(mockInterceptorService, result);
  }

  @Test
  public void testSetInterceptorService() {
    fhirClient.setInterceptorService(mockInterceptorService);
    verify(mockClient).setInterceptorService(mockInterceptorService);
  }

  @Test
  public void testFetchResourceFromUrl() {
    String testUrl = "http://test.com/resource";
    when(mockClient.fetchResourceFromUrl(any(Class.class), eq(testUrl))).thenReturn(mockResource);

    IBaseResource result = fhirClient.fetchResourceFromUrl(IBaseResource.class, testUrl);
    verify(mockInterceptor).reset();
    assertSame(mockResource, result);
    verify(mockClient).fetchResourceFromUrl(eq(IBaseResource.class), eq(testUrl));
  }

  @Test
  public void testGetEncoding() {
    EncodingEnum expected = EncodingEnum.JSON;
    when(mockClient.getEncoding()).thenReturn(expected);

    EncodingEnum result = fhirClient.getEncoding();
    assertEquals(expected, result);
  }

  @Test
  public void testSetEncoding() {
    EncodingEnum encoding = EncodingEnum.XML;
    fhirClient.setEncoding(encoding);
    verify(mockClient).setEncoding(encoding);
  }

  @Test
  public void testGetFhirContext() {
    FhirContext mockContext = mock(FhirContext.class);
    when(mockClient.getFhirContext()).thenReturn(mockContext);

    FhirContext result = fhirClient.getFhirContext();
    assertSame(mockContext, result);
  }

  @Test
  public void testGetHttpClient() {
    when(mockClient.getHttpClient()).thenReturn(mockHttpClient);

    IHttpClient result = fhirClient.getHttpClient();
    assertSame(mockHttpClient, result);
  }

  @Test
  public void testGetServerBase() {
    String expected = "http://test.com";
    when(mockClient.getServerBase()).thenReturn(expected);

    String result = fhirClient.getServerBase();
    assertEquals(expected, result);
  }

  @Test
  public void testRegisterInterceptor() {
    Object interceptor = new Object();
    fhirClient.registerInterceptor(interceptor);
    verify(mockClient).registerInterceptor(interceptor);
  }

  @Test
  public void testSetPrettyPrint() {
    fhirClient.setPrettyPrint(true);
    verify(mockClient).setPrettyPrint(true);
  }

  @Test
  public void testSetSummary() {
    SummaryEnum summary = SummaryEnum.TRUE;
    fhirClient.setSummary(summary);
    verify(mockClient).setSummary(summary);
  }

  @Test
  public void testSearch() {
    IUntypedQuery mockQuery = mock(IUntypedQuery.class);
    when(mockClient.search()).thenReturn(mockQuery);

    IUntypedQuery result = fhirClient.search();
    verify(mockInterceptor).reset();
    assertSame(mockQuery, result);
  }

  @Test
  public void testSetLogRequestAndResponse() {
    fhirClient.setLogRequestAndResponse(true);
    verify(mockClient).setLogRequestAndResponse(true);
  }

  @Test
  public void testTransaction() {
    ITransaction mockTransaction = mock(ITransaction.class);
    when(mockClient.transaction()).thenReturn(mockTransaction);

    ITransaction result = fhirClient.transaction();
    verify(mockInterceptor).reset();
    assertSame(mockTransaction, result);
  }

  @Test
  public void testUnregisterInterceptor() {
    Object interceptor = new Object();
    fhirClient.unregisterInterceptor(interceptor);
    verify(mockClient).unregisterInterceptor(interceptor);
  }

  @Test
  public void testSetFormatParamStyle() {
    RequestFormatParamStyleEnum style = RequestFormatParamStyleEnum.NONE;
    fhirClient.setFormatParamStyle(style);
    verify(mockClient).setFormatParamStyle(style);
  }

  @Test
  public void testUpdate() {
    IUpdate mockUpdate = mock(IUpdate.class);
    when(mockClient.update()).thenReturn(mockUpdate);

    IUpdate result = fhirClient.update();
    verify(mockInterceptor).reset();
    assertSame(mockUpdate, result);
  }

  @Test
  public void testUpdateWithIdDtAndResource() {
    IdDt id = new IdDt("Patient/123");
    when(mockClient.update(any(IdDt.class), any(IBaseResource.class)))
        .thenReturn(mockMethodOutcome);

    MethodOutcome result = fhirClient.update(id, mockResource);
    verify(mockInterceptor).reset();
    assertSame(mockMethodOutcome, result);
  }

  @Test
  public void testUpdateWithStringAndResource() {
    String id = "Patient/123";
    when(mockClient.update(anyString(), any(IBaseResource.class))).thenReturn(mockMethodOutcome);

    MethodOutcome result = fhirClient.update(id, mockResource);
    verify(mockInterceptor).reset();
    assertSame(mockMethodOutcome, result);
  }

  @Test
  public void testValidate() {
    IValidate mockValidate = mock(IValidate.class);
    when(mockClient.validate()).thenReturn(mockValidate);

    IValidate result = fhirClient.validate();
    verify(mockInterceptor).reset();
    assertSame(mockValidate, result);
  }

  @Test
  public void testValidateWithResource() {
    when(mockClient.validate(any(IBaseResource.class))).thenReturn(mockMethodOutcome);

    MethodOutcome result = fhirClient.validate(mockResource);
    verify(mockInterceptor).reset();
    assertSame(mockMethodOutcome, result);
  }

  @Test
  public void testVreadWithClassAndIdDt() {
    IdDt id = new IdDt("Patient/123/_history/1");
    when(mockClient.vread(any(Class.class), any(IdDt.class))).thenReturn(mockResource);

    IBaseResource result = fhirClient.vread(IBaseResource.class, id);
    verify(mockInterceptor).reset();
    assertSame(mockResource, result);
  }

  @Test
  public void testVreadWithClassAndStrings() {
    String id = "123";
    String version = "1";
    when(mockClient.vread(any(Class.class), anyString(), anyString())).thenReturn(mockResource);

    IBaseResource result = fhirClient.vread(IBaseResource.class, id, version);
    verify(mockInterceptor).reset();
    assertSame(mockResource, result);
  }

  @Test
  public void testQueryType() {
    assertEquals(testQueryType, fhirClient.queryType);
  }
}
