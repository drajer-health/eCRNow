package com.drajer.ecrapp.fhir.utils.ecrretry;

import static com.helger.commons.mock.CommonsAssert.assertEquals;
import static com.jayway.jsonpath.internal.path.PathCompiler.fail;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.api.CacheControlDirective;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.PagingHttpMethodEnum;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.gclient.*;
import ca.uhn.fhir.rest.server.exceptions.NotImplementedOperationException;
import com.drajer.eca.model.EventTypes;
import com.drajer.ecrapp.config.SpringConfiguration;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplate;
import com.drajer.ecrapp.fhir.utils.FHIRRetryTemplateConfig;
import com.drajer.ecrapp.fhir.utils.RetryableException;
import com.drajer.sof.model.ClientDetails;
import com.drajer.sof.model.LaunchDetails;
import com.drajer.sof.utils.FhirContextInitializer;
import com.drajer.test.util.TestUtils;
import java.util.*;
import org.hl7.fhir.instance.model.api.IBaseBundle;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class EcrFhirRetryablePageTest {

  private LaunchDetails currentStateDetails;
  private ClientDetails clientDetails;

  @InjectMocks FHIRRetryTemplate fhirretryTemplate;

  @InjectMocks SpringConfiguration springConfiguration;
  @InjectMocks FHIRRetryTemplateConfig fhirRetryTemplateConfig;
  @Mock FhirContextInitializer fhirContextInitializer;
  @InjectMocks FHIRRetryTemplateConfig.HttpMethodType httpMethodType;

  @Before
  public void init() {
    MockitoAnnotations.initMocks(this);
    currentStateDetails =
        (LaunchDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/LaunchDetails/LaunchDetails.json", LaunchDetails.class);
    currentStateDetails.setLastUpdated(new Date());
    clientDetails =
        (ClientDetails)
            TestUtils.getResourceAsObject(
                "R4/Misc/ClientDetails/ClientDetail_IT_FullECR.json", ClientDetails.class);
  }

  @Test
  public void testRetryPage() {
    FhirContext context = mock(FhirContext.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IGetPage loadPage = mock(EcrFhirRetryablePage.class);
    IGetPageTyped getPageTyped = mock(EcrFhirRetryablePage.class);
    IBaseBundle bundle = mock(IBaseBundle.class);
    Map<String, FHIRRetryTemplateConfig.HttpMethodType> map = new HashMap<>();
    currentStateDetails.setFhirVersion("R4");

    httpMethodType.setMaxRetries(3);
    httpMethodType.setRetryWaitTimeInMillis(3000);
    httpMethodType.setRetryStatusCodes(
        new ArrayList<>(Arrays.asList(408, 429, 502, 503, 504, 500)));

    map.put("GET", httpMethodType);
    fhirRetryTemplateConfig.setHttpMethodTypeMap(map);
    fhirRetryTemplateConfig.setMaxRetries(3);
    fhirRetryTemplateConfig.setRetryWaitTimeInMillis(3000);

    RetryStatusCode retryStatusCode = new RetryStatusCode(fhirRetryTemplateConfig);
    fhirretryTemplate = new FHIRRetryTemplate(retryStatusCode.configureRetryTemplate());
    when(retryClient.getRetryTemplate()).thenReturn(fhirretryTemplate);

    when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion()))
        .thenReturn(context);

    when(fhirContextInitializer.createClient(
            context, currentStateDetails, EventTypes.QueryType.NONE))
        .thenReturn(retryClient);

    when(retryClient.loadPage()).thenReturn(loadPage);
    when(loadPage.next(bundle)).thenReturn(getPageTyped);
    when(getPageTyped.execute()).thenReturn(bundle);
    when(getPageTyped.execute())
        .thenThrow(new RetryableException("INTERNAL_SERVER_ERROR", 500, "GET"));
    try {
      retryClient
          .getRetryTemplate()
          .execute(
              retryContext -> {
                return getPageTyped.execute();
              },
              null);
    } catch (Exception e) {
      verify(getPageTyped, times(3)).execute();
    }
  }

  @Test
  public void testNextMethod() {
    IGetPage mockPage = mock(IGetPage.class);
    IGetPageTyped<IBaseBundle> mockPageTyped = mock(IGetPageTyped.class);
    EcrFhirRetryClient mockClient = mock(EcrFhirRetryClient.class);
    IBaseBundle mockBundle = mock(IBaseBundle.class);
    when(mockPage.next(mockBundle)).thenReturn(mockPageTyped);
    EcrFhirRetryablePage retryablePage =
        new EcrFhirRetryablePage(mockPage, mockPageTyped, mockClient);

    IGetPageTyped<IBaseBundle> nextPage = retryablePage.next(mockBundle);
    assertNotNull(nextPage);
    verify(mockPage, times(1)).next(mockBundle);
  }

  @Test
  public void testFirstConstructor() {
    IGetPage mockPage = mock(IGetPage.class);
    EcrFhirRetryClient mockClient = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(mockPage, mockClient);
    assertNotNull(retryablePage);
  }

  @Test
  public void testAndLogRequestAndResponseThrows() {
    IGetPage mockPage = mock(IGetPage.class);
    EcrFhirRetryClient mockClient = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(mockPage, mockClient);
    try {
      retryablePage.andLogRequestAndResponse(true);
      fail("Expected NotImplementedOperationException to be thrown");
    } catch (NotImplementedOperationException ex) {
      assertEquals("The requested operation is not implemented", ex.getMessage());
    }
  }

  @Test
  public void testCacheControlThrows() {
    IGetPage mockPage = mock(IGetPage.class);
    EcrFhirRetryClient mockClient = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(mockPage, mockClient);
    try {
      retryablePage.cacheControl(new CacheControlDirective());
      fail("Expected NotImplementedOperationException to be thrown");
    } catch (NotImplementedOperationException ex) {
      assertEquals("The requested operation is not implemented", ex.getMessage());
    }
  }

  @Test
  public void testElementsSubset_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.elementsSubset("id", "status");
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException ex) {
      assertEquals("The requested operation is not implemented", ex.getMessage());
    }
  }

  @Test
  public void testEncoded_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.encoded(EncodingEnum.JSON);
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException ex) {
      assertEquals("The requested operation is not implemented", ex.getMessage());
    }
  }

  @Test
  public void testEncodedJson_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.encodedJson();
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException ex) {
      assertEquals("The requested operation is not implemented", ex.getMessage());
    }
  }

  @Test
  public void testEncodedXml_notImplemented() {

    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.encodedXml();
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testWithAdditionalHeader_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.withAdditionalHeader("X-Test-Header", "value");
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testPreferResponseType_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.preferResponseType(IBaseResource.class);
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testPreferResponseTypes_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    List<Class<? extends IBaseResource>> types = new ArrayList<>();
    try {
      retryablePage.preferResponseTypes(types);
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testPrettyPrint_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.prettyPrint();
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testSummaryMode_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.summaryMode(SummaryEnum.TRUE);
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testPrevious_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IBaseBundle bundle = mock(IBaseBundle.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.previous(bundle);
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testByUrl_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.byUrl("http://example.com/page/1");
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }

  @Test
  public void testUsingMethod_notImplemented() {
    IGetPage page = mock(IGetPage.class);
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryablePage retryablePage = new EcrFhirRetryablePage(page, client);
    try {
      retryablePage.usingMethod(PagingHttpMethodEnum.GET);
      fail("Expected NotImplementedOperationException");
    } catch (NotImplementedOperationException e) {
      assertEquals("The requested operation is not implemented", e.getMessage());
    }
  }
}
