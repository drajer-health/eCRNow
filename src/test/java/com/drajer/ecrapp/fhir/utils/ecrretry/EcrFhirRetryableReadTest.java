package com.drajer.ecrapp.fhir.utils.ecrretry;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.api.CacheControlDirective;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadTyped;
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
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.instance.model.api.IIdType;
import org.hl7.fhir.r4.model.Encounter;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class EcrFhirRetryableReadTest {

  private LaunchDetails currentStateDetails;
  private ClientDetails clientDetails;

  @InjectMocks FHIRRetryTemplate fhirretryTemplate;

  @InjectMocks FHIRRetryTemplateConfig fhirRetryTemplateConfig;
  @InjectMocks FHIRRetryTemplateConfig.HttpMethodType httpMethodType;
  @InjectMocks SpringConfiguration springConfiguration;
  @Mock FhirContextInitializer fhirContextInitializer;

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
  public void testRetryRead() {
    FhirContext context = mock(FhirContext.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IRead read = mock(IRead.class);
    IReadTyped<IBaseResource> readType = mock(IReadTyped.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);
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
    fhirretryTemplate =
        new FHIRRetryTemplate(
            retryStatusCode.configureRetryTemplate(), fhirRetryTemplateConfig, true);
    when(retryClient.getRetryTemplate()).thenReturn(fhirretryTemplate);

    when(fhirContextInitializer.getFhirContext(currentStateDetails.getFhirVersion()))
        .thenReturn(context);

    when(fhirContextInitializer.createClient(
            context, currentStateDetails, EventTypes.QueryType.NONE))
        .thenReturn(retryClient);

    when(retryClient.read()).thenReturn(read);
    when(read.resource("Encounter")).thenReturn(readType);
    when(readType.withId(currentStateDetails.getEncounterId())).thenReturn(readExecutable);
    when(readExecutable.execute())
        .thenThrow(new RetryableException("INTERNAL_SERVER_ERROR", 500, "GET"));
    try {
      retryClient
          .getRetryTemplate()
          .execute(
              retryContext -> {
                return readExecutable.execute();
              },
              null);
    } catch (Exception e) {
      verify(readExecutable, times(3)).execute();
    }
  }

  @Test
  public void testResourceWithString() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IRead read = mock(IRead.class);
    IReadTyped<IBaseResource> readTyped = mock(IReadTyped.class);
    when(read.resource("Patient")).thenReturn(readTyped);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(read, client);
    IReadTyped<IBaseResource> result = retryRead.resource("Patient");
    assertNotNull(result);
    assertTrue(result instanceof EcrFhirRetryableRead);
    verify(read, times(1)).resource("Patient");
  }

  @Test
  public void testResourceWithClass_ThrowsException() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IRead read = mock(IRead.class);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(read, client);
    assertThrows(NotImplementedOperationException.class, () -> retryRead.resource(Encounter.class));
  }

  @Test
  public void testWithIdString() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IReadTyped<IBaseResource> readTyped = mock(IReadTyped.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);
    when(readTyped.withId("encounter-123")).thenReturn(readExecutable);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(readTyped, client);
    IReadExecutable result = retryRead.withId("encounter-123");
    assertNotNull(result);
    assertTrue(result instanceof EcrFhirRetryableRead);
    verify(readTyped, times(1)).withId("encounter-123");
  }

  @Test
  public void testWithIdAndVersion_ThrowsException() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IReadTyped<IBaseResource> readTyped = mock(IReadTyped.class);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(readTyped, client);
    assertThrows(
        NotImplementedOperationException.class, () -> retryRead.withIdAndVersion("123", "1"));
  }

  @Test
  public void testWithIdLong_ThrowsException() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IReadTyped<IBaseResource> readTyped = mock(IReadTyped.class);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(readTyped, client);
    assertThrows(NotImplementedOperationException.class, () -> retryRead.withId(123L));
  }

  @Test
  public void testWithIdIIdType_ThrowsException() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IReadTyped<IBaseResource> readTyped = mock(IReadTyped.class);
    IIdType idType = mock(IIdType.class);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(readTyped, client);
    assertThrows(NotImplementedOperationException.class, () -> retryRead.withId(idType));
  }

  @Test
  public void testWithUrlIIdType_ThrowsException() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IReadTyped<IBaseResource> readTyped = mock(IReadTyped.class);
    IIdType urlType = mock(IIdType.class);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(readTyped, client);
    assertThrows(NotImplementedOperationException.class, () -> retryRead.withUrl(urlType));
  }

  @Test
  public void testNotImplementedMethods() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(readExecutable, client);
    assertThrows(
        NotImplementedOperationException.class, () -> retryRead.andLogRequestAndResponse(true));
    assertThrows(
        NotImplementedOperationException.class,
        () -> retryRead.cacheControl(mock(CacheControlDirective.class)));
    assertThrows(NotImplementedOperationException.class, () -> retryRead.elementsSubset("elem"));
    assertThrows(
        NotImplementedOperationException.class, () -> retryRead.encoded(EncodingEnum.JSON));
    assertThrows(NotImplementedOperationException.class, () -> retryRead.encodedJson());
    assertThrows(NotImplementedOperationException.class, () -> retryRead.encodedXml());
    assertThrows(
        NotImplementedOperationException.class, () -> retryRead.withAdditionalHeader("h", "v"));
  }

  @Test
  public void testResponseFormatMethods_ThrowException() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IReadExecutable readExecutable = mock(IReadExecutable.class);
    EcrFhirRetryableRead retryRead = new EcrFhirRetryableRead(readExecutable, client);
    assertThrows(
        NotImplementedOperationException.class,
        () -> retryRead.preferResponseType(Encounter.class));
    assertThrows(
        NotImplementedOperationException.class,
        () -> retryRead.preferResponseTypes(new ArrayList<>()));
    assertThrows(NotImplementedOperationException.class, () -> retryRead.prettyPrint());
    assertThrows(
        NotImplementedOperationException.class, () -> retryRead.summaryMode(SummaryEnum.TRUE));
    assertThrows(
        NotImplementedOperationException.class, () -> retryRead.accept("application/json"));
    assertThrows(NotImplementedOperationException.class, () -> retryRead.ifVersionMatches("1.0"));
  }
}
