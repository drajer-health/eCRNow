package com.drajer.ecrapp.fhir.utils.ecrretry;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.model.api.Include;
import ca.uhn.fhir.rest.api.*;
import ca.uhn.fhir.rest.gclient.*;
import ca.uhn.fhir.rest.param.DateRangeParam;
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
import org.hl7.fhir.r4.model.Bundle;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class EcrFhirRetryableSearchTest {

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
  public void testRetrySearch() {
    FhirContext context = mock(FhirContext.class);
    EcrFhirRetryClient retryClient = mock(EcrFhirRetryClient.class);
    IQuery iQuery = mock(EcrFhirRetryableSearch.class);
    IUntypedQuery iUntypedQuery = mock(EcrFhirRetryableSearch.class);
    String url = "http://localhost:9011/FHIR/Condition?patient=12742571";
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

    when(retryClient.search()).thenReturn(iUntypedQuery);
    when(iUntypedQuery.byUrl(url)).thenReturn(iQuery);
    when(iQuery.returnBundle(Bundle.class)).thenReturn(iQuery);
    when(iQuery.execute()).thenThrow(new RetryableException("INTERNAL_SERVER_ERROR", 500, "GET"));

    try {
      retryClient
          .getRetryTemplate()
          .execute(
              retryContext -> {
                return iQuery.execute();
              },
              null);
    } catch (Exception e) {
      verify(iQuery, times(3)).execute();
    }
  }

  @Test
  public void testConstructorsInitialize() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    IQuery query = mock(IQuery.class);
    IUntypedQuery untypedQuery = mock(IUntypedQuery.class);

    EcrFhirRetryableSearch searchWithQuery = new EcrFhirRetryableSearch(query, client);
    EcrFhirRetryableSearch searchWithUntypedQuery =
        new EcrFhirRetryableSearch(untypedQuery, client);

    assertNotNull(searchWithQuery);
    assertNotNull(searchWithUntypedQuery);
    assertEquals("EcrFhirRetryableSearch", searchWithQuery.getClass().getSimpleName());
    assertEquals("EcrFhirRetryableSearch", searchWithUntypedQuery.getClass().getSimpleName());
  }

  @Test
  public void testUntypedQueryNotImplementedMethods() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryableSearch untypedSearch =
        new EcrFhirRetryableSearch(mock(IUntypedQuery.class), client);

    assertThrows(NotImplementedOperationException.class, () -> untypedSearch.where(new HashMap()));
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.whereMap(new HashMap()));
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.encoded(EncodingEnum.JSON));
    assertThrows(NotImplementedOperationException.class, () -> untypedSearch.encodedJson());
    assertThrows(NotImplementedOperationException.class, () -> untypedSearch.encodedXml());
    assertThrows(
        NotImplementedOperationException.class,
        () -> untypedSearch.preferResponseType(Bundle.class));
    assertThrows(
        NotImplementedOperationException.class,
        () -> untypedSearch.preferResponseTypes(new ArrayList()));
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.accept("application/json"));
    assertThrows(NotImplementedOperationException.class, () -> untypedSearch.prettyPrint());
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.summaryMode(SummaryEnum.TRUE));
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.elementsSubset("elem"));
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.andLogRequestAndResponse(true));
    assertThrows(
        NotImplementedOperationException.class,
        () -> untypedSearch.cacheControl(mock(CacheControlDirective.class)));
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.withAdditionalHeader("h", "v"));
    assertThrows(NotImplementedOperationException.class, () -> untypedSearch.forAllResources());
    assertThrows(
        NotImplementedOperationException.class, () -> untypedSearch.forResource("Patient"));
  }

  @Test
  public void testQueryPaginationAndSortNotImplementedMethods() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryableSearch search = new EcrFhirRetryableSearch(mock(IQuery.class), client);

    assertThrows(NotImplementedOperationException.class, () -> search.offset(10));
    assertThrows(NotImplementedOperationException.class, () -> search.limitTo(50));
    assertThrows(
        NotImplementedOperationException.class,
        () -> search.lastUpdated(mock(DateRangeParam.class)));
    assertThrows(NotImplementedOperationException.class, () -> search.include(mock(Include.class)));
    assertThrows(
        NotImplementedOperationException.class, () -> search.revInclude(mock(Include.class)));
    assertThrows(NotImplementedOperationException.class, () -> search.sort());
    assertThrows(NotImplementedOperationException.class, () -> search.sort(mock(SortSpec.class)));
    assertThrows(
        NotImplementedOperationException.class, () -> search.usingStyle(SearchStyleEnum.GET));
    assertThrows(
        NotImplementedOperationException.class,
        () -> search.totalMode(SearchTotalModeEnum.ACCURATE));
  }

  @Test
  public void testQueryMetadataNotImplementedMethods() {
    EcrFhirRetryClient client = mock(EcrFhirRetryClient.class);
    EcrFhirRetryableSearch search = new EcrFhirRetryableSearch(mock(IQuery.class), client);

    assertThrows(NotImplementedOperationException.class, () -> search.withProfile("uri"));
    assertThrows(
        NotImplementedOperationException.class, () -> search.withAnyProfile(new ArrayList()));
    assertThrows(NotImplementedOperationException.class, () -> search.withTag("s", "c"));
    assertThrows(NotImplementedOperationException.class, () -> search.withSecurity("s", "c"));
    assertThrows(
        NotImplementedOperationException.class, () -> search.withIdAndCompartment("id", "c"));
  }
}
