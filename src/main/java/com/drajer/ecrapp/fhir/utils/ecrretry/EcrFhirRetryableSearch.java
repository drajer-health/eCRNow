package com.drajer.ecrapp.fhir.utils.ecrretry;

import ca.uhn.fhir.model.api.Include;
import ca.uhn.fhir.rest.api.CacheControlDirective;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.SearchStyleEnum;
import ca.uhn.fhir.rest.api.SearchTotalModeEnum;
import ca.uhn.fhir.rest.api.SortSpec;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.gclient.IBaseQuery;
import ca.uhn.fhir.rest.gclient.IClientExecutable;
import ca.uhn.fhir.rest.gclient.ICriterion;
import ca.uhn.fhir.rest.gclient.IQuery;
import ca.uhn.fhir.rest.gclient.ISort;
import ca.uhn.fhir.rest.gclient.IUntypedQuery;
import ca.uhn.fhir.rest.param.DateRangeParam;
import ca.uhn.fhir.rest.server.exceptions.NotImplementedOperationException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.commons.text.StringEscapeUtils;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;

public class EcrFhirRetryableSearch<K> implements IQuery, IUntypedQuery<IQuery> {
  private IQuery<?> query;
  private final EcrFhirRetryClient client;
  private IUntypedQuery<IQuery> untypedQuery;
  private static final Logger logger = LoggerFactory.getLogger(EcrFhirRetryableSearch.class);
  private static String url;

  public EcrFhirRetryableSearch(final IUntypedQuery untypedQuery, final EcrFhirRetryClient client) {
    this.untypedQuery = untypedQuery;
    this.client = client;
  }

  public EcrFhirRetryableSearch(final IQuery query, final EcrFhirRetryClient client) {
    this.query = query;
    this.client = client;
  }

  @Override
  public IBaseQuery where(Map theCriterion) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IBaseQuery whereMap(Map theRawMap) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable andLogRequestAndResponse(boolean theLogRequestAndResponse) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable cacheControl(CacheControlDirective theCacheControlDirective) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable elementsSubset(String... theElements) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable encoded(EncodingEnum theEncoding) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable encodedJson() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable encodedXml() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable withAdditionalHeader(String theHeaderName, String theHeaderValue) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public Object execute() {
    return client
        .getRetryTemplate()
        .execute(
            context -> {
              try {
                logger.info(
                    "Retrying FHIR search url {}. Count {} ",
                    StringEscapeUtils.escapeJava(url),
                    context.getRetryCount());
                return query.execute();
              } catch (final Exception ex) {
                throw client.handleException(ex, HttpMethod.GET.name());
              }
            },
            null);
  }

  @Override
  public IClientExecutable preferResponseType(Class theType) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable preferResponseTypes(List theTypes) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable prettyPrint() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable summaryMode(SummaryEnum theSummary) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IClientExecutable accept(String theHeaderValue) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery<IQuery> forAllResources() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery<IQuery> forResource(String theResourceName) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery<IQuery> forResource(Class<? extends IBaseResource> theClass) {
    query = ((IUntypedQuery) query).forResource(theClass);
    return new EcrFhirRetryableSearch(query, client);
  }

  @Override
  public IQuery<IQuery> byUrl(String theSearchUrl) {
    this.url = theSearchUrl;
    return new EcrFhirRetryableSearch<>(untypedQuery.byUrl(theSearchUrl), client);
  }

  @Override
  public IQuery and(ICriterion theCriterion) {
    query = query.and(theCriterion);
    return new EcrFhirRetryableSearch(query, client);
  }

  @Override
  public IQuery count(int theCount) {
    query = query.count(theCount);
    return new EcrFhirRetryableSearch(query, client);
  }

  @Override
  public IQuery offset(int i) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery include(Include theInclude) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery lastUpdated(DateRangeParam theLastUpdated) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery limitTo(int theLimitTo) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery returnBundle(Class theClass) {
    return new EcrFhirRetryableSearch(query.returnBundle(theClass), client);
  }

  @Override
  public IQuery totalMode(SearchTotalModeEnum theTotalMode) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery revInclude(Include theIncludeTarget) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public ISort sort() {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery sort(SortSpec theSortSpec) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery usingStyle(SearchStyleEnum theStyle) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery where(ICriterion theCriterion) {
    query = query.where(theCriterion);
    return new EcrFhirRetryableSearch(query, client);
  }

  @Override
  public IQuery withAnyProfile(Collection theProfileUris) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery withIdAndCompartment(String theResourceId, String theCompartmentName) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery withProfile(String theProfileUri) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery withSecurity(String theSystem, String theCode) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IQuery withTag(String theSystem, String theCode) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }
}
