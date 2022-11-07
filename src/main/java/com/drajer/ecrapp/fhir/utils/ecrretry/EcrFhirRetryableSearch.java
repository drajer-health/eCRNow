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
import java.util.concurrent.atomic.AtomicInteger;
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
    return query.where(theCriterion);
  }

  @Override
  public IBaseQuery whereMap(Map theRawMap) {
    return query.whereMap(theRawMap);
  }

  @Override
  public IClientExecutable andLogRequestAndResponse(boolean theLogRequestAndResponse) {
    return query.andLogRequestAndResponse(theLogRequestAndResponse);
  }

  @Override
  public IClientExecutable cacheControl(CacheControlDirective theCacheControlDirective) {
    return query.cacheControl(theCacheControlDirective);
  }

  @Override
  public IClientExecutable elementsSubset(String... theElements) {
    return query.elementsSubset(theElements);
  }

  @Override
  public IClientExecutable encoded(EncodingEnum theEncoding) {
    return query.encoded(theEncoding);
  }

  @Override
  public IClientExecutable encodedJson() {
    return query.encodedJson();
  }

  @Override
  public IClientExecutable encodedXml() {
    return query.encodedXml();
  }

  @Override
  public IClientExecutable withAdditionalHeader(String theHeaderName, String theHeaderValue) {
    return query.withAdditionalHeader(theHeaderName, theHeaderValue);
  }

  @Override
  public Object execute() {
    AtomicInteger retryCount = new AtomicInteger();
    return client
        .getRetryTemplate()
        .execute(
            context -> {
              try {
                retryCount.getAndIncrement();
                logger.info("Retrying FHIR search url {}. Count {} ", url, retryCount);
                return query.execute();
              } catch (final Exception ex) {
                throw client.handleException(ex, HttpMethod.GET.name());
              }
            },
            null);
  }

  @Override
  public IClientExecutable preferResponseType(Class theType) {
    return query.preferResponseType(theType);
  }

  @Override
  public IClientExecutable preferResponseTypes(List theTypes) {
    return query.preferResponseTypes(theTypes);
  }

  @Override
  public IClientExecutable prettyPrint() {
    return query.prettyPrint();
  }

  @Override
  public IClientExecutable summaryMode(SummaryEnum theSummary) {
    return query.summaryMode(theSummary);
  }

  @Override
  public IClientExecutable accept(String theHeaderValue) {
    return query.accept(theHeaderValue);
  }

  @Override
  public IQuery<IQuery> forAllResources() {
    throw new NotImplementedOperationException("The request operation is not implemented");
  }

  @Override
  public IQuery<IQuery> forResource(String theResourceName) {
    throw new NotImplementedOperationException("The request operation is not implemented");
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
    throw new NotImplementedOperationException("The request operation is not implemented");
  }

  @Override
  public IQuery include(Include theInclude) {
    return query.revInclude(theInclude);
  }

  @Override
  public IQuery lastUpdated(DateRangeParam theLastUpdated) {
    return query.lastUpdated(theLastUpdated);
  }

  @Override
  public IQuery limitTo(int theLimitTo) {
    return query.limitTo(theLimitTo);
  }

  @Override
  public IQuery returnBundle(Class theClass) {
    return new EcrFhirRetryableSearch(query.returnBundle(theClass), client);
  }

  @Override
  public IQuery totalMode(SearchTotalModeEnum theTotalMode) {
    return query.totalMode(theTotalMode);
  }

  @Override
  public IQuery revInclude(Include theIncludeTarget) {
    return query.revInclude(theIncludeTarget);
  }

  @Override
  public ISort sort() {
    return query.sort();
  }

  @Override
  public IQuery sort(SortSpec theSortSpec) {
    return query.sort(theSortSpec);
  }

  @Override
  public IQuery usingStyle(SearchStyleEnum theStyle) {
    return query.usingStyle(theStyle);
  }

  @Override
  public IQuery where(ICriterion theCriterion) {
    query = query.where(theCriterion);
    return new EcrFhirRetryableSearch(query, client);
  }

  @Override
  public IQuery withAnyProfile(Collection theProfileUris) {
    return query.withAnyProfile(theProfileUris);
  }

  @Override
  public IQuery withIdAndCompartment(String theResourceId, String theCompartmentName) {
    return query.withIdAndCompartment(theResourceId, theCompartmentName);
  }

  @Override
  public IQuery withProfile(String theProfileUri) {
    return query.withProfile(theProfileUri);
  }

  @Override
  public IQuery withSecurity(String theSystem, String theCode) {
    return query.withSecurity(theSystem, theCode);
  }

  @Override
  public IQuery withTag(String theSystem, String theCode) {
    return query.withTag(theSystem, theCode);
  }
}
