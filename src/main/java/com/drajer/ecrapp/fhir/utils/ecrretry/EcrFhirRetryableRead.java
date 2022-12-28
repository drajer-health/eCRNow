package com.drajer.ecrapp.fhir.utils.ecrretry;

import ca.uhn.fhir.rest.api.CacheControlDirective;
import ca.uhn.fhir.rest.api.EncodingEnum;
import ca.uhn.fhir.rest.api.SummaryEnum;
import ca.uhn.fhir.rest.gclient.IClientExecutable;
import ca.uhn.fhir.rest.gclient.IRead;
import ca.uhn.fhir.rest.gclient.IReadExecutable;
import ca.uhn.fhir.rest.gclient.IReadIfNoneMatch;
import ca.uhn.fhir.rest.gclient.IReadTyped;
import ca.uhn.fhir.rest.server.exceptions.NotImplementedOperationException;
import java.util.List;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.instance.model.api.IIdType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;

public class EcrFhirRetryableRead implements IRead, IReadTyped, IReadExecutable {

  private IRead readParent;
  private IReadTyped readTypedParent;
  private IReadExecutable readExecutableParent;
  private EcrFhirRetryClient client;

  private static final Logger logger = LoggerFactory.getLogger(EcrFhirRetryableRead.class);

  public EcrFhirRetryableRead(IRead read, EcrFhirRetryClient client) {
    super();
    this.readParent = read;
    this.client = client;
  }

  public EcrFhirRetryableRead(IReadTyped readTyped, EcrFhirRetryClient client) {
    super();
    this.readTypedParent = readTyped;
    this.client = client;
  }

  public EcrFhirRetryableRead(IReadExecutable readExecutable, EcrFhirRetryClient client) {
    super();
    this.readExecutableParent = readExecutable;
    this.client = client;
  }

  @Override
  public <T extends IBaseResource> IReadTyped<T> resource(Class<T> theResourceType) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IReadTyped<IBaseResource> resource(String theResourceType) {
    return new EcrFhirRetryableRead(readParent.resource(theResourceType), this.client);
  }

  @Override
  public IReadExecutable withId(String theId) {
    return new EcrFhirRetryableRead(readTypedParent.withId(theId), this.client);
  }

  @Override
  public IReadExecutable withIdAndVersion(String theId, String theVersion) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IReadExecutable withId(Long theId) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IReadExecutable withId(IIdType theId) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IReadExecutable withUrl(String theUrl) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }

  @Override
  public IReadExecutable withUrl(IIdType theUrl) {
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
            retryContext -> {
              try {
                logger.info("Retrying FHIR read. Count: {} ", retryContext.getRetryCount());
                return readExecutableParent.execute();
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
  public IReadIfNoneMatch ifVersionMatches(String theVersion) {
    throw new NotImplementedOperationException("The requested operation is not implemented");
  }
}
